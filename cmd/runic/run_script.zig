const std = @import("std");
const Allocator = std.mem.Allocator;
const utils = @import("main-utils.zig");
const runic = @import("runic");
const FrontendDocumentStore = runic.document.FrontendDocumentStore;
const TypeChecker = runic.semantic.TypeChecker;
const rainbow = runic.rainbow;
const ast = runic.ast;
const Stream = runic.stream.Stream;
const closeable = runic.closeable;
const ExitCode = runic.ExitCode;
const TraceWriter = runic.TraceWriter;
const Tracer = runic.trace.Tracer;
const ir = runic.ir;

const log_enabled = false;

fn log(comptime fmt: []const u8, args: anytype) void {
    if (!log_enabled) return;
    std.log.debug(fmt, args);
}

pub fn runScript(
    io: std.Io,
    allocator: Allocator,
    env_map: *std.process.Environ.Map,
    script: utils.CliConfig.ScriptInvocation,
    config: utils.CliConfig,
    stdin: *std.Io.Reader,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    tracer: *Tracer,
) !ExitCode {
    try utils.applyEnvOverridesToMap(env_map, config.env_overrides);
    try utils.exposeScriptMetadata(env_map, script);
    try utils.ensureExecutableDirOnPath(io, allocator, env_map);

    const script_dir = utils.computeScriptDirectory(allocator, script.path) catch |err| {
        try stderr.print("error: unable to resolve script directory: {s}\n", .{@errorName(err)});
        return .fromByte(1);
    };
    defer allocator.free(script_dir);

    if (config.print_tokens) {
        if (script.source) |source| {
            utils.printScriptTokens(io, allocator, env_map, stdout, script.path, source) catch |err| {
                try stderr.print(
                    "error: failed to print tokens for inline script '{s}': {s}\n",
                    .{ script.path, @errorName(err) },
                );
                return .fromByte(1);
            };
            return .success;
        }
        return .fromByte(try printTokens(io, allocator, env_map, stdout, stderr, script.path));
    }

    var document_store = FrontendDocumentStore.init(io, allocator, env_map);
    defer document_store.deinit();
    const entryDocument = if (script.source) |source|
        try document_store.putDocument(script.path, source)
    else
        try document_store.requestDocument(script.path);
    const resolvedPath = entryDocument.path;
    const parser_result = entryDocument.parser.parseScript(resolvedPath);
    const script_ast = try processResult(io, &document_store.document_store, stderr, parser_result) orelse return .fromByte(1);

    const parse_imports_result = try parseImports(
        io,
        allocator,
        stderr,
        &document_store.document_store,
        script_ast,
    );

    switch (parse_imports_result) {
        .success => {},
        inline else => return parse_imports_result,
    }

    if (config.print_ast) {
        for (document_store.map.values()) |document| {
            utils.printScriptAst(stdout, document.path, document.ast.?) catch |err| {
                try stderr.print(
                    "error: failed to print AST for script '{s}': {s}\n",
                    .{ script.path, @errorName(err) },
                );
                return .fromByte(1);
            };
        }

        return .success;
    }

    if (!config.skip_type_check) {
        var type_checker = TypeChecker.init(io, allocator, &document_store.document_store, env_map);
        defer type_checker.deinit();

        const type_checker_result = type_checker.typeCheck(resolvedPath) catch |err| {
            std.log.err("Type checker failed to run: {}", .{err});
            return .fromByte(1);
        };

        if (try processResult(io, &document_store.document_store, stderr, type_checker_result)) |_| {
            if (config.type_check_only) return .success;
        } else {
            return .fromByte(1);
        }
    }

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator);
    defer allocator.free(cwd);

    const stdin_stream = try Stream(u8).initReaderWriter(allocator, "<<<stdin_pipe>>>", .{
        .close_source = false,
        .disconnect_source = false,
        .close_destination = true,
        .disconnect_destination = true,
        .keep_open = true,
        .propagate_eof_on_source_close = true,
    }, tracer);
    defer stdin_stream.stream.deinit();
    const stdout_stream = try Stream(u8).initReaderWriter(allocator, "<<<stdout_pipe>>>", .{
        .close_source = true,
        .disconnect_source = true,
        .close_destination = false,
        .disconnect_destination = false,
        .keep_open = true,
    }, tracer);
    defer stdout_stream.stream.deinit();
    const stderr_stream = try Stream(u8).initReaderWriter(allocator, "<<<stderr_pipe>>>", .{
        .close_source = true,
        .disconnect_source = true,
        .close_destination = false,
        .disconnect_destination = false,
        .keep_open = true,
    }, tracer);
    defer stderr_stream.stream.deinit();

    var stdin_closeable = closeable.ManualCloseable(ExitCode){ .label = "<<<stdin>>>" };
    var stdout_closeable = closeable.NeverCloses(ExitCode){ .label = "<<<stdout>>>" };
    var stderr_closeable = closeable.NeverCloses(ExitCode){ .label = "<<<stderr>>>" };

    var wrapped_stdout_buffer: [1024]u8 = undefined;
    var wrapped_stderr_buffer: [1024]u8 = undefined;
    var wrapped_stdout = TraceWriter.init(&wrapped_stdout_buffer, stdout, null, "<<<t_stdout>>>");
    var wrapped_stderr = TraceWriter.init(&wrapped_stderr_buffer, stderr, null, "<<<t_stderr>>>");

    const stdin_closeable_reader = closeable.CloseableReader(ExitCode).init(
        stdin,
        &stdin_closeable.closeable,
    );
    const stdout_closeable_writer = closeable.CloseableWriter(ExitCode).init(
        &wrapped_stdout.writer,
        &stdout_closeable.closeable,
    );
    const stderr_closeable_writer = closeable.CloseableWriter(ExitCode).init(
        &wrapped_stderr.writer,
        &stderr_closeable.closeable,
    );

    try stdin_stream.connectSource(stdin_closeable_reader);
    try stdout_stream.connectDestination(stdout_closeable_writer);
    try stderr_stream.connectDestination(stderr_closeable_writer);

    log("stdout >>> {*}", .{&wrapped_stdout.writer});
    log("stderr >>> {*}", .{&wrapped_stderr.writer});

    var result: ir.runner.RunResult = .{ .success = .success };
    defer result.deinit();

    if (config.debug_ir) {
        result = try ir.runner.debugIR(
            io,
            allocator,
            env_map,
            &entryDocument.ast.?,
            &document_store.document_store,
            stdin_stream,
            stdout_stream,
            stderr_stream,
            try std.Io.File.stdout().isTty(io),
            try std.Io.File.stderr().isTty(io),
            tracer,
        );
    } else {
        result = try ir.runner.runIR(
            allocator,
            &document_store.document_store,
            &entryDocument.ast.?,
            .{
                .io = io,
                .verbose = config.verbose,
                .dry_run = config.dry_run,
                .script_args = script.args,
                .env = env_map,
                .stdin = stdin_stream,
                .stdout = stdout_stream,
                .stderr = stderr_stream,
                .stdout_is_tty = try std.Io.File.stdout().isTty(io),
                .stderr_is_tty = try std.Io.File.stderr().isTty(io),
                .tracer = tracer,
            },
        );
    }

    return try processResult(io, &document_store.document_store, stdout, result) orelse .fromByte(1);
}

const StatementExpressionIterator = struct {
    allocator: Allocator,
    script: runic.ast.Script,
    cursor: Cursor,

    const StackItem = union(enum) {
        statement: *runic.ast.Statement,
        expr: *runic.ast.Expression,
    };

    const Cursor = struct {
        statementIdx: usize = 0,
        stack: std.array_list.Managed(StackItem),

        pub fn init(allocator: Allocator, statements: []const *runic.ast.Statement) !Cursor {
            const list = try std.array_list.Managed(StackItem).initCapacity(allocator, statements.len);

            var cursor = Cursor{
                .stack = list,
            };

            try cursor.appendStatements(statements);

            return cursor;
        }

        pub fn appendStatements(self: *Cursor, statements: []const *runic.ast.Statement) !void {
            try self.stack.ensureUnusedCapacity(statements.len);
            for (0..statements.len) |i| {
                self.stack.appendAssumeCapacity(.{ .statement = statements[statements.len - i - 1] });
            }
        }

        pub fn appendExpressions(self: *Cursor, expressions: []const *runic.ast.Expression) !void {
            try self.stack.ensureUnusedCapacity(expressions.len);
            for (0..expressions.len) |i| {
                self.stack.appendAssumeCapacity(.{ .expr = expressions[expressions.len - i - 1] });
            }
        }

        pub fn appendStatement(self: *Cursor, statement: *runic.ast.Statement) !void {
            try self.stack.append(.{ .statement = statement });
        }

        pub fn appendExpr(self: *Cursor, expr: *runic.ast.Expression) !void {
            try self.stack.append(.{ .expr = expr });
        }
    };

    pub fn init(allocator: Allocator, script: runic.ast.Script) !StatementExpressionIterator {
        return .{
            .allocator = allocator,
            .script = script,
            .cursor = try .init(allocator, script.statements),
        };
    }

    pub fn deinit(self: *StatementExpressionIterator) void {
        self.cursor.stack.deinit();
    }

    pub fn next(self: *StatementExpressionIterator) !?StackItem {
        const item = self.cursor.stack.pop() orelse return null;

        try switch (item) {
            .expr => |expr| self.populateStackExpr(expr),
            .statement => |statement| self.populateStackStatement(statement),
        };

        return item;
    }

    fn populateStackStatement(self: *StatementExpressionIterator, statement: *runic.ast.Statement) !void {
        switch (statement.*) {
            .bash_block => {},
            .type_binding_decl => {},
            .binding_decl => |binding_decl| try self.cursor.appendExpr(binding_decl.initializer),
            .while_stmt => |while_stmt| {
                try self.cursor.appendStatements(while_stmt.body.statements);
                try self.cursor.appendExpr(while_stmt.condition);
            },
            .exit_stmt => |exit_stmt| if (exit_stmt.value) |v| try self.cursor.appendExpr(v),
            .yield_stmt => |yield_stmt| try self.cursor.appendExpr(yield_stmt.value),
            .expression => |expr| try self.cursor.appendExpr(expr.expression),
        }
    }

    fn populateStackExpr(self: *StatementExpressionIterator, expr: *runic.ast.Expression) !void {
        const cursor = &self.cursor;
        switch (expr.*) {
            .identifier, .env_var, .path, .literal, .map, .import_expr, .pipeline_deprecated, .builtin, .fd => {},
            .array => |array| try cursor.appendExpressions(array.elements),
            .struct_literal => |struct_literal| for (struct_literal.fields) |field| try cursor.appendExpr(field.value),
            .range => |range| {
                try cursor.appendExpr(range.start);
                if (range.end) |end| try cursor.appendExpr(end);
            },
            .pipeline => |pipeline| for (pipeline.stages) |stage| try cursor.appendExpr(stage),
            .call => |call| try cursor.appendExpr(call.callee),
            .member => |member| try cursor.appendExpr(member.object),
            .index => |index| {
                try cursor.appendExpr(index.index);
                try cursor.appendExpr(index.target);
            },
            .unary => |unary| try cursor.appendExpr(unary.operand),
            .binary => |binary| {
                try cursor.appendExpr(binary.left);
                try cursor.appendExpr(binary.right);
            },
            .block => |block| try populateBlockExpr(cursor, block),
            .fn_decl => |fn_decl| try self.cursor.appendExpr(fn_decl.body),
            .if_expr => |*if_expr| try populateIfExpr(cursor, if_expr),
            .match_expr => |match_expr| {
                for (match_expr.cases) |c| try populateBlockExpr(cursor, c.body);
                try cursor.appendExpr(match_expr.subject);
            },
            .try_expr => |try_expr| try cursor.appendExpr(try_expr.subject),
            .catch_expr => |catch_expr| {
                try cursor.appendExpr(catch_expr.handler);
                try cursor.appendExpr(catch_expr.subject);
            },
            .assignment => |assignment| try cursor.appendExpr(assignment.expr),
            .for_expr => |for_expr| {
                try cursor.appendExpr(for_expr.body);
                try self.cursor.appendExpressions(for_expr.sources);
            },
            .executable => {},
            .subshell => |subshell| try cursor.appendExpr(subshell.child),
        }
    }

    fn populateBlockExpr(cursor: *Cursor, block: runic.ast.Block) !void {
        try cursor.appendStatements(block.statements);
    }

    fn populateIfExpr(cursor: *Cursor, if_expr: *runic.ast.IfExpr) !void {
        if (if_expr.else_branch) |e| switch (e) {
            .if_expr => |else_if_expr| try populateIfExpr(cursor, else_if_expr),
            .expr => |expr| try cursor.appendExpr(expr),
            .condition => {},
        };
        try cursor.appendExpr(if_expr.then_expr);
        try cursor.appendExpr(if_expr.condition);
    }
};

fn processResult(
    io: std.Io,
    document_store: *runic.DocumentStore,
    writer: *std.Io.Writer,
    result: anytype,
) !?std.meta.fieldInfo(@TypeOf(result), .success).type {
    return switch (result) {
        .success => |payload| payload,
        .err => |err_info| {
            const diagnostics = err_info.diagnostics();
            for (diagnostics) |d| {
                const maybe_span: ?ast.Span = d.span();
                const source = if (maybe_span) |span| document_store.getSource(span.start.file) catch |err| brk: {
                    try writer.print("<error getting document {s} : {}>", .{ span.start.file, err });
                    break :brk null;
                } else null;
                try writer.print("[{s}]: ", .{d.severity()});
                if (maybe_span) |span| logFileLineAndCol(io, writer, span) catch |err| {
                    try writer.print("<error logging file path : {}>", .{err});
                };
                try writer.print(" {s}\n", .{d.message});
                try if (source) |src| if (maybe_span) |span| logSpan(writer, span, src) else writer.writeAll("<unable to get the source>\n\n");
                try writer.flush();
            }
            return null;
        },
    };
}

fn parseImports(
    io: std.Io,
    allocator: Allocator,
    writer: *std.Io.Writer,
    document_store: *runic.DocumentStore,
    script: runic.ast.Script,
) !runic.ExitCode {
    var it = try StatementExpressionIterator.init(allocator, script);
    defer it.deinit();
    while (try it.next()) |node| {
        if (node == .statement) continue;
        const expr = node.expr;
        switch (expr.*) {
            .import_expr => |import_expr| {
                const module_path = try runic.document.resolveModulePath(
                    io,
                    allocator,
                    import_expr.importer,
                    import_expr.module_name,
                );
                defer allocator.free(module_path);
                // Skip already-parsed modules (cycle detection)
                if ((document_store.getAst(module_path) catch null) != null) continue;
                const parser = try document_store.getParser(module_path);
                const result = parser.parseScript(module_path);
                const import_script = try processResult(io, document_store, writer, result) orelse return .fromByte(1);
                _ = try parseImports(io, allocator, writer, document_store, import_script);
            },
            else => {},
        }
    }

    return .success;
}

fn printTokens(
    io: std.Io,
    allocator: Allocator,
    env_map: *std.process.Environ.Map,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    path: []const u8,
) !u8 {
    var file = std.Io.Dir.cwd().openFile(io, path, .{}) catch |err| {
        try stderr.print("error: failed to open script '{s}': {s}\n", .{ path, @errorName(err) });
        return 1;
    };
    defer file.close(io);

    var buffer: [1024]u8 = undefined;
    var reader = file.reader(io, &buffer);
    const contents = reader.interface.allocRemaining(allocator, .unlimited) catch |err| {
        try stderr.print("error: failed to read script '{s}': {s}\n", .{ path, @errorName(err) });
        return 1;
    };
    defer allocator.free(contents);

    utils.printScriptTokens(io, allocator, env_map, stdout, path, contents) catch |err| {
        try stderr.print(
            "error: failed to lex script '{s}': {s}\n",
            .{ path, @errorName(err) },
        );
        return 1;
    };

    return 0;
}

const span_color = rainbow.beginBgColor(.red) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

fn logFileLineAndCol(io: std.Io, writer: *std.Io.Writer, span: ast.Span) !void {
    var fix_allocator = std.heap.FixedBufferAllocator.init(&.{});
    var buf_allocator = std.heap.stackFallback(1024, fix_allocator.allocator());
    const allocator = buf_allocator.get();

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(io, ".", allocator);
    const relative = try std.fs.path.relative(allocator, cwd, null, cwd, span.start.file);

    try writer.print("{s}:{}:{}:", .{ relative, span.start.line, span.start.column });
}

fn logSpan(writer: *std.Io.Writer, span: ast.Span, source: []const u8) !void {
    var lineIt = std.mem.splitScalar(u8, source, '\n');
    var i: usize = 0;
    while (lineIt.next()) |line| : (i += 1) {
        if (i >= span.start.line -| 3 and i <= span.end.line +| 3) {
            if (span.start.line == i + 1 and span.end.line == i + 1) {
                try writer.print("{:>4}:{s}{s}{s}{s}{s}\n", .{
                    i + 1,
                    line[0 .. span.start.column - 1],
                    span_color,
                    line[span.start.column - 1 .. span.end.column - 1],
                    end_color,
                    line[span.end.column - 1 ..],
                });
            } else if (span.start.line == i + 1) {
                try writer.print("{:>4}:{s}{s}{s}{s}\n", .{
                    i + 1,
                    line[0 .. span.start.column - 1],
                    span_color,
                    line[span.start.column - 1 ..],
                    end_color,
                });
            } else if (span.end.line == i + 1) {
                try writer.print("{:>4}:{s}{s}{s}{s}\n", .{
                    i + 1,
                    span_color,
                    line[0 .. span.end.column - 1],
                    end_color,
                    line[span.end.column - 1 ..],
                });
            } else if (span.start.line - 1 <= i and i <= span.end.line - 1) {
                try writer.print("{:>4}:{s}{s}{s}\n", .{
                    i + 1,
                    span_color,
                    line,
                    end_color,
                });
            } else {
                try writer.print("{:>4}:{s}\n", .{ i + 1, line });
            }
        }
    }

    try writer.writeByte('\n');
}
