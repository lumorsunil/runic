const std = @import("std");
const Allocator = std.mem.Allocator;
const utils = @import("main-utils.zig");
const runic = @import("runic");
const ScriptExecutor = runic.interpreter.ScriptExecutor;
const FrontendDocumentStore = runic.document.FrontendDocumentStore;
const Parser = runic.parser.Parser;
const TypeChecker = runic.semantic.TypeChecker;
const rainbow = runic.rainbow;
const ast = runic.ast;

pub fn runScript(
    allocator: Allocator,
    script: utils.CliConfig.ScriptInvocation,
    config: utils.CliConfig,
    // stdin: *std.Io.Reader,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !runic.command_runner.ExitCode {
    var env_map = std.process.getEnvMap(allocator) catch |err| {
        try stderr.print("error: unable to capture environment: {s}\n", .{@errorName(err)});
        return .fromByte(1);
    };
    defer env_map.deinit();

    try utils.applyEnvOverridesToMap(&env_map, config.env_overrides);
    try utils.exposeScriptMetadata(&env_map, script);
    try utils.ensureExecutableDirOnPath(allocator, &env_map);

    const script_dir = utils.computeScriptDirectory(allocator, script.path) catch |err| {
        try stderr.print("error: unable to resolve script directory: {s}\n", .{@errorName(err)});
        return .fromByte(1);
    };
    defer allocator.free(script_dir);

    if (config.print_tokens) {
        return .fromByte(try printTokens(allocator, stdout, stderr, script.path));
    }

    var document_store = FrontendDocumentStore.init(allocator);
    defer document_store.deinit();
    const entryDocument = try document_store.requestDocument(script.path);
    const resolvedPath = try document_store.resolvePath(script.path);
    const parser_result = entryDocument.parser.parseScript(resolvedPath);
    const script_ast = try processResult(&document_store.document_store, stderr, parser_result) orelse return .fromByte(1);

    const parse_imports_result = try parseImports(
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
        var type_checker = TypeChecker.init(allocator, &document_store.document_store);
        defer type_checker.deinit();

        const type_checker_result = type_checker.typeCheck(resolvedPath) catch |err| {
            std.log.err("Type checker failed to run: {}", .{err});
            return .fromByte(1);
        };

        if (try processResult(&document_store.document_store, stderr, type_checker_result)) |_| {
            if (config.type_check_only) return .success;
        } else {
            return .fromByte(1);
        }
    }

    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    const executeOptions = ScriptExecutor.ExecuteOptions.init(
        script.path,
        cwd,
        env_map,
        .init(
            // .init(stdin, .streaming),
            .blocked(),
            .init(stdout, .streaming),
            .init(stderr, .streaming),
        ),
    );

    entryDocument.script_executor = ScriptExecutor.initWithRunner(
        allocator,
        entryDocument.path,
        &env_map,
        executeOptions,
        &document_store.document_store,
    ) catch |err| {
        try stderr.print(
            "error: failed to initialize script executor: {s}\n",
            .{@errorName(err)},
        );
        return .fromByte(1);
    };
    var executor: *ScriptExecutor = undefined;
    if (entryDocument.script_executor) |*script_executor| executor = script_executor else return .{ .err = error.ExecutorNotDefined };

    executor.wireCommandBridge(entryDocument.script_executor.?.scopes);
    // try executor.reseedFromContext(&context);

    return try executor.execute(script_ast, executeOptions);
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
            .error_decl, .bash_block => {},
            .type_binding_decl => {},
            .binding_decl => |binding_decl| try self.cursor.appendExpr(binding_decl.initializer),
            .fn_decl => |fn_decl| try self.cursor.appendExpr(fn_decl.body),
            // .for_stmt => |for_stmt| {
            //     try self.cursor.appendStatements(for_stmt.body.statements);
            //     try self.cursor.appendExpressions(for_stmt.sources);
            // },
            .while_stmt => |while_stmt| {
                try self.cursor.appendStatements(while_stmt.body.statements);
                try self.cursor.appendExpr(while_stmt.condition);
            },
            .return_stmt => |return_stmt| if (return_stmt.value) |v| try self.cursor.appendExpr(v),
            .expression => |expr| try self.cursor.appendExpr(expr.expression),
        }
    }

    fn populateStackExpr(self: *StatementExpressionIterator, expr: *runic.ast.Expression) !void {
        const cursor = &self.cursor;
        switch (expr.*) {
            .identifier, .path, .literal, .map, .import_expr => {},
            .array => |array| try cursor.appendExpressions(array.elements),
            .range => |range| {
                try cursor.appendExpr(range.start);
                if (range.end) |end| try cursor.appendExpr(end);
            },
            .pipeline => |pipeline| {
                for (pipeline.stages) |stage| switch (stage.payload) {
                    .expression => |p_expr| try cursor.appendExpr(p_expr),
                    else => {},
                };
            },
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
            .fn_literal => |fn_literal| {
                try cursor.appendExpr(fn_literal.body);
                for (fn_literal.params) |param| if (param.default_value) |dv| try cursor.appendExpr(dv);
            },
            .if_expr => |*if_expr| try populateIfExpr(cursor, if_expr),
            .match_expr => |match_expr| {
                for (match_expr.cases) |c| try populateBlockExpr(cursor, c.body);
                try cursor.appendExpr(match_expr.subject);
            },
            .try_expr => |try_expr| try cursor.appendExpr(try_expr.subject),
            .catch_expr => |catch_expr| {
                try populateBlockExpr(cursor, catch_expr.handler.body);
                try cursor.appendExpr(catch_expr.subject);
            },
            .assignment => |assignment| try cursor.appendExpr(assignment.expr),
            .for_expr => |for_expr| {
                try self.cursor.appendStatements(for_expr.body.statements);
                try self.cursor.appendExpressions(for_expr.sources);
            },
            .executable => {},
        }
    }

    fn populateBlockExpr(cursor: *Cursor, block: runic.ast.Block) !void {
        try cursor.appendStatements(block.statements);
    }

    fn populateIfExpr(cursor: *Cursor, if_expr: *runic.ast.IfExpr) !void {
        if (if_expr.else_branch) |e| switch (e) {
            .if_expr => |else_if_expr| try populateIfExpr(cursor, else_if_expr),
            .expr => |expr| try cursor.appendExpr(expr),
        };
        try cursor.appendExpr(if_expr.then_expr);
        try cursor.appendExpr(if_expr.condition);
    }
};

fn processResult(
    document_store: *runic.DocumentStore,
    writer: *std.Io.Writer,
    result: anytype,
) !?std.meta.fieldInfo(@TypeOf(result), .success).type {
    return switch (result) {
        .success => |payload| payload,
        .err => |err_info| {
            const diagnostics = err_info.diagnostics();
            for (diagnostics) |d| {
                const span = d.span();
                const source = document_store.getSource(span.start.file) catch |err| brk: {
                    try writer.print("<error getting document {s} : {}>", .{ span.start.file, err });
                    break :brk null;
                };
                try writer.print("[{s}]: ", .{d.severity()});
                logFileLineAndCol(writer, span) catch |err| {
                    try writer.print("<error logging file path : {}>", .{err});
                };
                try writer.print(" {s}\n", .{d.message});
                try if (source) |src| logSpan(writer, span, src) else writer.writeAll("<unable to get the source>\n\n");
                try writer.flush();
            }
            return null;
        },
    };
}

fn parseImports(
    allocator: Allocator,
    writer: *std.Io.Writer,
    document_store: *runic.DocumentStore,
    script: runic.ast.Script,
) !runic.command_runner.ExitCode {
    var it = try StatementExpressionIterator.init(allocator, script);
    defer it.deinit();
    while (try it.next()) |node| {
        if (node == .statement) continue;
        const expr = node.expr;
        switch (expr.*) {
            .import_expr => |import_expr| {
                const module_path = try runic.document.resolveModulePath(
                    allocator,
                    import_expr.importer,
                    import_expr.module_name,
                );
                defer allocator.free(module_path);
                const parser = try document_store.getParser(module_path);
                const result = parser.parseScript(module_path);
                const import_script = try processResult(document_store, writer, result) orelse return .fromByte(1);
                _ = try parseImports(allocator, writer, document_store, import_script);
            },
            else => {},
        }
    }

    return .success;
}

fn printTokens(allocator: Allocator, stdout: *std.Io.Writer, stderr: *std.Io.Writer, path: []const u8) !u8 {
    var file = std.fs.cwd().openFile(path, .{}) catch |err| {
        try stderr.print("error: failed to open script '{s}': {s}\n", .{ path, @errorName(err) });
        return 1;
    };
    defer file.close();

    const contents = file.readToEndAlloc(allocator, std.math.maxInt(usize)) catch |err| {
        try stderr.print("error: failed to read script '{s}': {s}\n", .{ path, @errorName(err) });
        return 1;
    };
    defer allocator.free(contents);

    utils.printScriptTokens(allocator, stdout, path, contents) catch |err| {
        try stderr.print(
            "error: failed to lex script '{s}': {s}\n",
            .{ path, @errorName(err) },
        );
        return 1;
    };

    return 0;
}

fn handleParseScriptError(
    parser: *const Parser,
    stderr: *std.Io.Writer,
    err: anyerror,
) !void {
    const lexer = parser.stream.lexer;
    const line = lexer.line;
    const column = lexer.column;
    const contents = parser.source;

    const source_line = blk: {
        var current_line: usize = 1;
        var start: usize = 0;
        var i: usize = 0;
        while (i < contents.len and current_line < line) : (i += 1) {
            if (contents[i] == '\n') {
                current_line += 1;
                start = i + 1;
            }
        }
        if (current_line != line or start >= contents.len) break :blk "";
        var end = start;
        while (end < contents.len and contents[end] != '\n' and contents[end] != '\r') : (end += 1) {}
        break :blk contents[start..end];
    };

    try stderr.print(
        "error: failed to parse script '{s}' at line {d}, column {d}: {s}",
        .{ parser.path, line, column, @errorName(err) },
    );
    const expected = parser.expectedTokens();
    if (expected.len > 0) {
        try stderr.writeAll("\n       ");
        _ = try parser.writeExpectedTokens(stderr);
        try stderr.writeByte('\n');
    }
    try parser.writeBreadcrumbTrail(stderr);
    try stderr.writeByte('\n');
    if (source_line.len > 0) {
        try stderr.print("   {s}\n   ", .{source_line});
        var spaces: usize = if (column > 1) column - 2 else 0;
        while (spaces > 0) : (spaces -= 1) {
            try stderr.writeByte(' ');
        }
        try stderr.print("^\n", .{});
    }
    try stderr.flush();
}

const span_color = rainbow.beginBgColor(.red) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

fn logFileLineAndCol(writer: *std.Io.Writer, span: ast.Span) !void {
    var fix_allocator = std.heap.FixedBufferAllocator.init(&.{});
    var buf_allocator = std.heap.stackFallback(1024, fix_allocator.allocator());
    const allocator = buf_allocator.get();

    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    const relative = try std.fs.path.relative(allocator, cwd, span.start.file);

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
