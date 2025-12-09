const std = @import("std");
const ast = @import("../frontend/ast.zig");
const token = @import("../frontend/token.zig");
const command_runner = @import("../runtime/command_runner.zig");
const Value = @import("value.zig").Value;
const ScopeStack = @import("scope.zig").ScopeStack;
const Parser = @import("../frontend/parser.zig").Parser;
const DocumentStore = @import("../document_store.zig").DocumentStore;
const Document = @import("../frontend/document_store.zig").Document;
const resolveModulePath = @import("../frontend/document_store.zig").resolveModulePath;
const ScriptExecutor = @import("script_executor.zig").ScriptExecutor;
const ScriptContext = @import("script_context.zig").ScriptContext;
const rainbow = @import("../rainbow.zig");

const CommandRunner = command_runner.CommandRunner;
const ProcessHandle = command_runner.ProcessHandle;

const logging_name = "EXECUTOR";
const prefix_color = rainbow.beginColor(.blue);
const span_color = rainbow.beginBgColor(.green) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

fn ArrayListManaged(comptime T: type) type {
    return std.array_list.Managed(T);
}

/// Evaluator drives statement/expressions execution for AST blocks. It owns
/// the allocator used for temporary strings and forwards pipelines to the
/// command runner (or any adapter implementing `CommandExecutor`).
pub const Evaluator = struct {
    allocator: std.mem.Allocator,
    path: []const u8,
    executor: CommandExecutor,
    /// Refers to the outermost context
    executeOptions: ScriptExecutor.ExecuteOptions,
    document_store: DocumentStore,
    logging_enabled: bool,

    pub const Error = std.mem.Allocator.Error ||
        ScopeStack.Error ||
        CommandExecutor.Error ||
        ScriptContext.BindingError ||
        DocumentStore.Error ||
        Value.Error ||
        std.fmt.ParseIntError ||
        std.fmt.ParseFloatError ||
        std.fs.Dir.RealPathAllocError ||
        std.fs.File.OpenError ||
        std.Io.Reader.ShortError ||
        std.Io.Writer.Error ||
        error{
            UnsupportedStatement,
            UnsupportedExpression,
            UnsupportedBindingPattern,
            UnsupportedPipelineStage,
            UnsupportedCommandFeature,
            UnsupportedMemberAccess,
            UnsupportedCommandType,
            UnsupportedBinaryExpr,
            UnsupportedIterator,
            MemberNotFound,
            UnknownIdentifier,
            EmptyPipeline,
            InvalidStringCoercion,
            DocumentNotParsed,
            DocumentNotFound,
            ForSourcesDifferentLengths,
            NegativeLength,
            CannotCoerceValueIntoLength,
            InvalidScope,
        };

    pub const Context = struct {
        executeOptions: ScriptExecutor.ExecuteOptions,
        scopes: *ScopeStack,
        is_streaming: bool,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        path: []const u8,
        executor: CommandExecutor,
        executeOptions: ScriptExecutor.ExecuteOptions,
        documentStore: DocumentStore,
    ) Evaluator {
        const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_" ++ logging_name) catch null;
        defer if (logging_enabled_s) |le| allocator.free(le);
        const logging_enabled = if (logging_enabled_s) |le| std.mem.eql(u8, le, "1") else false;

        return .{
            .allocator = allocator,
            .path = path,
            .executor = executor,
            .executeOptions = executeOptions,
            .document_store = documentStore,
            .logging_enabled = logging_enabled,
        };
    }

    pub fn initWithRunner(
        allocator: std.mem.Allocator,
        runner: *CommandRunner,
        documentStore: *DocumentStore,
    ) Evaluator {
        return init(allocator, CommandExecutor.fromRunner(runner), documentStore);
    }

    pub fn log(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        if (@hasField(@This(), "logging_enabled")) {
            if (!self.logging_enabled) return;
        }

        var stderr = std.fs.File.stderr().writer(&.{});
        const writer = &stderr.interface;

        try writer.print("[{s}{*}{s}]\n", .{ prefix_color, self, end_color });
        try writer.print("{s}:\n", .{self.path});
        try writer.print(fmt ++ "\n", args);
    }

    pub fn logWithoutPrefix(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        if (@hasField(@This(), "logging_enabled")) {
            if (!self.logging_enabled) return;
        }

        var stderr = std.fs.File.stderr().writer(&.{});
        const writer = &stderr.interface;

        try writer.print(fmt, args);
    }

    pub fn logEvaluationTrace(self: *Evaluator, label: []const u8) !void {
        try self.log("{s}", .{label});
    }

    pub const RunFunctionResult = struct {
        stdout: []u8,
        stderr: []u8,
        started_at_ns: i128,
        finished_at_ns: i128,
    };

    pub fn runFunctionGetValue(
        self: *Evaluator,
        scopes: *ScopeStack,
        fn_ref: Value.FunctionRef,
    ) Error!Value {
        const result = try self.runFunction(scopes, fn_ref);
        return .{ .process_handle = try self.runFunctionResultToProcessHandle(result) };
    }

    pub fn runFunction(
        self: *Evaluator,
        scopes: *ScopeStack,
        fn_ref_ref: Value.FunctionRef,
    ) Error!RunFunctionResult {
        const fn_ref = try fn_ref_ref.getPtr();
        try scopes.pushFrame(@src().fn_name, .initBlocking());
        try scopes.pushFrame(@src().fn_name, .initScopeRefSpecial(fn_ref.closure.?));
        try scopes.pushFrame(@src().fn_name, try .initSingleNew(self.allocator));
        errdefer scopes.popFrameN(@src().fn_name, 3) catch {
            std.log.err(@src().fn_name ++ ": Could not pop the frames.", .{});
        };

        var fn_ref_value = Value{ .function = try fn_ref_ref.ref(.{}) };
        try scopes.declare(fn_ref.fn_decl.name.name, &fn_ref_value, false);

        const started_at = std.time.nanoTimestamp();

        // TODO: What to do with these values?
        switch (fn_ref.fn_decl.body) {
            .block => |block| {
                for (block.statements) |statement| {
                    var value = try self.runStatement(scopes, statement) orelse continue;
                    value.deinit();
                }
            },
            .expression => |expr| {
                var value = try self.evaluateExpression(scopes, expr);
                value.deinit();
            },
        }

        const finished_at = std.time.nanoTimestamp();

        try scopes.popFrameN(@src().fn_name, 3);

        return .{
            .started_at_ns = started_at,
            .finished_at_ns = finished_at,
            .stdout = try self.allocator.dupe(u8, ""),
            .stderr = try self.allocator.dupe(u8, ""),
        };
    }

    pub fn runBlockInOwnContext(
        self: *Evaluator,
        scopes: *ScopeStack,
        block: ast.Block,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        try scopes.pushFrame(@src().fn_name, try .initSingleNew(self.allocator));

        const started_at = std.time.nanoTimestamp();

        for (block.statements) |statement| {
            // TODO: do something with these values?
            if (try self.runStatement(scopes, statement)) |value| {
                var owned = value;
                owned.deinit();
            }
        }

        const finished_at = std.time.nanoTimestamp();

        try scopes.popFrame(
            @src().fn_name,
        );

        return .{
            .process_handle = try self.runFunctionResultToProcessHandle(.{
                .started_at_ns = started_at,
                .finished_at_ns = finished_at,
                // .stdout = try self.allocator.dupe(u8, stdout.buffered()),
                // .stderr = try self.allocator.dupe(u8, stderr.buffered()),
                .stdout = try self.allocator.dupe(u8, ""),
                .stderr = try self.allocator.dupe(u8, ""),
            }),
        };
    }

    fn runFunctionResultToProcessHandle(
        self: *Evaluator,
        result: RunFunctionResult,
    ) Error!ProcessHandle {
        // Fix when blocks can have errors which results in exit codes as errors
        const exitCode: command_runner.ExitCode = .success;

        const stage = command_runner.StageExecution{
            .pid = null,
            .started_at_ns = result.started_at_ns,
            .finished_at_ns = result.finished_at_ns,
            .status = .fromTerm(0, .{ .function = exitCode }),
            // TODO: fix when implementing typed stdout for functions
            .stdout_owned = result.stdout,
            .stderr_owned = result.stderr,
        };

        const stage_statuses = try self.allocator.dupe(command_runner.StageStatus, &.{stage.status});
        const stage_captures = try self.allocator.dupe(command_runner.StageCapture, &.{.{
            .stdout = stage.stdout_owned,
            .stderr = stage.stderr_owned,
        }});

        return ProcessHandle{
            .allocator = self.allocator,
            .handle_type = .function,
            .started_at_ns = result.started_at_ns,
            .finished_at_ns = result.finished_at_ns,
            .status = command_runner.ProcessStatus.fromStages(stage_statuses),
            .stage_statuses = stage_statuses,
            .stage_captures = stage_captures,
        };
    }

    /// Executes a single statement in the current scope and returns its result
    /// if the statement produces a value (expression statements).
    pub fn runStatement(
        self: *Evaluator,
        scopes: *ScopeStack,
        statement: *const ast.Statement,
    ) Error!?Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});
        return self.executeStatement(scopes, statement);
    }

    fn executeStatement(
        self: *Evaluator,
        scopes: *ScopeStack,
        statement: *const ast.Statement,
    ) Error!?Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});

        return switch (statement.*) {
            .binding_decl => |decl| blk: {
                try self.executeBinding(scopes, &decl);
                break :blk null;
            },
            .fn_decl => |*decl| blk: {
                try self.executeFnDecl(scopes, decl);
                break :blk null;
            },
            .expression => |expr_stmt| try self.executeExpression(scopes, expr_stmt.expression),
            else => Error.UnsupportedStatement,
        };
    }

    fn executeExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        expression: *ast.Expression,
    ) Error!?Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});

        return try self.evaluateExpression(scopes, expression);
    }

    fn executeBinding(self: *Evaluator, scopes: *ScopeStack, decl: *const ast.BindingDecl) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});

        var stdout_buf_writer = std.Io.Writer.Allocating.init(self.allocator);
        defer stdout_buf_writer.deinit();
        var stderr_buf_writer = std.Io.Writer.Allocating.init(self.allocator);
        defer stderr_buf_writer.deinit();

        const stdout = &stdout_buf_writer.writer;
        const stderr = &stderr_buf_writer.writer;

        try scopes.pushFrameForwarding(@src().fn_name, stdout, stderr);

        var value = try self.evaluateExpression(scopes, decl.initializer);

        try scopes.popFrame(
            @src().fn_name,
        );

        switch (value) {
            .process_handle => |*process_handle| {
                const capture = &process_handle.stage_captures[process_handle.stage_captures.len - 1];
                self.allocator.free(capture.stdout);
                self.allocator.free(capture.stderr);

                capture.stdout = try stdout_buf_writer.toOwnedSlice();
                capture.stderr = try stderr_buf_writer.toOwnedSlice();
            },
            else => {},
        }

        errdefer value.deinit();

        try bindPattern(scopes, decl.pattern.*, decl.is_mutable, &value);
    }

    fn executeFnDecl(
        self: *Evaluator,
        scopes: *ScopeStack,
        decl: *const ast.FunctionDecl,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});

        const closure = try scopes.closure();
        var value = Value{ .function = try .init(self.allocator, .{
            .fn_decl = decl,
            .closure = closure,
        }, .{}) };

        try bindPattern(scopes, .{ .identifier = decl.name }, false, &value);
    }

    fn evaluateExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        expr: *const ast.Expression,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);
        try self.log("<{s}>", .{@tagName(expr.*)});
        try self.logEvaluateExpression(expr);

        var v = switch (expr.*) {
            .literal => |literal| try self.evaluateLiteral(scopes, literal),
            .identifier => |identifier| try self.evaluateIdentifier(scopes, identifier),
            .pipeline => |pipeline| try self.evaluatePipeline(scopes, pipeline),
            .block => |block_expr| try self.evaluateBlockExpression(scopes, block_expr),
            .import_expr => |import_expr| try self.evaluateImportExpression(scopes, import_expr),
            .member => |member_expr| try self.evaluateMemberExpression(scopes, member_expr),
            .assignment => |assignment| try self.evaluateAssignment(scopes, assignment),
            .binary => |binary| try self.evaluateBinary(scopes, binary),
            .if_expr => |if_expr| try self.evaluateIfExpression(scopes, if_expr),
            .for_expr => |for_expr| try self.evaluateForExpression(scopes, for_expr),
            .range => |range| try self.evaluateRangeExpression(scopes, range),
            .array => |array| try self.evaluateArrayExpression(scopes, array),
            else => return error.UnsupportedExpression,
        };

        return switch (v) {
            .function => |*f| {
                defer f.deinit(.{});
                return try self.runFunctionGetValue(scopes, f.*);
            },
            else => v,
        };
    }

    fn logEvaluateExpression(self: *Evaluator, expr: *const ast.Expression) !void {
        if (@hasField(@This(), "logging_enabled")) {
            if (!self.logging_enabled) return;
        }

        const span = expr.span();
        const source = try self.document_store.getSource(span.start.file);
        var lineIt = std.mem.splitScalar(u8, source, '\n');
        var i: usize = 0;
        while (lineIt.next()) |line| : (i += 1) {
            if (i >= span.start.line -| 3 and i <= span.end.line +| 3) {
                if (span.start.line == i + 1 and span.end.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 ..],
                        end_color,
                    });
                } else if (span.end.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line[0 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line - 1 <= i and i <= span.end.line - 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line,
                        end_color,
                    });
                } else {
                    try self.logWithoutPrefix("{:>4}:{s}\n", .{ i + 1, line });
                }
            }
        }
    }

    fn evaluateLiteral(self: *Evaluator, scopes: *ScopeStack, literal: ast.Literal) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);
        return switch (literal) {
            .null => Value{ .void = {} },
            .bool => |payload| Value{ .boolean = payload.value },
            .integer => |payload| blk: {
                const parsed = try std.fmt.parseInt(i64, payload.text, 10);
                break :blk Value{ .integer = parsed };
            },
            .float => |payload| blk: {
                const parsed = try std.fmt.parseFloat(f64, payload.text);
                break :blk Value{ .float = parsed };
            },
            .string => |payload| blk: {
                const rendered = try self.renderStringLiteral(scopes, payload);
                break :blk Value{ .string = rendered };
            },
        };
    }

    fn evaluateIdentifier(self: *Evaluator, scopes: *ScopeStack, identifier: ast.Identifier) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);
        const binding = try scopes.lookup(identifier.name) orelse return error.UnknownIdentifier;
        return try binding.value.clone(self.allocator);
    }

    fn evaluateBlockExpression(self: *Evaluator, scopes: *ScopeStack, block: ast.Block) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        return try self.runBlockInOwnContext(scopes, block);
    }

    // TODO: change this to run the module as a function?
    fn evaluateImportExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        import: ast.ImportExpr,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        const module_path = try resolveModulePath(self.allocator, import.importer, import.module_name);
        const script_ast = try self.document_store.getAst(module_path) orelse return error.DocumentNotParsed;

        try scopes.pushFrame(@src().fn_name, .initBlocking());
        try scopes.pushFrame(@src().fn_name, try .initSingleNew(self.allocator));

        for (script_ast.statements) |statement| {
            // TODO: do something with these values?
            if (try self.runStatement(scopes, statement)) |value| {
                var owned = value;
                owned.deinit();
            }
        }

        const import_scopes = try scopes.detach(2);

        return .{ .scope = import_scopes };
    }

    fn evaluateMemberExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        member: ast.MemberExpr,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        var object = try self.evaluateExpression(scopes, member.object);
        defer object.deinit();

        return self.evaluateMemberExpressionInner(member.member.name, object) catch |err| handleMemberAccessError(member, object, err);
    }

    fn evaluateMemberExpressionInner(
        self: *Evaluator,
        name: []const u8,
        object: Value,
    ) Error!Value {
        switch (object) {
            .void, .boolean, .integer, .float, .string, .function, .array, .range => return Error.UnsupportedMemberAccess,
            .process_handle => |p| {
                if (std.mem.eql(u8, name, "stdout")) {
                    return .{ .string = try .dupe(self.allocator, p.stdoutBytes(), .{}) };
                } else if (std.mem.eql(u8, name, "stderr")) {
                    return .{ .string = try .dupe(self.allocator, p.stderrBytes(), .{}) };
                } else if (std.mem.eql(u8, name, "exitCode")) {
                    return Error.UnsupportedMemberAccess;
                    // return .{ .integer = p.status.exit_code.? };
                } else {
                    return Error.MemberNotFound;
                }
            },
            .scope => |scope_ref| {
                const scope = try scope_ref.getPtr();
                const ref = try scope.lookup(name) orelse return Error.MemberNotFound;
                return ref.value.clone(self.allocator);
            },
        }
    }

    fn handleMemberAccessError(member: ast.MemberExpr, object: Value, err: Error) Error {
        switch (err) {
            Error.MemberNotFound => {
                switch (member.object.*) {
                    .identifier => |identifier| {
                        std.log.err("Member `{s}` not found in `{s} : {s}`.", .{ member.member.name, identifier.name, @tagName(object) });
                        if (object == .scope) {
                            try (try object.scope.getPtr()).logBindings(@src().fn_name, 0, 0);
                        }
                    },
                    else => {
                        std.log.err("Member `{s}` not found in {s}.", .{ member.member.name, @tagName(member.object.*) });
                    },
                }
                return err;
            },
            else => return err,
        }
    }

    fn evaluateAssignment(
        self: *Evaluator,
        scopes: *ScopeStack,
        assignment: ast.Assignment,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        const bindingRef = try scopes.lookup(assignment.identifier.name) orelse return error.UnknownIdentifier;

        if (bindingRef.is_mutable) {
            var newValue = try self.evaluateExpression(scopes, assignment.expr);
            try bindingRef.value.reassign(&newValue);
            return bindingRef.value.clone(self.allocator);
        } else {
            return error.ImmutableBinding;
        }
    }

    fn evaluateBinary(
        self: *Evaluator,
        scopes: *ScopeStack,
        binary: ast.BinaryExpr,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        switch (binary.op) {
            .add => {
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                if (left == .string and right == .string) {
                    defer left.string.deinit(.{});
                    defer right.string.deinit(.{});
                    return .{ .string = try .print(self.allocator, "{s}{s}", .{
                        try left.string.get(),
                        try right.string.get(),
                    }, .{}) };
                }

                if (left == .integer and right == .integer) {
                    return .{ .integer = left.integer +| right.integer };
                } else if (left == .float and right == .float) {
                    return .{ .float = left.float + right.float };
                } else if (left == .integer and right == .float) {
                    const float_left: Value.Float = @floatFromInt(left.integer);
                    return .{ .float = float_left + right.float };
                } else if (left == .float and right == .integer) {
                    const float_right: Value.Float = @floatFromInt(right.integer);
                    return .{ .float = left.float + float_right };
                }
            },
            .subtract => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left == .integer and right == .integer) {
                    return .{ .integer = left.integer -| right.integer };
                } else if (left == .float and right == .float) {
                    return .{ .float = left.float - right.float };
                } else if (left == .integer and right == .float) {
                    const float_left: Value.Float = @floatFromInt(left.integer);
                    return .{ .float = float_left - right.float };
                } else if (left == .float and right == .integer) {
                    const float_right: Value.Float = @floatFromInt(right.integer);
                    return .{ .float = left.float - float_right };
                }
            },
            .multiply => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left == .integer and right == .integer) {
                    return .{ .integer = left.integer *| right.integer };
                } else if (left == .float and right == .float) {
                    return .{ .float = left.float * right.float };
                } else if (left == .integer and right == .float) {
                    const float_left: Value.Float = @floatFromInt(left.integer);
                    return .{ .float = float_left * right.float };
                } else if (left == .float and right == .integer) {
                    const float_right: Value.Float = @floatFromInt(right.integer);
                    return .{ .float = left.float * float_right };
                }
            },
            .divide => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left == .integer and right == .integer) {
                    const float_left: Value.Float = @floatFromInt(left.integer);
                    const float_right: Value.Float = @floatFromInt(right.integer);
                    return .{ .float = float_left / float_right };
                } else if (left == .float and right == .float) {
                    return .{ .float = left.float / right.float };
                } else if (left == .integer and right == .float) {
                    const float_left: Value.Float = @floatFromInt(left.integer);
                    return .{ .float = float_left / right.float };
                } else if (left == .float and right == .integer) {
                    const float_right: Value.Float = @floatFromInt(right.integer);
                    return .{ .float = left.float / float_right };
                }
            },
            .remainder => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left == .integer and right == .integer) {
                    const float_left: Value.Float = @floatFromInt(left.integer);
                    const float_right: Value.Float = @floatFromInt(right.integer);
                    return .{ .float = @mod(float_left, float_right) };
                } else if (left == .float and right == .float) {
                    return .{ .float = @mod(left.float, right.float) };
                } else if (left == .integer and right == .float) {
                    const float_left: Value.Float = @floatFromInt(left.integer);
                    return .{ .float = @mod(float_left, right.float) };
                } else if (left == .float and right == .integer) {
                    const float_right: Value.Float = @floatFromInt(right.integer);
                    return .{ .float = @mod(left.float, float_right) };
                }
            },
            .greater => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value > right_value };
                }
            },
            .greater_equal => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value >= right_value };
                }
            },
            .less => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value < right_value };
                }
            },
            .less_equal => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value <= right_value };
                }
            },
            .equal => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value == right_value };
                }
            },
            .not_equal => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value != right_value };
                }
            },
            .logical_and => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left == .boolean and right == .boolean) {
                    return .{ .boolean = left.boolean and right.boolean };
                }
            },
            .logical_or => {
                const left = try self.evaluateExpression(scopes, binary.left);
                const right = try self.evaluateExpression(scopes, binary.right);

                if (left == .boolean and right == .boolean) {
                    return .{ .boolean = left.boolean or right.boolean };
                }
            },
        }

        return error.UnsupportedBinaryExpr;
    }

    fn evaluateIfExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        if_expr: ast.IfExpr,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        var condition_value = try self.evaluateExpression(scopes, if_expr.condition);
        const condition = try expectBoolean(&condition_value);

        if (condition) {
            const result = try self.runBlockInOwnContext(scopes, if_expr.then_block);
            return result;
        } else if (if_expr.else_branch) |else_branch| {
            const result = switch (else_branch) {
                .if_expr => |else_if_expr| try self.evaluateIfExpression(scopes, else_if_expr.*),
                .block => |else_expr| try self.runBlockInOwnContext(scopes, else_expr),
            };
            return result;
        }

        return .void;
    }

    fn evaluateForExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        for_expr: ast.ForExpr,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        // for (0..) |i| {...}
        //
        // for (items, 0..) |item, i| {...}

        var sources_values = try std.ArrayList(Value).initCapacity(
            self.allocator,
            for_expr.sources.len,
        );
        defer {
            for (sources_values.items) |*item| item.deinit();
            sources_values.deinit(self.allocator);
        }

        for (for_expr.sources) |expr| {
            const value = try self.evaluateExpression(scopes, expr);
            sources_values.appendAssumeCapacity(value);
        }

        const len = try validateForSourcesLengths(sources_values.items);
        var results: std.ArrayList(Value) = .empty;
        defer results.deinit(self.allocator);

        const bindings = for_expr.capture.bindings;

        var iterators = try ForSourcesIterator.init(self.allocator, len, sources_values.items);
        defer iterators.deinit(self.allocator);

        while (try iterators.next()) |iteration_elements| {
            defer for (iteration_elements) |*element| element.deinit();

            try scopes.pushFrame(@src().fn_name, try .initSingleNew(self.allocator));
            try declareForSources(scopes, iteration_elements, bindings);

            // TODO: be able to forward block result directly for streaming stdout etc
            const result = try self.runBlockInOwnContext(scopes, for_expr.body);
            try results.append(self.allocator, result);

            try scopes.popFrame(
                @src().fn_name,
            );
        }

        const array_result: Value.Array = try .init(self.allocator, .empty, .{});
        try (try array_result.getPtr()).appendSlice(self.allocator, results.items);

        return .{ .array = array_result };
    }

    const ValueIterator = union(enum) {
        range: RangeIterator,
        array: ArrayIterator,

        pub fn fromSource(allocator: std.mem.Allocator, value: Value) Error!ValueIterator {
            return switch (value) {
                .range => |range| .{ .range = RangeIterator.init(range) },
                .array => |array| .{ .array = try ArrayIterator.init(allocator, array) },
                else => Error.UnsupportedIterator,
            };
        }

        pub fn deinit(self: *ValueIterator) void {
            switch (self.*) {
                inline else => |*it| it.deinit(),
            }
        }

        pub fn next(self: *ValueIterator) !?Value {
            return try switch (self.*) {
                inline else => |*it| it.next(),
            };
        }
    };

    const RangeIterator = struct {
        range: Value.Range,
        index: ?usize = 0,

        pub fn init(range: Value.Range) RangeIterator {
            return .{
                .range = range,
            };
        }

        pub fn deinit(_: *RangeIterator) void {}

        pub fn next(self: *RangeIterator) !?Value {
            const index = self.index orelse return null;
            const len = try self.range.getLen();
            if (len) |l| if (index >= l) {
                self.index = null;
                return null;
            };
            defer {
                if (self.index) |*idx| idx.* += 1;
            }
            return .{ .integer = @as(i64, @intCast(index)) + self.range.start };
        }
    };

    const ArrayIterator = struct {
        allocator: std.mem.Allocator,
        array: Value.Array,
        index: ?usize = 0,

        pub fn init(allocator: std.mem.Allocator, array: Value.Array) Error!ArrayIterator {
            return .{
                .allocator = allocator,
                .array = try array.ref(.{ .label = "ArrayIterator.array" }),
            };
        }

        pub fn deinit(self: *ArrayIterator) void {
            self.array.deinit(.{});
        }

        pub fn next(self: *ArrayIterator) !?Value {
            const index = self.index orelse return null;
            const array_slice = try self.array.get();
            if (index >= array_slice.items.len) {
                self.index = null;
                return null;
            }
            defer {
                if (self.index) |*idx| idx.* += 1;
            }
            return try array_slice.items[index].clone(self.allocator);
        }
    };

    const ForSourcesIterator = struct {
        len: ?usize,
        sources: []Value,
        index: ?usize = 0,
        iterators: []ValueIterator,
        iteration_elements: []Value,

        pub fn init(allocator: std.mem.Allocator, len: ?usize, sources: []Value) !ForSourcesIterator {
            const iterators = try allocator.alloc(ValueIterator, sources.len);
            for (iterators, 0..) |*iterator, i| {
                iterator.* = try .fromSource(allocator, sources[i]);
            }

            return .{
                .len = len,
                .sources = sources,
                .iterators = iterators,
                .iteration_elements = try allocator.alloc(Value, sources.len),
            };
        }

        pub fn deinit(self: ForSourcesIterator, allocator: std.mem.Allocator) void {
            for (self.iterators) |*iterator| iterator.deinit();
            allocator.free(self.iterators);
            allocator.free(self.iteration_elements);
        }

        /// Consumer owns the values returned
        pub fn next(self: *ForSourcesIterator) !?[]Value {
            if (self.index == null) return null;

            for (self.iteration_elements, 0..) |*element, i| {
                element.* = try self.iterators[i].next() orelse {
                    self.index = null;
                    return null;
                };
            }

            return self.iteration_elements;
        }
    };

    fn declareForSources(
        scopes: *ScopeStack,
        iteration_elements: []Value,
        bindings: []const *ast.BindingPattern,
    ) Error!void {
        for (bindings, 0..) |binding, i| {
            switch (binding.*) {
                .identifier => |identifier| {
                    const value = &iteration_elements[i];
                    try scopes.declare(identifier.name, value, false);
                },
                .discard => {},
                else => return Error.UnsupportedBindingPattern,
            }
        }
    }

    fn validateForSourcesLengths(sources: []Value) Error!?usize {
        var expected_len: ?usize = null;

        for (sources) |value| {
            const len = try evaluateLength(value) orelse continue;
            expected_len = expected_len orelse len;
            if (expected_len != len) return Error.ForSourcesDifferentLengths;
        }

        return expected_len;
    }

    pub fn evaluateLength(value: Value) Error!?usize {
        return switch (value) {
            .array => |array| (try array.get()).items.len,
            .range => |range| try range.getLen(),
            else => return Error.CannotCoerceValueIntoLength,
        };
    }

    fn evaluateRangeExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        range: ast.RangeLiteral,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        var start_value = try self.evaluateExpression(scopes, range.start);
        const start = try expectInteger(&start_value);
        var result: Value = .{
            .range = .{
                .start = start,
                .end = null,
                .inclusive_end = range.inclusive_end,
            },
        };

        if (range.end) |expr| {
            var end_value = try self.evaluateExpression(scopes, expr);
            result.range.end = try expectInteger(&end_value);
        }

        return result;
    }

    fn evaluateArrayExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        array: ast.ArrayLiteral,
    ) Error!Value {
        var elements = try std.ArrayList(Value).initCapacity(self.allocator, array.elements.len);

        for (array.elements) |element| {
            elements.appendAssumeCapacity(try self.evaluateExpression(scopes, element));
        }

        return .{
            .array = try .init(
                self.allocator,
                .fromOwnedSlice(try elements.toOwnedSlice(self.allocator)),
                .{},
            ),
        };
    }

    fn expectBoolean(value: *Value) Error!bool {
        defer value.deinit();
        return (try expectType(value.*, .boolean)).boolean;
    }

    fn expectInteger(value: *Value) Error!Value.Integer {
        defer value.deinit();
        return (try expectType(value.*, .integer)).integer;
    }

    fn expectType(value: Value, expected: std.meta.Tag(Value)) Error!Value {
        if (value == expected) return value;
        std.log.err("TypeMismatch error: expected {s}, actual {s}", .{ @tagName(expected), @tagName(std.meta.activeTag(value)) });
        return Error.TypeMismatch;
    }

    fn getCachedDocument(self: *Evaluator, importer: []const u8, moduleName: []const u8) Error!*Document {
        const path = try resolveModulePath(self.allocator, importer, moduleName);
        defer self.allocator.free(path);
        return try self.document_store.requestDocument(path);
    }

    fn getCurrentDocument(self: *Evaluator) Error!*Document {
        return try self.document_store.requestDocument(self.path);
    }

    fn evaluatePipeline(
        self: *Evaluator,
        scopes: *ScopeStack,
        pipeline: ast.Pipeline,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);
        switch (pipeline.stages[0].payload) {
            .command => |command| {
                var name = try self.renderCommandPart(scopes, command.name);
                defer name.deinit(.{});
                try self.log("<{s}{s}>", .{
                    try name.get(),
                    if (pipeline.stages.len > 1) " ..." else "",
                });
            },
            .expression => try self.log("<expr>", .{}),
        }

        if (pipeline.stages.len == 0) return error.EmptyPipeline;

        var specs = ArrayListManaged(CommandRunner.CommandSpec).init(self.allocator);
        defer {
            for (specs.items) |spec| {
                for (spec.argv) |*arg| arg.deinit(.{});
                self.allocator.free(spec.argv);
            }
            specs.deinit();
        }

        for (pipeline.stages, 0..) |stage, i| {
            try self.log("processing stage[{}] information", .{i});
            if (stage.role != .command) return error.UnsupportedPipelineStage;
            const command = stage.payload.command;

            if (command.env_assignments.len > 0 or
                command.redirects.len > 0 or
                command.capture != null or
                command.background)
            {
                return error.UnsupportedCommandFeature;
            }

            var argv = ArrayListManaged(Value.String).init(self.allocator);
            defer {
                for (argv.items) |*arg| arg.deinit(.{});
                argv.deinit();
            }

            var name_slice = try self.renderCommandPart(scopes, command.name);
            try argv.append(name_slice);

            var command_type: command_runner.CommandRunner.CommandType = undefined;

            if (try scopes.lookup(try name_slice.get())) |b| {
                try self.log("found {s}", .{@tagName(b.value.*)});
                switch (b.value.*) {
                    .function => |fn_ref| {
                        command_type = .{ .function = fn_ref };
                    },
                    .boolean, .integer, .float, .string, .array, .process_handle => command_type = .{ .value = b.value },
                    else => {
                        try self.log("unsupported command type {s}", .{@tagName(b.value.*)});
                        return error.UnsupportedCommandType;
                    },
                }
            } else command_type = .executable;

            try self.log("command_type = {s}", .{@tagName(command_type)});

            switch (command_type) {
                .value => |value| {
                    if (command.args.len == 0 and pipeline.stages.len == 1) {
                        return try value.clone(self.allocator);
                    }
                },
                else => {},
            }

            for (command.args) |arg_part| {
                const arg_slice = try self.renderCommandPart(scopes, arg_part);
                try argv.append(arg_slice);
            }

            const spec_argv = try argv.toOwnedSlice();
            try specs.append(.{ .command_type = command_type, .argv = spec_argv });
        }

        try self.log("calling executor.run", .{});

        const handle = try self.executor.run(specs.items);
        return Value{ .process_handle = handle };
    }

    fn renderCommandPart(
        self: *Evaluator,
        scopes: *ScopeStack,
        part: ast.CommandPart,
    ) Error!Value.String {
        return switch (part) {
            .word => |word| try .dupe(self.allocator, word.text, .{}),
            .string => |literal| try self.renderStringLiteral(scopes, literal),
            .expr => |expr_ptr| blk: {
                var value = try self.evaluateExpression(scopes, expr_ptr);
                defer value.deinit();
                break :blk try self.materializeString(&value);
            },
        };
    }

    fn trackString(self: *Evaluator, owned_strings: *ArrayListManaged([]u8), buffer: []u8) ![]const u8 {
        _ = self;
        try owned_strings.append(buffer);
        return buffer;
    }

    fn renderStringLiteral(self: *Evaluator, scopes: *ScopeStack, literal: ast.StringLiteral) Error!Value.String {
        var buffer = ArrayListManaged(u8).init(self.allocator);
        errdefer buffer.deinit();

        for (literal.segments) |segment| {
            switch (segment) {
                .text => |t| try buffer.appendSlice(t.payload),
                .interpolation => |expr_ptr| {
                    var value = try self.evaluateExpression(scopes, expr_ptr);
                    defer value.deinit();
                    var rendered = try self.materializeString(&value);
                    defer rendered.deinit(.{});
                    try buffer.appendSlice(try rendered.get());
                },
            }
        }

        return .init(self.allocator, try buffer.toOwnedSlice(), .{});
    }

    fn materializeString(self: *Evaluator, value: *Value) Error!Value.String {
        return switch (value.*) {
            .string => |ref| try ref.ref(.{}),
            .boolean => |flag| try .dupe(self.allocator, if (flag) "1" else "0", .{}),
            .integer => |int| try .print(self.allocator, "{}", .{int}, .{}),
            .float => |flt| try .print(self.allocator, "{}", .{flt}, .{}),
            .process_handle => |p| try .dupe(self.allocator, p.stdoutBytes(), .{}),
            .void, .scope, .function, .array, .range => Error.InvalidStringCoercion,
        };
    }

    fn bindPattern(
        scopes: *ScopeStack,
        pattern: ast.BindingPattern,
        is_mutable: bool,
        value: *Value,
    ) Error!void {
        switch (pattern) {
            .identifier => |identifier| try scopes.declare(identifier.name, value, is_mutable),
            .discard => value.deinit(),
            else => return Error.UnsupportedBindingPattern,
        }
    }
};

pub const CommandExecutor = struct {
    context: *anyopaque,
    runFn: RunFn,

    pub const RunFn = *const fn (
        context: *anyopaque,
        specs: []const CommandRunner.CommandSpec,
    ) CommandRunner.Error!ProcessHandle;

    pub const Error = CommandRunner.Error;

    pub fn fromRunner(runner: *CommandRunner) CommandExecutor {
        return .{
            .context = runner,
            .runFn = runWithRunner,
        };
    }

    pub fn run(self: CommandExecutor, specs: []const CommandRunner.CommandSpec) CommandRunner.Error!ProcessHandle {
        return self.runFn(self.context, specs);
    }

    fn runWithRunner(context: *anyopaque, specs: []const CommandRunner.CommandSpec) CommandRunner.Error!ProcessHandle {
        const runner: *CommandRunner = @ptrCast(@alignCast(context));
        return runner.*.runPipeline(specs);
    }
};
