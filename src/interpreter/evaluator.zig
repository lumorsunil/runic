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
const rainbow = @import("../rainbow.zig");
const Stream = @import("../stream.zig").Stream;
const StreamError = @import("../stream.zig").StreamError;
const Transformer = @import("../stream.zig").Transformer;

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
    loose_ends_arena: std.heap.ArenaAllocator,
    path: []const u8,
    /// Refers to the outermost context
    executeOptions: ScriptExecutor.ExecuteOptions,
    document_store: *DocumentStore,
    logging_enabled: bool,

    pub const Error = std.mem.Allocator.Error ||
        ScopeStack.Error ||
        DocumentStore.Error ||
        Value.Error ||
        StreamError ||
        std.fmt.ParseIntError ||
        std.fmt.ParseFloatError ||
        std.fs.Dir.RealPathAllocError ||
        std.fs.File.OpenError ||
        std.Io.Reader.ShortError ||
        std.Io.Reader.StreamError ||
        std.Io.Writer.Error ||
        std.process.Child.SpawnError ||
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
            UnsupportedCallee,
            ImmutableBinding,
            ArgvNotDefinedInExecutableCall,
        };

    pub const Context = struct {
        executeOptions: ScriptExecutor.ExecuteOptions,
        scopes: *ScopeStack,
        is_streaming: bool,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        path: []const u8,
        executeOptions: ScriptExecutor.ExecuteOptions,
        documentStore: *DocumentStore,
    ) Evaluator {
        const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_" ++ logging_name) catch null;
        defer if (logging_enabled_s) |le| allocator.free(le);
        const logging_enabled = if (logging_enabled_s) |le| std.mem.eql(u8, le, "1") else false;

        return .{
            .allocator = allocator,
            .loose_ends_arena = .init(allocator),
            .path = path,
            .executeOptions = executeOptions,
            .document_store = documentStore,
            .logging_enabled = logging_enabled,
        };
    }

    pub fn deinit(self: *Evaluator) void {
        self.loose_ends_arena.deinit();
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

    fn declareFunctionArgs(
        self: *Evaluator,
        scopes: *ScopeStack,
        fn_decl: *const ast.FunctionDecl,
        args: []Value,
    ) Error!void {
        switch (fn_decl.params) {
            ._non_variadic => |params| {
                for (params, 0..) |param, i| {
                    var value = try args[i].clone(self.allocator);
                    try bindPattern(
                        scopes,
                        param.pattern.*,
                        false,
                        &value,
                    );
                }
            },
            ._variadic => |param| {
                var argv = try std.ArrayList(Value).initCapacity(self.allocator, args.len + 1);
                argv.appendAssumeCapacity(
                    .{ .string = try .dupe(self.allocator, fn_decl.name.name, .{}) },
                );
                for (args) |arg| argv.appendAssumeCapacity(try arg.clone(self.allocator));
                var value: Value = .{ .array = try .init(self.allocator, argv, .{}) };
                try bindPattern(
                    scopes,
                    param.pattern.*,
                    false,
                    &value,
                );
            },
        }
    }

    pub const RunFunctionResult = struct {
        stdout: []u8,
        stderr: []u8,
        started_at_ns: i128,
        finished_at_ns: i128,
    };

    pub fn runFunction(
        self: *Evaluator,
        scopes: *ScopeStack,
        fn_ref_ref: Value.FunctionRef,
        args: []Value,
    ) Error!RunFunctionResult {
        const fn_ref = try fn_ref_ref.getPtr();
        const number_of_frames: usize = if (fn_ref.closure == null) 2 else 3;
        try scopes.pushFrame(@src().fn_name, .initBlocking());
        if (fn_ref.closure) |closure| try scopes.pushFrame(@src().fn_name, .initScopeRefSpecial(closure));
        try scopes.pushFrame(@src().fn_name, try .initSingleNew(self.allocator));
        errdefer scopes.popFrameN(@src().fn_name, number_of_frames) catch {
            std.log.err(@src().fn_name ++ ": Could not pop the frames.", .{});
        };

        var fn_ref_value = Value{ .function = try fn_ref_ref.ref(.{}) };
        try scopes.declare(fn_ref.fn_decl.name.name, &fn_ref_value, false);

        try self.declareFunctionArgs(scopes, fn_ref.fn_decl, args);

        const started_at = std.time.nanoTimestamp();

        // TODO: What to do with this value?
        var value = try self.evaluateExpression(scopes, fn_ref.fn_decl.body);
        value.deinit();

        const finished_at = std.time.nanoTimestamp();

        try scopes.popFrameN(@src().fn_name, number_of_frames);

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

        for (block.statements) |statement| {
            // TODO: do something with these values?
            if (try self.runStatement(scopes, statement)) |value| {
                var owned = value;
                owned.deinit();
            }
        }

        try scopes.popFrame(
            @src().fn_name,
        );

        return .void;
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
            .type_binding_decl => null,
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

        const v = try self.evaluateExpression(scopes, expression);
        var value_as_stream = try Stream([]const u8).fromValue(self.allocator, v);
        defer value_as_stream.deinit();

        try self.log("forwardStringStream: <{t}>\n", .{expression.*});
        try self.logEvaluateExpression(expression);
        try self.forwardStringStream(scopes, value_as_stream);

        return v;
    }

    fn forwardStringStream(
        self: *Evaluator,
        scopes: *ScopeStack,
        stream: *Stream([]const u8),
    ) Error!void {
        const stdout = scopes.getStdoutPipe();
        const writer = stdout.writer orelse return;

        while (stream.next() catch |err| {
            try self.log("error reading stream: {}", .{err});
            return;
        }) |e| {
            switch (e) {
                .next => |string| try writer.writeAll(string),
                .completed => break,
            }

            if (stdout.is_streaming == .streaming) {
                try writer.flush();
            }
        }
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

        try scopes.pushFrameForwarding(@src().fn_name, .{
            .stdin = .blocked(),
            .stdout = .init(stdout, .non_streaming),
            .stderr = .init(stderr, .non_streaming),
        });

        var value = try self.evaluateExpression(scopes, decl.initializer);

        try scopes.popFrame(
            @src().fn_name,
        );

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

        return switch (expr.*) {
            .literal => |literal| try self.evaluateLiteral(scopes, literal),
            .identifier => |identifier| try self.evaluateIdentifier(scopes, identifier),
            // .pipeline => |pipeline| try self.evaluatePipeline(scopes, pipeline),
            .block => |block_expr| try self.evaluateBlockExpression(scopes, block_expr),
            .import_expr => |import_expr| try self.evaluateImportExpression(scopes, import_expr),
            .member => |member_expr| try self.evaluateMemberExpression(scopes, member_expr),
            .assignment => |assignment| try self.evaluateAssignment(scopes, assignment),
            .binary => |binary| try self.evaluateBinary(scopes, binary),
            .if_expr => |if_expr| try self.evaluateIfExpression(scopes, if_expr),
            .for_expr => |for_expr| try self.evaluateForExpression(scopes, for_expr),
            .range => |range| try self.evaluateRangeExpression(scopes, range),
            .array => |array| try self.evaluateArrayExpression(scopes, array),
            .call => |call| try self.evaluateCallExpression(scopes, call),
            .executable => |executable| try self.evaluateExecutableExpression(scopes, executable),
            .path, .map, .index, .unary, .fn_literal, .match_expr, .try_expr, .catch_expr, .pipeline => return error.UnsupportedExpression,
        };
    }

    fn logEvaluateExpression(self: *Evaluator, expr: *const ast.Expression) !void {
        try self.logEvaluateSpan(expr.span());
    }

    fn logEvaluateSpan(self: *Evaluator, span: ast.Span) !void {
        if (@hasField(@This(), "logging_enabled")) {
            if (!self.logging_enabled) return;
        }

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

    fn createExecutableFnDecl(
        self: *Evaluator,
        identifier: ast.Identifier,
    ) Error!*ast.FunctionDecl {
        const fn_decl = try self.allocator.create(ast.FunctionDecl);

        const span = identifier.span;

        const body = try self.loose_ends_arena.allocator().create(ast.Expression);
        body.* = .{ .executable = .{
            .span = span,
        } };

        const pattern = try self.loose_ends_arena.allocator().create(ast.BindingPattern);
        pattern.* = .{ .identifier = .{
            .name = try self.loose_ends_arena.allocator().dupe(u8, "argv"),
            .span = span,
        } };

        const byte_type = try self.loose_ends_arena.allocator().create(ast.TypeExpr);
        byte_type.* = .{ .byte = .{
            .span = span,
        } };

        const string_type = try self.loose_ends_arena.allocator().create(ast.TypeExpr);
        string_type.* = .{ .array = .{
            .element = byte_type,
            .span = span,
        } };

        const parameter_type = try self.loose_ends_arena.allocator().create(ast.TypeExpr);
        parameter_type.* = .{ .array = .{
            .element = string_type,
            .span = span,
        } };

        const parameter = try self.loose_ends_arena.allocator().create(ast.Parameter);
        parameter.* = .{
            .pattern = pattern,
            .type_annotation = parameter_type,
            .default_value = null,
            .is_mutable = false,
            .span = span,
        };

        fn_decl.* = .{
            .name = identifier,
            .span = identifier.span,
            .is_async = false,
            .params = .variadic(parameter),
            .stdin_type = null, // TODO: set this to ?Stream(String)
            .return_type = null, // TODO: set this to Stream(Byte!String)
            .body = body,
        };

        return fn_decl;
    }

    fn evaluateIdentifier(
        self: *Evaluator,
        scopes: *ScopeStack,
        identifier: ast.Identifier,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);
        // TODO: add safety for unknown identifiers as executables (explicit executables?)
        // backup: const binding = try scopes.lookup(identifier.name) orelse return error.UnknownIdentifier;
        const binding = try scopes.lookup(identifier.name) orelse return .{
            .function = try .init(
                self.loose_ends_arena.allocator(),
                .{
                    .fn_decl = try self.createExecutableFnDecl(identifier),
                    .closure = null,
                },
                .{},
            ),
        };
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
            .void, .boolean, .integer, .float, .string, .function, .array, .range, .stream => return Error.UnsupportedMemberAccess,
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

                defer left.deinit();
                defer right.deinit();

                if (left == .string and right == .string) {
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
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

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
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

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
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

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
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

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
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value > right_value };
                }
            },
            .greater_equal => {
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value >= right_value };
                }
            },
            .less => {
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value < right_value };
                }
            },
            .less_equal => {
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value <= right_value };
                }
            },
            .equal => {
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value == right_value };
                } else if (left == .string and right == .string) {
                    const l = try left.string.get();
                    const r = try right.string.get();

                    return .{ .boolean = std.mem.eql(u8, l, r) };
                }
            },
            .not_equal => {
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

                if (left.isNumeric() and right.isNumeric()) {
                    const left_value: Value.Float = if (left == .integer) @floatFromInt(left.integer) else left.float;
                    const right_value: Value.Float = if (right == .integer) @floatFromInt(right.integer) else right.float;
                    return .{ .boolean = left_value != right_value };
                }
            },
            .logical_and => {
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

                if (left == .boolean and right == .boolean) {
                    return .{ .boolean = left.boolean and right.boolean };
                }
            },
            .logical_or => {
                var left = try self.evaluateExpression(scopes, binary.left);
                var right = try self.evaluateExpression(scopes, binary.right);

                defer left.deinit();
                defer right.deinit();

                if (left == .boolean and right == .boolean) {
                    return .{ .boolean = left.boolean or right.boolean };
                }
            },
            .apply => {
                try self.log(@src().fn_name ++ ": error, encoutered apply binary expression", .{});
                try self.logEvaluateSpan(binary.span);
            },
            .pipe => {},
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

        // TODO: handle captures

        if (condition) {
            const result = try self.evaluateExpression(scopes, if_expr.then_expr);
            return result;
        } else if (if_expr.else_branch) |else_branch| {
            const result = switch (else_branch) {
                .if_expr => |else_if_expr| try self.evaluateIfExpression(scopes, else_if_expr.*),
                .expr => |else_expr| try self.evaluateExpression(scopes, else_expr),
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
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

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

    fn evaluateCallExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        call: ast.CallExpr,
    ) Error!Value {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logEvaluationTrace(@src().fn_name);

        const callee = try self.evaluateExpression(scopes, call.callee);
        var args = try std.ArrayList(Value).initCapacity(self.allocator, call.arguments.len);
        defer {
            for (args.items) |*arg| arg.deinit();
            args.deinit(self.allocator);
        }
        for (call.arguments) |arg| {
            args.appendAssumeCapacity(try self.evaluateExpression(scopes, arg));
        }

        switch (callee) {
            .function => |fn_ref| _ = try self.runFunction(scopes, fn_ref, args.items),
            else => return error.UnsupportedCallee,
        }

        return .void;
    }

    const OptionalFileReader = struct {
        buffer: [512]u8 = undefined,
        file: ?std.fs.File,
        file_reader: ?std.fs.File.Reader = null,
        reader: *std.Io.Reader = undefined,
        empty_reader: std.Io.Reader = std.Io.Reader.fixed(&.{}),

        pub fn init(file: ?std.fs.File) OptionalFileReader {
            return .{
                .file = file,
            };
        }

        pub fn connect(self: *OptionalFileReader) *std.Io.Reader {
            self.file_reader = if (self.file) |f| f.readerStreaming(&self.buffer) else null;
            self.reader = if (self.file_reader) |*fr| &fr.interface else &self.empty_reader;
            return self.reader;
        }
    };

    const OptionalFileWriter = struct {
        buffer: [512]u8 = undefined,
        file: ?std.fs.File,
        file_writer: ?std.fs.File.Writer = null,
        writer: *std.Io.Writer = undefined,
        discarding_writer: std.Io.Writer.Discarding = .init(&.{}),

        pub fn init(file: ?std.fs.File) OptionalFileWriter {
            return .{
                .file = file,
            };
        }

        pub fn connect(self: *OptionalFileWriter) *std.Io.Writer {
            self.file_writer = if (self.file) |f| f.writerStreaming(&self.buffer) else null;
            self.writer = if (self.file_writer) |*fr| &fr.interface else &self.discarding_writer.writer;
            return self.writer;
        }
    };

    /// Returns true if stream was closed
    fn forwardStream(reader: *std.Io.Reader, writer: *std.Io.Writer) Error!bool {
        const bytes_streamed = reader.stream(
            writer,
            .unlimited,
        ) catch |err| switch (err) {
            std.Io.Reader.StreamError.EndOfStream => return true,
            else => return err,
        };
        if (bytes_streamed > 0) {
            try writer.flush();
        }

        return false;
    }

    fn getContextArgv(self: *Evaluator, scopes: *ScopeStack) Error![]const []const u8 {
        const argv = try scopes.lookup("argv") orelse return Error.ArgvNotDefinedInExecutableCall;
        const argv_list = try argv.value.array.get();
        const argv_strings = try self.allocator.alloc([]const u8, argv_list.items.len);
        for (argv_list.items, 0..) |argv_item, i| {
            var string_ref = try self.materializeString(argv_item);
            defer string_ref.deinit(.{});
            argv_strings[i] = try self.allocator.dupe(u8, try string_ref.get());
        }

        return argv_strings;
    }

    pub const Execution = struct {
        started_at_ns: i128,
        finished_at_ns: i128,
        exit_code: command_runner.ExitCode,
    };

    fn evaluateExecutableExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        _: ast.ExecutableExpr,
    ) Error!Value {
        errdefer self.log(@src().fn_name ++ ": error", .{}) catch {};
        try self.log(@src().fn_name, .{});

        const argv = try self.getContextArgv(scopes);
        defer {
            for (argv) |a| self.allocator.free(a);
            self.allocator.free(argv);
        }
        var stdin = scopes.getStdinPipe().reader;
        const stdout = scopes.getStdoutPipe().writer;
        const stderr = scopes.getStderrPipe().writer;
        var cwd = try scopes.getCwd();
        defer cwd.deinit(.{});
        const env_map = try scopes.getEnvMap();
        var std_env_map = try env_map.toStdEnvMap();
        defer std_env_map.deinit();

        try self.log(@src().fn_name, .{});
        try self.logWithoutPrefix("child init\n", .{});
        try self.logWithoutPrefix("cwd: {s}\n", .{try cwd.get()});
        try self.logWithoutPrefix("stdin: {s}, stdout: {s}, stderr: {s}\n", .{
            if (stdin) |_| "forwarding" else "blocking",
            if (stdout) |_| "forwarding" else "blocking",
            if (stderr) |_| "forwarding" else "blocking",
        });
        var child = std.process.Child.init(argv, self.allocator);
        child.stdin_behavior = if (stdin != null) .Pipe else .Ignore;
        child.stdout_behavior = if (stdout != null) .Pipe else .Ignore;
        child.stderr_behavior = if (stderr != null) .Pipe else .Ignore;
        child.cwd = try cwd.get();
        child.env_map = &std_env_map;

        try self.logWithoutPrefix("child spawn\n", .{});
        const started_at = std.time.nanoTimestamp();
        child.spawn() catch |err| switch (err) {
            error.FileNotFound => {
                std.log.err("command `{s}` not found", .{argv[0]});
                return err;
            },
            else => {
                std.log.err("error when trying to run command `{s}`: {}", .{ argv[0], err });
                return err;
            },
        };
        errdefer {
            _ = child.kill() catch {};
        }

        // TODO: Implement streaming input
        // if (args.stdin_data) |input| {
        //     var stdin_file = child.stdin orelse unreachable;
        //     defer {
        //         stdin_file.close();
        //         child.stdin = null;
        //     }
        //     try stdin_file.writeAll(input);
        // }

        var child_stdin_writer = OptionalFileWriter.init(child.stdin);
        const stdin_writer = child_stdin_writer.connect();

        var child_stdout_reader = OptionalFileReader.init(child.stdout);
        const stdout_reader = child_stdout_reader.connect();

        var child_stderr_reader = OptionalFileReader.init(child.stderr);
        const stderr_reader = child_stderr_reader.connect();

        var stdin_closed = stdin == null;
        var stdout_closed = stdout == null;
        var stderr_closed = stderr == null;

        while (true) {
            if (child.term != null) break;
            if (stdin) |_stdin| stdin_closed = try forwardStream(_stdin, stdin_writer);
            if (stdin_closed) {
                if (child_stdin_writer.file) |f| {
                    f.close();
                    child_stdin_writer.file = null;
                    stdin = null;
                }
            }
            if (stdout) |_stdout| stdout_closed = try forwardStream(stdout_reader, _stdout);
            if (stderr) |_stderr| stderr_closed = try forwardStream(stderr_reader, _stderr);

            if (stdin_closed and stdout_closed and stderr_closed) {
                break;
            }
        }

        const term = child.wait() catch |err| switch (err) {
            error.FileNotFound => {
                std.log.err("command `{s}` not found", .{argv[0]});
                return err;
            },
            else => {
                std.log.err("error when trying to run command `{s}`: {}", .{ argv[0], err });
                return err;
            },
        };
        const finished_at = std.time.nanoTimestamp();

        _ = started_at;
        _ = finished_at;
        _ = term;

        return .void;

        // return Execution{
        //     .started_at_ns = started_at,
        //     .finished_at_ns = finished_at,
        //     .exit_code = .fromTerm(term),
        // };
    }

    // fn runStageExecutable(
    //     self: *CommandRunner,
    //     scopes: *ScopeStack,
    //     args: *StageRunArgs,
    //     specs: []const CommandSpec,
    // ) Error!StageExecution {
    // }

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

    fn renderStringLiteral(
        self: *Evaluator,
        scopes: *ScopeStack,
        literal: ast.StringLiteral,
    ) Error!Value.String {
        var buffer = ArrayListManaged(u8).init(self.allocator);
        errdefer buffer.deinit();

        for (literal.segments) |segment| {
            switch (segment) {
                .text => |t| try decodeString(&buffer, t.payload),
                .interpolation => |expr_ptr| {
                    var value = try self.evaluateExpression(scopes, expr_ptr);
                    defer value.deinit();
                    var rendered = try self.materializeString(value);
                    defer rendered.deinit(.{});
                    try buffer.appendSlice(try rendered.get());
                },
            }
        }

        return .init(self.allocator, try buffer.toOwnedSlice(), .{});
    }

    fn decodeString(dynamic_string: *ArrayListManaged(u8), encoded_string: []const u8) !void {
        var i: usize = 0;
        while (i < encoded_string.len) : (i += 1) {
            const rest = encoded_string[i..];
            const ch = encoded_string[i];

            if (rest.len < 2) {
                try dynamic_string.append(ch);
                break;
            }

            if (std.mem.eql(u8, rest[0..2], "\\n")) {
                try dynamic_string.append('\n');
                i += 1;
                continue;
            } else if (std.mem.eql(u8, rest[0..2], "\\t")) {
                try dynamic_string.append('\t');
                i += 1;
                continue;
            } else if (std.mem.eql(u8, rest[0..2], "\\r")) {
                try dynamic_string.append('\r');
                i += 1;
                continue;
            }

            try dynamic_string.append(ch);
        }
    }

    fn materializeString(self: *Evaluator, value: Value) Error!Value.String {
        return switch (value) {
            .string => |ref| try ref.ref(.{}),
            .boolean => |flag| try .dupe(self.allocator, if (flag) "1" else "0", .{}),
            .integer => |int| try .print(self.allocator, "{}", .{int}, .{}),
            .float => |flt| try .print(self.allocator, "{}", .{flt}, .{}),
            .stream => |s| {
                const stream_allocator = try s.getAllocator();
                const stream_result = try s.takeAll();
                defer {
                    for (stream_result.list) |*v| v.deinit();
                    stream_allocator.free(stream_result.list);
                }

                if (stream_result.err) |err| {
                    return err;
                }

                var result = std.Io.Writer.Allocating.init(self.allocator);

                for (stream_result.list) |item| {
                    var item_s_ref = try self.materializeString(item);
                    defer item_s_ref.deinit(.{});
                    const item_s = try item_s_ref.get();
                    try result.writer.writeAll(item_s);
                }

                return .init(self.allocator, result.written(), .{});
            },
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
