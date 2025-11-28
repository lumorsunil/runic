const std = @import("std");
const ast = @import("../frontend/ast.zig");
const token = @import("../frontend/token.zig");
const command_runner = @import("../runtime/command_runner.zig");
const Value = @import("value.zig").Value;
const ScopeStack = @import("scope.zig").ScopeStack;
const Parser = @import("../frontend/document_store.zig").Parser;
const DocumentStore = @import("../frontend/document_store.zig").DocumentStore;
const Document = @import("../frontend/document_store.zig").Document;
const resolveModulePath = @import("../frontend/document_store.zig").resolveModulePath;
const ScriptExecutor = @import("script_executor.zig").ScriptExecutor;
const ScriptContext = @import("script_context.zig").ScriptContext;

const CommandRunner = command_runner.CommandRunner;
const ProcessHandle = command_runner.ProcessHandle;

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
    executeOptions: ScriptExecutor.ExecuteOptions,
    documentStore: *DocumentStore,

    pub const Error = std.mem.Allocator.Error ||
        ScopeStack.Error ||
        CommandExecutor.Error ||
        ScriptContext.BindingError ||
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
            MemberNotFound,
            UnknownIdentifier,
            EmptyPipeline,
            InvalidStringCoercion,
            DocumentNotParsed,
        };

    pub const BlockOptions = struct {
        push_scope: bool = true,
        capture_result: bool = false,
    };

    pub const BlockResult = struct {
        last_value: ?Value = null,

        pub fn take(self: *BlockResult) ?Value {
            const snapshot = self.last_value;
            self.last_value = null;
            return snapshot;
        }

        pub fn deinit(self: *BlockResult, allocator: std.mem.Allocator) void {
            if (self.last_value) |*value| {
                value.deinit(allocator);
                self.last_value = null;
            }
        }
    };

    pub fn init(
        allocator: std.mem.Allocator,
        path: []const u8,
        executor: CommandExecutor,
        executeOptions: ScriptExecutor.ExecuteOptions,
        documentStore: *DocumentStore,
    ) Evaluator {
        return .{
            .allocator = allocator,
            .path = path,
            .executor = executor,
            .executeOptions = executeOptions,
            .documentStore = documentStore,
        };
    }

    pub fn initWithRunner(
        allocator: std.mem.Allocator,
        runner: *CommandRunner,
        documentStore: *DocumentStore,
    ) Evaluator {
        return init(allocator, CommandExecutor.fromRunner(runner), documentStore);
    }

    /// Executes the provided block in the existing scope (callers should ensure
    /// a frame has already been pushed) and ignores the final expression value
    /// if present.
    pub fn runBlock(self: *Evaluator, scopes: *ScopeStack, block: ast.Block) Error!void {
        var result = try self.executeBlock(scopes, block, .{ .push_scope = false, .capture_result = false });
        defer result.deinit(self.allocator);
        _ = result.take();
    }

    pub fn runFunction(
        self: *Evaluator,
        scopes: *ScopeStack,
        fn_decl: *const ast.FunctionDecl,
    ) Error!Value {
        try scopes.pushFrame(.{ .blocking = true });
        defer scopes.popFrame() catch {};

        const document = self.documentStore.map.get(fn_decl.span.start.file).?;
        const executor = &document.script_executor.?;
        const currentDocument = self.documentStore.map.get(self.path).?;

        try executor.scopes.pushFrame(.{});

        _ = try executor.execute(.{
            .statements = fn_decl.body.block.statements,
            .span = fn_decl.span,
        }, .{
            .script_path = document.path,
            .stdout = currentDocument.script_executor.?.evaluator.executeOptions.stdout,
            .stderr = currentDocument.script_executor.?.evaluator.executeOptions.stderr,
            .context = undefined,
        });

        try executor.scopes.popFrame();

        return .void;

        // switch (fn_decl.body) {
        //     .block => |block| {
        //         try self.runBlock(scopes, block);
        //         return .void;
        //     },
        //     .expression => |expr| {
        //         const expr_stmt = ast.Statement{ .expression = .{
        //             .expression = expr,
        //             .span = expr.span(),
        //         } };
        //         return try self.runStatement(scopes, &expr_stmt) orelse return .void;
        //     },
        // }
    }

    pub fn runFunctionAsPipeline(
        self: *Evaluator,
        scopes: *ScopeStack,
        fn_decl: *const ast.FunctionDecl,
    ) Error!Value {
        const pipeline = ast.Pipeline{
            .stages = &.{
                ast.PipelineStage{
                    .role = .command,
                    .payload = .{
                        .command = .{
                            .span = fn_decl.span,
                            .name = .{ .word = .{ .text = fn_decl.name.name, .span = fn_decl.name.span } },
                            // TODO: fix this when implementing function arguments
                            .args = &.{},
                            // TODO: fix this when implementing background support for functions
                            .background = false,
                            // TODO: fix this when implementing captures for functions
                            .capture = null,
                            // TODO: ???
                            .env_assignments = &.{},
                            // TODO: fix this when implementing redirections for functions
                            .redirects = &.{},
                        },
                    },
                    .span = fn_decl.span,
                },
            },
            .span = fn_decl.span,
        };

        return try self.evaluatePipeline(scopes, pipeline);
    }

    /// Executes a single statement in the current scope and returns its result
    /// if the statement produces a value (expression statements).
    pub fn runStatement(
        self: *Evaluator,
        scopes: *ScopeStack,
        statement: *const ast.Statement,
    ) Error!?Value {
        return self.executeStatement(scopes, statement);
    }

    fn executeBlock(
        self: *Evaluator,
        scopes: *ScopeStack,
        block: ast.Block,
        options: BlockOptions,
    ) Error!BlockResult {
        var result = BlockResult{};

        var scope_active = false;
        if (options.push_scope) {
            try scopes.pushFrame(.{});
            scope_active = true;
        }
        errdefer if (scope_active) scopes.popFrame() catch {};

        for (block.statements) |stmt_ptr| {
            const maybe_value = try self.executeStatement(scopes, stmt_ptr);
            if (maybe_value) |value| {
                var owned = value;
                var consumed = false;
                defer if (!consumed) owned.deinit(self.allocator);

                if (options.capture_result) {
                    self.storeLastValue(&result, &owned);
                    consumed = true;
                }
            }
        }

        if (scope_active) {
            try scopes.popFrame();
            scope_active = false;
        }

        return result;
    }

    fn executeStatement(
        self: *Evaluator,
        scopes: *ScopeStack,
        statement: *const ast.Statement,
    ) Error!?Value {
        return switch (statement.*) {
            .binding_decl => |decl| blk: {
                try self.executeLet(scopes, &decl);
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
        return try self.evaluateExpression(scopes, expression);
    }

    fn executeLet(self: *Evaluator, scopes: *ScopeStack, decl: *const ast.BindingDecl) Error!void {
        var value = try self.evaluateExpression(scopes, decl.initializer);
        var consumed = false;
        defer if (!consumed) value.deinit(self.allocator);

        try self.bindPattern(scopes, decl.pattern.*, decl.is_mutable, &value);
        consumed = true;
    }

    fn executeFnDecl(
        self: *Evaluator,
        scopes: *ScopeStack,
        decl: *const ast.FunctionDecl,
    ) Error!void {
        var value = Value{ .function = decl };
        try self.bindPattern(scopes, .{ .identifier = decl.name }, false, &value);
    }

    fn evaluateExpression(self: *Evaluator, scopes: *ScopeStack, expr: *const ast.Expression) Error!Value {
        const v = switch (expr.*) {
            .literal => |literal| try self.evaluateLiteral(scopes, literal),
            .identifier => |identifier| try self.evaluateIdentifier(scopes, identifier),
            .pipeline => |pipeline| try self.evaluatePipeline(scopes, pipeline),
            .block => |block_expr| try self.evaluateBlockExpression(scopes, block_expr),
            .import_expr => |import_expr| try self.evaluateImportExpression(import_expr),
            .member => |member_expr| try self.evaluateMemberExpression(scopes, member_expr),
            else => return Error.UnsupportedExpression,
        };

        return switch (v) {
            .function => |f| try self.runFunctionAsPipeline(scopes, f),
            else => v,
        };
    }

    fn evaluateLiteral(self: *Evaluator, scopes: *ScopeStack, literal: ast.Literal) Error!Value {
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
        const binding = scopes.lookup(identifier.name) orelse return Error.UnknownIdentifier;
        return try binding.value.clone(self.allocator);
    }

    fn evaluateBlockExpression(self: *Evaluator, scopes: *ScopeStack, block: ast.Block) Error!Value {
        var result = try self.executeBlock(scopes, block, .{ .push_scope = true, .capture_result = true });
        defer result.deinit(self.allocator);
        if (result.take()) |value| {
            return value;
        }
        return Value{ .void = {} };
    }

    fn evaluateImportExpression(
        self: *Evaluator,
        import: ast.ImportExpr,
    ) Error!Value {
        const document = try self.getCachedDocument(import.importer, import.module_name);

        if (document.script_executor) |*script_executor| {
            return .{ .scope = &script_executor.scopes };
        }

        const script_ast = document.ast orelse return Error.DocumentNotParsed;

        const currentDocument = try self.documentStore.requestDocument(import.importer);
        const currentExecutor = currentDocument.script_executor;

        var stdout = std.fs.File.stdout().writer(&.{});
        var stderr = std.fs.File.stderr().writer(&.{});

        const context = try self.allocator.create(ScriptContext);
        context.* = ScriptContext.init(self.allocator);
        defer self.allocator.destroy(context);

        const executeOptions = ScriptExecutor.ExecuteOptions{
            .script_path = document.path,
            .stdout = &stdout.interface,
            .stderr = &stderr.interface,
            .context = context,
        };

        document.script_executor = try ScriptExecutor.initWithRunner(
            self.allocator,
            document.path,
            currentExecutor.?.command_bridge.?.runner,
            currentExecutor.?.command_bridge.?.env_map,
            executeOptions,
            self.documentStore,
        );
        document.script_executor.?.wireCommandBridge();

        const executor = &document.script_executor.?;

        try executor.scopes.pushFrame(.{});

        const exitCode = try executor.execute(script_ast, executeOptions);

        document.exitCode = exitCode;

        return .{ .scope = &executor.scopes };
    }

    fn evaluateMemberExpression(
        self: *Evaluator,
        scopes: *ScopeStack,
        member: ast.MemberExpr,
    ) Error!Value {
        var object = try self.evaluateExpression(scopes, member.object);
        defer object.deinit(self.allocator);

        switch (object) {
            .void, .boolean, .integer, .float, .string, .function => return Error.UnsupportedMemberAccess,
            .process_handle => |p| {
                if (std.mem.eql(u8, member.member.name, "stdout")) {
                    return .{ .string = try self.allocator.dupe(u8, p.stdoutBytes()) };
                } else if (std.mem.eql(u8, member.member.name, "stderr")) {
                    return .{ .string = try self.allocator.dupe(u8, p.stderrBytes()) };
                } else if (std.mem.eql(u8, member.member.name, "exitCode")) {
                    return Error.UnsupportedMemberAccess;
                    // return .{ .integer = p.status.exit_code.? };
                } else {
                    return Error.MemberNotFound;
                }
            },
            .scope => |s| {
                const ref = s.lookup(member.member.name) orelse return Error.MemberNotFound;
                return ref.value.clone(self.allocator);
            },
        }
    }

    fn getCachedDocument(self: *Evaluator, importer: []const u8, moduleName: []const u8) Error!*Document {
        const path = try resolveModulePath(self.allocator, importer, moduleName);
        defer self.allocator.free(path);
        return try self.documentStore.requestDocument(path);
    }

    fn evaluatePipeline(self: *Evaluator, scopes: *ScopeStack, pipeline: ast.Pipeline) Error!Value {
        if (pipeline.stages.len == 0) return Error.EmptyPipeline;

        var specs = ArrayListManaged(CommandRunner.CommandSpec).init(self.allocator);
        defer {
            for (specs.items) |spec| self.allocator.free(spec.argv);
            specs.deinit();
        }

        var owned_strings = ArrayListManaged([]u8).init(self.allocator);
        defer {
            for (owned_strings.items) |buffer| self.allocator.free(buffer);
            owned_strings.deinit();
        }

        for (pipeline.stages) |stage| {
            if (stage.role != .command) return Error.UnsupportedPipelineStage;
            const command = stage.payload.command;

            if (command.env_assignments.len > 0 or
                command.redirects.len > 0 or
                command.capture != null or
                command.background)
            {
                return Error.UnsupportedCommandFeature;
            }

            var argv = ArrayListManaged([]const u8).init(self.allocator);
            defer argv.deinit();

            const name_slice = try self.renderCommandPart(scopes, command.name, &owned_strings);
            try argv.append(name_slice);

            var command_type: command_runner.CommandRunner.CommandType = undefined;

            if (scopes.lookup(name_slice)) |b| {
                switch (b.value.*) {
                    .function => |fn_decl| command_type = .{ .function = fn_decl },
                    else => command_type = .executable,
                }
            } else command_type = .executable;

            for (command.args) |arg_part| {
                const arg_slice = try self.renderCommandPart(scopes, arg_part, &owned_strings);
                try argv.append(arg_slice);
            }

            const argv_owned = try self.allocator.alloc([]const u8, argv.items.len);
            @memcpy(argv_owned, argv.items);
            try specs.append(.{ .command_type = command_type, .argv = argv_owned });
        }

        const handle = try self.executor.run(specs.items);
        return Value{ .process_handle = handle };
    }

    fn renderCommandPart(
        self: *Evaluator,
        scopes: *ScopeStack,
        part: ast.CommandPart,
        owned_strings: *ArrayListManaged([]u8),
    ) Error![]const u8 {
        return switch (part) {
            .word => |word| try self.trackString(owned_strings, try self.allocator.dupe(u8, word.text)),
            .string => |literal| try self.trackString(owned_strings, try self.renderStringLiteral(scopes, literal)),
            .expr => |expr_ptr| blk: {
                var value = try self.evaluateExpression(scopes, expr_ptr);
                defer value.deinit(self.allocator);
                const rendered = try self.materializeString(&value);
                break :blk try self.trackString(owned_strings, rendered);
            },
        };
    }

    fn trackString(self: *Evaluator, owned_strings: *ArrayListManaged([]u8), buffer: []u8) ![]const u8 {
        _ = self;
        try owned_strings.append(buffer);
        return buffer;
    }

    fn renderStringLiteral(self: *Evaluator, scopes: *ScopeStack, literal: ast.StringLiteral) Error![]u8 {
        var buffer = ArrayListManaged(u8).init(self.allocator);
        errdefer buffer.deinit();

        for (literal.segments) |segment| {
            switch (segment) {
                .text => |t| try buffer.appendSlice(t.payload),
                .interpolation => |expr_ptr| {
                    var value = try self.evaluateExpression(scopes, expr_ptr);
                    defer value.deinit(self.allocator);
                    const rendered = try self.materializeString(&value);
                    defer self.allocator.free(rendered);
                    try buffer.appendSlice(rendered);
                },
            }
        }

        return buffer.toOwnedSlice();
    }

    fn materializeString(self: *Evaluator, value: *Value) Error![]u8 {
        return switch (value.*) {
            .string => |owned| blk: {
                const slice = owned;
                value.* = .{ .void = {} };
                break :blk slice;
            },
            .boolean => |flag| try self.allocator.dupe(u8, if (flag) "1" else "0"),
            .integer => |int| try std.fmt.allocPrint(self.allocator, "{d}", .{int}),
            .float => |flt| try std.fmt.allocPrint(self.allocator, "{d}", .{flt}),
            .process_handle => |p| try self.allocator.dupe(u8, p.stdoutBytes()),
            .void, .scope, .function => Error.InvalidStringCoercion,
        };
    }

    fn storeLastValue(self: *Evaluator, result: *BlockResult, value: *Value) void {
        if (result.last_value) |*existing| {
            existing.deinit(self.allocator);
            existing.* = value.move();
        } else {
            result.last_value = value.move();
        }
    }

    fn bindPattern(
        self: *Evaluator,
        scopes: *ScopeStack,
        pattern: ast.BindingPattern,
        is_mutable: bool,
        value: *Value,
    ) Error!void {
        switch (pattern) {
            .identifier => |identifier| try scopes.declare(identifier.name, value, is_mutable),
            .discard => value.deinit(self.allocator),
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
