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
    executor: CommandExecutor,
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
        executor: CommandExecutor,
        documentStore: *DocumentStore,
    ) Evaluator {
        return .{
            .allocator = allocator,
            .executor = executor,
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
            try scopes.pushFrame();
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

    fn evaluateExpression(self: *Evaluator, scopes: *ScopeStack, expr: *const ast.Expression) Error!Value {
        return switch (expr.*) {
            .literal => |literal| try self.evaluateLiteral(scopes, literal),
            .identifier => |identifier| try self.evaluateIdentifier(scopes, identifier),
            .pipeline => |pipeline| try self.evaluatePipeline(scopes, pipeline),
            .block => |block_expr| try self.evaluateBlockExpression(scopes, block_expr),
            .import_expr => |import_expr| try self.evaluateImportExpression(import_expr),
            else => Error.UnsupportedExpression,
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

        document.script_executor = try ScriptExecutor.initWithRunner(
            self.allocator,
            currentExecutor.?.command_bridge.?.runner,
            currentExecutor.?.command_bridge.?.env_map,
            self.documentStore,
        );

        const executor = &document.script_executor.?;

        try executor.scopes.pushFrame();

        var stdout = std.fs.File.stdout().writer(&.{});
        var stderr = std.fs.File.stderr().writer(&.{});

        const context = try self.allocator.create(ScriptContext);
        context.* = ScriptContext.init(self.allocator);
        defer self.allocator.destroy(context);

        const exitCode = try executor.execute(script_ast, .{
            .script_path = document.path,
            .stdout = &stdout.interface,
            .stderr = &stderr.interface,
            .context = context,
        });

        document.exitCode = exitCode;

        return .{ .scope = &executor.scopes };
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

            for (command.args) |arg_part| {
                const arg_slice = try self.renderCommandPart(scopes, arg_part, &owned_strings);
                try argv.append(arg_slice);
            }

            const argv_owned = try self.allocator.alloc([]const u8, argv.items.len);
            @memcpy(argv_owned, argv.items);
            try specs.append(.{ .argv = argv_owned });
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
            .void, .scope => Error.InvalidStringCoercion,
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

test "evaluator executes command pipelines and propagates handles" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var scopes = ScopeStack.init(alloc);
    defer scopes.deinit();
    try scopes.pushFrame();

    const stage_argv = [_][]const u8{ "echo", "hi" };
    var executor = TestExecutor{
        .allocator = alloc,
        .expected = &[_]TestExecutor.StageExpectation{
            .{ .argv = stage_argv[0..] },
        },
    };

    var evaluator = Evaluator.init(alloc, executor.asCommandExecutor());

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    const block = try buildPipelineBlock(arena.allocator(), "echo", "hi");
    try evaluator.runBlock(&scopes, block);
    try std.testing.expectEqual(@as(usize, 1), executor.calls);
}

test "let binding feeds into pipeline expression arguments" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var scopes = ScopeStack.init(alloc);
    defer scopes.deinit();
    try scopes.pushFrame();

    const stage_argv = [_][]const u8{ "echo", "hello" };
    var executor = TestExecutor{
        .allocator = alloc,
        .expected = &[_]TestExecutor.StageExpectation{
            .{ .argv = stage_argv[0..] },
        },
    };

    var evaluator = Evaluator.init(alloc, executor.asCommandExecutor());

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    const block = try buildLetAndPipelineBlock(arena.allocator(), "greeting", "hello");
    try evaluator.runBlock(&scopes, block);
    try std.testing.expectEqual(@as(usize, 1), executor.calls);
}

const TestExecutor = struct {
    allocator: std.mem.Allocator,
    expected: []const StageExpectation,
    calls: usize = 0,

    const StageExpectation = struct {
        argv: []const []const u8,
    };

    fn asCommandExecutor(self: *TestExecutor) CommandExecutor {
        return .{
            .context = self,
            .runFn = execute,
        };
    }

    fn execute(context: *anyopaque, specs: []const CommandRunner.CommandSpec) CommandRunner.Error!ProcessHandle {
        const self: *TestExecutor = @ptrCast(@alignCast(context));
        self.calls += 1;
        std.debug.assert(specs.len == self.expected.len);
        for (specs, 0..) |spec, idx| {
            const expected_stage = self.expected[idx];
            std.debug.assert(spec.argv.len == expected_stage.argv.len);
            for (spec.argv, 0..) |arg, arg_idx| {
                const expected_arg = expected_stage.argv[arg_idx];
                std.debug.assert(std.mem.eql(u8, arg, expected_arg));
            }
        }
        return fakeHandle(self.allocator, specs.len);
    }
};

fn fakeHandle(allocator: std.mem.Allocator, stage_count: usize) !ProcessHandle {
    const statuses = try allocator.alloc(command_runner.StageStatus, stage_count);
    const captures = try allocator.alloc(command_runner.StageCapture, stage_count);
    var index: usize = 0;
    while (index < stage_count) : (index += 1) {
        statuses[index] = .{
            .index = index,
            .term = .{ .Exited = 0 },
            .exit_code = 0,
            .signal = null,
            .ok = true,
        };
        captures[index] = .{
            .stdout = try allocator.alloc(u8, 0),
            .stderr = try allocator.alloc(u8, 0),
        };
    }
    return .{
        .allocator = allocator,
        .pid = 1,
        .started_at_ns = 0,
        .finished_at_ns = 0,
        .status = .{
            .ok = true,
            .exit_code = 0,
            .signal = null,
            .failed_stage = null,
        },
        .stage_statuses = statuses,
        .stage_captures = captures,
    };
}

fn buildPipelineBlock(
    allocator: std.mem.Allocator,
    command: []const u8,
    arg: []const u8,
) !ast.Block {
    const name_word = ast.CommandPart{ .word = .{ .text = command, .span = dummySpan() } };
    const arg_literal = try buildStringLiteral(allocator, arg);
    const arg_part = ast.CommandPart{ .string = arg_literal };

    var command_invocation = ast.CommandInvocation{
        .name = name_word,
        .args = undefined,
        .env_assignments = &[_]ast.EnvAssignment{},
        .redirects = &[_]ast.Redirection{},
        .capture = null,
        .background = false,
        .span = dummySpan(),
    };
    command_invocation.args = try allocator.alloc(ast.CommandPart, 1);
    command_invocation.args[0] = arg_part;

    var stages = try allocator.alloc(ast.PipelineStage, 1);
    stages[0] = .{
        .role = .command,
        .payload = .{ .command = command_invocation },
        .span = dummySpan(),
    };

    const pipeline = ast.Pipeline{ .stages = stages, .span = dummySpan() };
    const expr_ptr = try allocator.create(ast.Expression);
    expr_ptr.* = .{ .pipeline = pipeline };

    const stmt_ptr = try allocator.create(ast.Statement);
    stmt_ptr.* = .{
        .expression = .{
            .expression = expr_ptr,
            .span = dummySpan(),
        },
    };

    const stmts = try allocator.alloc(*ast.Statement, 1);
    stmts[0] = stmt_ptr;
    return .{ .statements = stmts, .span = dummySpan() };
}

fn buildLetAndPipelineBlock(
    allocator: std.mem.Allocator,
    name: []const u8,
    literal: []const u8,
) !ast.Block {
    const pattern_ptr = try allocator.create(ast.BindingPattern);
    pattern_ptr.* = .{ .identifier = .{ .name = name, .span = dummySpan() } };

    const string_lit = try buildStringLiteral(allocator, literal);
    const literal_expr = try allocator.create(ast.Expression);
    literal_expr.* = .{ .literal = .{ .string = string_lit } };

    const let_stmt_ptr = try allocator.create(ast.Statement);
    let_stmt_ptr.* = .{
        .binding_decl = .{
            .is_mutable = false,
            .pattern = pattern_ptr,
            .annotation = null,
            .initializer = literal_expr,
            .span = dummySpan(),
        },
    };

    const id_expr = try allocator.create(ast.Expression);
    id_expr.* = .{ .identifier = .{ .name = name, .span = dummySpan() } };

    var command_invocation = ast.CommandInvocation{
        .name = ast.CommandPart{ .word = .{ .text = "echo", .span = dummySpan() } },
        .args = undefined,
        .env_assignments = &[_]ast.EnvAssignment{},
        .redirects = &[_]ast.Redirection{},
        .capture = null,
        .background = false,
        .span = dummySpan(),
    };
    command_invocation.args = try allocator.alloc(ast.CommandPart, 1);
    command_invocation.args[0] = .{ .expr = id_expr };

    var stages = try allocator.alloc(ast.PipelineStage, 1);
    stages[0] = .{
        .role = .command,
        .payload = .{ .command = command_invocation },
        .span = dummySpan(),
    };

    const pipeline_expr = try allocator.create(ast.Expression);
    pipeline_expr.* = .{ .pipeline = .{ .stages = stages, .span = dummySpan() } };

    const expr_stmt_ptr = try allocator.create(ast.Statement);
    expr_stmt_ptr.* = .{
        .expression = .{
            .expression = pipeline_expr,
            .span = dummySpan(),
        },
    };

    const stmts = try allocator.alloc(*ast.Statement, 2);
    stmts[0] = let_stmt_ptr;
    stmts[1] = expr_stmt_ptr;
    return .{ .statements = stmts, .span = dummySpan() };
}

fn buildStringLiteral(allocator: std.mem.Allocator, contents: []const u8) !ast.StringLiteral {
    const segments = try allocator.alloc(ast.StringLiteral.Segment, 1);
    segments[0] = .{ .text = contents };
    return .{
        .segments = segments,
        .span = dummySpan(),
    };
}

fn dummySpan() token.Span {
    return .{
        .start = .{ .line = 1, .column = 1 },
        .end = .{ .line = 1, .column = 1 },
    };
}
