const std = @import("std");
const ScriptContext = @import("script_context.zig").ScriptContext;

const ast = @import("../frontend/ast.zig");
const utils = @import("../utils.zig");
const interpreter = @import("../interpreter/root.zig");
const Evaluator = interpreter.Evaluator;
const CommandExecutor = interpreter.CommandExecutor;
const ScopeStack = interpreter.ScopeStack;
const RuntimeValue = interpreter.Value;
const command_runner = @import("../runtime/command_runner.zig");
const CommandRunner = command_runner.CommandRunner;
const DocumentStore = @import("../frontend/document_store.zig").DocumentStore;
const BindingError = ScriptContext.BindingError;

const Allocator = std.mem.Allocator;

pub const ScriptExecutor = struct {
    allocator: Allocator,
    evaluator: Evaluator,
    scopes: ScopeStack,
    command_bridge: ?*CommandBridge = null,

    pub const Error = error{
        DuplicateBinding,
    };

    const CommandBridge = struct {
        allocator: Allocator,
        runner: *CommandRunner,
        env_map: ?*std.process.EnvMap,
    };

    pub const ExecuteOptions = struct {
        script_path: []const u8,
        stdout: *std.Io.Writer,
        stderr: *std.Io.Writer,
        context: *ScriptContext,
    };

    pub fn initWithExecutor(allocator: Allocator, executor: CommandExecutor) !ScriptExecutor {
        var scopes = ScopeStack.init(allocator);
        errdefer scopes.deinit();
        try scopes.pushFrame();

        return .{
            .allocator = allocator,
            .evaluator = Evaluator.init(allocator, executor),
            .scopes = scopes,
            .command_bridge = null,
        };
    }

    pub fn initWithRunner(
        allocator: Allocator,
        runner: *CommandRunner,
        env_map: ?*std.process.EnvMap,
        documentStore: *DocumentStore,
    ) !ScriptExecutor {
        const bridge = try allocator.create(CommandBridge);
        bridge.* = .{
            .allocator = allocator,
            .runner = runner,
            .env_map = env_map,
        };

        var scopes = ScopeStack.init(allocator);
        errdefer {
            scopes.deinit();
            allocator.destroy(bridge);
        }
        try scopes.pushFrame();

        const evaluator = Evaluator.init(allocator, CommandExecutor{
            .context = bridge,
            .runFn = runWithEnvMap,
        }, documentStore);

        return .{
            .allocator = allocator,
            .evaluator = evaluator,
            .scopes = scopes,
            .command_bridge = bridge,
        };
    }

    pub fn deinit(self: *ScriptExecutor) void {
        self.scopes.deinit();
        if (self.command_bridge) |bridge| {
            self.allocator.destroy(bridge);
        }
        self.* = undefined;
    }

    pub fn reseedFromContext(
        self: *ScriptExecutor,
        context: *const ScriptContext,
    ) (Allocator.Error || ScopeStack.Error)!void {
        try self.reseedInterpreterScopeWithContext(context);
    }

    pub fn reseedInterpreterScopeWithContext(
        self: *ScriptExecutor,
        context: *const ScriptContext,
    ) (Allocator.Error || ScopeStack.Error)!void {
        var fresh = ScopeStack.init(self.allocator);
        errdefer fresh.deinit();

        try fresh.pushFrame();

        for (context.bindings.items) |binding| {
            switch (binding.value) {
                .string => |literal| {
                    var runtime_value = RuntimeValue{ .string = try self.allocator.dupe(u8, literal) };
                    errdefer runtime_value.deinit(self.allocator);
                    try fresh.declare(binding.name, &runtime_value, binding.is_mutable);
                },
                else => {},
            }
        }

        var previous = self.scopes;
        self.scopes = fresh;
        previous.deinit();
    }

    pub fn execute(self: *ScriptExecutor, script: ast.Script, options: ExecuteOptions) !u8 {
        for (script.statements) |stmt| {
            const maybe_value = self.evaluator.runStatement(&self.scopes, stmt) catch |err| {
                try self.renderEvaluatorError(options.stderr, options.script_path, stmt.span(), err);
                return 1;
            };

            if (maybe_value) |value| {
                var owned = value;
                var consumed = false;
                defer if (!consumed) owned.deinit(self.allocator);

                switch (owned) {
                    .process_handle => |*handle| {
                        try utils.forwardHandleOutput(handle, options.stdout, options.stderr);
                        if (!handle.status.ok) {
                            const code = handle.status.exit_code orelse 1;
                            handle.deinit();
                            consumed = true;
                            return code;
                        }
                    },
                    else => {},
                }
            }
        }

        return 0;
    }

    fn renderStringExpression(self: *ScriptExecutor, expr: *const ast.Expression) ![]u8 {
        return switch (expr.*) {
            .literal => |literal| switch (literal) {
                .string => |str_lit| try self.renderStringLiteral(str_lit),
                .integer => |int_lit| try self.renderIntegerLiteralAsString(int_lit),
                .float => |float_lit| try self.renderFloatLiteralAsString(float_lit),
                .bool => |bool_lit| try self.renderBoolLiteralAsString(bool_lit),
                else => error.InvalidStringCoercion,
            },
            else => error.InvalidStringCoercion,
        };
    }

    fn renderStringLiteral(self: *ScriptExecutor, literal: ast.StringLiteral) ![]u8 {
        var expr = ast.Expression{ .literal = .{ .string = literal } };
        var stmt = ast.Statement{
            .expression = .{
                .expression = &expr,
                .span = literal.span,
            },
        };

        var value = try self.evaluator.runStatement(&self.scopes, &stmt) orelse return error.UnsupportedExpression;
        defer value.deinit(self.allocator);
        return switch (value) {
            .string => |owned| try self.allocator.dupe(u8, owned),
            else => error.InvalidStringCoercion,
        };
    }

    fn renderIntegerLiteralAsString(self: *ScriptExecutor, literal: ast.IntegerLiteral) ![]u8 {
        return try self.allocator.dupe(u8, literal.text);
    }

    fn renderFloatLiteralAsString(self: *ScriptExecutor, literal: ast.FloatLiteral) ![]u8 {
        return try self.allocator.dupe(u8, literal.text);
    }

    fn renderBoolLiteralAsString(self: *ScriptExecutor, literal: ast.BoolLiteral) ![]u8 {
        const s = try self.allocator.alloc(u8, 1);
        s[0] = if (literal.value) '1' else '0';
        return s;
    }

    fn renderEvaluatorError(
        self: *ScriptExecutor,
        stderr: *std.Io.Writer,
        script_path: []const u8,
        span: ast.Span,
        err: anyerror,
    ) !void {
        _ = self;
        try stderr.print(
            "{s}:{d}:{d}: script execution error: {s}\n",
            .{ script_path, span.start.line, span.start.column, @errorName(err) },
        );
    }
};

fn runWithEnvMap(
    context: *anyopaque,
    specs: []const CommandRunner.CommandSpec,
) CommandRunner.Error!command_runner.ProcessHandle {
    const bridge: *ScriptExecutor.CommandBridge = @ptrCast(@alignCast(context));
    if (specs.len == 0) return error.EmptyCommand;

    var patched = try bridge.allocator.alloc(CommandRunner.CommandSpec, specs.len);
    defer bridge.allocator.free(patched);

    for (specs, 0..) |spec, idx| {
        patched[idx] = spec;
        patched[idx].env_map = bridge.env_map;
    }

    return bridge.runner.*.runPipeline(patched);
}

const TestExecutor = struct {
    allocator: Allocator,
    calls: usize = 0,

    fn asCommandExecutor(self: *TestExecutor) CommandExecutor {
        return .{
            .context = self,
            .runFn = run,
        };
    }

    fn run(context: *anyopaque, specs: []const CommandRunner.CommandSpec) CommandRunner.Error!command_runner.ProcessHandle {
        const self: *TestExecutor = @ptrCast(@alignCast(context));
        self.calls += 1;
        std.debug.assert(specs.len > 0);
        return fakeHandle(self.allocator, specs.len);
    }
};

test "script executor binds module member functions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile("helpers.rn",
        \\fn ensure(value: Str) Str {
        \\  return value
        \\}
    );
    const script_dir = try tmp.dir.realpathAlloc(alloc, ".");
    defer alloc.free(script_dir);

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    const stmt = try buildImportMemberLet(arena.allocator(), "ensure", "helpers", "ensure");
    const statements = try arena.allocator().alloc(*ast.Statement, 1);
    statements[0] = stmt;
    const script = ast.Script{
        .statements = statements,
        .span = .dummy,
    };

    var executor = try ScriptExecutor.initWithExecutor(
        alloc,
        (TestExecutor{ .allocator = alloc }).asCommandExecutor(),
    );
    defer executor.deinit();

    var stdout_buf: [16]u8 = undefined;
    var stderr_buf: [256]u8 = undefined;
    var stdout_writer = std.Io.Writer.fixed(&stdout_buf);
    var stderr_writer = std.Io.Writer.fixed(&stderr_buf);

    var context = ScriptContext.init(alloc);
    defer context.deinit();

    try executor.execute(script, .{
        .script_path = "<test>",
        .stdout = &stdout_writer,
        .stderr = &stderr_writer,
        .context = &context,
    });

    const binding = context.getFunctionBinding("ensure") orelse return error.MissingEnsureBinding;
    try std.testing.expectEqualStrings("ensure", binding.module_alias);
    try std.testing.expectEqualStrings("ensure", binding.function_name);
}

test "script executor reports missing module members" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile("helpers.rn",
        \\fn other(value: Str) Str {
        \\  return value
        \\}
    );
    const script_dir = try tmp.dir.realpathAlloc(alloc, ".");
    defer alloc.free(script_dir);

    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    const stmt = try buildImportMemberLet(arena.allocator(), "ensure", "helpers", "ensure");
    const statements = try arena.allocator().alloc(*ast.Statement, 1);
    statements[0] = stmt;
    const script = ast.Script{
        .statements = statements,
        .span = .dummy,
    };

    var executor = try ScriptExecutor.initWithExecutor(
        alloc,
        (TestExecutor{ .allocator = alloc }).asCommandExecutor(),
    );
    defer executor.deinit();

    var stdout_buf: [16]u8 = undefined;
    var stderr_buf: [256]u8 = undefined;
    var stdout_writer = std.Io.Writer.fixed(&stdout_buf);
    var stderr_writer = std.Io.Writer.fixed(&stderr_buf);

    var context = ScriptContext.init(alloc);
    defer context.deinit();

    const result = try executor.execute(script, .{
        .script_path = "<test>",
        .stdout = &stdout_writer,
        .stderr = &stderr_writer,
        .context = &context,
    });
    try std.testing.expectEqual(@as(u8, 1), result.?);
    try std.testing.expect(std.mem.containsAtLeast(u8, stderr_writer.buffer[0..stderr_writer.end], "no member", 1));
}

fn fakeHandle(allocator: Allocator, stages: usize) !command_runner.ProcessHandle {
    const statuses = try allocator.alloc(command_runner.StageStatus, stages);
    const captures = try allocator.alloc(command_runner.StageCapture, stages);
    var idx: usize = 0;
    while (idx < stages) : (idx += 1) {
        statuses[idx] = .{
            .index = idx,
            .term = .{ .Exited = 0 },
            .exit_code = 0,
            .signal = null,
            .ok = true,
        };
        captures[idx] = .{
            .stdout = try allocator.dupe(u8, "stdout"),
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

fn buildStringLiteral(
    allocator: Allocator,
    spec: []const u8,
) !ast.StringLiteral {
    const segments = allocator.alloc(ast.StringLiteral.Segment, 1);
    // TODO: Implement interpolation?
    segments[0] = spec;

    return .{
        .span = .dummy,
        .segments = segments,
    };
}

fn buildImportMemberLet(
    allocator: std.mem.Allocator,
    binding_name: []const u8,
    spec: []const u8,
    member_name: []const u8,
) !*ast.Statement {
    const pattern = try allocator.create(ast.BindingPattern);
    pattern.* = .{ .identifier = .{ .name = binding_name, .span = .dummy } };

    const spec_literal = try buildStringLiteral(allocator, spec);
    const spec_expr = try allocator.create(ast.Expression);
    spec_expr.* = .{ .literal = .{ .string = spec_literal } };

    const import_ident = try allocator.create(ast.Expression);
    import_ident.* = .{ .identifier = .{ .name = "import", .span = .dummy } };

    var call_args = try allocator.alloc(ast.CallArgument, 1);
    call_args[0] = .{
        .label = null,
        .value = spec_expr,
        .span = .dummy,
    };

    const call_expr = try allocator.create(ast.Expression);
    call_expr.* = .{
        .call = .{
            .callee = import_ident,
            .arguments = call_args,
            .span = .dummy,
        },
    };

    const member_expr = try allocator.create(ast.Expression);
    member_expr.* = .{
        .member = .{
            .object = call_expr,
            .member = .{ .name = member_name, .span = .dummy },
            .span = .dummy,
        },
    };

    const stmt = try allocator.create(ast.Statement);
    stmt.* = .{
        .binding_decl = .{
            .is_mutable = false,
            .pattern = pattern,
            .annotation = null,
            .initializer = member_expr,
            .span = .dummy,
        },
    };
    return stmt;
}
