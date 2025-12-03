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
    scopes: *ScopeStack,
    command_bridge: ?*CommandBridge = null,

    pub const Error = error{
        DuplicateBinding,
    };

    const CommandBridge = struct {
        allocator: Allocator,
        runner: *CommandRunner,
        evaluator: *Evaluator,
        env_map: ?*std.process.EnvMap,
    };

    pub const ExecuteOptions = struct {
        script_path: []const u8,
        stdout: *std.Io.Writer,
        stderr: *std.Io.Writer,
        context: *ScriptContext,
    };

    pub fn initWithExecutor(allocator: Allocator, executor: CommandExecutor) !ScriptExecutor {
        var scopes = try allocator.create(ScopeStack);
        scopes.* = ScopeStack.init(allocator);
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
        path: []const u8,
        runner: *CommandRunner,
        env_map: ?*std.process.EnvMap,
        executeOptions: ExecuteOptions,
        documentStore: *DocumentStore,
    ) !ScriptExecutor {
        const bridge = try allocator.create(CommandBridge);
        bridge.* = .{
            .allocator = allocator,
            .runner = runner,
            .evaluator = undefined,
            .env_map = env_map,
        };

        var scopes = try allocator.create(ScopeStack);
        scopes.* = ScopeStack.init(allocator);
        errdefer {
            scopes.deinit();
            allocator.destroy(scopes);
            allocator.destroy(bridge);
        }
        try scopes.pushFrame(.{});

        const evaluator = Evaluator.init(allocator, path, CommandExecutor{
            .context = bridge,
            .runFn = runWithEnvMap,
        }, executeOptions, documentStore);

        return .{
            .allocator = allocator,
            .evaluator = evaluator,
            .scopes = scopes,
            .command_bridge = bridge,
        };
    }

    pub fn deinit(self: *ScriptExecutor) void {
        self.scopes.deinit();
        self.allocator.destroy(self.scopes);
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

        try fresh.pushFrame(.{});

        for (context.bindings.items) |binding| {
            switch (binding.value) {
                .string => |literal| {
                    var runtime_value = RuntimeValue{ .string = try .dupe(self.allocator, literal) };
                    errdefer runtime_value.deinit(self.allocator);
                    try fresh.declare(binding.name, &runtime_value, binding.is_mutable);
                },
                else => {},
            }
        }

        var previous = self.scopes.*;
        self.scopes.* = fresh;
        previous.deinit();
    }

    pub fn wireCommandBridge(self: *ScriptExecutor) void {
        if (self.command_bridge) |bridge| {
            bridge.evaluator = &self.evaluator;
        }
    }

    pub fn execute(self: *ScriptExecutor, script: ast.Script, options: ExecuteOptions) !command_runner.ExitCode {
        for (script.statements) |stmt| {
            const maybe_value = self.evaluator.runStatement(self.scopes, stmt) catch |err| {
                try self.renderEvaluatorError(options.stderr, options.script_path, stmt.span(), err);
                return .fromProcess(1);
            };

            if (maybe_value) |value| {
                var owned = value;
                var consumed = false;
                defer if (!consumed) owned.deinit(self.allocator);

                switch (owned) {
                    .process_handle => |*handle| {
                        try utils.forwardHandleOutput(handle, options.stdout, options.stderr);
                        if (!handle.status.ok) {
                            const code: command_runner.ExitCode = handle.status.exit_code orelse .fromProcess(1);
                            handle.deinit();
                            consumed = true;
                            return code;
                        }
                    },
                    else => {},
                }
            }
        }

        return .success;
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

    return bridge.runner.runPipeline(bridge.evaluator, patched);
}
