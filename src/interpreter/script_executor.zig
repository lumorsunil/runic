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
const BindingError = ScriptContext.BindingError;
const rainbow = @import("../rainbow.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;

const span_color = rainbow.beginBgColor(.red) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

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
        scopes: *ScopeStack,
        env_map: ?*std.process.EnvMap,
    };

    pub const ExecuteOptions = struct {
        script_path: []const u8,
        forward_context: ScopeStack.ForwardContext,
        context: *ScriptContext,

        pub fn init(
            script_path: []const u8,
            forward_context: ScopeStack.ForwardContext,
            context: *ScriptContext,
        ) ExecuteOptions {
            return .{
                .script_path = script_path,
                .forward_context = forward_context,
                .context = context,
            };
        }
    };

    pub fn initWithRunner(
        allocator: Allocator,
        path: []const u8,
        runner: *CommandRunner,
        env_map: ?*std.process.EnvMap,
        executeOptions: ExecuteOptions,
        documentStore: DocumentStore,
    ) !ScriptExecutor {
        const bridge = try allocator.create(CommandBridge);
        bridge.* = .{
            .allocator = allocator,
            .runner = runner,
            .evaluator = undefined,
            .scopes = undefined,
            .env_map = env_map,
        };

        var scopes = try allocator.create(ScopeStack);
        scopes.* = .init(allocator, .main);
        try scopes.log(@src().fn_name ++ ": init", .{});
        errdefer {
            scopes.deinit();
            allocator.destroy(scopes);
            allocator.destroy(bridge);
        }
        try scopes.pushFrame(
            @src().fn_name,
            .initForwardContext(executeOptions.forward_context),
        );
        try scopes.pushFrame(@src().fn_name, try .initSingleNew(allocator));

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

    pub fn wireCommandBridge(self: *ScriptExecutor, scopes: *ScopeStack) void {
        if (self.command_bridge) |bridge| {
            bridge.evaluator = &self.evaluator;
            bridge.scopes = scopes;
        }
    }

    fn forwardHandleOutputIfProcessHandle(value: *interpreter.Value, options: ExecuteOptions) !?command_runner.ExitCode {
        switch (value.*) {
            .array => |*array| {
                const items = try array.get();
                for (items) |item| {
                    var copy = item;
                    _ = try forwardHandleOutputIfProcessHandle(&copy, options);
                }
            },
            .process_handle => |*handle| {
                try utils.forwardHandleOutput(handle, options.stdout, options.stderr);
                if (!handle.status.ok) {
                    return handle.status.exit_code orelse .fromProcess(1);
                }
            },
            else => {},
        }

        return null;
    }

    fn getExitCode(value: *interpreter.Value) ?command_runner.ExitCode {
        switch (value.*) {
            .process_handle => |*handle| {
                if (!handle.status.ok) {
                    return handle.status.exit_code orelse .fromProcess(1);
                }
            },
            else => {},
        }

        return null;
    }

    pub fn execute(self: *ScriptExecutor, script: ast.Script, options: ExecuteOptions) !command_runner.ExitCode {
        try self.scopes.pushFrame(@src().fn_name, .initForwardContext(options.forward_context));

        for (script.statements) |stmt| {
            const maybe_value = self.evaluator.runStatement(self.scopes, stmt) catch |err| {
                // TODO: redirect properly when error handling is implemented for function calls and blocks etc.
                var stderr_file_writer = std.fs.File.stderr().writer(&.{});
                // const stderr = self.scopes.getForwardContext().stderr.materialized.writer;
                try self.renderEvaluatorError(&stderr_file_writer.interface, options.script_path, stmt.span(), err);
                return .fromProcess(1);
            };

            if (maybe_value) |value| {
                var owned = value;
                defer owned.deinit();
                // const code = try forwardHandleOutputIfProcessHandle(
                const code = getExitCode(&owned);
                if (code) |the_code| {
                    if (the_code == .success) {} else {
                        // TODO: redirect properly when error handling is implemented for function calls and blocks etc.
                        var stderr_file_writer = std.fs.File.stderr().writer(&.{});
                        try self.renderExitCodeError(&stderr_file_writer.interface, stmt.span(), the_code);
                        return the_code;
                    }
                }
            }
        }

        try self.scopes.popFrame(@src().fn_name);

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

    fn renderExitCodeError(
        self: *ScriptExecutor,
        writer: *std.Io.Writer,
        span: ast.Span,
        exit_code: command_runner.ExitCode,
    ) !void {
        const source = try self.evaluator.document_store.getSource(span.start.file);

        try logSpan(writer, span, source);

        switch (exit_code) {
            .success => return,
            .byte => |byte| try writer.print(
                "{s}:{d}:{d}: script execution error: terminated with exit code {}\n",
                .{ span.start.file, span.start.line, span.start.column, byte },
            ),
            .err => |err| try writer.print(
                "{s}:{d}:{d}: script execution error: {s}\n",
                .{ span.start.file, span.start.line, span.start.column, @errorName(err) },
            ),
        }
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

    return bridge.runner.runPipeline(bridge.evaluator, bridge.scopes, patched);
}
