const std = @import("std");

const ast = @import("../frontend/ast.zig");
const utils = @import("../utils.zig");
const interpreter = @import("../interpreter/root.zig");
const Evaluator = interpreter.Evaluator;
const CommandExecutor = interpreter.CommandExecutor;
const ScopeStack = interpreter.ScopeStack;
const RuntimeValue = interpreter.Value;
const command_runner = @import("../runtime/command_runner.zig");
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
        evaluator: *Evaluator,
        scopes: *ScopeStack,
        env_map: ?*std.process.EnvMap,
    };

    pub const ExecuteOptions = struct {
        script_path: []const u8,
        cwd: []const u8,
        env_map: std.process.EnvMap,
        forward_context: ScopeStack.ForwardContext,

        pub fn init(
            script_path: []const u8,
            cwd: []const u8,
            env_map: std.process.EnvMap,
            forward_context: ScopeStack.ForwardContext,
        ) ExecuteOptions {
            return .{
                .cwd = cwd,
                .env_map = env_map,
                .script_path = script_path,
                .forward_context = forward_context,
            };
        }
    };

    pub fn initWithRunner(
        allocator: Allocator,
        path: []const u8,
        env_map: ?*std.process.EnvMap,
        executeOptions: ExecuteOptions,
        documentStore: *DocumentStore,
    ) !ScriptExecutor {
        const bridge = try allocator.create(CommandBridge);
        bridge.* = .{
            .allocator = allocator,
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
        try scopes.pushAllocating(@src().fn_name);
        try scopes.pushFrameForwarding(
            @src().fn_name,
            executeOptions.forward_context,
        );
        try scopes.pushFrame(@src().fn_name, try .initSingleNew(allocator));

        const evaluator = Evaluator.init(allocator, path, executeOptions, documentStore);

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
        self.evaluator.deinit();
        self.* = undefined;
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
                    return handle.status.exit_code orelse .fromTerm(1);
                }
            },
            else => {},
        }

        return null;
    }

    pub fn execute(
        self: *ScriptExecutor,
        script: ast.Script,
        options: ExecuteOptions,
    ) !command_runner.ExitCode {
        try self.scopes.pushCwd(@src().fn_name, options.cwd);
        try self.scopes.pushEnvMap(@src().fn_name);
        try self.scopes.pushFrameForwarding(@src().fn_name, options.forward_context);
        try self.scopes.pushProcesses(@src().fn_name);

        var env_it = options.env_map.iterator();
        while (env_it.next()) |entry| {
            var value = interpreter.Value{
                .string = try .dupe(self.allocator, entry.value_ptr.*, .{}),
            };
            try self.scopes.declareEnvVar(entry.key_ptr.*, &value, true);
        }

        for (script.statements) |stmt| {
            const maybe_value = self.evaluator.runStatement(self.scopes, stmt) catch |err| {
                // TODO: redirect properly when error handling is implemented for function calls and blocks etc.
                var stderr_file_writer = std.fs.File.stderr().writer(&.{});
                // const stderr = self.scopes.getForwardContext().stderr.materialized.writer;
                try self.renderEvaluatorError(&stderr_file_writer.interface, options.script_path, stmt.span(), err);
                return .fromByte(1);
            };

            if (maybe_value) |value| {
                var owned = value;
                defer owned.deinit();
                // const code = try forwardHandleOutputIfProcessHandle(
                // const code = getExitCode(&owned);
                // if (code) |the_code| {
                //     if (the_code == .success) {} else {
                //         // TODO: redirect properly when error handling is implemented for function calls and blocks etc.
                //         var stderr_file_writer = std.fs.File.stderr().writer(&.{});
                //         try self.renderExitCodeError(&stderr_file_writer.interface, stmt.span(), the_code);
                //         return the_code;
                //     }
                // }
            }
        }

        try self.scopes.popFrameN(@src().fn_name, 4);

        return .success;
    }

    fn renderEvaluatorError(
        self: *ScriptExecutor,
        stderr: *std.Io.Writer,
        script_path: []const u8,
        span: ast.Span,
        err: anyerror,
    ) !void {
        try stderr.print(
            "{s}:{d}:{d}: script execution error: {s}\n",
            .{ script_path, span.start.line, span.start.column, @errorName(err) },
        );
        self.evaluator.logging_enabled = true;
        defer self.evaluator.logging_enabled = false;
        try self.evaluator.logEvaluateSpan(span);
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
            .term => |term| switch (term) {
                .Exited => |byte| try writer.print(
                    "{s}:{d}:{d}: script execution error: terminated with exit code {}\n",
                    .{ span.start.file, span.start.line, span.start.column, byte },
                ),
                inline else => |signal| try writer.print(
                    "{s}:{d}:{d}: script execution error: terminated with signal {x:04}\n",
                    .{ span.start.file, span.start.line, span.start.column, signal },
                ),
            },
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
