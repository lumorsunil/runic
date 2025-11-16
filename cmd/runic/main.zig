const std = @import("std");
const ManagedArrayList = std.array_list.Managed;
const repl = @import("repl.zig");

const CliConfig = struct {
    mode: Mode,
    trace_topics: [][]const u8,
    module_paths: [][]const u8,
    env_overrides: []EnvOverride,

    const Mode = union(enum) {
        script: ScriptInvocation,
        repl,
    };

    const ScriptInvocation = struct {
        path: []const u8,
        args: [][]const u8,
    };

    const EnvOverride = struct {
        key: []const u8,
        value: []const u8,
    };

    fn deinit(self: *CliConfig, allocator: std.mem.Allocator) void {
        switch (self.mode) {
            .script => |script| {
                allocator.free(script.path);
                freeStringList(allocator, script.args);
            },
            .repl => {},
        }

        freeStringList(allocator, self.trace_topics);
        freeStringList(allocator, self.module_paths);

        freeEnvOverrides(allocator, self.env_overrides);
        self.* = undefined;
    }
};

const ParseResult = union(enum) {
    show_help,
    usage_error: []const u8,
    ready: CliConfig,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) std.log.err("runic CLI leaked memory", .{});
    }

    const allocator = gpa.allocator();

    var stdout_buffer: [1024]u8 = undefined;
    var stderr_buffer: [1024]u8 = undefined;
    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    var stdoutWriter = std.fs.File.stdout().writer(&stdout_buffer);
    var stderrWriter = std.fs.File.stderr().writer(&stderr_buffer);
    const stdout = &stdoutWriter.interface;
    const stderr = &stderrWriter.interface;

    const result = try parseCommandLine(allocator, argv);
    switch (result) {
        .show_help => {
            try printUsage(stdout);
            return;
        },
        .usage_error => |message| {
            defer allocator.free(message);
            try stderr.print("error: {s}\n\n", .{message});
            try printUsage(stderr);
            try stderr.flush();
            std.process.exit(2);
        },
        .ready => |config| {
            defer {
                var cfg = config;
                cfg.deinit(allocator);
            }
            try dispatch(allocator, config, stdout);
        },
    }
}

fn dispatch(allocator: std.mem.Allocator, config: CliConfig, writer: *std.Io.Writer) !void {
    switch (config.mode) {
        .script => |script| {
            try writer.print(
                "Runic CLI stub — interpreter wiring is coming soon.\n",
                .{},
            );
            try writer.print("Mode: script\n", .{});
            try writer.print("  script: {s}\n", .{script.path});
            try printStringList(writer, "  args", script.args);
            try printStringList(writer, "Tracing", config.trace_topics);
            try printStringList(writer, "Module paths", config.module_paths);
            try printEnvOverrides(writer, config.env_overrides);
            try writer.print(
                "Pass --help for usage details while runtime execution remains under construction.\n",
                .{},
            );
            try writer.flush();
        },
        .repl => {
            try repl.run(allocator, .{
                .prompt = "runic> ",
                .continuation_prompt = "...> ",
                .history_limit = 256,
                .trace_topics = config.trace_topics,
                .module_paths = config.module_paths,
            });
        },
    }
}

fn parseCommandLine(allocator: std.mem.Allocator, argv: []const []const u8) !ParseResult {
    const trace_prefix = "--trace=";
    const module_prefix = "--module-path=";
    const env_prefix = "--env=";

    var trace_topics = ManagedArrayList([]const u8).init(allocator);
    var trace_cleanup = true;
    defer if (trace_cleanup) trace_topics.deinit();

    var module_paths = ManagedArrayList([]const u8).init(allocator);
    var module_cleanup = true;
    defer if (module_cleanup) module_paths.deinit();

    var env_overrides = ManagedArrayList(CliConfig.EnvOverride).init(allocator);
    var env_cleanup = true;
    defer if (env_cleanup) env_overrides.deinit();

    var script_args = ManagedArrayList([]const u8).init(allocator);
    var args_cleanup = true;
    defer if (args_cleanup) script_args.deinit();

    var script_path: ?[]const u8 = null;
    var repl_requested = false;
    var parsing_options = true;
    var idx: usize = 1;

    while (idx < argv.len) {
        const arg = argv[idx];
        idx += 1;

        if (parsing_options and std.mem.eql(u8, arg, "--")) {
            parsing_options = false;
            continue;
        }

        if (parsing_options and arg.len > 0 and arg[0] == '-') {
            if (argEqual(arg, "--help") or argEqual(arg, "-h")) {
                return ParseResult.show_help;
            }
            if (argEqual(arg, "--repl")) {
                repl_requested = true;
                continue;
            }
            if (std.mem.startsWith(u8, arg, trace_prefix)) {
                const value = arg[trace_prefix.len..];
                if (value.len == 0) return usageError(allocator, "--trace requires a topic name", .{});
                try trace_topics.append(try dupSlice(allocator, value));
                continue;
            }
            if (argEqual(arg, "--trace")) {
                if (idx >= argv.len) return usageError(allocator, "--trace requires a topic name", .{});
                const value = argv[idx];
                idx += 1;
                if (value.len == 0) return usageError(allocator, "--trace requires a topic name", .{});
                try trace_topics.append(try dupSlice(allocator, value));
                continue;
            }
            if (std.mem.startsWith(u8, arg, module_prefix)) {
                const value = arg[module_prefix.len..];
                if (value.len == 0) return usageError(allocator, "--module-path requires a directory", .{});
                try module_paths.append(try dupSlice(allocator, value));
                continue;
            }
            if (argEqual(arg, "--module-path")) {
                if (idx >= argv.len) return usageError(allocator, "--module-path requires a directory", .{});
                const value = argv[idx];
                idx += 1;
                if (value.len == 0) return usageError(allocator, "--module-path requires a directory", .{});
                try module_paths.append(try dupSlice(allocator, value));
                continue;
            }
            if (std.mem.startsWith(u8, arg, env_prefix)) {
                const raw = arg[env_prefix.len..];
                if (raw.len == 0) return usageError(allocator, "--env requires KEY=VALUE", .{});
                const override = parseEnvOverride(allocator, raw) catch {
                    return usageError(allocator, "Environment overrides must look like KEY=VALUE (got '{s}')", .{raw});
                };
                try env_overrides.append(override);
                continue;
            }
            if (argEqual(arg, "--env")) {
                if (idx >= argv.len) return usageError(allocator, "--env requires KEY=VALUE", .{});
                const raw = argv[idx];
                idx += 1;
                const override = parseEnvOverride(allocator, raw) catch {
                    return usageError(allocator, "Environment overrides must look like KEY=VALUE (got '{s}')", .{raw});
                };
                try env_overrides.append(override);
                continue;
            }

            return usageError(allocator, "Unknown option '{s}'", .{arg});
        }

        if (!parsing_options and script_path != null and argEqual(arg, "--") and script_args.items.len == 0) {
            continue;
        }

        if (script_path == null) {
            script_path = try dupSlice(allocator, arg);
            parsing_options = false;
            continue;
        }

        try script_args.append(try dupSlice(allocator, arg));
    }

    if (script_path != null and repl_requested) {
        return usageError(allocator, "Cannot combine --repl with a script path.", .{});
    }
    if (script_path == null and !repl_requested) {
        return usageError(allocator, "Provide a Runic script path or pass --repl for interactive mode.", .{});
    }

    const trace_slice = try finalizeList([]const u8, &trace_topics, &trace_cleanup);
    const module_slice = try finalizeList([]const u8, &module_paths, &module_cleanup);
    const env_slice = try finalizeList(CliConfig.EnvOverride, &env_overrides, &env_cleanup);
    const args_slice = try finalizeList([]const u8, &script_args, &args_cleanup);

    if (script_path) |path| {
        return ParseResult{
            .ready = .{
                .mode = .{ .script = .{ .path = path, .args = args_slice } },
                .trace_topics = trace_slice,
                .module_paths = module_slice,
                .env_overrides = env_slice,
            },
        };
    }

    freeStringList(allocator, args_slice);
    return ParseResult{
        .ready = .{
            .mode = .repl,
            .trace_topics = trace_slice,
            .module_paths = module_slice,
            .env_overrides = env_slice,
        },
    };
}

fn printUsage(writer: *std.Io.Writer) !void {
    try writer.print(
        \\Runic CLI
        \\Usage:
        \\  runic [options] path/to/script.rn [-- <script args>...]
        \\  runic --repl
        \\
        \\Options:
        \\  --help, -h           Display this help text.
        \\  --repl               Start the interactive REPL (history + multiline editing).
        \\  --trace <topic>      Enable detailed tracing of interpreter subsystems.
        \\  --module-path <dir>  Append an additional module lookup directory.
        \\  --env KEY=VALUE      Override an environment variable during execution.
        \\
        \\Additional arguments after -- are forwarded to the script unchanged.
        \\Script execution remains stubbed while the interpreter matures.
        \\
    , .{});
    try writer.flush();
}

fn argEqual(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

fn parseEnvOverride(allocator: std.mem.Allocator, raw: []const u8) !CliConfig.EnvOverride {
    const eq_index = std.mem.indexOfScalar(u8, raw, '=') orelse return error.InvalidEnvOverride;
    if (eq_index == 0) return error.InvalidEnvOverride;

    const key = try dupSlice(allocator, raw[0..eq_index]);
    errdefer allocator.free(key);

    const value = try dupSlice(allocator, raw[eq_index + 1 ..]);
    return .{ .key = key, .value = value };
}

fn usageError(allocator: std.mem.Allocator, comptime message_fmt: []const u8, args: anytype) !ParseResult {
    const message = try std.fmt.allocPrint(allocator, message_fmt, args);
    return ParseResult{ .usage_error = message };
}

fn dupSlice(allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
    const copy = try allocator.alloc(u8, bytes.len);
    @memcpy(copy, bytes);
    return copy;
}

fn finalizeList(comptime T: type, list: *ManagedArrayList(T), cleanup_flag: *bool) ![]T {
    if (list.items.len == 0) {
        return &[_]T{};
    }
    const owned = try list.toOwnedSlice();
    cleanup_flag.* = false;
    return owned;
}

fn printStringList(writer: *std.Io.Writer, label: []const u8, values: [][]const u8) !void {
    if (values.len == 0) {
        try writer.print("{s}: (none)\n", .{label});
        try writer.flush();
        return;
    }
    try writer.print("{s}: ", .{label});
    for (values, 0..) |value, idx| {
        if (idx > 0) try writer.print(", ", .{});
        try writer.print("{s}", .{value});
    }
    try writer.writeByte('\n');
    try writer.flush();
}

fn printEnvOverrides(writer: *std.Io.Writer, overrides: []const CliConfig.EnvOverride) !void {
    if (overrides.len == 0) {
        try writer.print("Env overrides: (none)\n", .{});
        try writer.flush();
        return;
    }
    try writer.print("Env overrides:\n", .{});
    for (overrides) |override| {
        try writer.print("  {s}={s}\n", .{ override.key, override.value });
    }
    try writer.flush();
}

fn freeStringList(allocator: std.mem.Allocator, values: [][]const u8) void {
    if (values.len == 0) return;
    for (values) |value| allocator.free(value);
    allocator.free(values);
}

fn freeEnvOverrides(allocator: std.mem.Allocator, overrides: []CliConfig.EnvOverride) void {
    if (overrides.len == 0) return;
    for (overrides) |override| {
        allocator.free(override.key);
        allocator.free(override.value);
    }
    allocator.free(overrides);
}

test "parse script invocation with tracing" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const args = [_][]const u8{
        "runic",
        "--trace",
        "lexer",
        "--module-path",
        "lib",
        "script.rn",
        "--",
        "--flag",
    };

    const argv = args[0..args.len];
    const result = try parseCommandLine(gpa.allocator(), @constCast(argv));
    defer switch (result) {
        .ready => |config| {
            var cfg = config;
            cfg.deinit(gpa.allocator());
        },
        .usage_error => |message| gpa.allocator().free(message),
        else => {},
    };

    const config = switch (result) {
        .ready => |cfg| cfg,
        .usage_error => return error.UnexpectedUsageFailure,
        .show_help => return error.UnexpectedHelp,
    };
    try std.testing.expect(std.meta.activeTag(config.mode) == .script);
    const script = config.mode.script;
    try std.testing.expectEqualStrings("script.rn", script.path);
    try std.testing.expectEqual(@as(usize, 1), script.args.len);
    try std.testing.expectEqualStrings("--flag", script.args[0]);
    try std.testing.expectEqual(@as(usize, 1), config.trace_topics.len);
    try std.testing.expectEqualStrings("lexer", config.trace_topics[0]);
    try std.testing.expectEqual(@as(usize, 1), config.module_paths.len);
    try std.testing.expectEqualStrings("lib", config.module_paths[0]);
}

test "parse repl invocation with env override" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const args = [_][]const u8{
        "runic",
        "--repl",
        "--env",
        "PORT=8080",
    };

    const argv = args[0..args.len];
    const result = try parseCommandLine(gpa.allocator(), @constCast(argv));
    defer switch (result) {
        .ready => |config| {
            var cfg = config;
            cfg.deinit(gpa.allocator());
        },
        .usage_error => |message| gpa.allocator().free(message),
        else => {},
    };

    const config = switch (result) {
        .ready => |cfg| cfg,
        .usage_error => return error.UnexpectedUsageFailure,
        .show_help => return error.UnexpectedHelp,
    };
    try std.testing.expect(std.meta.activeTag(config.mode) == .repl);
    try std.testing.expectEqual(@as(usize, 1), config.env_overrides.len);
    try std.testing.expectEqualStrings("PORT", config.env_overrides[0].key);
    try std.testing.expectEqualStrings("8080", config.env_overrides[0].value);
}
