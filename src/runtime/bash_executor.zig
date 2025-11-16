const std = @import("std");
const command_runner = @import("command_runner.zig");

const CommandRunner = command_runner.CommandRunner;
const ProcessHandle = command_runner.ProcessHandle;

/// BashExecutor runs `bash { ... }` compatibility blocks by shelling out via
/// the system `bash` binary and returning a standard `ProcessHandle` so the
/// rest of the interpreter interacts with them like any other command stage.
pub const BashExecutor = struct {
    runner: CommandRunner,
    config: Config,

    pub const Error = CommandRunner.Error;

    pub const Config = struct {
        /// Path (relative or absolute) to the bash binary.
        shell_path: []const u8 = "bash",
        /// Additional arguments inserted before the script body (defaults to -c).
        shell_args: []const []const u8 = &.{"-c"},
        /// Working directory to apply when no per-run override is provided.
        cwd: ?[]const u8 = null,
        /// Optional environment map shared across executions unless overridden.
        env_map: ?*const std.process.EnvMap = null,
        /// Upper bound on captured stdout/stderr unless overridden.
        max_output_bytes: ?usize = null,
    };

    pub const RunOptions = struct {
        cwd: ?[]const u8 = null,
        env_map: ?*const std.process.EnvMap = null,
        max_output_bytes: ?usize = null,
    };

    pub fn init(allocator: std.mem.Allocator, config: Config) BashExecutor {
        return .{
            .runner = CommandRunner.init(allocator),
            .config = config,
        };
    }

    pub fn withRunner(runner: CommandRunner, config: Config) BashExecutor {
        return .{ .runner = runner, .config = config };
    }

    /// Executes `body` verbatim inside `bash`, capturing stdout/stderr into a
    /// `ProcessHandle` so Runic scripts can treat the block like a standard
    /// command invocation.
    pub fn run(self: BashExecutor, body: []const u8) Error!ProcessHandle {
        return self.runWithOptions(body, .{});
    }

    /// Same as `run` but allows overriding the working directory, environment,
    /// or capture limits for a single block execution.
    pub fn runWithOptions(self: BashExecutor, body: []const u8, options: RunOptions) Error!ProcessHandle {
        var argv_builder = std.ArrayList([]const u8).init(self.runner.allocator);
        defer argv_builder.deinit();

        try argv_builder.append(self.config.shell_path);
        for (self.config.shell_args) |arg| {
            if (arg.len == 0) continue;
            try argv_builder.append(arg);
        }
        try argv_builder.append(body);

        var spec = CommandRunner.CommandSpec{
            .argv = argv_builder.items,
            .cwd = options.cwd orelse self.config.cwd,
            .env_map = options.env_map orelse self.config.env_map,
        };
        if (options.max_output_bytes) |limit| {
            spec.max_output_bytes = limit;
        } else if (self.config.max_output_bytes) |limit| {
            spec.max_output_bytes = limit;
        }

        return self.runner.runSync(spec);
    }
};

fn expectStringEqual(expected: []const u8, actual: []const u8) !void {
    try std.testing.expectEqualStrings(expected, actual);
}

test "bash executor captures stdout and stderr" {
    var executor = BashExecutor.init(std.testing.allocator, .{});
    var handle = try executor.run(
        \\printf 'alpha';
        \\printf 'beta' 1>&2;
    );
    defer handle.deinit();

    try expectStringEqual("alpha", handle.stdoutBytes());
    try expectStringEqual("beta", handle.stderrBytes());
    try std.testing.expect(handle.status.ok);
}

test "bash executor surfaces non-zero exit codes" {
    var executor = BashExecutor.init(std.testing.allocator, .{});
    var handle = try executor.run(
        \\printf 'before';
        \\exit 7;
    );
    defer handle.deinit();

    try expectStringEqual("before", handle.stdoutBytes());
    try std.testing.expect(!handle.status.ok);
    try std.testing.expectEqual(@as(?u8, 7), handle.status.exit_code);
    try std.testing.expectEqual(@as(?usize, 0), handle.status.failed_stage);
}

test "bash executor respects environment overrides" {
    var env_map = try std.process.EnvMap.init(std.testing.allocator);
    defer env_map.deinit();
    try env_map.put("RUNIC_BASH_VALUE", "legacy");

    var executor = BashExecutor.init(std.testing.allocator, .{});
    var handle = try executor.runWithOptions(
        \\printf '%s' "$RUNIC_BASH_VALUE";
    , .{ .env_map = &env_map });
    defer handle.deinit();

    try expectStringEqual("legacy", handle.stdoutBytes());
    try std.testing.expect(handle.status.ok);
}
