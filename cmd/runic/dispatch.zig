const std = @import("std");
const Allocator = std.mem.Allocator;
const utils = @import("main-utils.zig");
const runScript = @import("run_script.zig").runScript;
const runic = @import("runic");

pub fn dispatch(
    allocator: Allocator,
    config: utils.CliConfig,
    stdin: *std.Io.Reader,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    tracer: *runic.trace.Tracer,
) !runic.ExitCode {
    return try runScript(
        allocator,
        config.script,
        config,
        stdin,
        stdout,
        stderr,
        tracer,
    );
}
