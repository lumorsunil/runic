const std = @import("std");
const command_runner = @import("runtime/command_runner.zig");
const ProcessHandle = command_runner.ProcessHandle;

pub fn forwardHandleOutput(handle: *const ProcessHandle, stdout: *std.Io.Writer, stderr: *std.Io.Writer) !void {
    const captures = handle.stageCaptures();
    if (captures.len == 0) return;

    const stdout_bytes = handle.stdoutBytes();
    if (stdout_bytes.len > 0) try stdout.writeAll(stdout_bytes);

    const stderr_bytes = handle.stderrBytes();
    if (stderr_bytes.len > 0) try stderr.writeAll(stderr_bytes);

    try stdout.flush();
    try stderr.flush();
}
