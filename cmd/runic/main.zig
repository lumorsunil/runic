const std = @import("std");
const utils = @import("main-utils.zig");
const dispatch = @import("dispatch.zig").dispatch;
const runic = @import("runic");

pub fn main() !void {
    const exit_code = mainImpl() catch |err| {
        std.log.err("runic exited with error: {t}", .{err});
        std.process.exit(1);
    };
    if (exit_code != .success) {
        std.process.exit(exit_code.getErrorCode());
    }
}

fn mainImpl() !runic.command_runner.ExitCode {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 24 }){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) std.log.err("runic CLI leaked memory", .{});
    }

    const allocator = gpa.allocator();

    // var stdin_buffer: [1024]u8 = undefined;
    var stdout_buffer: [1024]u8 = undefined;
    var stderr_buffer: [1024]u8 = undefined;
    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    // var stdin_reader = std.fs.File.stdin().readerStreaming(&stdin_buffer);
    var stdout_writer = std.fs.File.stdout().writerStreaming(&stdout_buffer);
    var stderr_writer = std.fs.File.stderr().writerStreaming(&stderr_buffer);
    // const stdin = &stdin_reader.interface;
    const stdout = &stdout_writer.interface;
    const stderr = &stderr_writer.interface;
    defer stdout.flush() catch {};
    defer stderr.flush() catch {};

    const result = try utils.parseCommandLine(allocator, argv);
    switch (result) {
        .show_help => {
            try utils.printUsage(stdout);
            return .success;
        },
        .usage_error => |message| {
            defer allocator.free(message);
            try stderr.print("error: {s}\n\n", .{message});
            try utils.printUsage(stderr);
            try stderr.flush();
            return .fromByte(2);
        },
        .ready => |config| {
            defer {
                var cfg = config;
                cfg.deinit(allocator);
            }
            return try dispatch(
                allocator,
                config,
                // stdin,
                stdout,
                stderr,
            );
        },
    }

    return .success;
}
