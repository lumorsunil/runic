const std = @import("std");
const utils = @import("main-utils.zig");
const dispatch = @import("dispatch.zig").dispatch;

pub fn main() !void {
    const exit_code = try mainImpl();
    if (exit_code != 0) {
        std.process.exit(exit_code);
    }
}

fn mainImpl() !u8 {
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
    defer stdout.flush() catch {};
    defer stderr.flush() catch {};

    const result = try utils.parseCommandLine(allocator, argv);
    switch (result) {
        .show_help => {
            try utils.printUsage(stdout);
            return 0;
        },
        .usage_error => |message| {
            defer allocator.free(message);
            try stderr.print("error: {s}\n\n", .{message});
            try utils.printUsage(stderr);
            try stderr.flush();
            return 2;
        },
        .ready => |config| {
            defer {
                var cfg = config;
                cfg.deinit(allocator);
            }
            return try dispatch(allocator, config, stdout, stderr);
        },
    }

    return 0;
}
