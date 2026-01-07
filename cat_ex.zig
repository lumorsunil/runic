const std = @import("std");

pub fn main() !void {
    var child = std.process.Child.init(&.{"cat"}, std.heap.page_allocator);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    try child.spawn();
    try child.waitForSpawn();
    const child_stdin = child.stdin.?;
    const child_stdout = child.stdout.?;
    const child_stderr = child.stderr.?;

    var stdin_buffer: [1024]u8 = undefined;
    var child_stdin_writer = child_stdin.writer(&stdin_buffer);
    var stdout_buffer: [1024]u8 = undefined;
    var child_stdout_reader = child_stdout.reader(&stdout_buffer);
    var stderr_buffer: [1024]u8 = undefined;
    var child_stderr_reader = child_stderr.reader(&stderr_buffer);

    try child_stdin_writer.interface.writeAll("hello\n");
    try child_stdin_writer.interface.flush();
    child_stdin.close();

    var stdout = std.Io.Writer.Allocating.init(std.heap.page_allocator);
    var stderr = std.Io.Writer.Allocating.init(std.heap.page_allocator);

    var stdout_ended: bool = false;
    var stderr_ended: bool = false;

    while (true) {
        const stdout_bytes_read = child_stdout_reader.interface.stream(&stdout.writer, .unlimited) catch |err| brk: switch (err) {
            error.EndOfStream => {
                std.log.debug("[stdout]: end of stream", .{});
                stdout_ended = true;
                if (stdout_ended and stderr_ended) break;
                break :brk 0;
            },
            else => return err,
        };

        const stderr_bytes_read = child_stderr_reader.interface.stream(&stderr.writer, .unlimited) catch |err| brk: switch (err) {
            error.EndOfStream => {
                std.log.debug("[stderr]: end of stream", .{});
                stderr_ended = true;
                if (stdout_ended and stderr_ended) break;
                break :brk 0;
            },
            else => return err,
        };

        if (stdout_bytes_read > 0) {
            std.log.debug("[stdout]: bytes read: {}", .{stdout_bytes_read});
            std.log.debug("[stdout]: current: \"{s}\"", .{stdout.written()});
        }
        if (stderr_bytes_read > 0) {
            std.log.debug("[stderr]: bytes read: {}", .{stderr_bytes_read});
            std.log.debug("[stderr]: current: \"{s}\"", .{stderr.written()});
        }
    }

    std.log.debug("[stdout]: result: \"{s}\"", .{stdout.written()});
    std.log.debug("[stderr]: result: \"{s}\"", .{stderr.written()});
}
