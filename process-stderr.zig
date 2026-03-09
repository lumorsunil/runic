const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var child = std.process.Child.init(&.{ "echo", "hello, world!" }, allocator);
    child.stdin_behavior = .Close;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    try child.spawn();
    try child.waitForSpawn();

    var allocating_writer = std.Io.Writer.Allocating.init(allocator);

    const child_stdout = child.stdout.?;
    const child_stderr = child.stderr.?;

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file_reader = child_stdout.reader(&stdout_buffer);

    std.log.debug("checking stdout", .{});

    while (true) {
        _ = stdout_file_reader.interface.stream(&allocating_writer.writer, .unlimited) catch |err| {
            switch (err) {
                std.Io.Reader.StreamError.EndOfStream => {
                    std.log.debug("stdout closed", .{});
                    break;
                },
                else => {
                    std.log.debug("stdout error: {}", .{err});
                    break;
                },
            }
        };
    }

    std.log.debug("checking stderr", .{});

    var stderr_buffer: [1024]u8 = undefined;
    var stderr_file_reader = child_stderr.reader(&stderr_buffer);

    while (true) {
        _ = stderr_file_reader.interface.stream(&allocating_writer.writer, .unlimited) catch |err| {
            switch (err) {
                std.Io.Reader.StreamError.EndOfStream => {
                    std.log.debug("stderr closed", .{});
                    break;
                },
                else => {
                    std.log.debug("stderr error: {}", .{err});
                    break;
                },
            }
        };
    }
}
