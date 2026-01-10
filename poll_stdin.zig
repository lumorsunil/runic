const std = @import("std");

pub fn main() !void {
    var child = std.process.Child.init(&.{ "grep", "hello" }, std.heap.page_allocator);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    try child.spawn();
    try child.waitForSpawn();
    const child_stdin = child.stdin.?;
    const child_stdout = child.stdout.?;
    const child_stderr = child.stderr.?;

    var stdin_buffer: [1024]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&stdin_buffer);
    const stdin_reader = &stdin.interface;

    while (true) {
        std.log.debug("Press enter for next events.", .{});
        const command = try stdin_reader.takeDelimiterExclusive('\n');

        var pollfd = [_]std.posix.pollfd{ .{
            .fd = child_stdin.handle,
            .events = std.posix.POLL.OUT,
            .revents = 0,
        }, .{
            .fd = child_stdout.handle,
            .events = std.posix.POLL.IN | std.posix.POLL.ERR,
            .revents = 0,
        }, .{
            .fd = child_stderr.handle,
            .events = std.posix.POLL.IN | std.posix.POLL.ERR,
            .revents = 0,
        } };
        _ = try std.posix.poll(&pollfd, 0);

        std.log.debug("stdin: POLLOUT: {}, POLLHUP: {}, POLLNVAL: {}, POLLERR: {}", .{
            pollfd[0].revents & std.posix.POLL.OUT,
            pollfd[0].revents & std.posix.POLL.HUP,
            pollfd[0].revents & std.posix.POLL.NVAL,
            pollfd[0].revents & std.posix.POLL.ERR,
        });

        if (std.mem.startsWith(u8, command, "in:")) {
            if (pollfd[0].revents & std.posix.POLL.OUT > 0) {
                const forward_in = command[3..];
                std.log.debug("writing to stdin: \"{s}\"", .{forward_in});
                try child_stdin.writeAll(forward_in);
            }
        }

        std.log.debug("stdout: POLLIN: {}, POLLHUP: {}, POLLNVAL: {}, POLLERR: {}", .{
            pollfd[1].revents & std.posix.POLL.IN,
            pollfd[1].revents & std.posix.POLL.HUP,
            pollfd[1].revents & std.posix.POLL.NVAL,
            pollfd[1].revents & std.posix.POLL.ERR,
        });

        if (std.mem.eql(u8, command, "out")) {
            std.log.debug("printing output from stdout", .{});
            if (pollfd[1].revents & std.posix.POLL.IN > 0) {
                var stdout_buffer: [1024]u8 = undefined;
                const bytes_read = try child_stdout.read(&stdout_buffer);
                std.log.debug("stdout:{s}", .{stdout_buffer[0..bytes_read]});
            }
        }

        std.log.debug("stderr: POLLIN: {}, POLLHUP: {}, POLLNVAL: {}, POLLERR: {}", .{
            pollfd[2].revents & std.posix.POLL.IN,
            pollfd[2].revents & std.posix.POLL.HUP,
            pollfd[2].revents & std.posix.POLL.NVAL,
            pollfd[2].revents & std.posix.POLL.ERR,
        });
    }
}
