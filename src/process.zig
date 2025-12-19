const std = @import("std");
const Closeable = @import("closeable.zig").Closeable;
const CloseableReader = @import("closeable.zig").CloseableReader;
const CloseableWriter = @import("closeable.zig").CloseableWriter;
const ReaderWriterStream = @import("stream.zig").ReaderWriterStream;
const ExitCode = @import("runtime/command_runner.zig").ExitCode;

const log_enabled = false;

fn log(comptime fmt: []const u8, args: anytype) void {
    if (!log_enabled) return;
    std.log.debug(fmt, args);
}

fn spawn(
    argv: []const []const u8,
    stdin_behavior: std.process.Child.StdIo,
    stdout_behavior: std.process.Child.StdIo,
    stderr_behavior: std.process.Child.StdIo,
) !std.process.Child {
    log("Child.init {s}", .{argv[0]});
    var child = std.process.Child.init(argv, std.heap.page_allocator);
    child.stdin_behavior = stdin_behavior;
    child.stdout_behavior = stdout_behavior;
    child.stderr_behavior = stderr_behavior;
    log("Child.spawn {s}", .{argv[0]});
    try child.spawn();
    log("Child.waitForSpawn {s}", .{argv[0]});
    try child.waitForSpawn();
    return child;
}

pub const ProcessCloseable = struct {
    process: *std.process.Child,
    waited: bool = false,
    closeable: Closeable(ExitCode) = .{ .vtable = &vtable },
    stdin: Closeable(ExitCode) = .{ .vtable = &stdin_vtable },
    stdin_term: ?ExitCode = null,
    stdout: Closeable(ExitCode) = .{ .vtable = &stdout_vtable },
    stdout_term: ?ExitCode = null,
    stderr: Closeable(ExitCode) = .{ .vtable = &stderr_vtable },
    stderr_term: ?ExitCode = null,

    const vtable = Closeable(ExitCode).VTable{
        .close = close,
        .getResult = getResult,
    };

    const stdin_vtable = Closeable(ExitCode).VTable{
        .close = stdin_close,
        .getResult = stdin_getResult,
    };

    const stdout_vtable = Closeable(ExitCode).VTable{
        .close = stdout_close,
        .getResult = stdout_getResult,
    };

    const stderr_vtable = Closeable(ExitCode).VTable{
        .close = stderr_close,
        .getResult = stderr_getResult,
    };

    pub fn init(process: *std.process.Child) @This() {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        log("{s}: stdin {}", .{ process.argv[0], process.stdin != null });
        log("{s}: stdout {}", .{ process.argv[0], process.stdout != null });
        log("{s}: stderr {}", .{ process.argv[0], process.stderr != null });

        return .{
            .process = process,
            .stdin_term = if (process.stdin) |_| null else .success,
            .stdout_term = if (process.stdout) |_| null else .success,
            .stderr_term = if (process.stderr) |_| null else .success,
        };
    }

    fn close(self: *Closeable(ExitCode)) ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("closeable", self);
        log("closing {s}", .{parent.process.argv[0]});
        if (parent.waited) return .fromTerm(parent.process.term.?);
        parent.waited = true;
        log("waiting for {s} to terminate", .{parent.process.argv[0]});
        return .fromTerm(parent.process.wait());
    }

    fn getResult(self: *Closeable(ExitCode)) ?ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("closeable", self);
        return if (parent.process.term) |term| .fromTerm(term) else null;
    }

    fn check_close_parent(self: *@This()) void {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        if (self.stdin.isClosed() and self.stdout.isClosed() and self.stderr.isClosed()) {
            log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
            _ = self.closeable.close();
        }
    }

    fn stdin_close(self: *Closeable(ExitCode)) ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stdin", self);
        log("closing stdin of {s}", .{parent.process.argv[0]});
        if (parent.process.stdin) |stdin| {
            stdin.close();
            parent.process.stdin = null;
        }
        if (parent.stdin_term) |term| return term;
        parent.stdin_term = .success;
        parent.check_close_parent();
        return parent.stdin_term.?;
    }

    fn stdin_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stdin", self);
        return parent.stdin_term;
    }

    fn stdout_close(self: *Closeable(ExitCode)) ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stdout", self);
        log("closing stdout of {s}", .{parent.process.argv[0]});
        if (parent.process.stdout) |stdout| {
            stdout.close();
            parent.process.stdout = null;
        }
        if (parent.stdout_term) |term| return term;
        parent.stdout_term = .success;
        parent.check_close_parent();
        return parent.stdout_term.?;
    }

    fn stdout_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stdout", self);
        return parent.stdout_term;
    }

    fn stderr_close(self: *Closeable(ExitCode)) ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stderr", self);
        log("closing stderr of {s}", .{parent.process.argv[0]});
        if (parent.process.stderr) |stderr| {
            stderr.close();
            parent.process.stderr = null;
        }
        if (parent.stderr_term) |term| return term;
        parent.stderr_term = .success;
        parent.check_close_parent();
        return parent.stderr_term.?;
    }

    fn stderr_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stderr", self);
        return parent.stderr_term;
    }
};

const PipeReader = struct {
    file: ?std.fs.File,
    file_reader: ?std.fs.File.Reader,
    reader: std.Io.Reader = .{ .vtable = &vtable, .buffer = &.{}, .seek = 0, .end = 0 },

    const vtable = std.Io.Reader.VTable{
        .stream = stream,
    };

    pub fn init(file: ?std.fs.File, buffer: []u8) PipeReader {
        return .{
            .file = file,
            .file_reader = if (file) |f| f.reader(buffer) else null,
        };
    }

    fn getParent(r: *std.Io.Reader) *PipeReader {
        return @fieldParentPtr("reader", r);
    }

    pub fn stream(
        r: *std.Io.Reader,
        w: *std.Io.Writer,
        limit: std.Io.Limit,
    ) std.Io.Reader.StreamError!usize {
        const parent = getParent(r);
        const file = parent.file orelse return 0;
        const file_reader = if (parent.file_reader) |*fr| fr else return 0;

        var poll_fds = [_]std.posix.pollfd{
            .{
                .fd = file.handle,
                .events = std.posix.POLL.IN,
                .revents = 0,
            },
        };
        const poll_fd = &poll_fds[0];

        const result = std.posix.poll(&poll_fds, 0) catch return 0;
        std.log.debug("poll result: {}", .{result});
        std.log.debug("POLLIN: {}", .{poll_fd.revents & std.posix.POLL.IN});
        std.log.debug("POLLHUP: {}", .{poll_fd.revents & std.posix.POLL.HUP});
        std.log.debug("POLLNVAL: {}", .{poll_fd.revents & std.posix.POLL.NVAL});
        if (std.posix.errno(result) == std.posix.E.SUCCESS) {
            if (poll_fd.revents & std.posix.POLL.IN > 0) {
                return try file_reader.interface.stream(w, limit);
            } else if (poll_fd.revents & (std.posix.POLL.HUP | std.posix.POLL.NVAL) > 0) {
                return error.EndOfStream;
            }

            return 0;
        } else {
            return error.ReadFailed;
        }
    }
};

pub const CloseableProcessIo = struct {
    process: *std.process.Child,
    stdin_buffer: [1024]u8 = undefined,
    stdin_writer: ?std.fs.File.Writer = null,
    stdout_buffer: [1024]u8 = undefined,
    // stdout_reader: ?std.fs.File.Reader = null,
    stdout_reader: ?PipeReader = null,
    stderr_buffer: [1024]u8 = undefined,
    // stderr_reader: ?std.fs.File.Reader = null,
    stderr_reader: ?PipeReader = null,
    process_closeable: ProcessCloseable = undefined,

    pub fn init(process: *std.process.Child) @This() {
        return .{ .process = process };
    }

    pub fn connect(self: *@This()) void {
        if (self.process.stdin) |f| self.stdin_writer = f.writer(&self.stdin_buffer);
        // if (self.process.stdout) |f| self.stdout_reader = f.reader(&self.stdout_buffer);
        // if (self.process.stderr) |f| self.stderr_reader = f.reader(&self.stderr_buffer);
        if (self.process.stdout) |f| self.stdout_reader = .init(f, &self.stdout_buffer);
        if (self.process.stderr) |f| self.stderr_reader = .init(f, &self.stderr_buffer);
        self.process_closeable = .init(self.process);
    }

    pub fn stdin(self: *@This()) *std.Io.Writer {
        return &self.stdin_writer.?.interface;
    }

    pub fn stdout(self: *@This()) *std.Io.Reader {
        return &self.stdout_reader.?.reader;
    }

    pub fn stderr(self: *@This()) *std.Io.Reader {
        return &self.stderr_reader.?.reader;
    }

    pub fn closeable(self: *@This()) *Closeable(ExitCode) {
        return &self.process_closeable.closeable;
    }

    pub fn closeableStdin(self: *@This()) CloseableWriter(ExitCode) {
        return .init(self.stdin(), &self.process_closeable.stdin);
    }

    pub fn closeableStdout(self: *@This()) CloseableReader(ExitCode) {
        return .init(self.stdout(), &self.process_closeable.stdout);
    }

    pub fn closeableStderr(self: *@This()) CloseableReader(ExitCode) {
        return .init(self.stderr(), &self.process_closeable.stderr);
    }

    pub fn pipeStdout(
        self: *@This(),
        label: []const u8,
        destination: *@This(),
    ) *ReaderWriterStream {
        return .init(
            label,
            self.closeableStdout(),
            destination.closeableStdin(),
        );
    }

    pub fn pipeStdoutWriter(
        self: *@This(),
        label: []const u8,
        destination: CloseableWriter,
    ) *ReaderWriterStream {
        return .init(
            label,
            self.closeableStdout(),
            destination,
        );
    }

    pub fn pipeStderr(
        self: *@This(),
        label: []const u8,
        destination: *@This(),
    ) *ReaderWriterStream {
        return .init(
            label,
            self.closeableStderr(),
            destination.closeableStdin(),
        );
    }
};
