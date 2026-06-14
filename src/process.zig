const std = @import("std");
const Closeable = @import("closeable.zig").Closeable;
const CloseableReader = @import("closeable.zig").CloseableReader;
const CloseableWriter = @import("closeable.zig").CloseableWriter;
const ReaderWriterStream = @import("stream.zig").ReaderWriterStream;
const ExitCode = @import("runtime/exit_code.zig").ExitCode;
const TraceWriter = @import("trace-writer.zig").TraceWriter;
const Tracer = @import("trace.zig").Tracer;

const log_enabled = false;

fn log(comptime fmt: []const u8, args: anytype) void {
    if (!log_enabled) return;
    std.log.debug(fmt, args);
}

pub const ProcessCloseable = struct {
    io: std.Io,
    process: *std.process.Child,
    label: []const u8,
    term: ?ExitCode = null,
    closeable: Closeable(ExitCode) = .{ .vtable = &vtable },
    stdin: Closeable(ExitCode) = .{ .vtable = &stdin_vtable },
    stdin_term: ?ExitCode = null,
    stdout: Closeable(ExitCode) = .{ .vtable = &stdout_vtable },
    stdout_term: ?ExitCode = null,
    stderr: Closeable(ExitCode) = .{ .vtable = &stderr_vtable },
    stderr_term: ?ExitCode = null,
    tracer: *Tracer,

    const vtable = Closeable(ExitCode).VTable{
        .close = close,
        .getResult = getResult,
        .getLabel = getLabel,
    };

    const stdin_vtable = Closeable(ExitCode).VTable{
        .close = stdin_close,
        .getResult = stdin_getResult,
        .getLabel = stdin_getLabel,
    };

    const stdout_vtable = Closeable(ExitCode).VTable{
        .close = stdout_close,
        .getResult = stdout_getResult,
        .getLabel = stdout_getLabel,
    };

    const stderr_vtable = Closeable(ExitCode).VTable{
        .close = stderr_close,
        .getResult = stderr_getResult,
        .getLabel = stderr_getLabel,
    };

    pub fn init(io: std.Io, process: *std.process.Child, label: []const u8, tracer: *Tracer) @This() {
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, @typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{label});
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "{s}: stdin: {}", .{ label, process.stdin != null });
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "{s}: stdout: {}", .{ label, process.stdout != null });
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "{s}: stderr: {}", .{ label, process.stderr != null });

        return .{
            .io = io,
            .process = process,
            .label = label,
            .stdin_term = if (process.stdin) |_| null else .success,
            .stdout_term = if (process.stdout) |_| null else .success,
            .stderr_term = if (process.stderr) |_| null else .success,
            .tracer = tracer,
        };
    }

    fn close(self: *Closeable(ExitCode)) ExitCode {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        parent.tracer.trace(.information, &.{ "process", @src().fn_name }, null, @typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.label});
        parent.tracer.trace(.information, &.{ "process", @src().fn_name }, null, "closing {s}", .{parent.label});
        if (parent.term) |term| return term;
        // `Child.wait` is one-shot and asserts `id != null`. If the process was
        // already reaped elsewhere (e.g. the owning thread's cleanup wait), don't
        // wait again — fall back to success.
        if (parent.process.id == null) {
            parent.term = .success;
            return .success;
        }
        parent.tracer.trace(.information, &.{ "process", @src().fn_name }, null, "waiting for {s} to terminate", .{parent.label});
        const exit_code: ExitCode = .fromTerm(parent.process.wait(parent.io));
        parent.term = exit_code;
        parent.tracer.trace(.information, &.{ "process", @src().fn_name }, null, "{s} exited with {any}", .{ parent.label, exit_code });
        return exit_code;
    }

    fn logState(self: *@This()) void {
        log("stdin: {}, stdout: {}, stderr: {}", .{
            self.stdin.isClosed(), self.stdout.isClosed(), self.stderr.isClosed(),
        });
    }

    fn getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.label});
        if (parent.term) |term| {
            log("result: {f}", .{term});
            return term;
        } else {
            log("result: null", .{});
            return null;
        }
    }

    pub fn getLabel(self: *Closeable(ExitCode)) []const u8 {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        return parent.label;
    }

    fn check_close_parent(self: *@This()) ?ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{self.label});

        log("stdin: {}, stdout: {}, stderr: {}", .{
            self.stdin.isClosed(), self.stdout.isClosed(), self.stderr.isClosed(),
        });

        // Process completion must not depend on stdin reaching EOF. Commands such
        // as `head -n 1` exit early after producing output, leaving the parent-side
        // stdin pipe open even though stdout/stderr are already drained.
        if (self.stdout.isClosed() and self.stderr.isClosed()) {
            if (!self.stdin.isClosed()) _ = self.stdin.close();
            return self.closeable.close();
        }

        return null;
    }

    fn stdin_close(self: *Closeable(ExitCode)) ExitCode {
        const parent: *@This() = @fieldParentPtr("stdin", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.label});
        log("closing stdin of {s}", .{parent.label});
        if (parent.process.stdin) |stdin| {
            stdin.close(parent.io);
            parent.process.stdin = null;
        }
        if (parent.stdin_term) |term| return term;
        parent.stdin_term = .success;
        parent.stdin_term = parent.check_close_parent() orelse .success;
        return parent.stdin_term.?;
    }

    fn stdin_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("stdin", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.label});
        return parent.closeable.getResult() orelse parent.stdin_term orelse {
            if (parent.process.stdin) |stdin| {
                const revents = poll(stdin, std.posix.POLL.OUT, parent.tracer);
                if (revents & std.posix.POLL.ERR > 0) {
                    parent.stdin_term = .success;
                    parent.stdin_term = parent.check_close_parent() orelse .success;
                }
            }

            return parent.stdin_term;
        };
    }

    fn stdin_getLabel(self: *Closeable(ExitCode)) []const u8 {
        const parent: *@This() = @fieldParentPtr("stdin", self);
        return parent.closeable.getLabel();
    }

    fn stdout_close(self: *Closeable(ExitCode)) ExitCode {
        const parent: *@This() = @fieldParentPtr("stdout", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.label});
        log("closing stdout of {s}", .{parent.label});
        if (parent.process.stdout) |stdout| {
            stdout.close(parent.io);
            parent.process.stdout = null;
        }
        if (parent.stdout_term) |term| return term;
        parent.stdout_term = .success;
        parent.stdout_term = parent.check_close_parent() orelse .success;
        return parent.stdout_term.?;
    }

    fn stdout_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("stdout", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.label});
        return parent.closeable.getResult() orelse parent.stdout_term orelse {
            if (parent.process.stdout) |stdout| {
                const revents = poll(stdout, std.posix.POLL.IN, parent.tracer);
                if (revents & std.posix.POLL.IN == 0 and revents & std.posix.POLL.HUP > 0) {
                    parent.stdout_term = .success;
                    parent.stdout_term = parent.check_close_parent() orelse .success;
                }
            }

            return parent.stdout_term;
        };
    }

    fn stdout_getLabel(self: *Closeable(ExitCode)) []const u8 {
        const parent: *@This() = @fieldParentPtr("stdout", self);
        return parent.closeable.getLabel();
    }

    fn stderr_close(self: *Closeable(ExitCode)) ExitCode {
        const parent: *@This() = @fieldParentPtr("stderr", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.label});
        log("closing stderr of {s}", .{parent.label});
        if (parent.process.stderr) |stderr| {
            stderr.close(parent.io);
            parent.process.stderr = null;
        }
        if (parent.stderr_term) |term| return term;
        parent.stderr_term = .success;
        parent.stderr_term = parent.check_close_parent() orelse .success;
        return parent.stderr_term.?;
    }

    fn stderr_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("stderr", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.label});
        return parent.closeable.getResult() orelse parent.stderr_term orelse {
            if (parent.process.stderr) |stderr| {
                const revents = poll(stderr, std.posix.POLL.IN, parent.tracer);
                if (revents & std.posix.POLL.IN == 0 and revents & std.posix.POLL.HUP > 0) {
                    parent.stderr_term = .success;
                    parent.stderr_term = parent.check_close_parent() orelse .success;
                }
            }

            return parent.stderr_term;
        };
    }

    fn stderr_getLabel(self: *Closeable(ExitCode)) []const u8 {
        const parent: *@This() = @fieldParentPtr("stderr", self);
        return parent.closeable.getLabel();
    }
};

fn poll(file: std.Io.File, events: i16, tracer: *Tracer) i16 {
    var poll_fds = [_]std.posix.pollfd{
        .{
            .fd = file.handle,
            .events = events,
            .revents = 0,
        },
    };
    const poll_fd = &poll_fds[0];

    const result = std.posix.errno(std.posix.poll(&poll_fds, 0) catch return std.posix.POLL.ERR);
    switch (result) {
        .SUCCESS => {
            tracer.trace(.information, &.{ "process", @src().fn_name }, null, "revents: {x}", .{poll_fd.revents});
            tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLHUP: {}", .{poll_fd.revents & std.posix.POLL.HUP});
            tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLNVAL: {}", .{poll_fd.revents & std.posix.POLL.NVAL});
            tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLERR: {}", .{poll_fd.revents & std.posix.POLL.ERR});
            return poll_fd.revents;
        },
        else => return std.posix.POLL.ERR,
    }
}

pub const FileSink = struct {
    io: std.Io,
    file_writer: std.Io.File.Writer,
    closeable: Closeable(ExitCode) = .{ .vtable = &vtable },
    path: []const u8,
    result: ?ExitCode = null,

    const vtable = Closeable(ExitCode).VTable{
        .close = close,
        .getResult = getResult,
        .getLabel = getLabel,
    };

    /// Initializes in place: the file writer keeps a pointer into `self`, so the
    /// `FileSink` must already live at its final address (e.g. heap allocated).
    pub fn init(
        self: *@This(),
        io: std.Io,
        file: std.Io.File,
        path: []const u8,
        append_mode: bool,
    ) !void {
        self.* = .{
            .io = io,
            .file_writer = file.writer(io, &.{}),
            .path = path,
        };
        if (append_mode) {
            const stat = try file.stat(io);
            try self.file_writer.seekTo(stat.size);
        }
    }

    pub fn writerPtr(self: *@This()) *std.Io.Writer {
        return &self.file_writer.interface;
    }

    pub fn closeableWriter(self: *@This()) CloseableWriter(ExitCode) {
        return .init(self.writerPtr(), &self.closeable);
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        allocator.destroy(self);
    }

    fn close(self: *Closeable(ExitCode)) ExitCode {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        if (parent.result) |result| return result;

        parent.file_writer.interface.flush() catch {};
        parent.file_writer.file.close(parent.io);
        parent.result = .success;
        return parent.result.?;
    }

    fn getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        return parent.result;
    }

    fn getLabel(self: *Closeable(ExitCode)) []const u8 {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        return parent.path;
    }
};

const PipeWriter = struct {
    file: ?std.Io.File,
    file_writer: ?std.Io.File.Writer,
    trace_file_writer: TraceWriter = undefined,
    writer: std.Io.Writer = .{ .vtable = &vtable, .buffer = &.{}, .end = 0 },
    tracer: *Tracer,

    const vtable = std.Io.Writer.VTable{
        .drain = drain,
    };

    pub fn init(io: std.Io, file: ?std.Io.File, buffer: []u8, tracer: *Tracer) PipeWriter {
        return .{
            .file = file,
            .file_writer = if (file) |f| f.writerStreaming(io, buffer) else null,
            .tracer = tracer,
        };
    }

    fn getParent(w: *std.Io.Writer) *PipeWriter {
        return @fieldParentPtr("writer", w);
    }

    pub fn drain(
        w: *std.Io.Writer,
        data: []const []const u8,
        splat: usize,
    ) std.Io.Writer.Error!usize {
        const parent = getParent(w);
        parent.tracer.trace(.information, &.{ "process", @typeName(@This()), @src().fn_name }, null, "[{*}]: " ++ @src().fn_name ++ " (data.len={}, data[0].len={}, splat={})", .{ w, data.len, data[0].len, splat });
        const file = parent.file orelse return 0;
        // const file_writer = if (parent.file_writer) |*fw| fw else return 0;
        if (parent.file_writer == null) return 0;
        const writer = &parent.trace_file_writer.writer;

        const revents = poll(file, std.posix.POLL.OUT, parent.tracer);
        parent.tracer.trace(.information, &.{ "process", @typeName(@This()), @src().fn_name }, null, "[{*}]: " ++ @src().fn_name ++ ": poll {x}", .{ w, revents });
        if (revents & std.posix.POLL.ERR > 0) {
            parent.tracer.trace(.information, &.{ "process", @typeName(@This()), @src().fn_name }, null, "[{*}]: " ++ @src().fn_name ++ ": POLLERR", .{w});
            return error.WriteFailed;
        } else if (revents & std.posix.POLL.OUT > 0) {
            parent.tracer.trace(.information, &.{ "process", @typeName(@This()), @src().fn_name }, null, "[{*}]: " ++ @src().fn_name ++ ": POLLOUT", .{w});
            var bytes_written: usize = 0;
            if (w.buffered().len > 0) {
                bytes_written = try writer.write(w.buffered());
                _ = w.consume(bytes_written);
            } else {
                bytes_written = try writer.writeSplat(data, splat);
            }
            try writer.flush();
            return bytes_written;
        } else if (revents & (std.posix.POLL.HUP | std.posix.POLL.NVAL) > 0) {
            parent.tracer.trace(.information, &.{ "process", @typeName(@This()), @src().fn_name }, null, "[{*}]: " ++ @src().fn_name ++ ": POLLHUP | POLLNVAL", .{w});
            return error.WriteFailed;
        }

        return 0;
    }
};

pub const PipeReader = struct {
    file: ?std.Io.File,
    file_reader: ?std.Io.File.Reader,
    reader: std.Io.Reader = .{ .vtable = &vtable, .buffer = &.{}, .seek = 0, .end = 0 },
    tracer: *Tracer,

    const vtable = std.Io.Reader.VTable{
        .stream = stream,
    };

    pub fn init(io: std.Io, file: ?std.Io.File, buffer: []u8, tracer: *Tracer) PipeReader {
        return .{
            .file = file,
            .file_reader = if (file) |f| f.readerStreaming(io, buffer) else null,
            .tracer = tracer,
        };
    }

    fn getParent(r: *std.Io.Reader) *PipeReader {
        return @fieldParentPtr("reader", r);
    }

    pub fn stream(
        r: *std.Io.Reader,
        w: *std.Io.Writer,
        _: std.Io.Limit,
    ) std.Io.Reader.StreamError!usize {
        const parent = getParent(r);
        const file = parent.file orelse return 0;

        var revents = poll(file, std.posix.POLL.IN, parent.tracer);

        if (revents & std.posix.POLL.ERR > 0) {
            return error.EndOfStream;
        } else if (revents & (std.posix.POLL.IN | std.posix.POLL.HUP) > 0) {
            var buffer: [256]u8 = undefined;
            var bytes_read: usize = 0;
            while (bytes_read < buffer.len) {
                const n = std.posix.read(file.handle, buffer[bytes_read .. bytes_read + 1]) catch return error.ReadFailed;
                if (n == 0) {
                    if (bytes_read == 0) return error.EndOfStream;
                    break;
                }
                bytes_read += n;

                if (buffer[bytes_read - 1] == '\n') break;

                revents = poll(file, std.posix.POLL.IN, parent.tracer);
                if (revents & std.posix.POLL.ERR > 0) return error.EndOfStream;
                if (revents & std.posix.POLL.IN == 0) break;
            }

            try w.writeAll(buffer[0..bytes_read]);
            return bytes_read;
        } else if (revents & std.posix.POLL.NVAL > 0) {
            return error.EndOfStream;
        }

        return 0;
    }
};

pub const CloseableProcessIo = struct {
    io: std.Io,
    process: *std.process.Child,
    label: []const u8,
    stdin_buffer: [1024]u8 = undefined,
    stdin_trace_buffer: [1024]u8 = undefined,
    stdin_writer: ?PipeWriter = null,
    // stdin_writer: ?std.fs.File.Writer = null,
    stdout_buffer: [1024]u8 = undefined,
    // stdout_reader: ?std.fs.File.Reader = null,
    stdout_reader: ?PipeReader = null,
    stderr_buffer: [1024]u8 = undefined,
    // stderr_reader: ?std.fs.File.Reader = null,
    stderr_reader: ?PipeReader = null,
    process_closeable: ProcessCloseable = undefined,
    tracer: *Tracer,

    pub fn init(io: std.Io, process: *std.process.Child, label: []const u8, tracer: *Tracer) @This() {
        return .{ .io = io, .process = process, .label = label, .tracer = tracer };
    }

    pub fn connect(self: *@This()) void {
        if (self.process.stdin) |f| {
            self.stdin_writer = .init(self.io, f, &self.stdin_buffer, self.tracer);
            self.stdin_writer.?.trace_file_writer = .init(
                &self.stdin_trace_buffer,
                &self.stdin_writer.?.file_writer.?.interface,
                null,
                "process_stdin",
            );
        }
        if (self.process.stdout) |f| self.stdout_reader = .init(self.io, f, &self.stdout_buffer, self.tracer);
        if (self.process.stderr) |f| self.stderr_reader = .init(self.io, f, &self.stderr_buffer, self.tracer);
        self.process_closeable = .init(self.io, self.process, self.label, self.tracer);
    }

    pub fn stdin(self: *@This()) *std.Io.Writer {
        // return &self.stdin_writer.?.interface;
        return &self.stdin_writer.?.writer;
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
