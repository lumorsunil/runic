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
                const revents = poll(stdin, POLL.OUT, parent.tracer);
                if (revents.ERR) {
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
                const revents = poll(stdout, POLL.IN, parent.tracer);
                if (!revents.IN and revents.HUP) {
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
                const revents = poll(stderr, POLL.IN, parent.tracer);
                if (!revents.IN and revents.HUP) {
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

fn poll(file: std.Io.File, events: POLL, tracer: *Tracer) LINUXPOLLEVENTS {
    if (comptime @import("builtin").os.tag == .windows) {
        return poll_windows(file, events, tracer);
    } else {
        return poll_posix(file, events, tracer);
    }
}

fn poll_posix(file: std.Io.File, events: LINUXPOLL, tracer: *Tracer) LINUXPOLLEVENTS {
    var poll_fds = [_]std.posix.pollfd{
        .{
            .fd = file.handle,
            .events = @intFromEnum(events),
            .revents = 0,
        },
    };
    const poll_fd = &poll_fds[0];

    const result = std.posix.errno(std.posix.poll(&poll_fds, 0) catch return .err);
    switch (result) {
        .SUCCESS => {
            const revents: LINUXPOLLEVENTS = @bitCast(poll_fd.revents);
            tracer.trace(.information, &.{ "process", @src().fn_name }, null, "revents: {x}", .{revents.asInt()});
            tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLHUP: {}", .{revents.HUP});
            tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLNVAL: {}", .{revents.NVAL});
            tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLERR: {}", .{revents.ERR});
            return revents;
        },
        else => return .err,
    }
}

const POLL = switch (@import("builtin").os.tag) {
    .windows => WSAPOLL,
    else => LINUXPOLL,
};

const POLLEVENTS = switch (@import("builtin").os.tag) {
    .windows => WSAPOLLEVENTS,
    else => LINUXPOLLEVENTS,
};

pub const WSAPOLL = enum(i16) {
    IN = 512 + 256,
    OUT = 16,
};

pub const LINUXPOLL = enum(i16) {
    IN = 1,
    OUT = 4,
};

pub const WSAPOLLEVENTS = packed struct(u16) {
    ERR: bool = false, // 1
    HUP: bool = false, // 2
    NVAL: bool = false, // 4
    _: bool = false, // _
    OUT: bool = false, // 16
    __: u3 = 0, // _
    // _
    // _
    IN: u2 = 3, // 256,512
    ___: u6 = 0,

    pub const empty: @This() = .{};
    pub const full: @This() = .{
        .ERR = true,
        .HUP = true,
        .NVAL = true,
        .OUT = true,
        .IN = std.math.maxInt(@TypeOf(std.meta.fieldInfo(WSAPOLLEVENTS, .IN).type)),
    };

    pub fn toLinux(e: @This()) LINUXPOLLEVENTS {
        return .{
            .IN = e.IN != 0,
            .OUT = e.OUT,
            .ERR = e.ERR,
            .HUP = e.HUP,
            .NVAL = e.NVAL,
        };
    }
};

pub const LINUXPOLLEVENTS = packed struct(u16) {
    IN: bool = true, // 1
    __: bool = true, // _
    OUT: bool = true, // 4
    ERR: bool = false, // 8
    HUP: bool = false, // 0x10
    NVAL: bool = false, // 0x20
    ___: u10 = 0,

    pub const empty: @This() = .{};
    pub const err: @This() = .{
        .ERR = true,
    };

    pub fn asInt(self: @This()) u16 {
        return @bitCast(self);
    }
};

const WSAPollfd = struct {
    fd: std.os.windows.HANDLE,
    events: WSAPOLL,
    revents: WSAPOLLEVENTS,
};

pub extern "ws2_32" fn WSAPoll(
    fdArray: [*]WSAPollfd,
    fds: std.os.windows.ULONG,
    timeout: std.os.windows.INT,
) callconv(.winapi) c_int;

pub extern "ws2_32" fn WSAGetLastError() callconv(.winapi) c_int;

fn poll_windows(file: std.Io.File, events: WSAPOLL, tracer: *Tracer) LINUXPOLLEVENTS {
    var poll_fds = [_]WSAPollfd{
        .{
            .fd = file.handle,
            .events = events,
            .revents = .empty,
        },
    };
    const poll_fd = &poll_fds[0];

    const result = WSAPoll(&poll_fds, 1, 0);

    if (result <= 0) {
        return .err;
    } else {
        const revents = poll_fd.revents.toLinux();

        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "revents: {x}", .{revents.asInt()});
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLHUP: {}", .{revents.HUP});
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLNVAL: {}", .{revents.NVAL});
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "POLLERR: {}", .{revents.ERR});
        return revents;
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

        const revents = poll(file, POLL.OUT, parent.tracer);
        parent.tracer.trace(.information, &.{ "process", @typeName(@This()), @src().fn_name }, null, "[{*}]: " ++ @src().fn_name ++ ": poll {x}", .{ w, revents.asInt() });
        if (revents.ERR) {
            parent.tracer.trace(.information, &.{ "process", @typeName(@This()), @src().fn_name }, null, "[{*}]: " ++ @src().fn_name ++ ": POLLERR", .{w});
            return error.WriteFailed;
        } else if (revents.OUT) {
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
        } else if (revents.HUP or revents.NVAL) {
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

        var revents = poll(file, POLL.IN, parent.tracer);

        if (revents.ERR) {
            return error.EndOfStream;
        } else if (revents.IN or revents.HUP) {
            var buffer: [256]u8 = undefined;
            var bytes_read: usize = 0;
            while (bytes_read < buffer.len) {
                switch (@import("builtin").os.tag) {
                    .windows => {
                        var lr = r.limited(.limited(2), buffer[bytes_read .. bytes_read + 1]);
                        buffer[bytes_read] = (lr.interface.take(1) catch |err| switch (err) {
                            error.EndOfStream => if (bytes_read == 0) return error.EndOfStream else break,
                            else => return err,
                        })[0];
                        bytes_read += 1;
                    },
                    else => {
                        const n = std.posix.read(file.handle, buffer[bytes_read .. bytes_read + 1]) catch return error.ReadFailed;
                        if (n == 0) {
                            if (bytes_read == 0) return error.EndOfStream;
                            break;
                        }
                        bytes_read += n;
                    },
                }

                if (buffer[bytes_read - 1] == '\n') break;

                revents = poll(file, POLL.IN, parent.tracer);
                if (revents.ERR) return error.EndOfStream;
                if (!revents.IN) break;
            }

            try w.writeAll(buffer[0..bytes_read]);
            return bytes_read;
        } else if (revents.NVAL) {
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
