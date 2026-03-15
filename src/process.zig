const std = @import("std");
const Closeable = @import("closeable.zig").Closeable;
const CloseableReader = @import("closeable.zig").CloseableReader;
const CloseableWriter = @import("closeable.zig").CloseableWriter;
const ReaderWriterStream = @import("stream.zig").ReaderWriterStream;
const ExitCode = @import("runtime/command_runner.zig").ExitCode;
const TraceWriter = @import("trace-writer.zig").TraceWriter;
const Tracer = @import("trace.zig").Tracer;

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
    tracer: *Tracer,
) !std.process.Child {
    tracer.trace(.information, &.{ "process", @src().fn_name }, null, "Child.init {s}", .{argv[0]});
    var child = std.process.Child.init(argv, std.heap.page_allocator);
    child.stdin_behavior = stdin_behavior;
    child.stdout_behavior = stdout_behavior;
    child.stderr_behavior = stderr_behavior;
    tracer.trace(.information, &.{ "process", @src().fn_name }, null, "Child.spawn {s}", .{argv[0]});
    try child.spawn();
    tracer.trace(.information, &.{ "process", @src().fn_name }, null, "Child.waitForSpawn {s}", .{argv[0]});
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

    pub fn init(process: *std.process.Child, tracer: *Tracer) @This() {
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, @typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{process.argv[0]});
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "{s}: stdin: {}", .{ process.argv[0], process.stdin != null });
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "{s}: stdout: {}", .{ process.argv[0], process.stdout != null });
        tracer.trace(.information, &.{ "process", @src().fn_name }, null, "{s}: stderr: {}", .{ process.argv[0], process.stderr != null });

        return .{
            .process = process,
            .stdin_term = if (process.stdin) |_| null else .success,
            .stdout_term = if (process.stdout) |_| null else .success,
            .stderr_term = if (process.stderr) |_| null else .success,
            .tracer = tracer,
        };
    }

    fn close(self: *Closeable(ExitCode)) ExitCode {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        parent.tracer.trace(.information, &.{ "process", @src().fn_name }, null, @typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.process.argv[0]});
        parent.tracer.trace(.information, &.{ "process", @src().fn_name }, null, "closing {s}", .{parent.process.argv[0]});
        if (parent.waited) return .fromTerm(parent.process.term.?);
        parent.waited = true;
        parent.tracer.trace(.information, &.{ "process", @src().fn_name }, null, "waiting for {s} to terminate", .{parent.process.argv[0]});
        const exit_code: ExitCode = .fromTerm(parent.process.wait());
        parent.tracer.trace(.information, &.{ "process", @src().fn_name }, null, "{s} exited with {any}", .{ parent.process.argv[0], exit_code });
        return exit_code;
    }

    fn logState(self: *@This()) void {
        log("stdin: {}, stdout: {}, stderr: {}", .{
            self.stdin.isClosed(), self.stdout.isClosed(), self.stderr.isClosed(),
        });
    }

    fn getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.process.argv[0]});
        if (parent.process.term) |term| {
            const exit_code: ExitCode = .fromTerm(term);
            log("result: {f}", .{exit_code});
            return exit_code;
        } else {
            log("result: null", .{});
            return null;
        }
    }

    pub fn getLabel(self: *Closeable(ExitCode)) []const u8 {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        return parent.process.argv[0];
    }

    fn check_close_parent(self: *@This()) ?ExitCode {
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{self.process.argv[0]});

        log("stdin: {}, stdout: {}, stderr: {}", .{
            self.stdin.isClosed(), self.stdout.isClosed(), self.stderr.isClosed(),
        });

        if (self.stdin.isClosed() and self.stdout.isClosed() and self.stderr.isClosed()) {
            return self.closeable.close();
        }

        return null;
    }

    fn stdin_close(self: *Closeable(ExitCode)) ExitCode {
        const parent: *@This() = @fieldParentPtr("stdin", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.process.argv[0]});
        log("closing stdin of {s}", .{parent.process.argv[0]});
        if (parent.process.stdin) |stdin| {
            stdin.close();
            parent.process.stdin = null;
        }
        if (parent.stdin_term) |term| return term;
        parent.stdin_term = .success;
        parent.stdin_term = parent.check_close_parent() orelse .success;
        return parent.stdin_term.?;
    }

    fn stdin_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("stdin", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.process.argv[0]});
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
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.process.argv[0]});
        log("closing stdout of {s}", .{parent.process.argv[0]});
        if (parent.process.stdout) |stdout| {
            stdout.close();
            parent.process.stdout = null;
        }
        if (parent.stdout_term) |term| return term;
        parent.stdout_term = .success;
        parent.stdout_term = parent.check_close_parent() orelse .success;
        return parent.stdout_term.?;
    }

    fn stdout_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("stdout", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.process.argv[0]});
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
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.process.argv[0]});
        log("closing stderr of {s}", .{parent.process.argv[0]});
        if (parent.process.stderr) |stderr| {
            stderr.close();
            parent.process.stderr = null;
        }
        if (parent.stderr_term) |term| return term;
        parent.stderr_term = .success;
        parent.stderr_term = parent.check_close_parent() orelse .success;
        return parent.stderr_term.?;
    }

    fn stderr_getResult(self: *Closeable(ExitCode)) ?ExitCode {
        const parent: *@This() = @fieldParentPtr("stderr", self);
        log(@typeName(@This()) ++ "." ++ @src().fn_name ++ "({s})", .{parent.process.argv[0]});
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

fn poll(file: std.fs.File, events: i16, tracer: *Tracer) i16 {
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

const PipeWriter = struct {
    file: ?std.fs.File,
    file_writer: ?std.fs.File.Writer,
    trace_file_writer: TraceWriter = undefined,
    writer: std.Io.Writer = .{ .vtable = &vtable, .buffer = &.{}, .end = 0 },
    tracer: *Tracer,

    const vtable = std.Io.Writer.VTable{
        .drain = drain,
    };

    pub fn init(file: ?std.fs.File, buffer: []u8, tracer: *Tracer) PipeWriter {
        return .{
            .file = file,
            .file_writer = if (file) |f| f.writer(buffer) else null,
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
    file: ?std.fs.File,
    file_reader: ?std.fs.File.Reader,
    reader: std.Io.Reader = .{ .vtable = &vtable, .buffer = &.{}, .seek = 0, .end = 0 },
    tracer: *Tracer,

    const vtable = std.Io.Reader.VTable{
        .stream = stream,
    };

    pub fn init(file: ?std.fs.File, buffer: []u8, tracer: *Tracer) PipeReader {
        return .{
            .file = file,
            .file_reader = if (file) |f| f.reader(buffer) else null,
            .tracer = tracer,
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

        const revents = poll(file, std.posix.POLL.IN, parent.tracer);

        if (revents & std.posix.POLL.ERR > 0) {
            return error.EndOfStream;
        } else if (revents & std.posix.POLL.IN > 0) {
            return try file_reader.interface.stream(w, limit);
        } else if (revents & (std.posix.POLL.HUP | std.posix.POLL.NVAL) > 0) {
            return error.EndOfStream;
        }

        return 0;
    }
};

pub const CloseableProcessIo = struct {
    process: *std.process.Child,
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

    pub fn init(process: *std.process.Child, tracer: *Tracer) @This() {
        return .{ .process = process, .tracer = tracer };
    }

    pub fn connect(self: *@This()) void {
        if (self.process.stdin) |f| {
            self.stdin_writer = .init(f, &self.stdin_buffer, self.tracer);
            self.stdin_writer.?.trace_file_writer = .init(
                &self.stdin_trace_buffer,
                &self.stdin_writer.?.file_writer.?.interface,
                "process_stdin",
            );
        }
        // if (self.process.stdin) |f| self.stdin_writer = f.writer(&self.stdin_buffer);
        if (self.process.stdout) |f| self.stdout_reader = .init(f, &self.stdout_buffer, self.tracer);
        if (self.process.stderr) |f| self.stderr_reader = .init(f, &self.stderr_buffer, self.tracer);
        self.process_closeable = .init(self.process, self.tracer);
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
