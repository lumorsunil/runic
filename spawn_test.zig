const std = @import("std");

const Term = PipeStreamError!std.process.Child.Term;

const Root = @This();

const log_enabled = true;

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

const PipeStreamError = std.process.Child.SpawnError || std.Io.Reader.StreamError;

const ProcessCloseable = struct {
    process: *std.process.Child,
    waited: bool = false,
    closeable: Closeable = .{ .vtable = &vtable },
    stdin: Closeable = .{ .vtable = &stdin_vtable },
    stdin_term: ?Term = null,
    stdout: Closeable = .{ .vtable = &stdout_vtable },
    stdout_term: ?Term = null,
    stderr: Closeable = .{ .vtable = &stderr_vtable },
    stderr_term: ?Term = null,

    const vtable = Closeable.VTable{
        .close = close,
        .getTerm = getTerm,
    };

    const stdin_vtable = Closeable.VTable{
        .close = stdin_close,
        .getTerm = stdin_getTerm,
    };

    const stdout_vtable = Closeable.VTable{
        .close = stdout_close,
        .getTerm = stdout_getTerm,
    };

    const stderr_vtable = Closeable.VTable{
        .close = stderr_close,
        .getTerm = stderr_getTerm,
    };

    pub fn init(process: *std.process.Child) @This() {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        log("{s}: stdin {}", .{ process.argv[0], process.stdin != null });
        log("{s}: stdout {}", .{ process.argv[0], process.stdout != null });
        log("{s}: stderr {}", .{ process.argv[0], process.stderr != null });

        return .{
            .process = process,
            .stdin_term = if (process.stdin) |_| null else .{ .Exited = 0 },
            .stdout_term = if (process.stdout) |_| null else .{ .Exited = 0 },
            .stderr_term = if (process.stderr) |_| null else .{ .Exited = 0 },
        };
    }

    fn close(self: *Closeable) Term {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("closeable", self);
        log("closing {s}", .{parent.process.argv[0]});
        if (parent.waited) return parent.process.term.?;
        parent.waited = true;
        log("waiting for {s} to terminate", .{parent.process.argv[0]});
        return parent.process.wait();
    }

    fn getTerm(self: *Closeable) ?Term {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("closeable", self);
        return parent.process.term;
    }

    fn check_close_parent(self: *@This()) !void {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        if (self.stdin_term != null and self.stdout_term != null) {
            log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
            _ = try self.closeable.close();
        }
    }

    fn stdin_close(self: *Closeable) Term {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stdin", self);
        log("closing stdin of {s}", .{parent.process.argv[0]});
        if (parent.process.stdin) |stdin| {
            stdin.close();
            parent.process.stdin = null;
        }
        if (parent.stdin_term) |term| return term;
        parent.stdin_term = .{ .Exited = 0 };
        try parent.check_close_parent();
        return parent.stdin_term.?;
    }

    fn stdin_getTerm(self: *Closeable) ?Term {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stdin", self);
        return parent.stdin_term;
    }

    fn stdout_close(self: *Closeable) Term {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stdout", self);
        log("closing stdout of {s}", .{parent.process.argv[0]});
        if (parent.process.stdout) |stdout| {
            stdout.close();
            parent.process.stdout = null;
        }
        if (parent.stdout_term) |term| return term;
        parent.stdout_term = .{ .Exited = 0 };
        try parent.check_close_parent();
        return parent.stdout_term.?;
    }

    fn stdout_getTerm(self: *Closeable) ?Term {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stdout", self);
        return parent.stdout_term;
    }

    fn stderr_close(self: *Closeable) Term {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stderr", self);
        log("closing stderr of {s}", .{parent.process.argv[0]});
        if (parent.process.stderr) |stderr| {
            stderr.close();
            parent.process.stderr = null;
        }
        if (parent.stderr_term) |term| return term;
        parent.stderr_term = .{ .Exited = 0 };
        try parent.check_close_parent();
        return parent.stderr_term.?;
    }

    fn stderr_getTerm(self: *Closeable) ?Term {
        log(@typeName(@This()) ++ "." ++ @src().fn_name, .{});
        const parent: *@This() = @fieldParentPtr("stderr", self);
        return parent.stderr_term;
    }
};

const ManualCloseable = struct {
    term: ?Term = null,
    closeable: Closeable = .{ .vtable = &vtable },

    const vtable = Closeable.VTable{
        .close = close,
        .getTerm = getTerm,
    };

    fn close(self: *Closeable) Term {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        parent.term = parent.term orelse .{ .Exited = 0 };
        return parent.term.?;
    }

    fn getTerm(self: *Closeable) ?Term {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        return parent.term;
    }

    pub fn setTerm(self: *ManualCloseable, term: Term) void {
        self.term = term;
        _ = self.closeable.close();
    }
};

const Closeable = struct {
    vtable: *const VTable,

    pub const VTable = struct {
        close: *const fn (*Closeable) Term,
        getTerm: *const fn (*Closeable) ?Term,
    };

    pub fn close(self: *Closeable) Term {
        return self.vtable.close(self);
    }

    pub fn getTerm(self: *Closeable) ?Term {
        return self.vtable.getTerm(self);
    }
};

const CloseableProcessIo = struct {
    process: *std.process.Child,
    stdin_buffer: [1024]u8 = undefined,
    stdin_writer: ?std.fs.File.Writer = null,
    stdout_buffer: [1024]u8 = undefined,
    stdout_reader: ?std.fs.File.Reader = null,
    stderr_buffer: [1024]u8 = undefined,
    stderr_reader: ?std.fs.File.Reader = null,
    process_closeable: ProcessCloseable = undefined,

    pub fn init(process: *std.process.Child) @This() {
        return .{ .process = process };
    }

    pub fn connect(self: *@This()) void {
        if (self.process.stdin) |f| self.stdin_writer = f.writer(&self.stdin_buffer);
        if (self.process.stdout) |f| self.stdout_reader = f.reader(&self.stdout_buffer);
        if (self.process.stderr) |f| self.stderr_reader = f.reader(&self.stderr_buffer);
        self.process_closeable = .init(self.process);

        log("{s}: stdin {}", .{ self.process.argv[0], self.process.stdin != null });
        log("{s}: stdout {}", .{ self.process.argv[0], self.process.stdout != null });
        log("{s}: stderr {}", .{ self.process.argv[0], self.process.stderr != null });
    }

    pub fn stdin(self: *@This()) *std.Io.Writer {
        return &self.stdin_writer.?.interface;
    }

    pub fn stdout(self: *@This()) *std.Io.Reader {
        return &self.stdout_reader.?.interface;
    }

    pub fn stderr(self: *@This()) *std.Io.Reader {
        return &self.stderr_reader.?.interface;
    }

    pub fn closeable(self: *@This()) *Closeable {
        return &self.process_closeable.closeable;
    }

    pub fn closeableStdin(self: *@This()) CloseableWriter {
        return .init(self.stdin(), &self.process_closeable.stdin);
    }

    pub fn closeableStdout(self: *@This()) CloseableReader {
        return .init(self.stdout(), &self.process_closeable.stdout);
    }

    pub fn closeableStderr(self: *@This()) CloseableReader {
        return .init(self.stderr(), &self.process_closeable.stderr);
    }

    pub fn pipeStdout(self: *@This(), label: []const u8, destination: *@This()) Pipe {
        return .init(
            label,
            self.closeableStdout(),
            destination.closeableStdin(),
        );
    }

    pub fn pipeStdoutWriter(self: *@This(), label: []const u8, destination: CloseableWriter) Pipe {
        return .init(
            label,
            self.closeableStdout(),
            destination,
        );
    }

    pub fn pipeStderr(self: *@This(), label: []const u8, destination: *@This()) Pipe {
        return .init(
            label,
            self.closeableStderr(),
            destination.closeableStdin(),
        );
    }
};

const CloseableReader = struct {
    reader: *std.Io.Reader,
    closeable: *Closeable,

    pub fn init(reader: *std.Io.Reader, closeable: *Closeable) CloseableReader {
        return .{
            .reader = reader,
            .closeable = closeable,
        };
    }
};

const CloseableWriter = struct {
    writer: *std.Io.Writer,
    closeable: *Closeable,

    pub fn init(writer: *std.Io.Writer, closeable: *Closeable) CloseableWriter {
        return .{
            .writer = writer,
            .closeable = closeable,
        };
    }
};

const Pipe = struct {
    label: []const u8,
    source: CloseableReader,
    destination: CloseableWriter,

    pub fn init(
        label: []const u8,
        source: CloseableReader,
        destination: CloseableWriter,
    ) Pipe {
        return .{
            .label = label,
            .source = source,
            .destination = destination,
        };
    }

    fn log(self: *Pipe, comptime fmt: []const u8, args: anytype) void {
        var buffer: [1024]u8 = undefined;
        const header = std.fmt.bufPrint(
            &buffer,
            "{*}({s})." ++ @src().fn_name ++ ": ",
            .{ self, self.label },
        ) catch unreachable;
        const message = std.fmt.bufPrint(buffer[header.len..], fmt, args) catch unreachable;
        Root.log("{s}", .{buffer[0 .. header.len + message.len]});
    }

    pub fn stream(self: *Pipe, limit: std.Io.Limit) !usize {
        self.log("stream", .{});

        if (self.isClosed()) return 0;

        self.log("streaming", .{});
        const bytes_streamed = self.source.reader.stream(
            self.destination.writer,
            limit,
        ) catch |err| switch (err) {
            error.EndOfStream => {
                self.log("stream ended", .{});
                _ = try self.source.closeable.close();
                _ = try self.destination.closeable.close();
                return 0;
            },
            else => {
                self.log("stream errored: {t}", .{err});
                _ = try self.source.closeable.close();
                _ = try self.destination.closeable.close();
                return 0;
            },
        };

        self.log("stream flushing", .{});
        try self.destination.writer.flush();

        self.log("stream {} bytes forwarded", .{bytes_streamed});

        return bytes_streamed;
    }

    pub fn isClosed(self: *Pipe) bool {
        const term = self.source.closeable.getTerm();
        self.log(@src().fn_name ++ ": {?!}", .{term});
        return term != null;
    }
};

fn wait(child: *std.process.Child) !std.process.Child.Term {
    log("Child.wait {s}", .{child.argv[0]});
    return child.wait();
}

pub fn main() !void {
    var stdout_buffer: [1024]u8 = undefined;
    const stdout = std.fs.File.stdout();
    var stdout_file_writer = stdout.writer(&stdout_buffer);
    const stdout_writer = &stdout_file_writer.interface;
    var stdout_closeable = ManualCloseable{};
    const stdout_closeable_writer = CloseableWriter.init(stdout_writer, &stdout_closeable.closeable);

    var echo = try spawn(&.{ "echo", "hello" }, .Close, .Pipe, .Pipe);
    var cat = try spawn(&.{"cat"}, .Pipe, .Pipe, .Pipe);

    var echo_io = CloseableProcessIo.init(&echo);
    echo_io.connect();
    var cat_io = CloseableProcessIo.init(&cat);
    cat_io.connect();

    // const fixed_reader_buffer: []const u8 = "this is from the fixed reader";
    // var fixed_reader = std.Io.Reader.fixed(fixed_reader_buffer);
    // var fixed_reader_closeable = ManualCloseable{};
    // const fixed_closeable_reader = CloseableReader.init(
    //     &fixed_reader,
    //     &fixed_reader_closeable.closeable,
    // );

    var echo_cat_pipe = echo_io.pipeStdout("echo_cat", &cat_io);
    echo_cat_pipe.log("init", .{});
    // var fw_cat_pipe = Pipe.init("fw_cat", fixed_closeable_reader, cat_io.closeableStdin());
    // fw_cat_pipe.log("init", .{});
    var cat_stdout_pipe = cat_io.pipeStdoutWriter("cat_stdout", stdout_closeable_writer);
    cat_stdout_pipe.log("init", .{});

    const pipeline: []const *Pipe = &.{ &echo_cat_pipe, &cat_stdout_pipe };
    // const pipeline: []const *Pipe = &.{ &fw_cat_pipe, &cat_stdout_pipe };

    outer: while (true) {
        for (pipeline, 0..) |stage, i| {
            log("\n\n ========= PROCESSING STAGE {} ========== \n\n", .{i});
            if (stage.isClosed()) continue;
            _ = try stage.stream(.unlimited);
        }
        for (pipeline) |stage| {
            if (!stage.isClosed()) continue :outer;
        }
        break;
    }

    log("end", .{});
}
