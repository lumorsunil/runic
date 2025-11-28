const std = @import("std");
const runic = @import("runic");
const ScriptExecutor = runic.interpreter.ScriptExecutor;
const ScriptContext = runic.interpreter.ScriptContext;
const utils = @import("main-utils.zig");
const DocumentStore = runic.document.DocumentStore;
const Parser = runic.document.Parser;

const CommandRunner = runic.command_runner.CommandRunner;
const Tracer = runic.tracing.Tracer;

const writer_buffer_len: usize = 1024;

pub const Options = struct {
    prompt: []const u8 = "runic> ",
    continuation_prompt: []const u8 = "...> ",
    history_limit: usize = 256,
    trace_topics: [][]const u8 = &[_][]const u8{},
    module_paths: [][]const u8 = &[_][]const u8{},
};

pub fn run(allocator: std.mem.Allocator, options: Options) !void {
    var session = try Session.init(allocator, options);
    session.tracer.setSink(&session.stderr.interface);
    defer session.deinit();
    try session.loop();
}

const Session = struct {
    allocator: std.mem.Allocator,
    options: Options,
    editor: LineEditor,
    stdout_buffer: []u8,
    stderr_buffer: []u8,
    stdout: std.fs.File.Writer,
    stderr: std.fs.File.Writer,
    tracer: Tracer,
    context: ScriptContext,
    env_map: std.process.EnvMap,
    runner: CommandRunner,
    executor: ScriptExecutor,
    script_dir: []u8,

    fn init(allocator: std.mem.Allocator, options: Options) !Session {
        const stdout_file = std.fs.File.stdout();
        const stderr_file = std.fs.File.stderr();

        const stdout_buffer = try allocator.alloc(u8, writer_buffer_len);
        errdefer allocator.free(stdout_buffer);
        const stderr_buffer = try allocator.alloc(u8, writer_buffer_len);
        errdefer allocator.free(stderr_buffer);

        var editor = try LineEditor.init(allocator, options.history_limit);
        errdefer editor.deinit();

        var context = ScriptContext.init(allocator);
        errdefer context.deinit();

        var env_map = std.process.getEnvMap(allocator) catch |err| {
            return err;
        };
        errdefer env_map.deinit();

        const script_dir = try std.fs.cwd().realpathAlloc(allocator, ".");
        errdefer allocator.free(script_dir);

        var session = Session{
            .allocator = allocator,
            .options = options,
            .editor = editor,
            .stdout_buffer = stdout_buffer,
            .stderr_buffer = stderr_buffer,
            .stdout = stdout_file.writer(stdout_buffer),
            .stderr = stderr_file.writer(stderr_buffer),
            .tracer = Tracer.init(options.trace_topics, null),
            .context = context,
            .env_map = env_map,
            .runner = undefined,
            .executor = undefined,
            .script_dir = script_dir,
        };
        session.runner = CommandRunner.initWithTracer(allocator, &session.tracer);
        session.executor = try ScriptExecutor.initWithRunner(allocator, &session.runner, &session.env_map, undefined);
        return session;
    }

    fn deinit(self: *Session) void {
        self.tracer.clearSink();
        self.executor.deinit();
        self.env_map.deinit();
        self.context.deinit();
        self.allocator.free(self.script_dir);
        self.editor.deinit();
        self.allocator.free(self.stdout_buffer);
        self.allocator.free(self.stderr_buffer);
        self.* = undefined;
    }

    fn loop(self: *Session) !void {
        try self.printBanner();

        var aggregated = std.ArrayList(u8).empty;
        defer aggregated.deinit(self.allocator);

        var continuing = false;

        while (true) {
            const prompt = if (continuing) self.options.continuation_prompt else self.options.prompt;
            const line_opt = try self.editor.readLine(prompt);
            if (line_opt == null) {
                try self.stdout.interface.writeAll("\n");
                try self.stdout.interface.flush();
                break;
            }

            const line = line_opt.?;
            defer self.allocator.free(line);

            var trimmed = std.mem.trimRight(u8, line, " \t");
            var wants_continuation = false;
            if (trimmed.len > 0 and trimmed[trimmed.len - 1] == '\\') {
                wants_continuation = true;
                trimmed = trimmed[0 .. trimmed.len - 1];
            }

            if (!continuing) {
                aggregated.clearRetainingCapacity();
            } else {
                try aggregated.append(self.allocator, '\n');
            }

            try aggregated.appendSlice(self.allocator, trimmed);
            continuing = wants_continuation;

            if (continuing) continue;

            const command_slice = std.mem.trim(u8, aggregated.items, " \t\r\n");
            if (command_slice.len == 0) {
                continuing = false;
                continue;
            }

            const command = try self.allocator.dupe(u8, command_slice);
            defer self.allocator.free(command);

            const should_exit = try self.handleCommand(command);
            if (should_exit) break;
        }
    }

    fn printBanner(self: *Session) !void {
        try self.stdout.interface.print(
            "Runic REPL — history enabled, end lines with '\\\\' to continue.\n" ++
                "Meta commands: :help, :history, :quit\n",
            .{},
        );
        try self.stdout.interface.flush();
    }

    fn handleCommand(self: *Session, command: []const u8) !bool {
        if (command.len == 0) return false;
        if (command[0] == ':') {
            return self.handleMeta(command[1..]);
        }
        return try self.executeScript(command);
    }

    fn handleMeta(self: *Session, command: []const u8) !bool {
        const trimmed = std.mem.trim(u8, command, " \t");
        defer self.stdout.interface.flush() catch {};
        defer self.stderr.interface.flush() catch {};
        if (trimmed.len == 0) {
            try self.stdout.interface.print(":help, :history, :quit\n", .{});
            return false;
        }
        if (std.mem.eql(u8, trimmed, "quit") or std.mem.eql(u8, trimmed, "exit")) {
            return true;
        }
        if (std.mem.eql(u8, trimmed, "history")) {
            try self.showHistory();
            return false;
        }
        if (std.mem.eql(u8, trimmed, "help")) {
            try self.stdout.interface.print(
                ":history — list recorded inputs\n" ++
                    ":quit/:exit — close the REPL\n" ++
                    "Anything else runs as a Runic script snippet.\n",
                .{},
            );
            return false;
        }
        try self.stderr.interface.print("Unknown REPL command ':{s}'. Try :help.\n", .{trimmed});
        return false;
    }

    fn executeScript(self: *Session, source: []const u8) !bool {
        defer self.stderr.interface.flush() catch {};
        defer self.stdout.interface.flush() catch {};

        // var parser = Parser.init(self.allocator, source);
        var documentStoreArena = std.heap.ArenaAllocator.init(self.allocator);
        defer documentStoreArena.deinit();
        var documentStore = DocumentStore.init(documentStoreArena.allocator());
        var parser = Parser.init(self.allocator, &documentStore);
        defer parser.deinit();

        const script = parser.parseSource(source) catch |err| {
            try self.stderr.interface.print("Parse error: {s}", .{@errorName(err)});
            const expected = parser.expectedTokens();
            if (expected.len > 0) {
                try self.stderr.interface.writeAll(" (");
                _ = try parser.writeExpectedTokens(&self.stderr.interface);
                try self.stderr.interface.writeAll(")");
            }
            try self.stderr.interface.writeByte('\n');
            return false;
        };

        const code = try self.executor.execute(script, .{
            .script_path = ":repl",
            .stdout = &self.stdout.interface,
            .stderr = &self.stderr.interface,
            .context = &self.context,
        });

        if (code != 0) {
            try self.stderr.interface.print("Command exited with {d}\n", .{code});
        }

        return false;
    }

    fn showHistory(self: *Session) !void {
        const entries = self.editor.historyEntries();
        defer self.stdout.interface.flush() catch {};
        if (entries.len == 0) {
            try self.stdout.interface.print("(history empty)\n", .{});
            return;
        }
        for (entries, 0..) |entry, idx| {
            try self.stdout.interface.print("{d}: {s}\n", .{ idx + 1, entry });
        }
    }
};

const LineEditor = struct {
    allocator: std.mem.Allocator,
    stdin_file: std.fs.File,
    stdout_file: std.fs.File,
    writer_buffer: []u8,
    writer: std.fs.File.Writer,
    buffer: std.ArrayList(u8),
    saved_current: std.ArrayList(u8),
    history: History,
    history_pos: usize = 0,
    cursor: usize = 0,
    is_tty: bool = false,
    raw_enabled: bool = false,
    original_termios: ?std.posix.termios = null,

    fn init(allocator: std.mem.Allocator, history_limit: usize) !LineEditor {
        const stdin_file = std.fs.File.stdin();
        const stdout_file = std.fs.File.stdout();
        const writer_buffer = try allocator.alloc(u8, writer_buffer_len);
        errdefer allocator.free(writer_buffer);
        var editor = LineEditor{
            .allocator = allocator,
            .stdin_file = stdin_file,
            .stdout_file = stdout_file,
            .writer_buffer = writer_buffer,
            .writer = stdout_file.writer(writer_buffer),
            .buffer = .empty,
            .saved_current = .empty,
            .history = .init(allocator, history_limit),
        };
        editor.is_tty = stdin_file.isTty();
        if (editor.is_tty and @import("builtin").os.tag == .linux) {
            editor.original_termios = try std.posix.tcgetattr(stdin_file.handle);
            try editor.enableRawMode();
        }
        return editor;
    }

    fn deinit(self: *LineEditor) void {
        if (self.raw_enabled) {
            self.restoreTerminal() catch {};
        }
        self.buffer.deinit(self.allocator);
        self.saved_current.deinit(self.allocator);
        self.history.deinit();
        self.allocator.free(self.writer_buffer);
    }

    fn readLine(self: *LineEditor, prompt: []const u8) !?[]u8 {
        if (!self.is_tty) return self.readLinePlain(prompt);

        self.buffer.clearRetainingCapacity();
        self.saved_current.clearRetainingCapacity();
        self.cursor = 0;
        self.history_pos = self.history.entries.items.len;

        try self.writer.interface.writeAll(prompt);
        try self.writer.interface.flush();

        while (true) {
            const key = try self.readKey();
            switch (key) {
                .char => |byte| {
                    try self.buffer.insert(self.allocator, self.cursor, byte);
                    self.cursor += 1;
                    try self.render(prompt);
                },
                .tab => {
                    try self.buffer.insertSlice(self.allocator, self.cursor, "    ");
                    self.cursor += 4;
                    try self.render(prompt);
                },
                .backspace => {
                    if (self.cursor == 0) continue;
                    self.cursor -= 1;
                    _ = self.buffer.orderedRemove(self.cursor);
                    try self.render(prompt);
                },
                .delete => {
                    if (self.cursor >= self.buffer.items.len) continue;
                    _ = self.buffer.orderedRemove(self.cursor);
                    try self.render(prompt);
                },
                .left => {
                    if (self.cursor == 0) continue;
                    self.cursor -= 1;
                    try self.render(prompt);
                },
                .right => {
                    if (self.cursor >= self.buffer.items.len) continue;
                    self.cursor += 1;
                    try self.render(prompt);
                },
                .home => {
                    if (self.cursor == 0) continue;
                    self.cursor = 0;
                    try self.render(prompt);
                },
                .end => {
                    if (self.cursor == self.buffer.items.len) continue;
                    self.cursor = self.buffer.items.len;
                    try self.render(prompt);
                },
                .up => {
                    try self.historyStep(prompt, .older);
                },
                .down => {
                    try self.historyStep(prompt, .newer);
                },
                .enter => {
                    try self.writer.interface.writeAll("\r\n");
                    try self.writer.interface.flush();
                    const owned = try self.buffer.toOwnedSlice(self.allocator);
                    if (owned.len != 0) {
                        try self.history.append(owned);
                    }
                    return owned;
                },
                .ctrl_d => {
                    if (self.buffer.items.len == 0) {
                        return null;
                    }
                    if (self.cursor < self.buffer.items.len) {
                        _ = self.buffer.orderedRemove(self.cursor);
                        try self.render(prompt);
                    }
                },
                .ctrl_c => {
                    try self.writer.interface.writeAll("^C\r\n");
                    self.buffer.clearRetainingCapacity();
                    self.saved_current.clearRetainingCapacity();
                    self.cursor = 0;
                    self.history_pos = self.history.entries.items.len;
                    try self.writer.interface.writeAll(prompt);
                    try self.writer.interface.flush();
                },
                .eof => return null,
                .unknown => {},
            }
        }
    }

    fn readLinePlain(self: *LineEditor, prompt: []const u8) !?[]u8 {
        try self.writer.interface.writeAll(prompt);
        try self.writer.interface.flush();
        var buffer: [1024 * 64]u8 = undefined;
        var buffered = self.stdin_file.reader(&buffer);
        var slice = buffered.interface.takeDelimiterExclusive('\n') catch return null;
        if (slice.len > 0 and slice[slice.len - 1] == '\r') {
            slice = slice[0 .. slice.len - 1];
        }
        if (slice.len != 0) try self.history.append(slice);
        return slice;
    }

    fn render(self: *LineEditor, prompt: []const u8) !void {
        try self.writer.interface.writeAll("\r\x1b[2K");
        try self.writer.interface.writeAll(prompt);
        try self.writer.interface.writeAll(self.buffer.items);
        const tail = self.buffer.items.len - self.cursor;
        if (tail > 0) {
            var seq_buf: [16]u8 = undefined;
            const seq = try std.fmt.bufPrint(&seq_buf, "\x1b[{d}D", .{tail});
            try self.writer.interface.writeAll(seq);
        }
        try self.writer.interface.flush();
    }

    const HistoryStep = enum { older, newer };

    fn historyStep(self: *LineEditor, prompt: []const u8, direction: HistoryStep) !void {
        const total = self.history.entries.items.len;
        if (total == 0) return;

        if (direction == .older) {
            if (self.history_pos == 0) return;
            if (self.history_pos == total) {
                self.saved_current.clearRetainingCapacity();
                try self.saved_current.appendSlice(self.allocator, self.buffer.items);
            }
            self.history_pos -= 1;
            try self.replaceBuffer(self.history.entries.items[self.history_pos]);
        } else {
            if (self.history_pos >= total) {
                return;
            }
            self.history_pos += 1;
            if (self.history_pos == total) {
                try self.replaceBuffer(self.saved_current.items);
            } else {
                try self.replaceBuffer(self.history.entries.items[self.history_pos]);
            }
        }
        self.cursor = self.buffer.items.len;
        try self.render(prompt);
    }

    fn replaceBuffer(self: *LineEditor, data: []const u8) !void {
        self.buffer.clearRetainingCapacity();
        try self.buffer.appendSlice(self.allocator, data);
    }

    fn readKey(self: *LineEditor) !Key {
        var buffer: [512]u8 = undefined;
        var reader = self.stdin_file.reader(&buffer);
        const byte = reader.interface.takeByte() catch |err| switch (err) {
            error.EndOfStream => return Key.eof,
            else => return err,
        };
        return switch (byte) {
            '\r', '\n' => Key.enter,
            0x7f => Key.backspace,
            0x04 => Key.ctrl_d,
            0x03 => Key.ctrl_c,
            '\t' => Key.tab,
            0x1b => try self.readEscape(),
            else => Key{ .char = byte },
        };
    }

    fn readEscape(self: *LineEditor) !Key {
        const first = try self.readRawByte() orelse return Key.unknown;
        if (first == '[') {
            const second = try self.readRawByte() orelse return Key.unknown;
            return switch (second) {
                'A' => Key.up,
                'B' => Key.down,
                'C' => Key.right,
                'D' => Key.left,
                'H' => Key.home,
                'F' => Key.end,
                '1', '4', '3' => {
                    const third = try self.readRawByte() orelse return Key.unknown;
                    if (third != '~') return Key.unknown;
                    return switch (second) {
                        '1' => Key.home,
                        '4' => Key.end,
                        '3' => Key.delete,
                        else => Key.unknown,
                    };
                },
                else => Key.unknown,
            };
        } else if (first == 'O') {
            const second = try self.readRawByte() orelse return Key.unknown;
            return switch (second) {
                'H' => Key.home,
                'F' => Key.end,
                else => Key.unknown,
            };
        }
        return Key.unknown;
    }

    fn readRawByte(self: *LineEditor) !?u8 {
        var buffer: [512]u8 = undefined;
        var reader = self.stdin_file.reader(&buffer);
        return reader.interface.takeByte() catch |err| switch (err) {
            error.EndOfStream => null,
            else => err,
        };
    }

    fn enableRawMode(self: *LineEditor) !void {
        if (!self.is_tty) return;
        var raw = self.original_termios orelse return;
        raw.iflag.BRKINT = false;
        raw.iflag.ICRNL = false;
        raw.iflag.INPCK = false;
        raw.iflag.ISTRIP = false;
        raw.iflag.IXON = false;
        raw.oflag.OPOST = false;
        raw.cflag.CSIZE = .CS8;
        raw.cflag.CREAD = true;
        raw.cflag.CLOCAL = true;
        raw.lflag.ECHO = false;
        raw.lflag.ECHONL = false;
        raw.lflag.ICANON = false;
        raw.lflag.IEXTEN = false;
        raw.lflag.ISIG = false;
        try std.posix.tcsetattr(self.stdin_file.handle, .FLUSH, raw);
        self.raw_enabled = true;
    }

    fn restoreTerminal(self: *LineEditor) !void {
        if (!self.is_tty or @import("builtin").os.tag != .linux) return;
        if (self.original_termios) |original| {
            try std.posix.tcsetattr(self.stdin_file.handle, .FLUSH, original);
        }
        self.raw_enabled = false;
    }

    fn historyEntries(self: *LineEditor) []const []const u8 {
        return self.history.entries.items;
    }
};

const Key = union(enum) {
    char: u8,
    enter,
    backspace,
    delete,
    ctrl_d,
    ctrl_c,
    tab,
    left,
    right,
    up,
    down,
    home,
    end,
    eof,
    unknown,
};

const History = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayList([]const u8),
    max_entries: usize,

    fn init(allocator: std.mem.Allocator, limit: usize) History {
        return .{
            .allocator = allocator,
            .entries = .empty,
            .max_entries = if (limit == 0) 1 else limit,
        };
    }

    fn deinit(self: *History) void {
        for (self.entries.items) |entry| {
            self.allocator.free(@constCast(entry));
        }
        self.entries.deinit(self.allocator);
    }

    fn append(self: *History, line: []const u8) !void {
        if (line.len == 0) return;
        if (self.entries.items.len > 0) {
            const last = self.entries.items[self.entries.items.len - 1];
            if (std.mem.eql(u8, last, line)) return;
        }
        if (self.entries.items.len == self.max_entries) {
            const removed = self.entries.orderedRemove(0);
            self.allocator.free(@constCast(removed));
        }
        try self.entries.append(self.allocator, try self.allocator.dupe(u8, line));
    }
};
