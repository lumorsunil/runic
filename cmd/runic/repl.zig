const std = @import("std");
const runic = @import("runic");

const CommandRunner = runic.command_runner.CommandRunner;
const ProcessHandle = runic.command_runner.ProcessHandle;
const StageStatus = runic.command_runner.StageStatus;
const CommandDisplay = runic.command_runner.CommandDisplay;
const StackTrace = runic.stack_trace.StackTrace;
const Tracer = runic.tracing.Tracer;

pub const Options = struct {
    prompt: []const u8 = "runic> ",
    continuation_prompt: []const u8 = "...> ",
    history_limit: usize = 256,
    trace_topics: [][]const u8 = &[_][]const u8{},
    module_paths: [][]const u8 = &[_][]const u8{},
};

pub fn run(allocator: std.mem.Allocator, options: Options) !void {
    var session = try Session.init(allocator, options);
    defer session.deinit();
    try session.loop();
}

const Session = struct {
    allocator: std.mem.Allocator,
    options: Options,
    editor: LineEditor,
    stdout: std.fs.File.Writer,
    stderr: std.fs.File.Writer,
    tracer: Tracer,

    fn init(allocator: std.mem.Allocator, options: Options) !Session {
        const stdout_file = std.fs.File.stdout();
        const stderr_file = std.fs.File.stderr();

        var session = Session{
            .allocator = allocator,
            .options = options,
            .editor = try LineEditor.init(allocator, options.history_limit),
            .stdout = stdout_file.writer(&.{}),
            .stderr = stderr_file.writer(&.{}),
            .tracer = Tracer.init(options.trace_topics, null),
        };
        session.tracer.setSink(session.stderr.any());
        return session;
    }

    fn deinit(self: *Session) void {
        self.tracer.clearSink();
        self.editor.deinit();
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
    }

    fn handleCommand(self: *Session, command: []const u8) !bool {
        if (command.len == 0) return false;
        if (command[0] == ':') {
            return self.handleMeta(command[1..]);
        }
        return try self.executePipeline(command);
    }

    fn handleMeta(self: *Session, command: []const u8) !bool {
        const trimmed = std.mem.trim(u8, command, " \t");
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
                    "Anything else runs as a bare command pipeline via CommandRunner.\n",
                .{},
            );
            return false;
        }
        try self.stderr.interface.print("Unknown REPL command ':{s}'. Try :help.\n", .{trimmed});
        return false;
    }

    fn executePipeline(self: *Session, command: []const u8) !bool {
        var tokens = TokenList.init(self.allocator);
        defer tokens.deinit();

        tokens.populate(command) catch |err| {
            try self.stderr.interface.print("Parse error: {s}\n", .{@errorName(err)});
            return false;
        };
        if (tokens.items.len == 0) return false;

        var pipeline = Pipeline.init(self.allocator);
        defer pipeline.deinit();

        pipeline.build(tokens.items) catch |err| {
            try self.stderr.interface.print("Pipeline error: {s}\n", .{@errorName(err)});
            return false;
        };

        var runner = CommandRunner.initWithTracer(self.allocator, &self.tracer);
        var handle = runner.runPipeline(pipeline.specs) catch |err| {
            try self.renderRunnerErrorStackTrace(command, err);
            try self.stderr.interface.print("Command error: {s}\n", .{@errorName(err)});
            return false;
        };
        defer handle.deinit();

        if (!handle.status.ok) {
            try self.renderStageFailureStackTrace(command, pipeline.specs, &handle);
        }

        try self.displayHandle(handle);
        return false;
    }

    fn showHistory(self: *Session) !void {
        const entries = self.editor.historyEntries();
        if (entries.len == 0) {
            try self.stdout.interface.print("(history empty)\n", .{});
            return;
        }
        for (entries, 0..) |entry, idx| {
            try self.stdout.interface.print("{d}: {s}\n", .{ idx + 1, entry });
        }
    }

    fn displayHandle(self: *Session, handle: ProcessHandle) !void {
        const stdout_bytes = handle.stdoutBytes();
        if (stdout_bytes.len > 0) try self.stdout.interface.writeAll(stdout_bytes);

        const stderr_bytes = handle.stderrBytes();
        if (stderr_bytes.len > 0) try self.stderr.interface.writeAll(stderr_bytes);

        const status = handle.status;
        if (status.ok) {
            const code = status.exit_code orelse 0;
            try self.stdout.interface.print("\n[ok] exit code {d}\n", .{code});
        } else {
            const failed = status.failed_stage orelse (handle.stageStatuses().len - 1);
            const code = status.exit_code orelse 0;
            if (status.signal) |sig| {
                try self.stderr.interface.print(
                    "\n[err] stage {d} stopped by signal {d}\n",
                    .{ failed + 1, sig },
                );
            } else {
                try self.stderr.interface.print(
                    "\n[err] stage {d} exited with code {d}\n",
                    .{ failed + 1, code },
                );
            }
        }
    }

    fn renderStageFailureStackTrace(
        self: *Session,
        command: []const u8,
        specs: []const CommandRunner.CommandSpec,
        handle: *const ProcessHandle,
    ) !void {
        if (specs.len == 0) return;
        const failed_idx = handle.status.failed_stage orelse (specs.len - 1);
        if (failed_idx >= specs.len) return;
        const statuses = handle.stageStatuses();
        if (failed_idx >= statuses.len) return;

        var trace = StackTrace.init(self.allocator);
        defer trace.deinit();

        const stage_label = try std.fmt.allocPrint(self.allocator, "stage {d}", .{failed_idx + 1});
        defer self.allocator.free(stage_label);

        const stage_detail = try describeStage(self.allocator, specs[failed_idx].argv, statuses[failed_idx]);
        defer self.allocator.free(stage_detail);

        try trace.push(.{ .label = stage_label, .detail = stage_detail });
        try trace.push(.{ .label = "pipeline", .detail = command });
        try trace.render(&self.stderr.interface);
    }

    fn renderRunnerErrorStackTrace(self: *Session, command: []const u8, err: anyerror) !void {
        var trace = StackTrace.init(self.allocator);
        defer trace.deinit();

        try trace.push(.{ .label = "command runner", .detail = @errorName(err) });
        try trace.push(.{ .label = "pipeline", .detail = command });
        try trace.render(&self.stderr.interface);
    }
};

const LineEditor = struct {
    allocator: std.mem.Allocator,
    stdin_file: std.fs.File,
    stdout_file: std.fs.File,
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
        var editor = LineEditor{
            .allocator = allocator,
            .stdin_file = stdin_file,
            .stdout_file = stdout_file,
            .writer = stdout_file.writer(&.{}),
            .buffer = .empty,
            .saved_current = .empty,
            .history = .init(allocator, history_limit),
        };
        editor.is_tty = stdin_file.isTty();
        if (editor.is_tty) {
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
        var byte_buf: [1]u8 = undefined;
        var buffer: [1024 * 4]u8 = undefined;
        var reader = self.stdin_file.reader(&buffer);
        const read = try reader.read(&byte_buf);
        if (read == 0) return Key.eof;
        const byte = byte_buf[0];
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
        var byte_buf: [1]u8 = undefined;
        var buffer: [1024 * 4]u8 = undefined;
        var reader = self.stdin_file.reader(&buffer);
        const read = try reader.read(&byte_buf);
        if (read == 0) return null;
        return byte_buf[0];
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
        if (!self.is_tty) return;
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

const TokenList = struct {
    allocator: std.mem.Allocator,
    items: [][]const u8 = &[_][]const u8{},

    fn init(allocator: std.mem.Allocator) TokenList {
        return .{ .allocator = allocator };
    }

    fn deinit(self: *TokenList) void {
        if (self.items.len == 0) return;
        for (self.items) |item| self.allocator.free(item);
        self.allocator.free(self.items);
    }

    fn populate(self: *TokenList, command: []const u8) !void {
        var tokens = std.ArrayList([]const u8).empty;
        defer tokens.deinit(self.allocator);

        var current = std.ArrayList(u8).empty;
        defer current.deinit(self.allocator);

        var in_single = false;
        var in_double = false;
        var escape = false;

        for (command) |byte| {
            if (escape) {
                try current.append(self.allocator, byte);
                escape = false;
                continue;
            }
            if (!in_single and byte == '\\') {
                escape = true;
                continue;
            }
            if (!in_double and byte == '\'') {
                in_single = !in_single;
                continue;
            }
            if (!in_single and byte == '"') {
                in_double = !in_double;
                continue;
            }
            if (!in_single and !in_double and (byte == ' ' or byte == '\t')) {
                try flushToken(self.allocator, &tokens, &current);
                continue;
            }
            if (!in_single and !in_double and byte == '|') {
                try flushToken(self.allocator, &tokens, &current);
                try tokens.append(self.allocator, try self.allocator.dupe(u8, "|"));
                continue;
            }
            try current.append(self.allocator, byte);
        }

        if (in_single or in_double) return error.UnterminatedQuote;
        if (escape) try current.append(self.allocator, '\\');

        try flushToken(self.allocator, &tokens, &current);
        self.items = try tokens.toOwnedSlice(self.allocator);
    }
};

fn flushToken(allocator: std.mem.Allocator, tokens: *std.ArrayList([]const u8), current: *std.ArrayList(u8)) !void {
    if (current.items.len == 0) return;
    const owned = try allocator.dupe(u8, current.items);
    try tokens.append(allocator, owned);
    current.clearRetainingCapacity();
}

const Pipeline = struct {
    allocator: std.mem.Allocator,
    specs: []CommandRunner.CommandSpec = &[_]CommandRunner.CommandSpec{},

    fn init(allocator: std.mem.Allocator) Pipeline {
        return .{ .allocator = allocator };
    }

    fn deinit(self: *Pipeline) void {
        if (self.specs.len == 0) return;
        for (self.specs) |spec| {
            self.allocator.free(spec.argv);
        }
        self.allocator.free(self.specs);
    }

    fn build(self: *Pipeline, tokens: [][]const u8) !void {
        var specs = std.ArrayList(CommandRunner.CommandSpec).empty;
        defer specs.deinit(self.allocator);

        var args = std.ArrayList([]const u8).empty;
        defer args.deinit(self.allocator);

        for (tokens) |token| {
            if (token.len == 1 and token[0] == '|') {
                try self.flushStage(&specs, &args);
                continue;
            }
            try args.append(self.allocator, token);
        }

        try self.flushStage(&specs, &args);
        self.specs = try specs.toOwnedSlice(self.allocator);
    }

    fn flushStage(self: *Pipeline, specs: *std.ArrayList(CommandRunner.CommandSpec), args: *std.ArrayList([]const u8)) !void {
        if (args.items.len == 0) return error.MissingCommand;
        const argv = try self.allocator.alloc([]const u8, args.items.len);
        for (args.items, 0..) |token, idx| {
            argv[idx] = token;
        }
        try specs.append(self.allocator, .{ .argv = argv });
        args.clearRetainingCapacity();
    }
};

fn describeStage(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    status: StageStatus,
) ![]u8 {
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(allocator);
    var writer = buffer.writer(allocator);

    try writer.print("{any}", .{CommandDisplay{ .argv = argv }});

    if (status.signal) |sig| {
        try writer.print(" (signal {d})", .{sig});
    } else if (status.exit_code) |code| {
        if (!status.ok or code != 0) {
            try writer.print(" (exit code {d})", .{code});
        }
    } else if (!status.ok) {
        try writer.writeAll(" (failed)");
    }

    return try buffer.toOwnedSlice(allocator);
}

test "tokenize pipe with quotes" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var tokens = TokenList.init(gpa.allocator());
    defer tokens.deinit();
    try tokens.populate("echo \"hello world\" | upper");

    try std.testing.expectEqual(@as(usize, 4), tokens.items.len);
    try std.testing.expectEqualStrings("echo", tokens.items[0]);
    try std.testing.expectEqualStrings("hello world", tokens.items[1]);
    try std.testing.expectEqualStrings("|", tokens.items[2]);
    try std.testing.expectEqualStrings("upper", tokens.items[3]);
}

test "build pipeline from tokens" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var tokens = TokenList.init(gpa.allocator());
    defer tokens.deinit();
    try tokens.populate("ls | wc -l");

    var pipeline = Pipeline.init(gpa.allocator());
    defer pipeline.deinit();
    try pipeline.build(tokens.items);

    try std.testing.expectEqual(@as(usize, 2), pipeline.specs.len);
    try std.testing.expectEqual(@as(usize, 1), pipeline.specs[0].argv.len);
    try std.testing.expectEqualStrings("ls", pipeline.specs[0].argv[0]);
    try std.testing.expectEqual(@as(usize, 2), pipeline.specs[1].argv.len);
    try std.testing.expectEqualStrings("wc", pipeline.specs[1].argv[0]);
    try std.testing.expectEqualStrings("-l", pipeline.specs[1].argv[1]);
}
