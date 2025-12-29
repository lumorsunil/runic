const std = @import("std");
const Allocator = std.mem.Allocator;
const IREvaluator = @import("evaluator.zig").IREvaluator;
const IREvaluatorError = @import("evaluator.zig").Error;
const ir = @import("../ir.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;
const DocumentStore = @import("../document_store.zig").DocumentStore;
const ast = @import("../frontend/ast.zig");
const rainbow = @import("../rainbow.zig");

const span_color = rainbow.beginBgColor(.yellow) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

pub const Error =
    IREvaluatorError ||
    DocumentStore.Error ||
    Allocator.Error ||
    IRDebugger.StdinHandler.TestStdinError ||
    std.Io.Writer.Error ||
    std.Io.Reader.Error ||
    std.Io.Reader.DelimiterError ||
    std.fmt.ParseIntError ||
    error{};

pub const IRDebugger = struct {
    allocator: Allocator,
    evaluator: IREvaluator,
    stdout_file_writer: std.fs.File.Writer,
    stdin_file_reader: std.fs.File.Reader,
    document_store: *DocumentStore,
    command_history: std.ArrayList(Command) = .empty,
    command_history_cursor: ?usize = null,
    command_writer: std.Io.Writer.Allocating,

    pub fn init(
        allocator: Allocator,
        config: IREvaluator.Config,
        document_store: *DocumentStore,
        context: *ir.context.IRProgramContext,
    ) Allocator.Error!@This() {
        return .{
            .allocator = allocator,
            .evaluator = .init(allocator, config, context),
            .stdout_file_writer = std.fs.File.stdout().writer(try allocator.alloc(u8, 1024)),
            .stdin_file_reader = std.fs.File.stdin().reader(try allocator.alloc(u8, 1024)),
            .document_store = document_store,
            .command_writer = .init(allocator),
        };
    }

    pub fn step(self: *IRDebugger) Error!RunningEvent {
        try self.writeAll("\n");

        const thread = self.evaluator.context.getCurrentThread();
        const ic = thread.private.instruction_counter;
        const instruction = thread.shared.instructions[ic];
        const maybe_span = instruction.span();
        if (maybe_span) |span| {
            try self.logEvaluateSpan(span);
        }
        try self.print("{}: {f}\n", .{ ic, instruction });

        const event = try self.evaluator.step() orelse return .quit;

        switch (event) {
            .cont => return .cont,
            .exit => |exit_code| {
                try self.print("Program exited with code: {f}", .{exit_code});
                return .quit;
            },
        }
    }

    pub fn run(self: *IRDebugger) Error!ExitCode {
        while (true) switch (try self.loop()) {
            .quit => break,
            .cont => continue,
        };
        return .success;
    }

    fn printDebugCommandStuff(self: *IRDebugger) !void {
        var prpos = try self.getCursorPosition();
        prpos.row += 1;
        prpos.col = 1;
        try self.printToPos(prpos, "command: \"{any}\"{s}", .{ self.commandWriter().buffered(), clear_line_to_end });
    }

    fn stdoutWriter(self: *IRDebugger) *std.Io.Writer {
        return &self.stdout_file_writer.interface;
    }

    fn stdinReader(self: *IRDebugger) *std.Io.Reader {
        return &self.stdin_file_reader.interface;
    }

    fn commandWriter(self: *IRDebugger) *std.Io.Writer {
        return &self.command_writer.writer;
    }

    fn print(self: *IRDebugger, comptime fmt: []const u8, args: anytype) !void {
        defer self.stdoutWriter().flush() catch {};
        return self.stdoutWriter().print(fmt, args);
    }

    fn writeAll(self: *IRDebugger, string: []const u8) !void {
        defer self.stdoutWriter().flush() catch {};
        return self.stdoutWriter().writeAll(string);
    }

    const command_delimiters: []const []const u8 = &.{"\n"};

    fn loop(self: *IRDebugger) Error!RunningEvent {
        // try self.printDebugCommandStuff();

        const stdin_reader = self.stdinReader();

        var stdin_allocating_writer = AllocatingWriter.init(self.allocator);
        defer stdin_allocating_writer.deinit();
        const stdin_writer = stdin_allocating_writer.get();

        _ = try stdin_reader.stream(&stdin_writer.writer, .unlimited);

        switch (try self.processStdin(stdin_writer)) {
            .quit => return .quit,
            .cont => {},
        }
        try self.updateCommand(stdin_writer.written());

        return .cont;
    }

    fn updateCommand(self: *IRDebugger, stdin_slice: []const u8) !void {
        const pos_before = try self.getCursorPosition();
        const end_index = pos_before.col - 1;
        const after_slice = try self.allocator.dupe(
            u8,
            self.commandWriter().buffered()[end_index..],
        );
        // try self.printToPos(pos_before.add(2, 0), "\rafter_slice: \"{s}\"", .{after_slice});
        self.commandWriter().end = end_index;
        try self.writeAll(stdin_slice);
        const pos_after = try self.getCursorPosition();
        try self.writeAll(after_slice);
        try self.commandWriter().writeAll(stdin_slice);
        try self.commandWriter().writeAll(after_slice);
        try self.setCursorPosition(pos_after);
    }

    const AllocatingWriter = struct {
        fallback_allocator: std.heap.StackFallbackAllocator(128),
        allocating_writer: std.Io.Writer.Allocating,

        pub fn init(allocator: Allocator) @This() {
            return .{
                .fallback_allocator = std.heap.stackFallback(128, allocator),
                .allocating_writer = undefined,
            };
        }

        pub fn deinit(self: *@This()) void {
            self.allocating_writer.deinit();
        }

        pub fn get(self: *@This()) *std.Io.Writer.Allocating {
            self.allocating_writer = .init(self.fallback_allocator.get());
            return &self.allocating_writer;
        }
    };

    const StdinHandlerEvent = union(enum) {
        replace_all: union(enum) {
            text: []const u8,
            command: Command,

            pub fn format(
                self: @This(),
                writer: *std.Io.Writer,
            ) std.Io.Writer.Error!void {
                try switch (self) {
                    .text => |text| writer.writeAll(text),
                    .command => |command| writer.print("{f}", .{command}),
                };
            }
        },
        write: []const u8,
        delete,
        write_and_delete: []const u8,
        horizontal_move: isize,
        run_command,
        none,

        pub fn clear() @This() {
            return .{ .replace_all = .{ .text = "" } };
        }
    };

    const StdinHandlerMatcher = union(enum) {
        exact: []const u8,
        starts_with: []const u8,
        ends_with: []const u8,
        starts_and_ends_with: struct { []const u8, []const u8 },
    };

    const StdinHandlerFn = *const fn (*IRDebugger, match: []const u8) StdinHandlerEvent;

    const Range = struct {
        start: usize,
        end: usize,
    };

    const StdinHandler = struct {
        matcher: StdinHandlerMatcher,
        handler: StdinHandlerFn,

        const TestStdinError = error{UnsupportedMatcher};

        fn test_stdin(self: StdinHandler, stdin: []const u8) TestStdinError!?Range {
            switch (self.matcher) {
                .exact => |exact| {
                    const index = std.mem.indexOf(u8, stdin, exact) orelse return null;
                    return .{ .start = index, .end = index + exact.len };
                },
                else => return TestStdinError.UnsupportedMatcher,
            }
        }

        fn up(self: *IRDebugger, _: []const u8) StdinHandlerEvent {
            const command = self.commandHistoryUp(1) orelse return .clear();
            return .{ .replace_all = .{ .command = command } };
        }

        fn down(self: *IRDebugger, _: []const u8) StdinHandlerEvent {
            const command = self.commandHistoryDown(1) orelse return .clear();
            return .{ .replace_all = .{ .command = command } };
        }

        fn forward(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .{ .horizontal_move = 1 };
        }

        fn back(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .{ .horizontal_move = -1 };
        }

        fn backspace(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .{ .write_and_delete = &.{8} };
        }

        fn delete(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .delete;
        }

        fn home(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .{ .horizontal_move = -std.math.maxInt(isize) };
        }

        fn end(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .{ .horizontal_move = std.math.maxInt(isize) };
        }

        fn enter(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .run_command;
        }
    };

    const stdin_handlers: []const StdinHandler = &.{
        .{
            .matcher = .{ .exact = &.{ 27, 91, 65 } },
            .handler = StdinHandler.up,
        },
        .{
            .matcher = .{ .exact = &.{ 27, 91, 66 } },
            .handler = StdinHandler.down,
        },
        .{
            .matcher = .{ .exact = &.{ 27, 91, 67 } },
            .handler = StdinHandler.forward,
        },
        .{
            .matcher = .{ .exact = &.{ 27, 91, 68 } },
            .handler = StdinHandler.back,
        },
        .{
            .matcher = .{ .exact = &.{127} },
            .handler = StdinHandler.backspace,
        },
        .{
            .matcher = .{ .exact = &.{ 27, 91, 51, 126 } },
            .handler = StdinHandler.delete,
        },
        .{
            .matcher = .{ .exact = &.{ 27, 91, 72 } },
            .handler = StdinHandler.home,
        },
        .{
            .matcher = .{ .exact = &.{ 27, 91, 70 } },
            .handler = StdinHandler.end,
        },
        .{
            .matcher = .{ .exact = &.{10} },
            .handler = StdinHandler.enter,
        },
        // { 27, 91, 53, 126 } page up
        // { 27, 91, 54, 126 } page down
        // { 9 } tab
    };

    const RunningEvent = enum { cont, quit };

    fn processStdin(
        self: *IRDebugger,
        stdin_writer: *std.Io.Writer.Allocating,
    ) Error!RunningEvent {
        // std.log.debug("stdin: {any}", .{stdin_writer.written()});

        while (try matchHandler(stdin_writer.written())) |handler| {
            const before_slice = stdin_writer.written()[0..handler.range.start];
            const match = stdin_writer.written()[handler.range.start..handler.range.end];
            const event = handler.handler.handler(self, match);
            switch (try self.handleStdinHandlerEvent(event, before_slice)) {
                .quit => return .quit,
                .cont => {},
            }
            _ = stdin_writer.writer.consume(handler.range.end);
        }

        var alloc_writer = AllocatingWriter.init(self.allocator);
        const writer = alloc_writer.get();
        for (0..stdin_writer.written().len) |i| {
            const ch = stdin_writer.written()[i];
            if (std.ascii.isPrint(ch)) {
                try writer.writer.writeByte(ch);
            }
        }

        _ = stdin_writer.writer.consumeAll();
        try stdin_writer.writer.writeAll(try writer.toOwnedSlice());

        return .cont;
    }

    const csi = "\x1B[";
    const clear_line = csi ++ "2K";
    const clear_line_to_end = csi ++ "0K";
    const request_position = csi ++ "6n";

    fn handleStdinHandlerEvent(
        self: *IRDebugger,
        event: StdinHandlerEvent,
        before_slice: []const u8,
    ) Error!RunningEvent {
        switch (event) {
            .replace_all => |replace_all| {
                _ = self.commandWriter().consumeAll();
                try self.commandWriter().print("{f}", .{replace_all});
                try self.print("{s}\r{f}", .{ clear_line, replace_all });
            },
            .write => |text| {
                try self.commandWriter().writeAll(before_slice);
                try self.writeAll(before_slice);
                // try self.commandWriter().writeAll(text);
                try self.writeAll(text);
            },
            .delete => {
                try self.commandWriter().writeAll(before_slice);
                try self.writeAll(before_slice);
                const pos = try self.getCursorPosition();

                if (pos.col > self.commandWriter().buffered().len) return .cont;

                var fallback_allocator = std.heap.stackFallback(128, self.allocator);
                const allocator = fallback_allocator.get();
                const after_start = @min(self.commandWriter().buffered().len, pos.col);
                const after_slice = try allocator.dupe(
                    u8,
                    self.commandWriter().buffered()[after_start..],
                );
                defer allocator.free(after_slice);
                self.commandWriter().undo(after_slice.len + 1);
                try self.commandWriter().writeAll(after_slice);
                try self.print("{s}{s}", .{ clear_line_to_end, after_slice });
                try self.setCursorPosition(pos);
            },
            .write_and_delete => |text| {
                try self.commandWriter().writeAll(before_slice);
                try self.writeAll(before_slice);

                _ = try self.handleStdinHandlerEvent(.{ .write = text }, "");
                _ = try self.handleStdinHandlerEvent(.delete, "");
            },
            .horizontal_move => |delta| {
                try self.commandWriter().writeAll(before_slice);
                try self.writeAll(before_slice);

                const pos = try self.getCursorPosition();
                var npos = pos.add(0, delta);
                npos.col = @min(npos.col, self.commandWriter().buffered().len + 1);

                try self.setCursorPosition(npos);
            },
            .run_command => {
                try self.commandWriter().writeAll(before_slice);
                try self.writeAll(before_slice);

                defer _ = self.commandWriter().consumeAll();

                const command_string = self.commandWriter().buffered();
                const command = try self.parseCommand(command_string) orelse return .cont;

                return self.handleCommand(command);
            },
            .none => {
                try self.commandWriter().writeAll(before_slice);
                try self.writeAll(before_slice);
            },
        }

        return .cont;
    }

    fn handleCommand(self: *IRDebugger, command: Command) Error!RunningEvent {
        try self.command_history.append(self.allocator, command);
        self.command_history_cursor = null;

        switch (command) {
            .quit => return .quit,
            .step => return self.step(),
        }
    }

    const CursorPosition = struct {
        row: usize,
        col: usize,

        pub fn add(self: CursorPosition, row: isize, col: isize) CursorPosition {
            return .{
                .row = @intCast(@max(1, @as(isize, @intCast(self.row)) +| row)),
                .col = @intCast(@max(1, @as(isize, @intCast(self.col)) +| col)),
            };
        }
    };

    fn setCursorPosition(
        self: *IRDebugger,
        position: CursorPosition,
    ) std.Io.Writer.Error!void {
        try self.print(csi ++ "{};{}H", .{ position.row, position.col });
    }

    fn getCursorPosition(
        self: *IRDebugger,
    ) Error!CursorPosition {
        // response: ESC[n;mR
        // { 27, 91, 49, 59, 51, 82 }
        try self.writeAll(request_position);
        const response = try self.stdinReader().takeDelimiterInclusive('R');
        var it = std.mem.tokenizeAny(u8, response[2..], ";R");
        const row_s = it.next().?;
        const col_s = it.next().?;
        const row = try std.fmt.parseUnsigned(usize, row_s, 10);
        const col = try std.fmt.parseUnsigned(usize, col_s, 10);
        return .{ .row = row, .col = col };
    }

    fn printToPos(
        self: *IRDebugger,
        pos: CursorPosition,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        const current_pos = try self.getCursorPosition();
        try self.setCursorPosition(pos);
        try self.print(fmt, args);
        try self.setCursorPosition(current_pos);
    }

    const MatchHandler = struct {
        range: Range,
        handler: StdinHandler,
    };

    fn matchHandler(slice: []const u8) StdinHandler.TestStdinError!?MatchHandler {
        var min_handler: ?MatchHandler = null;

        for (stdin_handlers) |stdin_handler| {
            const range = try stdin_handler.test_stdin(slice) orelse continue;

            const handler = min_handler orelse {
                min_handler = .{ .range = range, .handler = stdin_handler };
                continue;
            };

            if (range.start >= handler.range.start) continue;

            min_handler = .{ .range = range, .handler = stdin_handler };
        }

        return min_handler;
    }

    fn parseCommand(self: *IRDebugger, source: []const u8) std.Io.Writer.Error!?Command {
        if (std.mem.eql(u8, source, "quit")) {
            return .quit;
        }

        if (std.mem.eql(u8, source, "step")) {
            return .step;
        }

        try self.print("\nUnknown command \"{s}\".\n\n", .{source});

        return null;
    }

    fn commandHistoryUp(self: *IRDebugger, delta: usize) ?Command {
        const len = self.command_history.items.len;
        if (len == 0) {
            self.command_history_cursor = null;
            return null;
        }
        if (self.command_history_cursor == len - 1) {
            self.command_history_cursor = null;
            return null;
        }
        if (self.command_history_cursor) |*cursor| {
            cursor.* = @min(cursor.* + delta, len - 1);
        } else {
            self.command_history_cursor = 0;
        }

        return self.getCommandAtHistoryCursor();
    }

    fn commandHistoryDown(self: *IRDebugger, delta: usize) ?Command {
        const len = self.command_history.items.len;
        if (len == 0) {
            self.command_history_cursor = null;
            return null;
        }
        if (self.command_history_cursor == 0) {
            self.command_history_cursor = null;
            return null;
        }
        if (self.command_history_cursor) |*cursor| {
            cursor.* = cursor.* -| delta;
        } else {
            self.command_history_cursor = len - 1;
        }

        return self.getCommandAtHistoryCursor();
    }

    fn getCommandAtHistoryCursor(self: *IRDebugger) ?Command {
        if (self.command_history_cursor) |cursor| {
            return self.command_history.items[(self.command_history.items.len - cursor) - 1];
        }

        return null;
    }

    fn logEvaluateSpan(self: *IRDebugger, span: ast.Span) Error!void {
        if (span.isGlobal()) {
            try self.print("{s}\n", .{span.start.file});
            return;
        }

        const source = try self.document_store.getSource(span.start.file);
        var lineIt = std.mem.splitScalar(u8, source, '\n');
        var i: usize = 0;
        while (lineIt.next()) |line| : (i += 1) {
            if (i >= span.start.line -| 3 and i <= span.end.line +| 3) {
                if (span.start.line == i + 1 and span.end.line == i + 1) {
                    try self.print("{:>4}:{s}{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line == i + 1) {
                    try self.print("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 ..],
                        end_color,
                    });
                } else if (span.end.line == i + 1) {
                    try self.print("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line[0 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line - 1 <= i and i <= span.end.line - 1) {
                    try self.print("{:>4}:{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line,
                        end_color,
                    });
                } else {
                    try self.print("{:>4}:{s}\n", .{ i + 1, line });
                }
            }
        }
    }
};

const Command = union(enum) {
    quit,
    step,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("{t}", .{self});
    }
};

pub fn takeDelimiterInclusive(
    r: *std.Io.Reader,
    delimiters: []const []const u8,
) std.Io.Reader.DelimiterError![]u8 {
    const result = try peekDelimiterInclusive(r, delimiters);
    r.toss(result.len);
    return result;
}

pub fn peekDelimiterInclusive(
    r: *std.Io.Reader,
    delimiters: []const []const u8,
) std.Io.Reader.DelimiterError![]u8 {
    const buffer = r.buffer[0..r.end];
    const seek = r.seek;
    for (delimiters) |delimiter| if (std.mem.indexOfPos(u8, buffer, seek, delimiter)) |end| {
        @branchHint(.likely);
        return buffer[seek .. end + delimiter.len];
    };
    // TODO take a parameter for max search length rather than relying on buffer capacity
    try r.rebase(r.buffer.len);
    while (r.buffer.len - r.end != 0) {
        const end_cap = r.buffer[r.end..];
        var writer: std.Io.Writer = .fixed(end_cap);
        const n = r.vtable.stream(r, &writer, .limited(end_cap.len)) catch |err| switch (err) {
            error.WriteFailed => unreachable,
            else => |e| return e,
        };
        r.end += n;
        for (delimiters) |delimiter| if (std.mem.indexOfPos(u8, end_cap[0..n], 0, delimiter)) |end| {
            return r.buffer[0 .. r.end - n + end + delimiter.len];
        };
    }
    return error.StreamTooLong;
}
