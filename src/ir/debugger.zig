const std = @import("std");
const Allocator = std.mem.Allocator;
const IREvaluator = @import("evaluator.zig").IREvaluator;
const IREvaluatorError = @import("evaluator.zig").Error;
const ir = @import("ir.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;
const DocumentStore = @import("../document_store.zig").DocumentStore;
const ast = @import("../frontend/ast.zig");
const rainbow = @import("../rainbow.zig");

const span_color = rainbow.beginBgColor(.yellow) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

pub const Error =
    IRDebugger.GetCommandError ||
    IREvaluatorError ||
    DocumentStore.Error ||
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
        context: *ir.IRContext,
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

    pub fn step(self: *IRDebugger) Error!void {
        const ic = self.evaluator.context.instruction_counter;
        const instruction = self.evaluator.context.read_only.instructions[ic];
        const maybe_span = instruction.span();
        if (maybe_span) |span| {
            try self.logEvaluateSpan(span);
        }
        try self.print("{}: {f}\n", .{ ic, instruction });
        _ = try self.evaluator.step();
    }

    pub fn run(self: *IRDebugger) Error!ExitCode {
        while (true) {
            const command = try self.getCommand() orelse continue;

            try self.command_history.append(self.allocator, command);
            self.command_history_cursor = null;

            switch (command) {
                .quit => break,
                .step => try self.step(),
            }
        }
        return .success;
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

    const GetCommandError = std.Io.Reader.DelimiterError || ProcessStdinError;

    const command_delimiters: []const []const u8 = &.{"\n"};

    fn getCommand(self: *IRDebugger) GetCommandError!?Command {
        const stdin_reader = self.stdinReader();
        const cw = self.commandWriter();

        var stdin_allocating_writer = AllocatingWriter.init(self.allocator);
        defer stdin_allocating_writer.deinit();
        const stdin_writer = stdin_allocating_writer.get();

        _ = try stdin_reader.stream(&stdin_writer.writer, .unlimited);

        try self.processStdin(stdin_writer);
        try self.writeAll(stdin_writer.written());
        try self.commandWriter().writeAll(stdin_writer.written());

        // TODO: iterate through all commands possibly generated from the slice
        for (command_delimiters) |delimiter| {
            if (std.mem.indexOf(u8, cw.buffered(), delimiter)) |index| {
                defer _ = cw.consume(index + delimiter.len);
                const command = cw.buffered()[0..index];
                return self.parseCommand(command);
            }
        }

        return null;
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
        none,
    };

    const StdinHandlerMatcher = union(enum) {
        exact: []const u8,
        starts_with: []const u8,
        ends_with: []const u8,
        starts_and_ends_with: struct { []const u8, []const u8 },
    };

    const StdinHandlerFn = *const fn (*IRDebugger, match: []const u8) StdinHandlerEvent;

    const StdinHandler = struct {
        matcher: StdinHandlerMatcher,
        handler: StdinHandlerFn,

        const Range = struct {
            start: usize,
            end: usize,
        };

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
            const command = self.commandHistoryUp(1) orelse return .{
                .replace_all = .{ .text = "" },
            };
            return .{ .replace_all = .{ .command = command } };
        }

        fn down(self: *IRDebugger, _: []const u8) StdinHandlerEvent {
            const command = self.commandHistoryDown(1) orelse return .{
                .replace_all = .{ .text = "" },
            };
            return .{ .replace_all = .{ .command = command } };
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
    };

    const ProcessStdinError =
        std.Io.Writer.Error ||
        Allocator.Error ||
        StdinHandler.TestStdinError;

    fn processStdin(
        self: *IRDebugger,
        stdin_writer: *std.Io.Writer.Allocating,
    ) ProcessStdinError!void {
        // std.log.debug("stdin: {any}", .{stdin_writer.written()});

        while (try matchHandler(stdin_writer.written())) |handler| {
            const before_slice = stdin_writer.written()[0..handler.range.start];
            const match = stdin_writer.written()[handler.range.start..handler.range.end];
            const event = handler.handler.handler(self, match);
            try self.handleStdinHandlerEvent(event, before_slice);
            _ = stdin_writer.writer.consume(handler.range.end);
        }
    }

    fn handleStdinHandlerEvent(
        self: *IRDebugger,
        event: StdinHandlerEvent,
        before_slice: []const u8,
    ) std.Io.Writer.Error!void {
        switch (event) {
            .replace_all => |replace_all| {
                _ = self.commandWriter().consumeAll();
                try self.commandWriter().print("{f}", .{replace_all});
                try self.print("\x1B[2K\r{f}", .{replace_all});
            },
            .none => {
                try self.commandWriter().writeAll(before_slice);
            },
        }
    }

    const MatchHandler = struct {
        range: StdinHandler.Range,
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
