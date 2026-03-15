const std = @import("std");
const Allocator = std.mem.Allocator;
const IREvaluator = @import("evaluator.zig").IREvaluator;
const IREvaluatorError = @import("evaluator.zig").Error;
const ir = @import("../ir.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;
const DocumentStore = @import("../document_store.zig").DocumentStore;
const ast = @import("../frontend/ast.zig");
const rainbow = @import("../rainbow.zig");
const TraceFilter = @import("../trace.zig").TraceFilter;
const signals = @import("../signals.zig");
const ThreadId = @import("context.zig").ThreadId;
const PipeHandle = @import("context.zig").PipeHandle;

const span_color = rainbow.beginBgColor(.yellow) ++ rainbow.beginColor(.black);
const current_instr_color = rainbow.beginBgColor(.yellow) ++ rainbow.beginColor(.black);
const inactive_color = rainbow.beginColor(.yellow);
const end_color = rainbow.endColor();

const prompt = "> ";

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
    command_cursor: usize = 0,
    is_continuing: bool = false,
    breakpoints: std.ArrayList(Breakpoint) = .empty,
    skip_stdio: bool = true,
    skip_threads: std.AutoArrayHashMapUnmanaged(ThreadId, void) = .empty,
    trace_filter: ?TraceFilter = null,
    info_window_layout: ?InfoLayout = .initial,
    pending_input: std.ArrayList(u8) = .empty,
    reusable_allocating_writer: std.Io.Writer.Allocating,

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
            .reusable_allocating_writer = .init(allocator),
        };
    }

    pub fn step(self: *IRDebugger) Error!RunningEvent {
        try self.printInstruction(self.stdoutWriter());
        var event = try self.evaluator.step() orelse return .quit;

        if (event == .exit) {
            try self.print("Program exited with code: {f}", .{event.exit});
            return .quit;
        }

        const stdio_ids: []const usize = &.{ 1, 2, 3 };

        while (true) {
            const is_stdio_and_skipped = self.skip_stdio and std.mem.indexOfScalar(usize, stdio_ids, (self.evaluator.context.getCurrentThread() orelse unreachable).id) != null;
            const is_thread_skipped = std.mem.indexOfScalar(usize, self.skip_threads.keys(), (self.evaluator.context.getCurrentThread() orelse unreachable).id) != null;

            if (is_stdio_and_skipped or is_thread_skipped) {
                try self.printInstruction(self.stdoutWriter());
                _ = try self.evaluator.step() orelse return .quit;
                continue;
            }

            if (event != .skip) break;

            event = try self.evaluator.step() orelse return .quit;

            if (event != .skip) break;
        }

        switch (event) {
            .cont, .cont_no_instr_counter_inc, .skip => return .cont,
            .exit => unreachable,
        }
    }

    pub fn cont(self: *IRDebugger) Error!RunningEvent {
        signals.trap(std.posix.SIG.INT);
        self.is_continuing = true;
        return .cont;
    }

    fn printInstruction(self: *IRDebugger, writer: *std.Io.Writer) !void {
        const thread = self.evaluator.context.getCurrentThread() orelse return;
        const ic = thread.getCurrentInstructionAddr();
        const instruction = thread.currentInstruction() orelse {
            return writer.print("{}:{f}: <end>\n", .{ thread.id, ic });
        };
        const maybe_span = instruction.span();
        if (maybe_span) |span| {
            try self.logEvaluateSpan(writer, span);
        }
        try writer.print("{}:{f}: {f}\n", .{ thread.id, ic, instruction });
    }

    pub fn run(self: *IRDebugger) Error!ExitCode {
        try self.printInfoWindow();

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
        try self.stdoutWriter().print(fmt, args);
        try self.stdoutWriter().flush();
    }

    fn writeAll(self: *IRDebugger, string: []const u8) !void {
        try self.stdoutWriter().writeAll(string);
        try self.stdoutWriter().flush();
    }

    const command_delimiters: []const []const u8 = &.{"\n"};

    fn loop(self: *IRDebugger) Error!RunningEvent {
        // try self.printDebugCommandStuff();
        const event = try self.readCommandInput();

        try self.printInfoWindow();

        return event;
    }

    fn readCommandInput(self: *IRDebugger) Error!RunningEvent {
        try self.updateCommand("");

        if (self.is_continuing) {
            if (signals.consume(std.posix.SIG.INT)) {
                try self.writeAll("\nReceived interrupt signal.\n\n");
                signals.untrap(std.posix.SIG.INT);
                self.is_continuing = false;
                return .cont;
            }
            const result = try self.step();
            const thread = self.evaluator.context.getCurrentThread();
            if (thread) |current_thread| {
                if (self.hasBreakpoint(current_thread.getCurrentInstructionAddr())) |bp| {
                    self.is_continuing = false;
                    try self.print("\nStopped at breakpoint: {f}\n\n", .{bp});
                }
            }
            return result;
        } else {
            const stdin_reader = self.stdinReader();

            var stdin_allocating_writer = AllocatingWriter.init(self.allocator);
            defer stdin_allocating_writer.deinit();
            const stdin_writer = stdin_allocating_writer.get();

            if (self.pending_input.items.len > 0) {
                try stdin_writer.writer.writeAll(self.pending_input.items);
                self.pending_input.clearRetainingCapacity();
            }

            _ = try stdin_reader.stream(&stdin_writer.writer, .unlimited);

            switch (try self.processStdin(stdin_writer)) {
                .quit => return .quit,
                .cont => {},
            }
            try self.updateCommand(stdin_writer.written());

            return .cont;
        }
    }

    fn updateCommand(self: *IRDebugger, stdin_slice: []const u8) !void {
        if (stdin_slice.len == 0) {
            return self.printPrompt();
        }
        // const pos_before = try self.getCursorPosition();
        // const end_index = pos_before.col - 1;
        const end_index = self.command_cursor;
        const after_slice = try self.allocator.dupe(
            u8,
            self.commandWriter().buffered()[end_index..],
        );
        // try self.printToPos(pos_before.add(2, 0), "\rafter_slice: \"{s}\"", .{after_slice});
        self.commandWriter().end = end_index;
        try self.commandWriter().writeAll(stdin_slice);
        try self.commandWriter().writeAll(after_slice);
        self.command_cursor += stdin_slice.len;
        try self.printPrompt();
        // try self.writeAll(stdin_slice);
        // const pos_after = try self.getCursorPosition();
        // try self.writeAll(after_slice);
        // try self.commandWriter().writeAll(stdin_slice);
        // try self.commandWriter().writeAll(after_slice);
        // try self.setCursorPosition(pos_after);
    }

    fn printPrompt(self: *IRDebugger) Error!void {
        const size = try self.getTerminalSize();
        try self.setCursorPosition(.init(size.row, 1));
        try self.writeAll(clear_line);
        try self.writeAll(prompt);
        try self.writeAll(self.commandWriter().buffered());
        try self.setCursorPosition(.init(size.row, self.command_cursor + 1 + prompt.len));
    }

    fn addBreakpoint(
        self: *IRDebugger,
        file: []const u8,
        line: usize,
        instr_addr: ir.ResolvedInstructionAddr,
    ) !void {
        if (self.hasBreakpoint(instr_addr)) |_| return;
        try self.breakpoints.append(self.allocator, .{
            .file = file,
            .line = line,
            .instr_addr = instr_addr,
        });
    }

    fn removeBreakpoint(self: *IRDebugger, instr_addr: ir.ResolvedInstructionAddr) !void {
        for (self.breakpoints.items, 0..) |bp, i| {
            if (bp.instr_addr.equals(instr_addr)) {
                _ = self.breakpoints.swapRemove(i);
                return;
            }
        }
    }

    fn hasBreakpoint(
        self: *IRDebugger,
        instr_addr: ir.ResolvedInstructionAddr,
    ) ?Breakpoint {
        for (self.breakpoints.items) |bp| if (bp.instr_addr.equals(instr_addr)) return bp;
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
        write: []const u8,
        delete: usize,
        write_and_delete: []const u8,
        horizontal_move: isize,
        horizontal_move_and_delete: isize,
        run_command,
        autocomplete,
        undo_word,
        undo_line,
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
            return .{ .horizontal_move_and_delete = -1 };
        }

        fn delete(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .{ .delete = 1 };
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

        fn tab(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .autocomplete;
        }

        fn c_w(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .undo_word;
        }

        fn c_u(_: *IRDebugger, _: []const u8) StdinHandlerEvent {
            return .undo_line;
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
        .{
            .matcher = .{ .exact = &.{9} },
            .handler = StdinHandler.tab,
        },
        .{
            .matcher = .{ .exact = &.{23} },
            .handler = StdinHandler.c_w,
        },
        .{
            .matcher = .{ .exact = &.{21} },
            .handler = StdinHandler.c_u,
        },
        // { 27, 91, 53, 126 } page up
        // { 27, 91, 54, 126 } page down
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
                self.command_cursor = self.commandWriter().end;
                try self.updateCommand("");
                // try self.print("{s}\r{f}", .{ clear_line, replace_all });
            },
            .write => |text| {
                try self.updateCommand(before_slice);
                // try self.commandWriter().writeAll(text);
                try self.writeAll(text);
            },
            .delete => |n| {
                try self.updateCommand(before_slice);

                // const pos = try self.getCursorPosition();

                // if (pos.col > self.commandWriter().buffered().len) return .cont;
                if (self.command_cursor >= self.commandWriter().buffered().len) return .cont;

                var fallback_allocator = std.heap.stackFallback(128, self.allocator);
                const allocator = fallback_allocator.get();
                const after_start = @min(
                    self.commandWriter().buffered().len,
                    self.command_cursor + n,
                );
                const after_slice = try allocator.dupe(
                    u8,
                    self.commandWriter().buffered()[after_start..],
                );
                defer allocator.free(after_slice);
                self.commandWriter().undo(after_slice.len + n);
                try self.commandWriter().writeAll(after_slice);
                try self.updateCommand("");
                // try self.print("{s}{s}", .{ clear_line_to_end, after_slice });
                // try self.setCursorPosition(.init(0, self.command_cursor + 1));
            },
            .write_and_delete => |text| {
                try self.updateCommand(before_slice);

                _ = try self.handleStdinHandlerEvent(.{ .write = text }, "");
                _ = try self.handleStdinHandlerEvent(.{ .delete = 1 }, "");
            },
            .horizontal_move => |delta| {
                try self.updateCommand(before_slice);
                if (delta < 0) {
                    self.command_cursor -|= @intCast(-delta);
                } else {
                    self.command_cursor +|= @intCast(delta);
                }
                self.command_cursor = @min(
                    self.command_cursor,
                    self.commandWriter().buffered().len,
                );
                try self.updateCommand("");

                // const pos = try self.getCursorPosition();
                // var npos = pos.add(0, delta);
                // npos.col = @min(npos.col, self.commandWriter().buffered().len + 1);
                //
                // try self.setCursorPosition(npos);
            },
            .horizontal_move_and_delete => |delta| {
                try self.updateCommand(before_slice);

                _ = try self.handleStdinHandlerEvent(.{ .horizontal_move = delta }, "");
                _ = try self.handleStdinHandlerEvent(.{ .delete = 1 }, "");
            },
            .run_command => {
                try self.updateCommand(before_slice);

                const command_string = self.commandWriter().buffered();
                const command = try self.parseCommand(command_string) orelse return .cont;

                _ = self.commandWriter().consumeAll();
                self.command_cursor = 0;
                try self.updateCommand("");
                try self.print("{f}", .{command});

                return self.handleCommand(command);
            },
            .autocomplete => {
                try self.updateCommand(before_slice);

                const command_before_cursor = self.commandWriter().buffered()[0..self.command_cursor];
                var result = autocomplete(command_before_cursor);
                switch (result) {
                    .none => {},
                    .complete => |s| try self.updateCommand(s),
                    .matches => |*ms| {
                        try self.writeAll("\n");
                        while (ms.next()) |m| try self.print("{s}\n", .{m});
                        try self.writeAll("\n");
                    },
                }
            },
            .undo_word => {
                try self.updateCommand(before_slice);

                const Mode = enum { found_word, finding_word };
                var mode = Mode.finding_word;

                var cursor = self.command_cursor -| 1;
                while (cursor > 0) : (cursor -= 1) {
                    const ch = self.commandWriter().buffered()[cursor];
                    const is_whitespace = std.ascii.isWhitespace(ch);

                    switch (mode) {
                        .finding_word => if (is_whitespace) continue else {
                            mode = .found_word;
                            continue;
                        },
                        .found_word => if (!is_whitespace) continue else {
                            cursor += 1;
                            break;
                        },
                    }
                }

                if (mode == .found_word) {
                    const end = self.command_cursor;
                    const start = cursor;
                    self.command_cursor = start;
                    _ = try self.handleStdinHandlerEvent(.{ .delete = end - start }, "");
                }
            },
            .undo_line => {
                try self.updateCommand(before_slice);

                const n = self.command_cursor;
                self.command_cursor = 0;
                _ = try self.handleStdinHandlerEvent(.{ .delete = n }, "");
            },
            .none => {
                try self.updateCommand(before_slice);
            },
        }

        return .cont;
    }

    fn handleCommand(self: *IRDebugger, command: Command) Error!RunningEvent {
        if (self.command_history.getLastOrNull()) |prev_command| {
            if (!command.equals(prev_command)) {
                try self.command_history.append(self.allocator, command);
            }
        } else {
            try self.command_history.append(self.allocator, command);
        }
        self.command_history_cursor = null;

        try self.writeAll("\n");

        switch (command) {
            .quit => return .quit,
            .step => return self.step(),
            .cont => return self.cont(),
            .breakpoint => |bp| return self.handleBreakpointCommand(bp),
            .threads => return self.printThreads(),
            .instructions => return self.printInstructions(),
            .instructions_all => return self.printInstructionsAll(),
            .registers => return self.printRegisters(),
            .pipes => return self.printPipes(),
            .processes => return self.printProcesses(),
            .pipe_data => |handle| return self.printPipeData(handle),
            .skip_stdio => return self.skipStdio(),
            .skip_thread => |handle| return self.skipThread(handle),
            .skip_thread_list => return self.skipThreadList(),
            .trace => |trace| return self.handleTraceCommand(trace),
            .stack => return self.printStack(),
            .heap => |heap| return self.printHeap(heap.addr, heap.len),
            .toggle_info_window => return self.toggleInfoWindow(),
        }
    }

    fn handleBreakpointCommand(
        self: *IRDebugger,
        command: Command.BreakpointCommand,
    ) Error!RunningEvent {
        switch (command) {
            .add => |bp| {
                const instr_addr = self.fileAndLineToInstrAddr(bp.file, bp.line) orelse {
                    try self.print("\nDid not find location to add breakpoint at {s}:{}.\n\n", .{
                        bp.file,
                        bp.line,
                    });
                    return .cont;
                };
                try self.addBreakpoint(bp.file, bp.line, instr_addr);
                return .cont;
            },
            .add_ir => |bp| {
                try self.addBreakpoint("<ir-breakpoint>", 1, bp.instr_addr);
                return .cont;
            },
            .remove => |bp| {
                const instr_addr = self.fileAndLineToInstrAddr(bp.file, bp.line) orelse {
                    try self.print("\nDid not find location to add breakpoint at {s}:{}.\n\n", .{
                        bp.file,
                        bp.line,
                    });
                    return .cont;
                };
                try self.removeBreakpoint(instr_addr);
                return .cont;
            },
            .list => {
                for (self.breakpoints.items) |bp| {
                    try self.print("\n{s}:{} {f}", .{ bp.file, bp.line, bp.instr_addr });
                }
                try self.writeAll("\n\n");
                return .cont;
            },
        }
    }

    fn fileAndLineToInstrAddr(
        self: *IRDebugger,
        file: []const u8,
        line: usize,
    ) ?ir.ResolvedInstructionAddr {
        for (self.evaluator.context.instructions(), 0..) |inst_set, i| {
            for (inst_set, 0..) |inst, j| {
                const span = inst.span() orelse continue;
                if (std.mem.endsWith(u8, span.start.file, file) and span.containsLine(line)) {
                    return .init(i, j);
                }
            }
        }

        return null;
    }

    fn printThreads(self: *IRDebugger) !RunningEvent {
        const thread_counter = self.evaluator.context.thread_counter;
        for (self.evaluator.context.threads.items, 0..) |thread, i| {
            if (thread_counter == i) {
                try self.print("\n{s}{}: {f}{s}", .{
                    current_instr_color,
                    thread.id,
                    thread.getCurrentInstructionAddr(),
                    end_color,
                });
            } else {
                try self.print("\n{}: {f}", .{ thread.id, thread.getCurrentInstructionAddr() });
            }

            if (!self.evaluator.context.isThreadActive(thread.id)) {
                try self.print(" ({s}inactive{s})", .{ inactive_color, end_color });
            }

            try self.print(": {?f}", .{thread.currentInstruction()});
        }
        try self.writeAll("\n\n");

        return .cont;
    }

    fn getCurrentExecutingThread(self: *IRDebugger) ?ir.context.IRThreadContext {
        if (self.evaluator.context.threads.items.len == 0) return null;
        const current_thread_i: ?usize = if (self.skip_stdio) if (self.evaluator.context.thread_counter >= 1 and self.evaluator.context.thread_counter <= 3) 4 else null else null;
        const thread_i = self.evaluator.context.getNextActiveThread(current_thread_i) orelse 0;
        return self.evaluator.context.threads.items[thread_i];
    }

    fn printInstructions(self: *IRDebugger) !RunningEvent {
        const thread = self.getCurrentExecutingThread() orelse unreachable;
        const instr_addr = thread.getCurrentInstructionAddr();
        const instr_set = self.evaluator.context.instructions()[instr_addr.instr_set];

        try self.print("\nthread: {}\n", .{thread.id});

        const start = instr_addr.local_addr -| 5;
        const end = @min(instr_addr.local_addr +| 5, instr_set.len);
        for (instr_set[start..end], start..) |instr, i| {
            const addr = ir.ResolvedInstructionAddr.init(instr_addr.instr_set, i);
            if (instr_addr.equals(addr)) {
                try self.print("\n{s}{f}: {f}{s}", .{
                    current_instr_color,
                    ir.ResolvedInstructionAddr.init(instr_addr.instr_set, i),
                    instr,
                    end_color,
                });
            } else {
                try self.print("\n{f}: {f}", .{
                    ir.ResolvedInstructionAddr.init(instr_addr.instr_set, i),
                    instr,
                });
            }
        }
        try self.writeAll("\n\n");

        return .cont;
    }

    fn printInstructionsAll(self: *IRDebugger) !RunningEvent {
        const thread = self.evaluator.context.getCurrentThread() orelse unreachable;
        const instr_addr = thread.getCurrentInstructionAddr();

        for (self.evaluator.context.instructions(), 0..) |instr_set, i| {
            try self.print("\nSet: {}\n", .{i});

            for (instr_set, 0..) |instr, j| {
                const addr = ir.ResolvedInstructionAddr.init(i, j);
                if (instr_addr.equals(addr)) {
                    try self.print("\n{s}{f}: {f}{s}", .{
                        current_instr_color,
                        addr,
                        instr,
                        end_color,
                    });
                } else {
                    try self.print("\n{f}: {f}", .{ addr, instr });
                }
            }
        }

        return .cont;
    }

    fn printRegisters(self: *IRDebugger) !RunningEvent {
        for (self.evaluator.context.threads.items) |thread| {
            try self.print("\n\nthread: {}\n", .{thread.id});
            try self.print("\nic: {f}", .{thread.private.instruction_counter});
            try self.print("\nsf: {}", .{thread.private.stack_frame});
            try self.print("\nsc: {}", .{thread.private.stack.items.len});
            try self.print("\nr: {f}", .{thread.private.result_register});
        }

        try self.writeAll("\n\n");

        return .cont;
    }

    fn printPipes(self: *IRDebugger) !RunningEvent {
        var it = self.evaluator.context.pipes.iterator();
        while (it.next()) |entry| {
            try self.print("\n{}: {f} {f} has_been_connected: {}", .{ entry.key_ptr.*, entry.value_ptr.*, entry.value_ptr.*.config, entry.value_ptr.*.has_been_connected });
        }
        try self.writeAll("\n\n");

        return .cont;
    }

    fn printProcesses(self: *IRDebugger) !RunningEvent {
        var it = self.evaluator.context.closeables.iterator();
        while (it.next()) |entry| {
            try self.print("\n{}: {s} is closed: {?f}", .{ entry.key_ptr.*, entry.value_ptr.*.getLabel(), entry.value_ptr.*.getResult() });
        }
        try self.writeAll("\n\n");

        return .cont;
    }

    fn printPipeData(self: *IRDebugger, handle: PipeHandle) !RunningEvent {
        const pipe = self.evaluator.context.pipes.get(handle) orelse {
            try self.print("\npipe {} does not exist\n\n", .{handle});
            return .cont;
        };
        try self.print("\ndata for pipe {}:\n{s}\n\n", .{ handle, pipe.buffer_writer.written() });

        return .cont;
    }

    fn printWrappedAt(
        self: *IRDebugger,
        pos: TermVector,
        width: usize,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        var fallback_allocator = std.heap.stackFallback(1024, self.allocator);
        const allocator = fallback_allocator.get();
        const tw = try textWrapper(
            allocator,
            self.stdinReader(),
            &self.pending_input,
            width,
            fmt,
            args,
        );
        defer tw.deinit(self.allocator);
        try self.printAt(pos, "{f}", .{tw});
    }

    fn printAt(
        self: *IRDebugger,
        position: TermVector,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        try self.setCursorPosition(position);
        try self.print(fmt, args);
    }

    const TextWrappingFormatter = struct {
        width: usize,
        s: []const u8,
        reader: *std.Io.Reader,
        allocator: Allocator,
        pending_input: ?*std.ArrayList(u8),

        pub fn deinit(self: @This(), allocator: Allocator) void {
            allocator.free(self.s);
        }

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            const pos: TermVector = getCursorPositionGen(
                writer,
                self.reader,
                self.allocator,
                self.pending_input,
            ) catch return;

            var it = std.mem.tokenizeScalar(u8, self.s, ' ');
            var i: usize = 0;
            var j: usize = 0;

            while (it.next()) |word| {
                if (word.len + i > self.width) {
                    j += 1;
                    i = 0;
                    try setCursorPositionGen(writer, pos.add(@intCast(j), 0));
                }

                try writer.writeAll(word);

                i += word.len;

                if (i + 1 <= self.width) {
                    try writer.writeByte(' ');
                    i += 1;
                }
            }
        }
    };

    fn textWrapper(
        allocator: Allocator,
        reader: *std.Io.Reader,
        pending_input: ?*std.ArrayList(u8),
        width: usize,
        comptime fmt: []const u8,
        args: anytype,
    ) !TextWrappingFormatter {
        const s = try std.fmt.allocPrint(allocator, fmt, args);
        return .{
            .s = s,
            .width = width,
            .reader = reader,
            .allocator = allocator,
            .pending_input = pending_input,
        };
    }

    fn skipStdio(self: *IRDebugger) !RunningEvent {
        self.skip_stdio = !self.skip_stdio;
        if (self.skip_stdio) {
            try self.writeAll("stdio skip is on");
        } else {
            try self.writeAll("stdio skip is off");
        }
        try self.writeAll("\n\n");
        return .cont;
    }

    fn skipThread(self: *IRDebugger, handle: ThreadId) !RunningEvent {
        const was_removed = self.skip_threads.swapRemove(handle);
        if (was_removed) {
            try self.print("thread {} is now not skipped anymore", .{handle});
        } else {
            try self.skip_threads.put(self.allocator, handle, {});
            try self.print("thread {} is now skipped", .{handle});
        }
        try self.writeAll("\n\n");
        return .cont;
    }

    fn skipThreadList(self: *IRDebugger) !RunningEvent {
        if (self.skip_threads.count() == 0) {
            try self.writeAll("no skipped threads");
        } else {
            try self.writeAll("skipped threads:\n");
            for (self.skip_threads.keys()) |handle| {
                try self.print("{}\n", .{handle});
            }
        }
        try self.writeAll("\n\n");
        return .cont;
    }

    fn handleTraceCommand(self: *IRDebugger, trace: Command.TraceCommand) !RunningEvent {
        switch (trace) {
            .on => self.evaluator.config.tracer.config.echo_to_stdout = true,
            .off => self.evaluator.config.tracer.config.echo_to_stdout = false,
            .filter => |filter| self.trace_filter = filter,
        }

        return .cont;
    }

    fn printStack(self: *IRDebugger) !RunningEvent {
        for (self.evaluator.context.threads.items) |thread| {
            try self.print("\nthread {}:\n", .{thread.id});

            for (thread.private.stack.items, 0..) |s, j| {
                try self.print("\n{}: {f}", .{ j, s });
            }
            try self.writeAll("\n");
        }
        try self.writeAll("\n");

        return .cont;
    }

    fn toggleInfoWindow(self: *IRDebugger) !RunningEvent {
        if (self.info_window_layout) |w| {
            self.info_window_layout = w.next();
        } else {
            self.info_window_layout = .initial;
        }

        if (self.info_window_layout == null) {
            const info_window = try self.getInfoWindow();
            try info_window.clear(self);
        }

        return .cont;
    }

    fn clearArea(self: *IRDebugger, pos: TermVector, area: TermVector) !void {
        for (pos.row..pos.row + area.row) |row| {
            try self.setCursorPosition(.{ .row = row, .col = pos.col });
            _ = try self.stdoutWriter().writeSplat(&.{" "}, area.col);
            try self.stdoutWriter().flush();
        }
    }

    fn drawVerticalBorder(self: *IRDebugger, pos: TermVector, height: usize) !void {
        for (pos.row..height) |i| {
            try self.setCursorPosition(.{ .row = i, .col = pos.col });
            try self.writeAll("|");
        }
    }

    const Window = struct {
        pos: TermVector,
        size: TermVector,
        render_cursor: ?TermVector = null,

        pub fn init(pos: TermVector, size: TermVector) @This() {
            return .{ .pos = pos, .size = size };
        }

        pub fn innerPos(self: Window) TermVector {
            return self.pos.add(0, 1);
        }

        pub fn innerSize(self: Window) TermVector {
            return self.size.add(0, -1);
        }

        pub fn clear(self: Window, ctx: *IRDebugger) !void {
            try ctx.clearArea(self.pos, self.size);
        }

        pub fn printLeftBorder(self: Window, ctx: *IRDebugger) !void {
            try ctx.drawVerticalBorder(self.pos, self.size.row);
        }

        pub fn text(
            self: *Window,
            ctx: *IRDebugger,
            comptime fmt: []const u8,
            args: anytype,
        ) !void {
            if (self.render_cursor == null) self.render_cursor = self.innerPos();
            try ctx.printWrappedAt(self.render_cursor.?, self.innerSize().col, fmt, args);
            self.render_cursor = try ctx.getCursorPosition();
            self.render_cursor.?.row += 1;
            self.render_cursor.?.col = self.innerPos().col;
        }

        pub fn modifyCursor(self: *Window, row: isize, col: isize) void {
            if (self.render_cursor) |*cursor| {
                cursor.* = cursor.add(row, col);
            } else {
                self.render_cursor = self.innerPos().add(row, col);
            }
        }
    };

    fn getInfoWindow(self: *IRDebugger) !Window {
        const size = try self.getTerminalSize();
        const info_window_width = 40;
        return Window{
            .pos = .{ .row = 1, .col = size.col -| info_window_width },
            .size = .{ .row = size.row, .col = info_window_width },
        };
    }

    fn printInfoWindow(self: *IRDebugger) !void {
        const layout = self.info_window_layout orelse return;
        const original_pos = try self.getCursorPosition();
        var info_window = try self.getInfoWindow();

        try info_window.clear(self);
        try info_window.printLeftBorder(self);

        try switch (layout) {
            .threads => self.printInfoThreads(&info_window),
            .current_thread => if (self.getCurrentExecutingThread()) |curr_thread| {
                try self.printInfoThread(&info_window, curr_thread);
            },
        };

        try self.setCursorPosition(original_pos);
    }

    fn printThreadInfoHeader(
        self: *IRDebugger,
        window: *Window,
        thread: ir.context.IRThreadContext,
    ) !void {
        const curr_thread = self.getCurrentExecutingThread() orelse return;
        const is_active = self.evaluator.context.isThreadActive(thread.id);
        var thread_suffix_buffer: [1024]u8 = undefined;
        const thread_suffix = if (thread.private.waiting_for) |waiting_for| std.fmt.bufPrint(&thread_suffix_buffer, " (waiting for {})", .{waiting_for}) catch unreachable else if (is_active) "" else " (inactive)";

        try window.text(
            self,
            "thread {}{s}{s}",
            .{
                thread.id,
                if (thread.id == curr_thread.id) " (executing)" else "",
                thread_suffix,
            },
        );
        try window.text(
            self,
            "%ic:{f} %sf:{} %sc:{} %r:{f} %r2:{f}",
            .{
                thread.private.instruction_counter,
                thread.private.stack_frame,
                thread.private.stack.items.len,
                thread.private.result_register,
                thread.private.result_register_2,
            },
        );
    }

    fn printThreadInfoCurrentInstruction(
        self: *IRDebugger,
        window: *Window,
        thread: ir.context.IRThreadContext,
        around: usize,
    ) !void {
        if (around == 0) {
            const instr = thread.currentInstruction();
            return window.text(self, "{?f}", .{instr});
        }

        const curr_addr = thread.getCurrentInstructionAddr();
        const instr_set = self.evaluator.context.instructions()[curr_addr.instr_set];
        const start = curr_addr.local_addr -| around;
        const end = @min(instr_set.len, curr_addr.local_addr +| around + 1);

        var addr = ir.ResolvedInstructionAddr.init(curr_addr.instr_set, start);
        for (start..end) |i| {
            const instr = instr_set[i];
            const prefix = if (i == curr_addr.local_addr) ">" else "|";
            try window.text(self, "{s} {f} {f}", .{ prefix, addr, instr });
            addr.inc();
        }
    }

    fn printThreadInfoStack(
        self: *IRDebugger,
        window: *Window,
        thread: ir.context.IRThreadContext,
    ) !void {
        try window.text(self, "stack:", .{});
        for (thread.private.stack.items, 0..) |s, i| {
            try window.text(self, "{}: {f}", .{ i, s });
        }
    }

    fn printInfoThreads(
        self: *IRDebugger,
        window: *Window,
    ) !void {
        for (self.evaluator.context.threads.items) |thread| {
            if (self.skip_stdio and thread.id >= 1 and thread.id <= 3) continue;
            try self.printThreadInfoHeader(window, thread);
            try self.printThreadInfoCurrentInstruction(window, thread, 0);
            window.modifyCursor(1, 0);
        }
    }

    fn printInfoThread(
        self: *IRDebugger,
        window: *Window,
        thread: ir.context.IRThreadContext,
    ) !void {
        try self.printThreadInfoHeader(window, thread);
        window.modifyCursor(1, 0);
        try self.printThreadInfoCurrentInstruction(window, thread, 2);
        window.modifyCursor(1, 0);
        try self.printThreadInfoStack(window, thread);
    }

    fn printHeap(
        self: *IRDebugger,
        addr: usize,
        len: usize,
    ) !RunningEvent {
        try self.print("heap at 0x{x}+{}:\n", .{ addr, len });
        for (0..len) |i| {
            const a = i + addr;
            const value = self.evaluator.context.shared.heap.get(a);
            try self.print("\n0x{x}: {?f}", .{ a, value });
        }
        try self.writeAll("\n\n");
        return .cont;
    }

    const TermVector = struct {
        row: usize,
        col: usize,

        pub fn init(row: usize, col: usize) @This() {
            return .{ .row = row, .col = col };
        }

        pub fn add(self: @This(), row: isize, col: isize) @This() {
            return .{
                .row = @intCast(@max(1, @as(isize, @intCast(self.row)) +| row)),
                .col = @intCast(@max(1, @as(isize, @intCast(self.col)) +| col)),
            };
        }
    };

    fn setCursorPosition(
        self: *IRDebugger,
        position: TermVector,
    ) std.Io.Writer.Error!void {
        return setCursorPositionGen(self.stdoutWriter(), position);
    }

    fn setCursorPositionGen(
        writer: *std.Io.Writer,
        position: TermVector,
    ) std.Io.Writer.Error!void {
        try writer.print(csi ++ "{};{}H", .{ position.row, position.col });
        try writer.flush();
    }

    fn setCursorColumnGen(
        writer: *std.Io.Writer,
        col: usize,
    ) std.Io.Writer.Error!void {
        try writer.print(csi ++ ";{}H", .{col});
        try writer.flush();
    }

    fn getCursorPosition(
        self: *IRDebugger,
    ) Error!TermVector {
        return getCursorPositionGen(
            self.stdoutWriter(),
            self.stdinReader(),
            self.allocator,
            &self.pending_input,
        );
    }

    fn getCursorPositionGen(
        writer: *std.Io.Writer,
        reader: *std.Io.Reader,
        allocator: Allocator,
        pending_input: ?*std.ArrayList(u8),
    ) Error!TermVector {
        // response: ESC[n;mR
        // { 27, 91, 49, 59, 51, 82 }
        try writer.writeAll(request_position);
        try writer.flush();

        var scratch: [32]u8 = undefined;
        var scratch_len: usize = 0;

        var row_buf: [16]u8 = undefined;
        var row_len: usize = 0;
        var col_buf: [16]u8 = undefined;
        var col_len: usize = 0;

        const State = enum { seeking_esc, seeking_bracket, row, col };
        var state: State = .seeking_esc;

        while (true) {
            const byte = try reader.takeByte();
            switch (state) {
                .seeking_esc => {
                    if (byte == 0x1B) {
                        scratch_len = 0;
                        scratch[scratch_len] = byte;
                        scratch_len += 1;
                        state = .seeking_bracket;
                    } else if (pending_input) |pending| {
                        try pending.append(allocator, byte);
                    }
                },
                .seeking_bracket => {
                    scratch[scratch_len] = byte;
                    scratch_len += 1;
                    if (byte == '[') {
                        state = .row;
                        row_len = 0;
                        col_len = 0;
                    } else {
                        if (pending_input) |pending| {
                            try pending.appendSlice(allocator, scratch[0..scratch_len]);
                        }
                        scratch_len = 0;
                        state = .seeking_esc;
                    }
                },
                .row => {
                    scratch[scratch_len] = byte;
                    scratch_len += 1;
                    if (std.ascii.isDigit(byte)) {
                        if (row_len < row_buf.len) {
                            row_buf[row_len] = byte;
                            row_len += 1;
                        }
                        continue;
                    }
                    if (byte == ';' and row_len > 0) {
                        state = .col;
                        continue;
                    }
                    if (pending_input) |pending| {
                        try pending.appendSlice(allocator, scratch[0..scratch_len]);
                    }
                    scratch_len = 0;
                    state = .seeking_esc;
                },
                .col => {
                    scratch[scratch_len] = byte;
                    scratch_len += 1;
                    if (std.ascii.isDigit(byte)) {
                        if (col_len < col_buf.len) {
                            col_buf[col_len] = byte;
                            col_len += 1;
                        }
                        continue;
                    }
                    if (byte == 'R' and row_len > 0 and col_len > 0) {
                        const row = try std.fmt.parseUnsigned(usize, row_buf[0..row_len], 10);
                        const col = try std.fmt.parseUnsigned(usize, col_buf[0..col_len], 10);
                        return .{ .row = row, .col = col };
                    }
                    if (pending_input) |pending| {
                        try pending.appendSlice(allocator, scratch[0..scratch_len]);
                    }
                    scratch_len = 0;
                    state = .seeking_esc;
                },
            }
        }
    }

    fn getTerminalSize(self: *IRDebugger) Error!TermVector {
        const orig_pos = try self.getCursorPosition();
        try self.setCursorPosition(.init(9999, 9999));
        const size = try self.getCursorPosition();
        try self.setCursorPosition(orig_pos);

        return size;
    }

    fn printToPos(
        self: *IRDebugger,
        pos: TermVector,
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

    fn parseCommand(self: *IRDebugger, source: []const u8) !?Command {
        if (std.mem.eql(u8, source, "quit")) {
            return .quit;
        }

        if (std.mem.eql(u8, source, "step")) {
            return .step;
        }

        if (std.mem.eql(u8, source, "continue")) {
            return .cont;
        }

        if (std.mem.eql(u8, source, "threads")) {
            return .threads;
        }

        if (std.mem.eql(u8, source, "instructions")) {
            return .instructions;
        }

        if (std.mem.eql(u8, source, "instructions all")) {
            return .instructions_all;
        }

        if (std.mem.eql(u8, source, "registers")) {
            return .registers;
        }

        if (std.mem.eql(u8, source, "pipes")) {
            return .pipes;
        }

        if (std.mem.eql(u8, source, "processes")) {
            return .processes;
        }

        if (std.mem.startsWith(u8, source, "pipe data ")) {
            var it = std.mem.tokenizeAny(u8, source[10..], " :");
            const handle_as_string = it.next() orelse return self.commandUsage(.pipe_data);
            const handle = std.fmt.parseInt(PipeHandle, handle_as_string, 10) catch return self.commandUsage(.pipe_data);
            return .{ .pipe_data = handle };
        }

        if (std.mem.eql(u8, source, "stdio skip")) {
            return .skip_stdio;
        }

        if (std.mem.eql(u8, source, "skip thread")) {
            return .skip_thread_list;
        }

        if (std.mem.startsWith(u8, source, "skip thread ")) {
            var it = std.mem.tokenizeAny(u8, source[12..], " :");
            const handle_as_string = it.next() orelse return self.commandUsage(.skip_thread);
            const handle = std.fmt.parseInt(ThreadId, handle_as_string, 10) catch return self.commandUsage(.skip_thread);
            return .{ .skip_thread = handle };
        }

        if (std.mem.eql(u8, source, "stack")) {
            return .stack;
        }

        if (std.mem.eql(u8, source, "info")) {
            return .toggle_info_window;
        }

        if (std.mem.startsWith(u8, source, "breakpoint ")) {
            var it = std.mem.tokenizeAny(u8, source, " :");
            _ = it.next();
            const sub_command = it.next() orelse return self.commandUsage(.breakpoint);

            if (std.mem.eql(u8, sub_command, "list")) {
                return .{ .breakpoint = .list };
            }

            const valid_sub_commands: []const []const u8 = &.{ "add", "add-ir", "remove" };
            for (valid_sub_commands) |sc| {
                if (std.mem.eql(u8, sub_command, sc)) break;
            } else return self.commandUsage(.breakpoint);

            if (std.mem.eql(u8, sub_command, "add") or std.mem.eql(u8, sub_command, "remove")) {
                const file = it.next() orelse return self.commandUsage(.breakpoint);
                const line_as_string = it.next() orelse return self.commandUsage(.breakpoint);

                const line = std.fmt.parseInt(usize, line_as_string, 10) catch {
                    try self.print(
                        "\nError: line needs to be an integer, actual: {s}",
                        .{line_as_string},
                    );
                    return null;
                };

                if (std.mem.eql(u8, "add", sub_command)) {
                    return .{ .breakpoint = .{ .add = .{
                        .file = try self.allocator.dupe(u8, file),
                        .line = line,
                    } } };
                } else if (std.mem.eql(u8, "remove", sub_command)) {
                    return .{ .breakpoint = .{ .remove = .{
                        .file = try self.allocator.dupe(u8, file),
                        .line = line,
                    } } };
                }
            }

            if (std.mem.eql(u8, sub_command, "add-ir")) {
                const set_as_string = it.next() orelse return self.commandUsage(.breakpoint);
                const addr_as_string = it.next() orelse return self.commandUsage(.breakpoint);
                const set = std.fmt.parseInt(usize, set_as_string, 10) catch return self.commandUsage(.breakpoint);
                const addr = std.fmt.parseInt(usize, addr_as_string, 16) catch return self.commandUsage(.breakpoint);
                return .{ .breakpoint = .{ .add_ir = .{ .instr_addr = .init(set, addr) } } };
            }
        }

        if (std.mem.startsWith(u8, source, "trace ")) {
            var it = std.mem.tokenizeAny(u8, source, " ,");
            _ = it.next();
            const sub_command = it.next() orelse return self.commandUsage(.trace);

            if (std.mem.eql(u8, sub_command, "on")) {
                return .{ .trace = .on };
            }
            if (std.mem.eql(u8, sub_command, "off")) {
                return .{ .trace = .off };
            }

            const valid_sub_commands: []const []const u8 = &.{"filter"};
            for (valid_sub_commands) |sc| {
                if (std.mem.eql(u8, sub_command, sc)) break;
            } else return self.commandUsage(.trace);

            const filter_type = it.next() orelse return self.commandUsage(.trace);

            if (std.mem.eql(u8, filter_type, "any")) {
                if (it.next() != null) return self.commandUsage(.trace);
                return .{ .trace = .{ .filter = .all } };
            }

            var tags = std.ArrayList([]const u8).empty;
            while (it.next()) |tag| try tags.append(
                self.allocator,
                try self.allocator.dupe(u8, tag),
            );

            if (std.mem.eql(u8, "and", sub_command)) {
                return .{ .trace = .{ .filter = .{
                    .tags_and = try tags.toOwnedSlice(self.allocator),
                } } };
            } else if (std.mem.eql(u8, "or", sub_command)) {
                return .{ .trace = .{ .filter = .{
                    .tags_or = try tags.toOwnedSlice(self.allocator),
                } } };
            }

            return self.commandUsage(.trace);
        }

        if (std.mem.startsWith(u8, source, "heap ")) {
            var it = std.mem.tokenizeAny(u8, source, " ,");
            _ = it.next();
            const addr_s = it.next() orelse return self.commandUsage(.heap);
            const len_s = it.next() orelse return self.commandUsage(.heap);
            const addr = std.fmt.parseInt(usize, addr_s, 16) catch return self.commandUsage(.heap);
            const len = std.fmt.parseInt(usize, len_s, 10) catch return self.commandUsage(.heap);

            return .{ .heap = .{ .addr = addr, .len = len } };
        }

        try self.print("\nUnknown command \"{s}\".\n\n", .{source});

        return null;
    }

    fn commandUsage(self: *IRDebugger, command_type: std.meta.Tag(Command)) !?Command {
        try self.print("\n{s}\n\n", .{Command.usage(command_type)});
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

    fn logEvaluateSpan(
        self: *IRDebugger,
        writer: *std.Io.Writer,
        span: ast.Span,
    ) Error!void {
        if (span.isGlobal()) {
            try writer.print("{s}\n", .{span.start.file});
            return;
        }

        const source = try self.document_store.getSource(span.start.file);
        var lineIt = std.mem.splitScalar(u8, source, '\n');
        var i: usize = 0;
        while (lineIt.next()) |line| : (i += 1) {
            if (i >= span.start.line -| 3 and i <= span.end.line +| 3) {
                if (span.start.line == i + 1 and span.end.line == i + 1) {
                    try writer.print("{:>4}:{s}{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line == i + 1) {
                    try writer.print("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 ..],
                        end_color,
                    });
                } else if (span.end.line == i + 1) {
                    try writer.print("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line[0 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line - 1 <= i and i <= span.end.line - 1) {
                    try writer.print("{:>4}:{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line,
                        end_color,
                    });
                } else {
                    try writer.print("{:>4}:{s}\n", .{ i + 1, line });
                }
            }
        }
    }
};

const Command = union(enum) {
    quit,
    step,
    cont,
    breakpoint: BreakpointCommand,
    threads,
    instructions,
    instructions_all,
    registers,
    pipes,
    pipe_data: PipeDataCommand,
    processes,
    skip_stdio,
    skip_thread: SkipThreadCommand,
    skip_thread_list,
    trace: TraceCommand,
    stack,
    heap: HeapCommand,
    toggle_info_window,

    pub const BreakpointCommand = union(enum) {
        add: Add,
        add_ir: AddIr,
        remove: Remove,
        list,

        pub const Add = struct {
            file: []const u8,
            line: usize,
        };

        pub const AddIr = struct {
            instr_addr: ir.ResolvedInstructionAddr,
        };

        pub const Remove = struct {
            file: []const u8,
            line: usize,
        };

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            switch (self) {
                inline .add, .remove => |s, t| try writer.print("{t} {s}:{}", .{ t, s.file, s.line }),
                inline .add_ir => |s, t| try writer.print("{t} {f}", .{ t, s.instr_addr }),
                else => try writer.print("{t}", .{self}),
            }
        }
    };

    pub const PipeDataCommand = PipeHandle;

    pub const SkipThreadCommand = ThreadId;

    pub const TraceCommand = union(enum) {
        on,
        off,
        filter: TraceFilter,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            switch (self) {
                inline .filter => |s, t| try writer.print("{t} {f}", .{ t, s }),
                else => try writer.print("{t}", .{self}),
            }
        }
    };

    pub const HeapCommand = struct {
        addr: usize,
        len: usize,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("{} {}", .{ self.addr, self.len });
        }
    };

    pub fn equals(self: @This(), other: @This()) bool {
        return std.meta.activeTag(self) == std.meta.activeTag(other) and switch (self) {
            .breakpoint => |bp| std.meta.activeTag(bp) == std.meta.activeTag(other.breakpoint) and switch (bp) {
                inline .add, .remove => |s, t| {
                    const o = @field(other.breakpoint, @tagName(t));
                    if (!std.mem.eql(u8, s.file, o.file)) return false;
                    if (s.line != o.line) return false;
                    return true;
                },
                else => true,
            },
            else => true,
        };
    }

    pub fn usage(
        c_type: std.meta.Tag(Command),
    ) []const u8 {
        return switch (c_type) {
            .quit, .step, .cont, .instructions, .instructions_all, .registers, .threads, .pipes, .processes, .skip_stdio, .stack, .toggle_info_window, .skip_thread_list => "",
            .breakpoint =>
            \\breakpoint add <file>:<line>
            \\breakpoint add-ir <set>:<addr>
            \\breakpoint remove <file>:<line>
            ,
            .trace =>
            \\trace on
            \\trace off
            \\trace filter any
            \\trace filter and <tags>
            \\trace filter or <tags>
            ,
            .heap => "heap <addr> <len>",
            .skip_thread => "skip thread <handle>",
            .pipe_data => "pipe data <handle>",
        };
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .skip_stdio => try writer.writeAll("skip stdio"),
            .skip_thread => |handle| try writer.print("skip thread {}", .{handle}),
            .skip_thread_list => try writer.writeAll("skip thread"),
            .pipe_data => |handle| try writer.print("pipe data {}", .{handle}),
            .toggle_info_window => try writer.writeAll("info"),
            .instructions_all => try writer.writeAll("instructions all"),
            .heap => |heap| try writer.print("heap {x} {}", .{ heap.addr, heap.len }),
            inline .breakpoint, .trace => |s, t| try writer.print("{t} {f}", .{ t, s }),
            else => try writer.print("{t}", .{self}),
        }
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

pub const Breakpoint = struct {
    file: []const u8,
    line: usize,
    instr_addr: ir.ResolvedInstructionAddr,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("{s}:{} {f}", .{ self.file, self.line, self.instr_addr });
    }
};

const main_commands: []const []const u8 = &.{
    "quit",
    "step",
    "continue",
    "breakpoint",
    "threads",
    "instructions",
    "registers",
    "pipes",
    "pipe",
    "processes",
    "stdio",
    "skip",
    "trace",
    "stack",
    "info",
};

fn autocomplete(
    command: []const u8,
) AutocompleteEvent {
    var it = std.mem.tokenizeAny(u8, command, " ,:");

    const main_command = it.next() orelse return .{
        .matches = matchIterator(main_commands, ""),
    };
    const after_command = it.next();
    var main_command_matches = matchIterator(main_commands, main_command);

    if (after_command == null) {
        if (main_command_matches.count() == 1) {
            const match = main_command_matches.next().?;
            return .{ .complete = match[main_command.len..] };
        } else {
            return .{ .matches = main_command_matches };
        }
    }

    return .none;
}

const MatchIterator = struct {
    available: []const []const u8,
    s: []const u8,
    index: usize = 0,

    pub fn next(self: *@This()) ?[]const u8 {
        while (true) : (self.index += 1) {
            if (self.index >= self.available.len) return null;
            if (std.mem.startsWith(u8, self.available[self.index], self.s)) {
                defer self.index += 1;
                return self.available[self.index];
            }
        }
    }

    pub fn reset(self: *@This()) void {
        self.index = 0;
    }

    pub fn count(self: @This()) usize {
        var copy = self;
        copy.reset();
        var sum: usize = 0;
        while (copy.next()) |_| sum += 1;
        return sum;
    }
};

fn matchIterator(
    available: []const []const u8,
    s: []const u8,
) MatchIterator {
    return .{ .available = available, .s = s };
}

pub const AutocompleteEvent = union(enum) {
    complete: []const u8,
    matches: MatchIterator,
    none,
};

const InfoLayout = enum {
    threads,
    current_thread,
    pub const initial = .current_thread;

    pub fn next(self: @This()) ?InfoLayout {
        return switch (self) {
            .threads => null,
            .current_thread => .threads,
        };
    }
};
