const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir.zig");
const runic = @import("runic");
const CloseableProcessIo = runic.process.CloseableProcessIo;
const ReaderWriterStream = runic.stream.ReaderWriterStream;
const ExitCode = runic.command_runner.ExitCode;
const Stream = runic.stream.Stream;
const Tracer = runic.trace.Tracer;

pub const Error =
    Allocator.Error ||
    std.process.Child.SpawnError ||
    std.process.Child.WaitError ||
    std.Io.Reader.Error ||
    runic.stream.StreamError ||
    ir.Value.DeserializeError ||
    IREvaluator.MaterializeStringError ||
    ir.Location.Error ||
    error{
        UnsupportedInstruction,
        UnsupportedWaitee,
        UnsupportedStreamee,
        UnsupportedForward,
        SetImmutableLocation,
        SetInstructionLocation,
        RefNotFound,
        DuplicateRef,
        ContNoInstrCounterIncInAtomic,
    };

pub const Result = union(enum) {
    cont,
    cont_no_instr_counter_inc,
    exit: runic.command_runner.ExitCode,
};

pub const IREvaluator = struct {
    allocator: Allocator,
    config: Config,
    context: *ir.context.IRProgramContext,

    pub const Config = struct {
        verbose: bool,
        stdin: *ReaderWriterStream,
        stdout: *ReaderWriterStream,
        stderr: *ReaderWriterStream,
        tracer: *Tracer,
    };

    pub fn init(
        allocator: Allocator,
        config: Config,
        context: *ir.context.IRProgramContext,
    ) @This() {
        return .{
            .allocator = allocator,
            .config = config,
            .context = context,
        };
    }

    fn log(self: IREvaluator, comptime fmt: []const u8, args: anytype) void {
        if (!self.config.verbose) return;
        std.log.debug("[IREvaluator]: " ++ fmt, args);
    }

    fn logTrace(self: *IREvaluator, comptime label: []const u8) void {
        const thread = self.context.getCurrentThread();
        const instr_addr = thread.getCurrentInstructionAddr();
        const instr = thread.currentInstruction();
        self.log(label ++ ": t:{}, i:{f}: {?f}", .{ thread.id, instr_addr, instr });
    }

    pub fn step(self: *IREvaluator) Error!?Result {
        // self.logTrace(@src().fn_name);

        const thread = self.context.getCurrentThread();

        const instruction = thread.currentInstruction() orelse {
            self.log("{}: no more instructions", .{thread.id});
            try self.context.closeThread(thread.id, null);
            return self.advanceThreadCounter();
        };

        const result = try self.runInstruction(thread, instruction);

        switch (result) {
            .exit => |exit_code| try self.context.closeThread(thread.id, exit_code),
            .cont => thread.incInstructionCounter(),
            .cont_no_instr_counter_inc => {},
        }

        self.tempCloseStdIoCheck();

        return self.advanceThreadCounter();
    }

    fn tempCloseStdIoCheck(self: *IREvaluator) void {
        const is_main_thread_done = self.context.isThreadClosed(0);
        if (is_main_thread_done) {
            if (!self.context.isThreadClosed(1)) {
                self.context.closeThread(1, .success) catch unreachable;
            }

            if (self.context.getThreadContext(2)) |stdout_thread| {
                const stdout_value = stdout_thread.private.stack.items[1];
                const stdout_handle = stdout_value.pipe;
                const stdout_pipe = self.context.getPipe(stdout_handle);
                if (stdout_pipe.isSourcesClosed()) {
                    self.context.closeThread(2, .success) catch unreachable;
                } else {
                    self.context.closeThread(2, .success) catch unreachable;
                }
            }

            if (self.context.getThreadContext(3)) |stderr_thread| {
                const stderr_value = stderr_thread.private.stack.items[2];
                const stderr_handle = stderr_value.pipe;
                const stderr_pipe = self.context.getPipe(stderr_handle);
                if (stderr_pipe.isSourcesClosed()) {
                    self.context.closeThread(3, .success) catch unreachable;
                } else {
                    self.context.closeThread(3, .success) catch unreachable;
                }
            }
        }
    }

    fn advanceThreadCounter(self: *IREvaluator) Result {
        return switch (self.context.advanceThreadCounter()) {
            .cont => .cont,
            .quit => .{ .exit = self.context.getMainThreadExitCode() },
        };
    }

    pub const GetSliceError = error{UnsupportedSliceLocation};

    fn getSlice(self: IREvaluator, slice: ir.Value.Slice) GetSliceError![]const u8 {
        self.log("getSlice: {any}", .{slice});
        return switch (self.context.mapAddr(slice.addr)) {
            .data => |data| data.get(
                slice.len * slice.element_size,
                self.context.readonlyData(),
            ),
            else => GetSliceError.UnsupportedSliceLocation,
        };
    }

    fn resolveAddr(
        self: IREvaluator,
        thread: ir.context.IRThreadContext,
        addr: ir.InstructionAddr,
    ) ir.ResolvedInstructionAddr {
        const abs: usize = switch (addr.local_addr) {
            .abs => |abs| abs,
            .rel => |rel| @intCast(
                @as(isize, @intCast(thread.getCurrentInstructionAddr().local_addr)) + rel,
            ),
            .label => |label| self.context.labels().get(label).?,
        };

        return .init(addr.instr_set, abs);
    }

    pub const ResolveValueError = error{UnsupportedResolveValueType};

    fn resolveValue(
        self: IREvaluator,
        thread: ir.context.IRThreadContext,
        value: ir.Value,
    ) ResolveValueError!ir.Value {
        return switch (value) {
            .addr => |addr| switch (self.context.mapAddr(addr)) {
                .data, .scope => ResolveValueError.UnsupportedResolveValueType,
                .ref => |ref| thread.refs().get(ref.addr).?,
                .stack => |stack| thread.private.stack.items[stack],
                .instruction => value,
            },
            else => value,
        };
    }

    pub const ResolveLocationError = ResolveValueError || ir.Location.Error;

    fn resolveLocation(
        self: IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
    ) ResolveLocationError!ir.Value {
        return self.resolveValue(thread, .fromAddr(try location.toAddr()));
    }

    fn runInstruction(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        instruction: ir.Instruction,
    ) Error!Result {
        self.log("[{f}] {f}", .{ thread.private.instruction_counter, instruction });

        return switch (instruction.type) {
            .fwd_stdio => {
                const stdin = try self.context.addPipe(self.config.stdin);
                const stdout = try self.context.addPipe(self.config.stdout);
                const stderr = try self.context.addPipe(self.config.stderr);

                try thread.private.stack.append(self.allocator, .{ .pipe = stdin });
                try thread.private.stack.append(self.allocator, .{ .pipe = stdout });
                try thread.private.stack.append(self.allocator, .{ .pipe = stderr });

                return .cont;
            },
            .push => |push| {
                try thread.private.stack.append(self.allocator, push);
                return .cont;
            },
            .pop => {
                _ = thread.private.stack.pop();
                return .cont;
            },
            .exec => |exec| {
                // const argv_len = self.context.stack.pop().?.uinteger + 1;
                const context_loc = thread.private.stack.pop().?;
                const context = (try self.resolveValue(thread, context_loc)).strct;
                const argv_value = context.fields[0];
                const argv_value_slice = try self.getSlice(argv_value.slice);

                // TODO: memory management
                var argv = try std.ArrayList([]const u8).initCapacity(
                    self.allocator,
                    argv_value.slice.len + 2,
                );
                defer argv.deinit(self.allocator);

                // TODO: figure out how to do this ourselves
                argv.appendSliceAssumeCapacity(&.{ "stdbuf", "-oL" });

                const element_size = argv_value.slice.element_size;
                for (0..argv_value.slice.len) |i| {
                    const start = i * element_size;
                    const end = start + element_size;
                    const slice_as_bytes = argv_value_slice[start..end];
                    var reader = std.Io.Reader.fixed(slice_as_bytes);
                    const arg_stream = try ir.Value.deserialize(.stream, &reader);
                    var arg_writer = std.Io.Writer.Allocating.init(self.allocator);
                    try self.materializeString(thread, arg_stream, &arg_writer.writer);
                    argv.appendAssumeCapacity(try arg_writer.toOwnedSlice());
                }

                // for (0..argv_len) |_| {
                //     const arg = self.context.stack.pop().?;
                //     const slice = try self.getSlice(arg.slice);
                //     argv.appendAssumeCapacity(slice);
                // }

                const child = try self.allocator.create(std.process.Child);
                child.* = std.process.Child.init(try argv.toOwnedSlice(self.allocator), self.allocator);
                child.stdin_behavior = .Pipe;
                child.stdout_behavior = .Pipe;
                child.stderr_behavior = .Pipe;
                try child.spawn();
                thread.private.process = child;

                const stdin_handle = thread.private.stack.items[0].pipe;
                const stdout_handle = thread.private.stack.items[1].pipe;
                const stderr_handle = thread.private.stack.items[2].pipe;

                const stdin_pipe = self.context.getPipe(stdin_handle);
                const stdout_pipe = self.context.getPipe(stdout_handle);
                const stderr_pipe = self.context.getPipe(stderr_handle);

                // TODO: memory management
                const process_io = try self.allocator.create(CloseableProcessIo);
                process_io.* = .init(child, self.config.tracer);
                process_io.connect();

                try child.waitForSpawn();

                try stdin_pipe.connectDestination(process_io.closeableStdin());
                try stdout_pipe.connectSource(process_io.closeableStdout());
                try stderr_pipe.connectSource(process_io.closeableStderr());

                if (exec.result) |loc| {
                    const ref_ptr = thread.refs().getPtr(loc.ref.addr) orelse return Error.RefNotFound;
                    const handle = try self.context.addCloseable(process_io.closeable());
                    ref_ptr.* = .{ .closeable = handle };
                }

                return .cont;
            },
            .jmp => |jmp| {
                const dest = self.resolveAddr(thread, jmp.dest);
                const cond = jmp.cond orelse {
                    thread.setInstructionCounter(dest);
                    return .cont_no_instr_counter_inc;
                };

                const cond_value = try self.resolveValue(thread, cond);

                if (cond_value.exit_code.toBoolean() == jmp.jump_if) {
                    thread.setInstructionCounter(dest);
                    return .cont_no_instr_counter_inc;
                }

                return .cont;
            },
            .ref => |ref| {
                const entry = try thread.refs().getOrPut(self.allocator, ref.addr);

                if (entry.found_existing) {
                    return Error.DuplicateRef;
                }

                entry.value_ptr.* = .void;

                return .cont;
            },
            .set => |set| {
                return switch (set.location) {
                    .data => Error.SetImmutableLocation,
                    .scope => Error.UnsupportedInstruction,
                    .instruction => Error.SetInstructionLocation,
                    .ref => |ref| {
                        const ref_ptr = thread.refs().getPtr(ref.addr) orelse return Error.RefNotFound;
                        ref_ptr.* = set.value;

                        return .cont;
                    },
                    .stack => |stack| {
                        thread.private.stack.items[stack] = set.value;
                        return .cont;
                    },
                };
            },
            .pipe => |instr_pipe| {
                const pipe = try Stream(u8).initReaderWriter(self.allocator, "pipe", .{}, self.config.tracer);
                const pipe_handle = try self.context.addPipe(pipe);

                const ref_ptr = thread.refs().getPtr(instr_pipe.result.ref.addr) orelse return Error.RefNotFound;
                ref_ptr.* = .{ .pipe = pipe_handle };

                return .cont;
            },
            .pipe_opt => |pipe_opt| {
                const pipe_handle = (try self.resolveLocation(thread, pipe_opt.handle)).pipe;
                const pipe = self.context.getPipe(pipe_handle);
                const value = try self.resolveValue(thread, pipe_opt.value);

                switch (pipe_opt.option) {
                    inline else => |t| @field(
                        pipe.config,
                        @tagName(t),
                    ) = value.exit_code.toBoolean(),
                }

                return .cont;
            },
            .pipe_fwd => |pipe_fwd| {
                const source_handle = (try self.resolveLocation(thread, pipe_fwd.source)).pipe;
                const destination_handle = (try self.resolveLocation(thread, pipe_fwd.destination)).pipe;
                const source_pipe = self.context.getPipe(source_handle);
                const destination_pipe = self.context.getPipe(destination_handle);

                try source_pipe.connectDestination(destination_pipe.closeableWriter());

                return .cont;
            },
            .atomic => |instr_set| {
                const instructions = self.context.instructions()[instr_set];

                for (instructions) |instr| {
                    switch (try self.runInstruction(thread, instr)) {
                        .cont => continue,
                        .cont_no_instr_counter_inc => return Error.ContNoInstrCounterIncInAtomic,
                        .exit => |exit_code| return .{ .exit = exit_code },
                    }
                }

                return .cont;
            },
            .fork => |fork| {
                const new_thread_handle = try self.context.spawnThread();
                const new_thread = self.context.getThreadContext(new_thread_handle).?;

                new_thread.setInstructionCounter(self.resolveAddr(thread, fork.dest));

                if (fork.result) |loc| {
                    const ref_ptr = thread.refs().getPtr(loc.ref.addr) orelse return Error.RefNotFound;
                    ref_ptr.* = .{ .thread = new_thread_handle };
                }

                try new_thread.private.stack.append(
                    self.allocator,
                    try self.resolveLocation(thread, fork.stdin),
                );
                try new_thread.private.stack.append(
                    self.allocator,
                    try self.resolveLocation(thread, fork.stdout),
                );
                try new_thread.private.stack.append(
                    self.allocator,
                    try self.resolveLocation(thread, fork.stderr),
                );

                return .cont;
            },
            .wait => |wait| {
                const waitee = try self.resolveValue(thread, .fromAddr(try wait.waitee.toAddr()));

                switch (waitee) {
                    .thread => |thread_handle| {
                        thread.waitFor(thread_handle);
                    },
                    .closeable => |closeable_handle| {
                        const closeable = self.context.getCloseable(closeable_handle);
                        if (closeable.isClosed()) {
                            return .cont;
                        } else {
                            return .cont_no_instr_counter_inc;
                        }
                    },
                    else => return Error.UnsupportedWaitee,
                }

                return .cont;
            },
            .stream => |stream| {
                const streamee = try self.resolveValue(thread, .fromAddr(try stream.toAddr()));

                return switch (streamee) {
                    .pipe => |pipe_handle| switch (try self.context.getPipe(pipe_handle).forward(.unlimited)) {
                        .no_source => .cont_no_instr_counter_inc,
                        .closed => .cont,
                        .not_done => .cont_no_instr_counter_inc,
                    },
                    else => Error.UnsupportedStreamee,
                };
            },
            .exit => |exit_code| return .{ .exit = exit_code },
            else => Error.UnsupportedInstruction,
        };
    }

    pub const MaterializeStringError =
        Allocator.Error ||
        GetSliceError ||
        ResolveValueError ||
        std.Io.Writer.Error ||
        error{UnsupportedType};

    fn materializeString(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        value: ir.Value,
        w: *std.Io.Writer,
    ) MaterializeStringError!void {
        const resolvedValue = try self.resolveValue(thread, value);

        switch (resolvedValue) {
            .stream => |stream| for (stream) |s| try self.materializeString(thread, s, w),
            .slice => |slice| {
                if (slice.element_size != 1) return MaterializeStringError.UnsupportedType;
                const string = try self.getSlice(slice);
                try w.writeAll(string);
            },
            inline .uinteger => |t| try w.print("{}", .{t}),
            inline .addr => |t| try w.print("0x{x}", .{t}),
            inline .exit_code => |t| try w.print("{f}", .{t}),
            .void, .strct, .pipe, .thread, .closeable, .fn_ref => {},
        }
    }
};
