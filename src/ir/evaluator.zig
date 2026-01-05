const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir.zig");
const runic = @import("runic");
const CloseableReader = runic.closeable.CloseableReader;
const CloseableWriter = runic.closeable.CloseableWriter;
const CloseableProcessIo = runic.process.CloseableProcessIo;
const ExitCode = runic.command_runner.ExitCode;

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
        stdin: CloseableReader(ExitCode),
        stdout: CloseableWriter(ExitCode),
        stderr: CloseableWriter(ExitCode),
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

    fn log(_: IREvaluator, comptime fmt: []const u8, args: anytype) void {
        // if (!self.config.verbose) return;
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
                const stdout_pipe = stdout_value.pipe;
                if (stdout_pipe.source) |source| {
                    if (source.isClosed()) {
                        self.context.closeThread(2, .success) catch unreachable;
                    }
                }
            }

            if (self.context.getThreadContext(3)) |stderr_thread| {
                const stderr_value = stderr_thread.private.stack.items[2];
                const stderr_pipe = stderr_value.pipe;
                if (stderr_pipe.source) |source| {
                    if (source.isClosed()) {
                        self.context.closeThread(3, .success) catch unreachable;
                    }
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

    fn runInstruction(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        instruction: ir.Instruction,
    ) Error!Result {
        self.log("[{f}] {f}", .{ thread.private.instruction_counter, instruction });

        return switch (instruction.type) {
            .fwd_stdio => {
                const stdin_addr = ir.Location{ .stack = 0 };
                const stdout_addr = ir.Location{ .stack = 1 };
                const stderr_addr = ir.Location{ .stack = 2 };
                const stdin = try self.resolveValue(thread, .fromAddr(try stdin_addr.toAddr()));
                const stdout = try self.resolveValue(thread, .fromAddr(try stdout_addr.toAddr()));
                const stderr = try self.resolveValue(thread, .fromAddr(try stderr_addr.toAddr()));

                self.log("stdin: {t}", .{stdin});
                self.log("stdout: {t}", .{stdout});
                self.log("stderr: {t}", .{stderr});

                switch (stdin) {
                    .pipe => |pipe| try pipe.connectSource(self.config.stdin),
                    else => return Error.UnsupportedForward,
                }
                switch (stdout) {
                    .pipe => |pipe| try pipe.connectDestination(self.config.stdout),
                    else => return Error.UnsupportedForward,
                }
                switch (stderr) {
                    .pipe => |pipe| try pipe.connectDestination(self.config.stderr),
                    else => return Error.UnsupportedForward,
                }

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
                    argv_value.slice.len,
                );
                defer argv.deinit(self.allocator);

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

                const stdin_pipe = thread.private.stack.items[0].pipe;
                const stdout_pipe = thread.private.stack.items[1].pipe;
                const stderr_pipe = thread.private.stack.items[2].pipe;

                // TODO: memory management
                const process_io = try self.allocator.create(CloseableProcessIo);
                process_io.* = .init(child);
                process_io.connect();

                try stdin_pipe.connectDestination(process_io.closeableStdin());
                try stdout_pipe.connectSource(process_io.closeableStdout());
                try stderr_pipe.connectSource(process_io.closeableStderr());

                try child.waitForSpawn();

                // const child_thread_id = try self.context.spawnThread();
                // const child_thread = self.context.getThreadContext(child_thread_id).?;
                // child_thread.private.instruction_counter.local_addr = self.context.shared.instructions[child_thread.private.instruction_counter.instr_set].len;
                // child_thread.private.process = child;
                // thread.waitFor(child_thread_id);

                if (exec.result) |loc| {
                    const ref_ptr = thread.refs().getPtr(loc.ref.addr) orelse return Error.RefNotFound;
                    ref_ptr.* = .{ .closeable = process_io.closeable() };
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
                    try self.resolveValue(thread, .fromAddr(try fork.stdin.toAddr())),
                );
                try new_thread.private.stack.append(
                    self.allocator,
                    try self.resolveValue(thread, .fromAddr(try fork.stdout.toAddr())),
                );
                try new_thread.private.stack.append(
                    self.allocator,
                    try self.resolveValue(thread, .fromAddr(try fork.stderr.toAddr())),
                );

                return .cont;
            },
            .wait => |wait| {
                const waitee = try self.resolveValue(thread, .fromAddr(try wait.waitee.toAddr()));

                switch (waitee) {
                    .thread => |thread_handle| {
                        thread.waitFor(thread_handle);
                    },
                    .closeable => |closeable| {
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
                    .pipe => |pipe| switch (try pipe.forward(.unlimited)) {
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
            .void, .strct, .pipe, .thread, .closeable => {},
        }
    }
};
