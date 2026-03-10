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
        UnsupportedBinaryOperator,
        UnsupportedBinaryExpression,
        SetImmutableLocation,
        SetInstructionLocation,
        RefNotFound,
        DuplicateRef,
        ContNoInstrCounterIncInAtomic,
    };

pub const Result = union(enum) {
    /// Advances instruction counter and thread counter
    cont,
    /// Advances thread counter, retains instruction counter
    cont_no_instr_counter_inc,
    /// Advances instruction counter, retains thread counter (used for comments)
    skip,
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

        const thread = self.context.getCurrentThread() orelse unreachable;

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
            .skip => {
                thread.incInstructionCounter();
                return .skip;
            },
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
        if (slice.len == 0) return "";
        // TODO: support addr mod?
        return switch (self.context.mapAddr(slice.addr).abs) {
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

    pub const DereferenceValueError = ir.Location.Error || error{UnsupportedDereferenceValueType};

    fn dereferenceValue(
        self: IREvaluator,
        thread: ir.context.IRThreadContext,
        value: ir.Value,
        mod: ?ir.LocationMod,
    ) DereferenceValueError!*ir.Value {
        return switch (value) {
            .addr => |addr| {
                const loc = self.context.mapAddr(addr);
                return switch (loc.abs) {
                    .data, .register, .instruction, .ref, .closure => {
                        std.log.err("Could not dereference location of type {t}", .{loc.abs});
                        return DereferenceValueError.UnsupportedDereferenceValueType;
                    },
                    .stack => |stack| &thread.private.stack.items[ir.LocationMod.applyMaybe(mod, stack)],
                    .heap => |heap| thread.shared.heap.getPtr(ir.LocationMod.applyMaybe(mod, heap)).?,
                };
            },
            // .register => |reg| switch (reg.abs) {
            //     .ic => DereferenceValueError.UnsupportedDereferenceValueType,
            //     .sf => .{ .uinteger = reg.applyMod(thread.private.stack_frame) },
            //     .sc => .{ .uinteger = reg.applyMod(thread.private.stack.items.len) },
            //     .r => thread.private.result_register,
            // },
            // .dereference => |der| self.resolveLocation(thread, .init(
            //     .{ .register = der.register.abs },
            //     der.register.mod,
            // )),
            else => {
                std.log.err("Could not dereference value of type {t}", .{value});
                return DereferenceValueError.UnsupportedDereferenceValueType;
            },
        };
    }

    fn resolveValueSource(
        self: IREvaluator,
        thread: ir.context.IRThreadContext,
        value_source: ir.ValueSource,
    ) ResolveLocationError!ir.Value {
        return switch (value_source) {
            .location => |loc| self.resolveLocation(thread, loc),
            .value => |value| value,
        };
    }

    pub const ResolveLocationError = DereferenceValueError || ir.Location.Error;

    fn resolveLocation(
        self: IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
    ) ResolveLocationError!ir.Value {
        if (location.isNoll()) {
            return .fromAddr(0);
        } else if (location.abs == .closure) {
            const dereferenced = try self.dereferenceLocation(thread, location);
            if (location.options.dereference) {
                return (try self.dereferenceValue(thread, dereferenced.*, null)).*;
            } else {
                return dereferenced.*;
            }
        } else if (location.options.dereference) {
            return (try self.dereferenceLocation(thread, location)).*;
        } else {
            switch (location.abs) {
                .register => |r| switch (r) {
                    .r => return thread.private.result_register,
                    .r2 => return thread.private.result_register_2,
                    else => {},
                },
                // TODO: always deference closure (you're not supposed to change where closure entries point to)
                .closure => return self.resolveLocation(thread, location.dereference()),
                else => {},
            }

            return .fromAddr(try location.toAddrWithContext(
                thread.private.stack_frame,
                thread.private.stack.items[3].addr,
            ));
        }
    }

    fn dereferenceLocation(
        self: IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
    ) ResolveLocationError!*ir.Value {
        return switch (location.abs) {
            .ref => |ref| thread.getRefPtr(ref, location.mod),
            .register => |reg| switch (reg) {
                .ic => DereferenceValueError.UnsupportedDereferenceValueType,
                .sf => self.dereferenceValue(thread, .fromAddr(location.applyMod(thread.private.stack_frame)), null),
                .sc => self.dereferenceValue(thread, .fromAddr(location.applyMod(thread.private.stack.items.len)), null),
                .r => self.dereferenceValue(thread, thread.private.result_register, location.mod),
                .r2 => self.dereferenceValue(thread, thread.private.result_register_2, location.mod),
                // .r => switch (thread.private.result_register) {
                //     .dereference => |deref| .{ .dereference = deref.applyMod(
                //         location.mod,
                //     ) },
                //     .register => |r| .{ .register = .{
                //         .abs = r.abs,
                //         .mod = ir.LocationMod.merge(
                //             location.mod,
                //             r.mod,
                //         ),
                //     } },
                //     else => thread.private.result_register,
                // },
            },
            else => self.dereferenceValue(thread, .fromAddr(try location.toAddrWithContext(
                thread.private.stack_frame,
                thread.private.stack.items[3].addr,
            )), null),
        };
    }

    fn runInstruction(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        instruction: ir.Instruction,
    ) Error!Result {
        self.log("[{f}] {f}", .{ thread.private.instruction_counter, instruction });

        return switch (instruction.type) {
            .comment => return .skip,
            .fwd_stdio => {
                const stdin = try self.context.addPipe(self.config.stdin);
                const stdout = try self.context.addPipe(self.config.stdout);
                const stderr = try self.context.addPipe(self.config.stderr);

                try thread.private.stack.append(self.allocator, .{ .pipe = stdin });
                try thread.private.stack.append(self.allocator, .{ .pipe = stdout });
                try thread.private.stack.append(self.allocator, .{ .pipe = stderr });
                try thread.private.stack.append(self.allocator, .{ .addr = 0 });

                return .cont;
            },
            .push => |push| {
                try thread.private.stack.append(
                    self.allocator,
                    try self.resolveValueSource(thread, push),
                );
                return .cont;
            },
            .pop => {
                const value = thread.private.stack.pop().?;
                thread.private.result_register = value;
                return .cont;
            },
            .inc => {
                thread.private.result_register_2 = evaluateArithmetic(.add, .fromValue(thread.private.result_register_2), .fromValue(.{ .uinteger = 1 })).?;
                return .cont;
            },
            .dec => {
                thread.private.result_register_2 = evaluateArithmetic(.sub, .fromValue(thread.private.result_register_2), .fromValue(.{ .uinteger = 1 })).?;
                return .cont;
            },
            .exec => |exec| {
                _ = exec;

                const argv_len = thread.private.stack.pop().?.uinteger + 1;
                // const context_loc = thread.private.stack.pop().?;
                // const context = (try self.resolveValue(thread, context_loc)).strct;
                // const argv_value = context.fields[0];
                // const argv_value_slice = try self.getSlice(argv_value.slice);

                // TODO: memory management
                var argv = try std.ArrayList([]const u8).initCapacity(
                    self.allocator,
                    // argv_value.slice.len + 2,
                    argv_len + 2,
                );
                defer argv.deinit(self.allocator);

                // TODO: figure out how to do this ourselves
                argv.appendSliceAssumeCapacity(&.{ "stdbuf", "-oL" });

                // const element_size = argv_value.slice.element_size;
                // for (0..argv_value.slice.len) |i| {
                //     const start = i * element_size;
                //     const end = start + element_size;
                //     const slice_as_bytes = argv_value_slice[start..end];
                //     var reader = std.Io.Reader.fixed(slice_as_bytes);
                //     const arg_stream = try ir.Value.deserialize(.stream, &reader);
                //     var arg_writer = std.Io.Writer.Allocating.init(self.allocator);
                //     try self.materializeString(thread, arg_stream, &arg_writer.writer);
                //     argv.appendAssumeCapacity(try arg_writer.toOwnedSlice());
                // }

                for (0..argv_len) |_| {
                    const arg = thread.private.stack.pop().?;
                    // const slice = try self.getSlice(arg.slice);
                    // argv.appendAssumeCapacity(slice);

                    // const start = i * element_size;
                    // const end = start + element_size;
                    // const slice_as_bytes = argv_value_slice[start..end];
                    // var reader = std.Io.Reader.fixed(slice_as_bytes);
                    // const arg_stream = try ir.Value.deserialize(.stream, &reader);
                    var arg_writer = std.Io.Writer.Allocating.init(self.allocator);
                    // try self.materializeString(thread, arg_stream, &arg_writer.writer);
                    try self.materializeString(thread, arg, &arg_writer.writer);
                    argv.appendAssumeCapacity(try arg_writer.toOwnedSlice());
                }

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

                const handle = try self.context.addCloseable(process_io.closeable());
                thread.private.result_register = .{ .closeable = handle };

                // if (exec.result) |loc| {
                //     const ref_ptr = thread.refs().getPtr(loc.ref.addr) orelse return Error.RefNotFound;
                //     const handle = try self.context.addCloseable(process_io.closeable());
                //     ref_ptr.* = .{ .closeable = handle };
                // }

                return .cont;
            },
            .jmp => |jmp| {
                const dest = self.resolveAddr(thread, jmp.dest);
                const cond = jmp.cond orelse {
                    thread.setInstructionCounter(dest);
                    return .cont_no_instr_counter_inc;
                };

                const cond_value = try self.resolveValueSource(thread, cond);

                if (cond_value.exit_code.toBoolean() == jmp.jump_if) {
                    thread.setInstructionCounter(dest);
                    return .cont_no_instr_counter_inc;
                }

                return .cont;
            },
            .ref => |ref| {
                _ = ref;
                try thread.private.stack.append(self.allocator, .void);

                return .cont;
            },
            .set => |set| {
                const source = try self.resolveValueSource(thread, set.source);

                return switch (set.destination.abs) {
                    .data => Error.SetImmutableLocation,
                    .instruction => Error.SetInstructionLocation,
                    .ref => |ref| {
                        var dest = thread.getRefPtr(ref, set.destination.mod);
                        if (set.destination.options.dereference) {
                            dest = try self.dereferenceValue(thread, dest.*, null);
                        }
                        dest.* = source;
                        return .cont;
                    },
                    .stack => |stack| {
                        const addr = set.destination.applyMod(stack);
                        var dest = &thread.private.stack.items[addr];
                        if (set.destination.options.dereference) {
                            dest = try self.dereferenceValue(thread, dest.*, null);
                        }
                        dest.* = source;
                        return .cont;
                    },
                    .heap => |heap| {
                        const addr = set.destination.applyMod(heap);
                        var dest = thread.shared.heap.getPtr(addr).?;
                        if (set.destination.options.dereference) {
                            dest = try self.dereferenceValue(thread, dest.*, null);
                        }
                        dest.* = source;
                        return .cont;
                    },
                    .closure => {
                        const addr = set.destination.applyMod(thread.private.stack.items[3].addr);
                        var dest = thread.shared.heap.getPtr(addr).?;
                        if (set.destination.options.dereference) {
                            dest = try self.dereferenceValue(thread, dest.*, null);
                        }
                        dest.* = source;
                        return .cont;
                    },
                    .register => {
                        try self.setLocation(thread, set.destination, source);
                        return .cont;
                    },
                };
            },
            .pipe => |instr_pipe| {
                const pipe = try Stream(u8).initReaderWriter(self.allocator, "pipe", .{}, self.config.tracer);
                const pipe_handle = try self.context.addPipe(pipe);

                try self.setLocation(thread, instr_pipe.result, .{ .pipe = pipe_handle });

                return .cont;
            },
            .pipe_opt => |pipe_opt| {
                const pipe_handle = (try self.resolveLocation(thread, pipe_opt.handle)).pipe;
                const pipe = self.context.getPipe(pipe_handle);
                const value = try self.resolveValueSource(thread, pipe_opt.source);

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
                        .cont, .skip => continue,
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

                thread.private.result_register = .{ .thread = new_thread_handle };

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
                try new_thread.private.stack.append(
                    self.allocator,
                    try self.resolveLocation(thread, fork.closure),
                );

                return .cont;
            },
            .wait => |wait| {
                const waitee = try self.resolveLocation(thread, wait.waitee);

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
                // const streamee = try self.resolveValue(thread, .fromAddr(try stream.toAddr()));
                const streamee = try self.resolveLocation(thread, stream);

                return switch (streamee) {
                    .pipe => |pipe_handle| switch (try self.context.getPipe(pipe_handle).forward(.unlimited)) {
                        .no_source => .cont_no_instr_counter_inc,
                        .closed => .cont,
                        .not_done => .cont_no_instr_counter_inc,
                    },
                    else => {
                        std.log.err("streamee: {t}", .{streamee});
                        return Error.UnsupportedStreamee;
                    },
                };
            },
            .alloc => |size| {
                thread.private.result_register = try thread.shared.alloc(self.allocator, size);
                return .cont;
            },
            .ath => |ath| {
                const left = try self.resolveValueSource(thread, ath.a);
                const right = try self.resolveValueSource(thread, ath.b);

                if (evaluateArithmetic(ath.op, .from(left), .from(right))) |result| {
                    try self.setLocation(thread, ath.result, result);
                } else {
                    return Error.UnsupportedBinaryExpression;
                }

                return .cont;
            },
            .log => |log_expr| {
                const left = try self.resolveValueSource(thread, log_expr.a);

                if (evaluateLogical(log_expr.op, .from(left))) |result| {
                    try self.setLocation(thread, log_expr.result, switch (result) {
                        .left => left,
                        .right => try self.resolveValueSource(thread, log_expr.b),
                    });
                } else {
                    return Error.UnsupportedBinaryExpression;
                }

                return .cont;
            },
            .cmp => |cmp| {
                const left = try self.resolveValueSource(thread, cmp.a);
                const right = try self.resolveValueSource(thread, cmp.b);

                if (evaluateCompare(cmp.op, .from(left), .from(right))) |result| {
                    try self.setLocation(thread, cmp.result, result);
                } else {
                    return Error.UnsupportedBinaryExpression;
                }

                return .cont;
            },
            .exit => |exit_code| .{ .exit = exit_code },
            // else => Error.UnsupportedInstruction,
        };
    }

    fn setLocation(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        loc: ir.Location,
        source: ir.Value,
    ) Error!void {
        switch (loc.abs) {
            .ref => |ref| thread.setRef(ref, loc.mod, source),
            .register => |reg| {
                try switch (reg) {
                    .ic => Error.UnsupportedInstruction,
                    .sf => {
                        if (loc.options.dereference) {
                            const addr = loc.applyMod(thread.private.stack_frame);
                            var dest = &thread.private.stack.items[addr];
                            if (loc.options.dereference) {
                                dest = try self.dereferenceValue(thread, dest.*, null);
                            }
                            dest.* = source;
                        } else {
                            thread.private.stack_frame = source.addr;
                        }
                    },
                    .sc => {
                        if (loc.options.dereference) {
                            const addr = loc.applyMod(thread.private.stack.items.len);
                            var dest = &thread.private.stack.items[addr];
                            if (loc.options.dereference) {
                                dest = try self.dereferenceValue(thread, dest.*, null);
                            }
                            dest.* = source;
                        } else {
                            try thread.private.stack.resize(
                                self.allocator,
                                source.addr,
                            );
                        }
                    },
                    .r => {
                        var dest = &thread.private.result_register;
                        if (loc.options.dereference) {
                            dest = try self.dereferenceValue(thread, dest.*, loc.mod);
                        }
                        dest.* = source;
                    },
                    .r2 => {
                        var dest = &thread.private.result_register_2;
                        if (loc.options.dereference) {
                            dest = try self.dereferenceValue(thread, dest.*, loc.mod);
                        }
                        dest.* = source;
                    },
                    // .r => if (set.location.mod) |mod| break: brk {
                    //     const loc = try self.resolveLocation(thread, .init(.{ .register = .r }, mod),);
                    // } else thread.private.result_register = set.value,
                };
            },
            else => return Error.UnsupportedInstruction,
        }
    }

    pub fn evaluateArithmetic(
        op: ir.Instruction.AthOp,
        left: ir.ValueSource,
        right: ir.ValueSource,
    ) ?ir.Value {
        switch (op) {
            .add => {
                if (left.isValueTag(.uinteger) and right.isValueTag(.uinteger)) {
                    return .{ .uinteger = left.value.uinteger +| right.value.uinteger };
                } else if (left.isValueTag(.float) and right.isValueTag(.float)) {
                    return .{ .float = left.value.float + right.value.float };
                } else if (left.isValueTag(.uinteger) and right.isValueTag(.float)) {
                    const float_left: f64 = @floatFromInt(left.value.uinteger);
                    return .{ .float = float_left + right.value.float };
                } else if (left.isValueTag(.float) and right.isValueTag(.uinteger)) {
                    const float_right: f64 = @floatFromInt(right.value.uinteger);
                    return .{ .float = left.value.float + float_right };
                } else if (left.isValueTag(.addr) and right.isValueTag(.uinteger)) {
                    return .{ .addr = left.value.addr +| right.value.uinteger };
                }
            },
            .sub => {
                if (left.isValueTag(.uinteger) and right.isValueTag(.uinteger)) {
                    return .{ .uinteger = left.value.uinteger -| right.value.uinteger };
                } else if (left.isValueTag(.float) and right.isValueTag(.float)) {
                    return .{ .float = left.value.float - right.value.float };
                } else if (left.isValueTag(.uinteger) and right.isValueTag(.float)) {
                    const float_left: f64 = @floatFromInt(left.value.uinteger);
                    return .{ .float = float_left - right.value.float };
                } else if (left.isValueTag(.float) and right.isValueTag(.uinteger)) {
                    const float_right: f64 = @floatFromInt(right.value.uinteger);
                    return .{ .float = left.value.float - float_right };
                }
            },
            .mul => {
                if (left.isValueTag(.uinteger) and right.isValueTag(.uinteger)) {
                    return .{ .uinteger = left.value.uinteger *| right.value.uinteger };
                } else if (left.isValueTag(.float) and right.isValueTag(.float)) {
                    return .{ .float = left.value.float * right.value.float };
                } else if (left.isValueTag(.uinteger) and right.isValueTag(.float)) {
                    const float_left: f64 = @floatFromInt(left.value.uinteger);
                    return .{ .float = float_left * right.value.float };
                } else if (left.isValueTag(.float) and right.isValueTag(.uinteger)) {
                    const float_right: f64 = @floatFromInt(right.value.uinteger);
                    return .{ .float = left.value.float * float_right };
                }
            },
            .div => {
                const float_left: f64 = if (left.isValueTag(.uinteger)) @floatFromInt(left.value.uinteger) else if (left.isValueTag(.float)) left.value.float else return null;
                const float_right: f64 = if (right.isValueTag(.uinteger)) @floatFromInt(right.value.uinteger) else if (right.isValueTag(.float)) right.value.float else return null;

                return .{ .float = float_left / float_right };
            },
            .mod => {
                const float_left: f64 = if (left.isValueTag(.uinteger)) @floatFromInt(left.value.uinteger) else if (left.isValueTag(.float)) left.value.float else return null;
                const float_right: f64 = if (right.isValueTag(.uinteger)) @floatFromInt(right.value.uinteger) else if (right.isValueTag(.float)) right.value.float else return null;

                return .{ .float = @mod(float_left, float_right) };
            },
        }

        return null;
    }

    pub const EvaluateLogicalResult = enum { left, right };

    pub fn evaluateLogical(
        op: ir.Instruction.LogOp,
        left: ir.ValueSource,
    ) ?EvaluateLogicalResult {
        switch (op) {
            .nd => {
                if (left.isValueTag(.exit_code)) {
                    if (left.value.exit_code == .success) return .right;
                    return .left;
                }
            },
            .r => {
                if (left.isValueTag(.exit_code)) {
                    if (left.value.exit_code == .success) return .left;
                    return .right;
                }
            },
        }

        return null;
    }

    pub fn evaluateCompare(
        op: ir.Instruction.CmpOp,
        left: ir.ValueSource,
        right: ir.ValueSource,
    ) ?ir.Value {
        return switch (op) {
            .gt => {
                if (left.toFloat()) |l| if (right.toFloat()) |r| {
                    return .fromBoolean(l > r);
                };
                return null;
            },
            .gte => {
                if (left.toFloat()) |l| if (right.toFloat()) |r| {
                    return .fromBoolean(l >= r);
                };
                return null;
            },
            .lt => {
                if (left.toFloat()) |l| if (right.toFloat()) |r| {
                    return .fromBoolean(l < r);
                };
                return null;
            },
            .lte => {
                if (left.toFloat()) |l| if (right.toFloat()) |r| {
                    return .fromBoolean(l <= r);
                };
                return null;
            },
            .eq => {
                if (left.toFloat()) |l| if (right.toFloat()) |r| {
                    return .fromBoolean(l == r);
                };
                return null;
            },
            .ne => {
                if (left.toFloat()) |l| if (right.toFloat()) |r| {
                    return .fromBoolean(l != r);
                };
                return null;
            },
        };
    }

    pub const MaterializeStringError =
        Allocator.Error ||
        GetSliceError ||
        DereferenceValueError ||
        std.Io.Writer.Error ||
        error{UnsupportedType};

    fn materializeString(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        value: ir.Value,
        w: *std.Io.Writer,
    ) MaterializeStringError!void {
        // const resolvedValue = try self.dereferenceValue(thread, value);

        switch (value) {
            .stream => |stream| for (0..stream.len) |i| {
                const stream_value = thread.shared.heap.get(i + stream.addr).?;
                try self.materializeString(thread, stream_value, w);
            },
            .executable => |executable| {
                if (executable.element_size != 1) return MaterializeStringError.UnsupportedType;
                const string = try self.getSlice(executable);
                try w.writeAll(string);
            },
            .slice => |slice| {
                if (slice.element_size != 1) return MaterializeStringError.UnsupportedType;
                const string = try self.getSlice(slice);
                try w.writeAll(string);
            },
            inline .uinteger, .float => |t| try w.print("{}", .{t}),
            .addr => |addr| {
                const loc = self.context.mapAddr(addr);
                switch (loc.abs) {
                    .heap => |heap_addr| {
                        // assume this is pointing to a slice on the heap
                        const len = thread.shared.heap.get(heap_addr).?.uinteger;
                        for (0..len) |i| {
                            const slice_element = thread.shared.heap.get(heap_addr + i + 1).?;
                            try self.materializeString(thread, slice_element, w);
                        }
                    },
                    else => try w.print("{f}", .{loc}),
                }
            },
            inline .exit_code => |t| try w.print("{f}", .{t}),
            .zig_string => |z| try w.writeAll(z),
            .pipe => |p| {
                const pipe = self.context.getPipe(p);
                try w.writeAll(pipe.buffer_writer.written());
            },
            .void, .strct, .thread, .closeable, .fn_ref => {},
        }
    }
};
