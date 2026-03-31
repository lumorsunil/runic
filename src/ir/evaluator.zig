const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir.zig");
const runic = @import("runic");
const CloseableProcessIo = runic.process.CloseableProcessIo;
const FileSink = runic.process.FileSink;
const ReaderWriterStream = runic.stream.ReaderWriterStream;
const ExitCode = runic.ExitCode;
const Stream = runic.stream.Stream;
const Tracer = runic.trace.Tracer;
const compiler = runic.ir.compiler;

const FastUIntSource = union(enum) {
    ptr: *ir.Value,
    immediate: usize,

    fn get(self: @This()) usize {
        return switch (self) {
            .ptr => |ptr| ptr.uinteger,
            .immediate => |value| value,
        };
    }
};

const MaterializedExecArgv = struct {
    argv: std.ArrayList([]const u8),
    owned: std.ArrayList(bool),
};

pub const Error =
    Allocator.Error ||
    std.process.Child.SpawnError ||
    std.process.Child.WaitError ||
    std.fs.File.OpenError ||
    std.fs.File.SeekError ||
    std.Io.Reader.Error ||
    runic.stream.StreamError ||
    ir.Value.DeserializeError ||
    IREvaluator.MaterializeStringError ||
    ir.Location.Error ||
    ir.context.Error ||
    error{
        UnsupportedInstruction,
        UnsupportedWaitee,
        UnsupportedStreamee,
        UnsupportedForward,
        UnsupportedExitCodeType,
        UnsupportedNegOperand,
        UnsupportedBinaryOperator,
        UnsupportedBinaryExpression,
        SetImmutableLocation,
        SetInstructionLocation,
        InvalidRegisterAssignment,
        RefNotFound,
        DuplicateRef,
        ContNoInstrCounterIncInAtomic,
        MissingCurrentThread,
        MissingLabelAddr,
        MissingThreadExitCode,
        MissingCloseableResult,
        MalformedExecutionResult,
        MalformedExecutionHandles,
        InvalidStructBaseAddr,
        MissingHeapValue,
        MalformedHeapSequence,
        MissingSpawnedThreadContext,
    };

pub const Result = union(enum) {
    /// Advances instruction counter and thread counter
    cont,
    /// Advances thread counter, retains instruction counter
    cont_no_instr_counter_inc,
    /// Advances instruction counter, retains thread counter (used for comments)
    skip,
    exit: runic.ExitCode,
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
        const thread = self.context.getCurrentThreadPtr().?;
        const instr_addr = thread.getCurrentInstructionAddr();
        const instr = thread.currentInstruction();
        self.log(label ++ ": t:{}, i:{f}: {?f}", .{ thread.id, instr_addr, instr });
    }

    pub fn step(self: *IREvaluator) Error!?Result {
        // self.logTrace(@src().fn_name);

        if (self.singleThreadFastPathEligible()) {
            return try self.stepSingleThreadFastPath();
        }

        const current_thread_index = self.context.thread_counter;
        const thread = self.context.getCurrentThreadPtr() orelse return Error.MissingCurrentThread;

        const instruction = thread.currentInstruction() orelse {
            self.log("{}: no more instructions", .{thread.id});
            try self.context.closeThread(thread.id, null);
            return try self.advanceThreadCounter();
        };

        const result = try self.runInstruction(thread.*, instruction);

        switch (result) {
            .exit => |exit_code| try self.context.closeThread(thread.id, exit_code),
            .cont => self.context.threads.items[current_thread_index].incInstructionCounter(),
            .cont_no_instr_counter_inc => {},
            .skip => {
                self.context.threads.items[current_thread_index].incInstructionCounter();
                return .skip;
            },
        }

        self.tempCloseStdIoCheck();

        return try self.advanceThreadCounter();
    }

    fn singleThreadFastPathEligible(self: *IREvaluator) bool {
        if (self.context.thread_counter != 0) return false;
        if (self.context.threads.items.len != 1) return false;
        if (self.context.threads_to_remove.count() != 0) return false;
        return self.context.threads.items[0].private.waiting_for == null;
    }

    fn stepSingleThreadFastPath(self: *IREvaluator) Error!?Result {
        while (self.singleThreadFastPathEligible()) {
            const instr_addr = self.context.threads.items[0].private.instruction_counter;
            const instruction_set = self.context.threads.items[0].shared.instructions[instr_addr.instr_set];

            if (instruction_set.len <= instr_addr.local_addr) {
                const thread = self.context.threads.items[0];
                self.log("{}: no more instructions", .{thread.id});
                try self.context.closeThread(thread.id, null);
                self.tempCloseStdIoCheck();
                return .{ .exit = try self.context.getMainThreadExitCode() };
            }

            if (try self.tryRunFastRangeLoop(&self.context.threads.items[0], instruction_set)) {
                continue;
            }

            const instruction = instruction_set[instr_addr.local_addr];
            const result = try self.runInstruction(self.context.threads.items[0], instruction);

            switch (result) {
                .exit => |exit_code| {
                    const thread = self.context.threads.items[0];
                    try self.context.closeThread(thread.id, exit_code);
                    self.tempCloseStdIoCheck();
                    return .{ .exit = try self.context.getMainThreadExitCode() };
                },
                .cont => {
                    self.context.threads.items[0].incInstructionCounter();
                    continue;
                },
                .cont_no_instr_counter_inc => return .cont_no_instr_counter_inc,
                .skip => {
                    self.context.threads.items[0].incInstructionCounter();
                    continue;
                },
            }
        }

        return .cont;
    }

    fn tryRunFastRangeLoop(
        self: *IREvaluator,
        thread: *ir.context.IRThreadContext,
        instruction_set: []const ir.Instruction,
    ) Error!bool {
        const current_addr = thread.getCurrentInstructionAddr();
        const start = current_addr.local_addr;
        if (instruction_set.len <= start + 4) return false;

        const cmp = switch (instruction_set[start].type) {
            .cmp => |cmp| cmp,
            else => return false,
        };
        if (cmp.op != .lt) return false;

        const jump_after = switch (instruction_set[start + 1].type) {
            .jmp => |jump| jump,
            else => return false,
        };
        if (jump_after.cond == null or jump_after.jump_if) return false;
        if (!std.meta.eql(jump_after.cond.?, ir.ValueSource.fromLocation(cmp.result))) return false;

        const body = switch (instruction_set[start + 2].type) {
            .ath => |ath| ath,
            else => return false,
        };

        const increment = switch (instruction_set[start + 3].type) {
            .ath => |ath| ath,
            else => return false,
        };
        if (increment.op != .add) return false;

        const jump_back = switch (instruction_set[start + 4].type) {
            .jmp => |jump| jump,
            else => return false,
        };
        if (jump_back.cond != null) return false;

        const exit_addr = try self.resolveAddr(thread.*, jump_after.dest);
        if (exit_addr.instr_set != current_addr.instr_set or exit_addr.local_addr != start + 5) return false;

        const loop_addr = try self.resolveAddr(thread.*, jump_back.dest);
        if (loop_addr.instr_set != current_addr.instr_set or loop_addr.local_addr != start) return false;

        const counter_loc = switch (cmp.a) {
            .location => |loc| loc,
            else => return false,
        };
        const counter_ptr = self.resolveFastUIntPointer(thread.*, counter_loc) orelse return false;
        const limit = self.resolveFastUIntSource(thread.*, cmp.b) orelse return false;

        const increment_result = increment.result;
        if (!std.meta.eql(counter_loc.undereference(), increment_result)) return false;
        if (!std.meta.eql(increment.a, cmp.a)) return false;
        const increment_by = self.resolveFastUIntSource(thread.*, increment.b) orelse return false;
        if (increment_by.get() != 1) return false;

        const body_dest = self.resolveFastUIntPointer(thread.*, body.result) orelse return false;
        const body_left = self.resolveFastUIntSource(thread.*, body.a) orelse return false;
        const body_right = self.resolveFastUIntSource(thread.*, body.b) orelse return false;

        while (counter_ptr.uinteger < limit.get()) {
            body_dest.* = .{ .uinteger = switch (body.op) {
                .add => body_left.get() +| body_right.get(),
                .sub => body_left.get() -| body_right.get(),
                .mul => body_left.get() *| body_right.get(),
                else => return false,
            } };
            counter_ptr.* = .{ .uinteger = counter_ptr.uinteger +| 1 };
        }

        if (!try self.setFastLocation(thread.*, cmp.result, .fromBoolean(false))) {
            try self.setLocation(thread.*, cmp.result, .fromBoolean(false));
        }
        thread.setInstructionCounter(exit_addr);
        return true;
    }

    fn runAtomicInstructionSet(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        instr_set: usize,
    ) Error!Result {
        const instructions = self.context.instructions()[instr_set];

        for (instructions) |instr| {
            switch (try self.runInstruction(thread, instr)) {
                .cont, .skip => continue,
                .cont_no_instr_counter_inc => return Error.ContNoInstrCounterIncInAtomic,
                .exit => |exit_code| return .{ .exit = exit_code },
            }
        }

        return .cont;
    }

    fn runCountedLoop(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        counted_loop: ir.Instruction.CountedLoop,
    ) Error!Result {
        while (true) {
            const counter_ptr = self.resolveFastUIntPointer(thread, counted_loop.counter.dereference()) orelse return Error.UnsupportedInstruction;
            if (counter_ptr.* != .uinteger) return Error.UnsupportedInstruction;

            const limit = self.resolveFastUIntSource(thread, counted_loop.limit) orelse return Error.UnsupportedInstruction;
            if (counter_ptr.uinteger >= limit.get()) break;

            switch (try self.runAtomicInstructionSet(thread, counted_loop.body_instr_set)) {
                .cont, .skip => {},
                .cont_no_instr_counter_inc => return Error.ContNoInstrCounterIncInAtomic,
                .exit => |exit_code| return .{ .exit = exit_code },
            }
            counter_ptr.* = .{ .uinteger = counter_ptr.uinteger +| 1 };
        }

        return .cont;
    }

    fn materializeExecArgv(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        executable: ir.ValueSource,
        arguments: []const ir.ValueSource,
    ) Error!MaterializedExecArgv {
        var argv = try std.ArrayList([]const u8).initCapacity(self.allocator, arguments.len + 1);
        errdefer argv.deinit(self.allocator);
        var owned = try std.ArrayList(bool).initCapacity(self.allocator, arguments.len + 1);
        errdefer {
            for (argv.items, owned.items) |arg, is_owned| {
                if (is_owned) self.allocator.free(arg);
            }
            owned.deinit(self.allocator);
        }

        const all_sources = try self.allocator.alloc(ir.ValueSource, arguments.len + 1);
        defer self.allocator.free(all_sources);
        all_sources[0] = executable;
        @memcpy(all_sources[1..], arguments);

        for (all_sources) |arg_source| {
            const arg = try self.resolveValueSource(thread, arg_source);
            switch (arg) {
                .executable => |slice| {
                    argv.appendAssumeCapacity(try self.getSlice(slice));
                    owned.appendAssumeCapacity(false);
                },
                .slice => |slice| {
                    if (slice.element_size != 1) return Error.UnsupportedType;
                    argv.appendAssumeCapacity(try self.getSlice(slice));
                    owned.appendAssumeCapacity(false);
                },
                .zig_string => |text| {
                    argv.appendAssumeCapacity(text);
                    owned.appendAssumeCapacity(false);
                },
                else => {
                    var arg_writer = std.Io.Writer.Allocating.init(self.allocator);
                    try self.materializeString(thread, arg, &arg_writer.writer);
                    argv.appendAssumeCapacity(try arg_writer.toOwnedSlice());
                    owned.appendAssumeCapacity(true);
                },
            }
        }

        return .{ .argv = argv, .owned = owned };
    }

    fn spawnSimpleExec(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        executable: ir.ValueSource,
        arguments: []const ir.ValueSource,
    ) Error!ExitCode {
        var argv = try self.materializeExecArgv(thread, executable, arguments);
        const owned_flags = try argv.owned.toOwnedSlice(self.allocator);
        defer self.allocator.free(owned_flags);

        const ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
        var child = std.process.Child.init(try argv.argv.toOwnedSlice(self.allocator), self.allocator);
        defer {
            for (child.argv, owned_flags) |arg, is_owned| {
                if (is_owned) self.allocator.free(arg);
            }
            self.allocator.free(child.argv);
        }
        child.env_map = ctx.env;
        child.stdin_behavior = .Pipe;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;
        child.cwd = ctx.cwd;
        try child.spawn();

        const child_ptr = try self.allocator.create(std.process.Child);
        child_ptr.* = child;

        const stdin_handle = thread.private.stack.items[0].pipe;
        const stdout_handle = thread.private.stack.items[1].pipe;
        const stderr_handle = thread.private.stack.items[2].pipe;

        const stdin_pipe = try self.context.getPipe(stdin_handle);
        const stdout_pipe = try self.context.getPipe(stdout_handle);
        const stderr_pipe = try self.context.getPipe(stderr_handle);

        const process_io = try self.allocator.create(CloseableProcessIo);
        process_io.* = .init(child_ptr, self.config.tracer);
        process_io.connect();

        try stdin_pipe.connectDestination(process_io.closeableStdin());
        try stdout_pipe.connectSource(process_io.closeableStdout());
        try stderr_pipe.connectSource(process_io.closeableStderr());

        const exit_code: ExitCode = switch (try child_ptr.wait()) {
            .Exited => |code| .fromByte(@intCast(code)),
            .Signal => |sig| .fromByte(@intCast(128 + sig)),
            else => .fromByte(1),
        };

        while (true) {
            const event = try stdout_pipe.forward(.unlimited);
            if (event != .not_done) break;
        }
        while (true) {
            const event = try stderr_pipe.forward(.unlimited);
            if (event != .not_done) break;
        }

        return exit_code;
    }

    fn tempCloseStdIoCheck(self: *IREvaluator) void {
        if (!self.context.isThreadClosed(0)) return;

        for (self.context.threads.items) |thread| {
            if (thread.id == 0) continue;
            if (self.context.isThreadClosed(thread.id)) continue;
            self.context.closeThread(thread.id, .success) catch |err| {
                self.log("failed to close lingering thread {}: {}", .{ thread.id, err });
            };
        }
    }

    fn advanceThreadCounter(self: *IREvaluator) Error!Result {
        return switch (self.context.advanceThreadCounter()) {
            .cont => .cont,
            .quit => .{ .exit = try self.context.getMainThreadExitCode() },
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
    ) Error!ir.ResolvedInstructionAddr {
        const abs: usize = switch (addr.local_addr) {
            .abs => |abs| abs,
            .rel => |rel| @intCast(
                @as(isize, @intCast(thread.getCurrentInstructionAddr().local_addr)) + rel,
            ),
            .label => |label| return self.context.labels().get(label) orelse Error.MissingLabelAddr,
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
            .addr => |addr| self.resolveAddrPointer(thread, addr, mod),
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

    fn resolveAddrPointer(
        self: IREvaluator,
        thread: ir.context.IRThreadContext,
        addr: usize,
        mod: ?ir.LocationMod,
    ) DereferenceValueError!*ir.Value {
        _ = self;
        const resolved_addr = ir.LocationMod.applyMaybe(mod, addr);
        if (resolved_addr >= ir.context.stack_start) {
            return &thread.private.stack.items[resolved_addr - ir.context.stack_start];
        }
        return thread.shared.heapGetPtr(resolved_addr) orelse {
            std.log.err("Could not dereference address 0x{x}", .{resolved_addr});
            return DereferenceValueError.UnsupportedDereferenceValueType;
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

    fn tryResolveFastValueSource(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        value_source: ir.ValueSource,
    ) ResolveLocationError!?ir.Value {
        return switch (value_source) {
            .value => |value| value,
            .location => |loc| switch (loc.abs) {
                .ref => |ref| if (!loc.options.dereference)
                    .fromAddr(try loc.toAddrWithContext(
                        thread.private.stack_frame,
                        thread.private.stack.items[3].addr,
                    ))
                else
                    thread.getRefPtr(ref, loc.mod).*,
                .closure => {
                    const closure_base = thread.private.stack.items[3].addr;
                    const closure_slot = thread.shared.heapGetPtr(loc.applyMod(closure_base)).?;
                    if (!loc.options.dereference) {
                        return closure_slot.*;
                    }

                    return switch (closure_slot.*) {
                        .addr => |addr| (try self.dereferenceValue(thread, .{ .addr = addr }, null)).*,
                        else => closure_slot.*,
                    };
                },
                .register => |reg| switch (reg) {
                    .r => if (!loc.options.dereference)
                        thread.private.result_register
                    else switch (thread.private.result_register) {
                        .addr => |addr| (try self.dereferenceValue(thread, .{ .addr = addr }, loc.mod)).*,
                        else => return null,
                    },
                    .r2 => if (!loc.options.dereference)
                        thread.private.result_register_2
                    else switch (thread.private.result_register_2) {
                        .addr => |addr| (try self.dereferenceValue(thread, .{ .addr = addr }, loc.mod)).*,
                        else => return null,
                    },
                    .sf => if (!loc.options.dereference)
                        .fromAddr(loc.applyMod(thread.private.stack_frame))
                    else
                        thread.private.stack.items[loc.applyMod(thread.private.stack_frame)],
                    .sc => if (!loc.options.dereference)
                        .fromAddr(loc.applyMod(thread.private.stack.items.len))
                    else
                        thread.private.stack.items[loc.applyMod(thread.private.stack.items.len)],
                    else => null,
                },
                else => null,
            },
        };
    }

    fn setFastLocation(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        loc: ir.Location,
        source: ir.Value,
    ) Error!bool {
        switch (loc.abs) {
            .ref => |ref| {
                if (loc.options.dereference) return false;
                thread.setRef(ref, loc.mod, source);
                return true;
            },
            .closure => {
                const closure_base = thread.private.stack.items[3].addr;
                const closure_addr = loc.applyMod(closure_base);
                var dest = thread.shared.heapGetPtr(closure_addr).?;
                if (loc.options.dereference) {
                    dest = try self.dereferenceValue(thread, dest.*, null);
                }
                dest.* = source;
                return true;
            },
            .register => |reg| switch (reg) {
                .r => {
                    if (loc.options.dereference) return false;
                    thread.private.result_register = source;
                    return true;
                },
                .r2 => {
                    if (loc.options.dereference) return false;
                    thread.private.result_register_2 = source;
                    return true;
                },
                else => return false,
            },
            else => return false,
        }
    }

    fn resolveFastUIntPointer(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        loc: ir.Location,
    ) ?*ir.Value {
        if (!loc.options.dereference) return null;

        const slot = blk: switch (loc.abs) {
            .ref => |ref| break :blk thread.getRefPtr(ref, loc.mod),
            .closure => {
                const closure_base = thread.private.stack.items[3].addr;
                const closure_addr = loc.applyMod(closure_base);
                break :blk thread.shared.heapGetPtr(closure_addr) orelse return null;
            },
            else => return null,
        };

        var current = slot;
        while (current.* == .addr) {
            current = self.resolveAddrPointer(thread, current.addr, null) catch return null;
        }
        return current;
    }

    fn resolveFastUIntSource(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        source: ir.ValueSource,
    ) ?FastUIntSource {
        return switch (source) {
            .value => |value| switch (value) {
                .uinteger => |uinteger| .{ .immediate = uinteger },
                else => null,
            },
            .location => |loc| .{ .ptr = self.resolveFastUIntPointer(thread, loc) orelse return null },
        };
    }

    fn runFastIntegerAth(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        ath: ir.Instruction.Ath,
    ) !bool {
        const dest = self.resolveFastUIntPointer(thread, ath.result) orelse return false;
        const left = switch (ath.a) {
            .location => |loc| self.resolveFastUIntPointer(thread, loc) orelse return false,
            else => return false,
        };
        const right = switch (ath.b) {
            .location => |loc| self.resolveFastUIntPointer(thread, loc) orelse return false,
            else => return false,
        };

        if (left.* != .uinteger or right.* != .uinteger or dest.* != .uinteger) return false;

        dest.* = switch (ath.op) {
            .add => .{ .uinteger = left.uinteger +| right.uinteger },
            .sub => .{ .uinteger = left.uinteger -| right.uinteger },
            .mul => .{ .uinteger = left.uinteger *| right.uinteger },
            else => return false,
        };

        return true;
    }

    fn runFastIntegerCmp(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        cmp: ir.Instruction.Cmp,
    ) !bool {
        const left = switch (cmp.a) {
            .location => |loc| self.resolveFastUIntPointer(thread, loc) orelse return false,
            else => return false,
        };
        const right = switch (cmp.b) {
            .location => |loc| self.resolveFastUIntPointer(thread, loc) orelse return false,
            else => return false,
        };

        if (left.* != .uinteger or right.* != .uinteger) return false;

        const result = switch (cmp.op) {
            .eq => left.uinteger == right.uinteger,
            .ne => left.uinteger != right.uinteger,
            .gt => left.uinteger > right.uinteger,
            .gte => left.uinteger >= right.uinteger,
            .lt => left.uinteger < right.uinteger,
            .lte => left.uinteger <= right.uinteger,
        };

        return self.setFastLocation(thread, cmp.result, .fromBoolean(result));
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
                return switch (dereferenced.*) {
                    .addr => (try self.dereferenceValue(thread, dereferenced.*, null)).*,
                    else => dereferenced.*,
                };
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
                    .sf => return .fromAddr(location.applyMod(thread.private.stack_frame)),
                    .sc => return .fromAddr(location.applyMod(thread.private.stack.items.len)),
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

    fn resolveExecutionResultExitCode(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
    ) Error!ExitCode {
        const is_thread_completion = switch (try self.executionResultFieldValue(
            thread,
            location,
            .completion_is_thread,
        )) {
            .exit_code => |exit_code| exit_code.toBoolean(),
            else => return Error.MalformedExecutionResult,
        };
        if (is_thread_completion) {
            const thread_handle = switch (try self.executionResultFieldValue(thread, location, .closeable)) {
                .thread => |thread_handle| thread_handle,
                else => return Error.MalformedExecutionResult,
            };
            return self.context.thread_exit_codes.get(thread_handle) orelse Error.MissingThreadExitCode;
        }
        const handle = switch (try self.executionResultFieldValue(thread, location, .closeable)) {
            .closeable => |handle| handle,
            else => return Error.MalformedExecutionResult,
        };
        const closeable = try self.context.getCloseable(handle);
        return closeable.getResult() orelse closeable.close();
    }

    fn resolveExecutionHandlesExitCode(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
    ) Error!ExitCode {
        const handle = switch (try self.executionHandlesFieldValue(thread, location, .closeable)) {
            .closeable => |handle| handle,
            else => return Error.MalformedExecutionHandles,
        };
        const closeable = try self.context.getCloseable(handle);
        return closeable.getResult() orelse closeable.close();
    }

    fn resolveStructBaseAddr(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
    ) Error!usize {
        const base_loc = location.undereference();
        const base = switch (base_loc.abs) {
            .ref, .stack, .heap, .closure => (try self.dereferenceLocation(thread, base_loc)).*,
            else => try self.resolveLocation(thread, base_loc),
        };
        return switch (base) {
            .addr => |addr| addr,
            else => Error.InvalidStructBaseAddr,
        };
    }

    fn executionResultFieldValue(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
        field: compiler.ExecutionResultField,
    ) Error!ir.Value {
        const base_addr = try self.resolveStructBaseAddr(thread, location);
        const heap_index = base_addr + compiler.executionResultFieldOffset(field);
        return self.context.shared.heapGet(heap_index) orelse Error.MalformedExecutionResult;
    }

    fn executionHandlesFieldValue(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
        field: compiler.ExecutionHandlesField,
    ) Error!ir.Value {
        const base_addr = try self.resolveStructBaseAddr(thread, location);
        const heap_index = base_addr + compiler.executionHandlesFieldOffset(field);
        return self.context.shared.heapGet(heap_index) orelse Error.MalformedExecutionHandles;
    }

    fn resolveThreadExitCode(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
    ) Error!ExitCode {
        const thread_value = try self.resolveLocation(thread, location);
        const thread_handle = thread_value.thread;
        return self.context.thread_exit_codes.get(thread_handle) orelse Error.MissingThreadExitCode;
    }

    fn coerceExitCode(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        value: ir.ValueSource,
    ) Error!ExitCode {
        if (value == .location and value.location.isType(compiler.execution_result_struct_type)) {
            return self.resolveExecutionResultExitCode(thread, value.location);
        }
        if (value == .location and value.location.isType(compiler.execution_handles_struct_type)) {
            return self.resolveExecutionHandlesExitCode(thread, value.location);
        }
        if (value == .location and value.location.isType(compiler.thread_type)) {
            return self.resolveThreadExitCode(thread, value.location);
        }

        const resolved = try self.resolveValueSource(thread, value);
        return switch (resolved) {
            .uinteger => |x| .fromByte(@intCast(@mod(x, 256))),
            .exit_code => |exit_code| exit_code,
            .closeable => |handle| (try self.context.getCloseable(handle)).getResult() orelse Error.MissingCloseableResult,
            else => error.UnsupportedExitCodeType,
        };
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
            .exit_with => |value| {
                const resolved = try self.resolveValueSource(thread, value);
                switch (resolved) {
                    .slice => |slice| {
                        if (slice.element_size == 1 and thread.private.stack.items.len > 1) {
                            const stdout_handle = thread.private.stack.items[1].pipe;
                            const stdout_pipe = try self.context.getPipe(stdout_handle);
                            const string = try self.getSlice(slice);
                            var w = stdout_pipe.closeableWriter().writer;
                            try w.writeAll(string);
                            try w.flush();
                        }
                        return .{ .exit = .success };
                    },
                    else => return .{ .exit = try self.coerceExitCode(thread, value) },
                }
            },
            .resolve_exit_code => |rec| {
                const exit_code = try self.coerceExitCode(thread, .fromLocation(rec.source));
                try self.setLocation(thread, rec.result, .{ .exit_code = exit_code });
                return .cont;
            },
            .cd => |path_source| {
                const ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                const resolved = try self.resolveValueSource(thread, path_source);
                const path: []const u8 = if (resolved == .void) blk: {
                    // cd with no argument: go to HOME
                    if (ctx.env) |env_map| {
                        if (env_map.get("HOME")) |home| {
                            break :blk try self.allocator.dupe(u8, home);
                        }
                    }
                    break :blk std.process.getEnvVarOwned(self.allocator, "HOME") catch "/";
                } else blk: {
                    var path_writer = std.Io.Writer.Allocating.init(self.allocator);
                    try self.materializeString(thread, resolved, &path_writer.writer);
                    break :blk try path_writer.toOwnedSlice();
                };
                defer self.allocator.free(path);

                // Resolve path to an absolute, normalised form.
                // Use Dir.realpathAlloc so that relative paths are resolved against the
                // current subshell context's cwd (or the OS process cwd if none is set yet).
                // std.fs.path.resolve must NOT be used here — on POSIX it does not prepend
                // the cwd, so it leaves relative paths relative.
                const abs_path: []const u8 = blk: {
                    if (ctx.cwd) |base| {
                        var base_dir = std.fs.openDirAbsolute(base, .{}) catch {
                            thread.private.result_register = .{ .exit_code = .fromByte(1) };
                            return .cont;
                        };
                        defer base_dir.close();
                        break :blk base_dir.realpathAlloc(self.allocator, path) catch {
                            thread.private.result_register = .{ .exit_code = .fromByte(1) };
                            return .cont;
                        };
                    } else {
                        break :blk std.fs.cwd().realpathAlloc(self.allocator, path) catch {
                            thread.private.result_register = .{ .exit_code = .fromByte(1) };
                            return .cont;
                        };
                    }
                };
                defer self.allocator.free(abs_path);

                // Verify abs_path is a directory (realpathAlloc verifies existence but not type).
                var target_dir = std.fs.openDirAbsolute(abs_path, .{}) catch {
                    thread.private.result_register = .{ .exit_code = .fromByte(1) };
                    return .cont;
                };
                target_dir.close();

                // Update the subshell context's cwd (do NOT call chdir — each subshell context
                // tracks its own directory; child processes receive it via child.cwd).
                if (ctx.cwd) |old| self.allocator.free(old);
                ctx.cwd = try self.allocator.dupe(u8, abs_path);
                thread.private.result_register = .{ .exit_code = .success };
                return .cont;
            },
            .enter_subshell => {
                // Push current subshell context handle onto the save stack.
                try thread.private.subshell_context_stack.append(
                    self.allocator,
                    thread.private.subshell_context,
                );
                // Clone the current context into a fresh one for the subshell.
                const parent_ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                const new_handle = try self.context.addSubshellContext(try parent_ctx.clone(self.allocator));
                thread.private.subshell_context = new_handle;
                return .cont;
            },
            .exit_subshell => {
                // Restore the saved subshell context handle.
                const saved = thread.private.subshell_context_stack.pop().?;
                thread.private.subshell_context = saved;
                return .cont;
            },
            .get_module_cache => |path| {
                if (self.context.module_cache.get(path)) |cached| {
                    thread.private.result_register = cached;
                    thread.private.result_register_2 = .fromBoolean(true);
                } else {
                    thread.private.result_register_2 = .fromBoolean(false);
                }
                return .cont;
            },
            .set_module_cache => |path| {
                const path_owned = try self.allocator.dupe(u8, path);
                try self.context.module_cache.put(self.allocator, path_owned, thread.private.result_register);
                return .cont;
            },
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
            .neg => |neg| {
                const operand = try self.resolveLocation(thread, neg.operand);
                const negated = switch (operand) {
                    .exit_code => |exit_code| exit_code.negate(),
                    .addr => |_| if (neg.operand.isType(compiler.execution_result_struct_type))
                        (try self.resolveExecutionResultExitCode(thread, neg.operand)).negate()
                    else
                        return Error.UnsupportedNegOperand,
                    .thread => if (neg.operand.isType(compiler.thread_type))
                        (try self.resolveThreadExitCode(thread, neg.operand)).negate()
                    else
                        return Error.UnsupportedNegOperand,
                    else => return Error.UnsupportedNegOperand,
                };
                try self.setLocation(thread, neg.result, .{ .exit_code = negated });
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
                    argv_len,
                );
                defer argv.deinit(self.allocator);

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
                const ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                child.env_map = ctx.env;
                child.stdin_behavior = .Pipe;
                child.stdout_behavior = .Pipe;
                child.stderr_behavior = .Pipe;
                child.cwd = ctx.cwd;
                try child.spawn();
                try child.waitForSpawn();
                thread.private.process = child;

                const stdin_handle = thread.private.stack.items[0].pipe;
                const stdout_handle = thread.private.stack.items[1].pipe;
                const stderr_handle = thread.private.stack.items[2].pipe;

                const stdin_pipe = try self.context.getPipe(stdin_handle);
                const stdout_pipe = try self.context.getPipe(stdout_handle);
                const stderr_pipe = try self.context.getPipe(stderr_handle);

                // TODO: memory management
                const process_io = try self.allocator.create(CloseableProcessIo);
                process_io.* = .init(child, self.config.tracer);
                process_io.connect();

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
                const dest = try self.resolveAddr(thread, jmp.dest);
                const cond = jmp.cond orelse {
                    thread.setInstructionCounter(dest);
                    return .cont_no_instr_counter_inc;
                };

                const cond_value = try self.resolveValueSource(thread, cond);

                if (cond == .location and cond.location.isType(compiler.execution_result_struct_type)) {
                    const exit_code = try self.resolveExecutionResultExitCode(thread, cond.location);

                    if (exit_code.toBoolean() == jmp.jump_if) {
                        thread.setInstructionCounter(dest);
                        return .cont_no_instr_counter_inc;
                    }
                } else if (cond == .location and cond.location.isType(compiler.execution_handles_struct_type)) {
                    const exit_code = try self.resolveExecutionHandlesExitCode(thread, cond.location);

                    if (exit_code.toBoolean() == jmp.jump_if) {
                        thread.setInstructionCounter(dest);
                        return .cont_no_instr_counter_inc;
                    }
                } else if (cond == .location and cond.location.isType(compiler.thread_type)) {
                    const exit_code = try self.resolveThreadExitCode(thread, cond.location);

                    if (exit_code.toBoolean() == jmp.jump_if) {
                        thread.setInstructionCounter(dest);
                        return .cont_no_instr_counter_inc;
                    }
                } else if (cond_value.exit_code.toBoolean() == jmp.jump_if) {
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
                        var dest = thread.shared.heapGetPtr(addr).?;
                        if (set.destination.options.dereference) {
                            dest = try self.dereferenceValue(thread, dest.*, null);
                        }
                        dest.* = source;
                        return .cont;
                    },
                    .closure => {
                        const addr = set.destination.applyMod(thread.private.stack.items[3].addr);
                        var dest = thread.shared.heapGetPtr(addr).?;
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
            .get_env => |get_env| {
                const ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                const value: ir.Value = if (ctx.env) |env_map|
                    if (env_map.get(get_env.name)) |text| .{ .zig_string = text } else .null
                else
                    .null;
                try self.setLocation(thread, get_env.result, value);
                return .cont;
            },
            .set_env => |set_env| {
                const ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                const env_map = try self.getOrCreateSubshellEnv(ctx);
                const value = try self.resolveValueSource(thread, set_env.value);
                if (value == .null) {
                    _ = env_map.remove(set_env.name);
                    return .cont;
                }
                const text = try self.materializeOwnedString(thread, value);
                defer self.allocator.free(text);
                try env_map.put(set_env.name, text);
                return .cont;
            },
            .pipe => |instr_pipe| {
                const pipe = try Stream(u8).initReaderWriter(self.allocator, "pipe", .{}, self.config.tracer);
                const pipe_handle = try self.context.addPipe(pipe);

                try self.setLocation(thread, instr_pipe.result, .{ .pipe = pipe_handle });

                return .cont;
            },
            .pipe_opt => |pipe_opt| {
                const pipe_handle = (try self.resolveLocation(thread, pipe_opt.handle)).pipe;
                const pipe = try self.context.getPipe(pipe_handle);
                const value = try self.resolveValueSource(thread, pipe_opt.source);

                switch (pipe_opt.option) {
                    inline else => |t| @field(
                        pipe.config,
                        @tagName(t),
                    ) = value.exit_code.toBoolean(),
                }

                return .cont;
            },
            .pipe_file => |pipe_file| {
                const pipe_handle = (try self.resolveLocation(thread, pipe_file.pipe)).pipe;
                const pipe = try self.context.getPipe(pipe_handle);

                var path_writer = std.Io.Writer.Allocating.init(self.allocator);
                defer path_writer.deinit();
                try self.materializeString(
                    thread,
                    try self.resolveValueSource(thread, pipe_file.target),
                    &path_writer.writer,
                );
                const path = try path_writer.toOwnedSlice();

                const file = switch (pipe_file.mode) {
                    .truncate => try std.fs.cwd().createFile(path, .{}),
                    .append => append: {
                        var f = std.fs.cwd().openFile(path, .{ .mode = .read_write }) catch |err| switch (err) {
                            error.FileNotFound => try std.fs.cwd().createFile(path, .{}),
                            else => return err,
                        };
                        try f.seekFromEnd(0);
                        break :append f;
                    },
                };

                const file_sink = try self.allocator.create(FileSink);
                file_sink.* = .init(file, path, pipe_file.mode == .append);
                try self.context.addFileSink(file_sink);
                try pipe.connectDestination(file_sink.closeableWriter());

                return .cont;
            },
            .pipe_write => |pipe_write| {
                const pipe_handle = (try self.resolveLocation(thread, pipe_write.pipe)).pipe;
                const pipe = try self.context.getPipe(pipe_handle);
                const value = try self.resolveValueSource(thread, pipe_write.source);
                try self.materializePipelineInput(thread, value, pipe.closeableWriter().writer);
                return .cont;
            },
            .pipe_fwd => |pipe_fwd| {
                const source_handle = (try self.resolveLocation(thread, pipe_fwd.source)).pipe;
                const destination_handle = (try self.resolveLocation(thread, pipe_fwd.destination)).pipe;
                const source_pipe = try self.context.getPipe(source_handle);
                const destination_pipe = try self.context.getPipe(destination_handle);

                try source_pipe.connectDestination(destination_pipe.closeableWriter());

                return .cont;
            },
            .atomic => |instr_set| return self.runAtomicInstructionSet(thread, instr_set),
            .counted_loop => |counted_loop| return self.runCountedLoop(thread, counted_loop),
            .simple_exec => |simple_exec| {
                thread.private.result_register = .{ .exit_code = try self.spawnSimpleExec(
                    thread,
                    simple_exec.executable,
                    simple_exec.arguments,
                ) };
                return .cont;
            },
            .fork => |fork| {
                const child_ctx_handle = switch (fork.subshell) {
                    .inherit => thread.private.subshell_context,
                    .new => blk: {
                        const parent_ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                        break :blk try self.context.addSubshellContext(try parent_ctx.clone(self.allocator));
                    },
                };
                const new_thread_handle = try self.context.spawnThread(child_ctx_handle);
                const new_thread = self.context.getThreadContextPtr(new_thread_handle) orelse {
                    return Error.MissingSpawnedThreadContext;
                };

                new_thread.setInstructionCounter(try self.resolveAddr(thread, fork.dest));

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
                        const closeable = try self.context.getCloseable(closeable_handle);
                        if (closeable.isClosed() or closeable.getResult() != null) {
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
                    .pipe => |pipe_handle| switch (try (try self.context.getPipe(pipe_handle)).forward(.unlimited)) {
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
                if (try self.runFastIntegerAth(thread, ath)) {
                    return .cont;
                }

                const left = (try self.tryResolveFastValueSource(thread, ath.a)) orelse try self.resolveValueSource(thread, ath.a);
                const right = (try self.tryResolveFastValueSource(thread, ath.b)) orelse try self.resolveValueSource(thread, ath.b);

                if (evaluateArithmetic(ath.op, .from(left), .from(right))) |result| {
                    if (!try self.setFastLocation(thread, ath.result, result)) {
                        try self.setLocation(thread, ath.result, result);
                    }
                } else {
                    return Error.UnsupportedBinaryExpression;
                }

                return .cont;
            },
            .log => |log_expr| {
                const left = (try self.tryResolveFastValueSource(thread, log_expr.a)) orelse try self.resolveValueSource(thread, log_expr.a);

                if (evaluateLogical(log_expr.op, .from(left))) |result| {
                    const resolved = switch (result) {
                        .left => left,
                        .right => (try self.tryResolveFastValueSource(thread, log_expr.b)) orelse try self.resolveValueSource(thread, log_expr.b),
                    };
                    if (!try self.setFastLocation(thread, log_expr.result, resolved)) {
                        try self.setLocation(thread, log_expr.result, resolved);
                    }
                } else {
                    return Error.UnsupportedBinaryExpression;
                }

                return .cont;
            },
            .cmp => |cmp| {
                if (try self.runFastIntegerCmp(thread, cmp)) {
                    return .cont;
                }
                const left = (try self.tryResolveFastValueSource(thread, cmp.a)) orelse try self.resolveValueSource(thread, cmp.a);
                const right = (try self.tryResolveFastValueSource(thread, cmp.b)) orelse try self.resolveValueSource(thread, cmp.b);

                if (evaluateCompare(cmp.op, .from(left), .from(right))) |result| {
                    if (!try self.setFastLocation(thread, cmp.result, result)) {
                        try self.setLocation(thread, cmp.result, result);
                    }
                } else if (try self.evaluateStringCompare(thread, cmp.op, left, right)) |result| {
                    if (!try self.setFastLocation(thread, cmp.result, result)) {
                        try self.setLocation(thread, cmp.result, result);
                    }
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
            .closure => {
                const addr = loc.applyMod(thread.private.stack.items[3].addr);
                var dest = thread.shared.heapGetPtr(addr).?;
                if (loc.options.dereference) {
                    dest = try self.dereferenceValue(thread, dest.*, null);
                }
                dest.* = source;
            },
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
                            switch (source) {
                                .addr => |addr| thread.private.stack_frame = addr,
                                else => return Error.InvalidRegisterAssignment,
                            }
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
                            switch (source) {
                                .addr => |addr| try thread.private.stack.resize(
                                    self.allocator,
                                    addr,
                                ),
                                else => return Error.InvalidRegisterAssignment,
                            }
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
            .eq => {
                if (left.isValueTag(.null) or right.isValueTag(.null)) {
                    return .fromBoolean(left.isValueTag(.null) and right.isValueTag(.null));
                }
                if (left.toFloat()) |l| if (right.toFloat()) |r| {
                    return .fromBoolean(l == r);
                };
                return null;
            },
            .ne => {
                if (left.isValueTag(.null) or right.isValueTag(.null)) {
                    return .fromBoolean(left.isValueTag(.null) != right.isValueTag(.null));
                }
                if (left.toFloat()) |l| if (right.toFloat()) |r| {
                    return .fromBoolean(l != r);
                };
                return null;
            },
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
        };
    }

    fn evaluateStringCompare(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        op: ir.Instruction.CmpOp,
        left: ir.Value,
        right: ir.Value,
    ) !?ir.Value {
        const left_text = self.materializeOwnedString(thread, left) catch return null;
        defer self.allocator.free(left_text);
        const right_text = self.materializeOwnedString(thread, right) catch return null;
        defer self.allocator.free(right_text);

        const order = std.mem.order(u8, left_text, right_text);
        return switch (op) {
            .eq => .fromBoolean(order == .eq),
            .ne => .fromBoolean(order != .eq),
            .gt => .fromBoolean(order == .gt),
            .gte => .fromBoolean(order == .gt or order == .eq),
            .lt => .fromBoolean(order == .lt),
            .lte => .fromBoolean(order == .lt or order == .eq),
        };
    }

    fn materializeOwnedString(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        value: ir.Value,
    ) ![]u8 {
        var alloc_writer = std.Io.Writer.Allocating.init(self.allocator);
        errdefer alloc_writer.deinit();
        try self.materializeString(thread, value, &alloc_writer.writer);
        return try alloc_writer.toOwnedSlice();
    }

    fn getOrCreateSubshellEnv(
        self: *IREvaluator,
        ctx: *ir.context.SubshellContext,
    ) Allocator.Error!*std.process.EnvMap {
        if (ctx.env) |env| return env;

        const env = try self.allocator.create(std.process.EnvMap);
        errdefer self.allocator.destroy(env);
        env.* = std.process.EnvMap.init(self.allocator);
        ctx.env = env;
        return env;
    }

    pub const MaterializeStringError =
        Allocator.Error ||
        GetSliceError ||
        DereferenceValueError ||
        std.Io.Writer.Error ||
        ir.context.Error ||
        error{
            UnsupportedType,
            MissingHeapValue,
            MalformedHeapSequence,
        };

    fn heapValueAt(
        self: *IREvaluator,
        addr: usize,
    ) MaterializeStringError!ir.Value {
        return self.context.shared.heapGet(addr) orelse MaterializeStringError.MissingHeapValue;
    }

    fn maybeHeapSequenceLen(
        self: *IREvaluator,
        addr: usize,
    ) MaterializeStringError!?struct { heap_addr: usize, len: usize } {
        const loc = self.context.mapAddr(addr);
        const heap_addr = switch (loc.abs) {
            .heap => |heap_addr| heap_addr,
            else => return null,
        };
        const heap_value = try self.heapValueAt(heap_addr);
        return switch (heap_value) {
            .uinteger => |len| .{ .heap_addr = heap_addr, .len = len },
            else => null,
        };
    }

    fn materializeHeapSequence(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        heap_addr: usize,
        len: usize,
        separator: ?u8,
        w: *std.Io.Writer,
    ) MaterializeStringError!void {
        for (0..len) |i| {
            if (separator) |sep| {
                if (i > 0) try w.writeByte(sep);
            }
            const element = try self.heapValueAt(heap_addr + i + 1);
            try self.materializeString(thread, element, w);
        }
    }

    fn materializeString(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        value: ir.Value,
        w: *std.Io.Writer,
    ) MaterializeStringError!void {
        // const resolvedValue = try self.dereferenceValue(thread, value);

        switch (value) {
            .stream => |stream| for (0..stream.len) |i| {
                const stream_value = try self.heapValueAt(i + stream.addr);
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
                if (try self.maybeHeapSequenceLen(addr)) |seq| {
                    try self.materializeHeapSequence(thread, seq.heap_addr, seq.len, null, w);
                } else {
                    const loc = self.context.mapAddr(addr);
                    switch (loc.abs) {
                        .heap => |heap_addr| {
                            const heap_value = try self.heapValueAt(heap_addr);
                            try self.materializeString(thread, heap_value, w);
                        },
                        else => try w.print("{f}", .{loc}),
                    }
                }
            },
            inline .exit_code => |t| try w.print("{f}", .{t}),
            .zig_string => |z| try w.writeAll(z),
            .pipe => |p| {
                const pipe = try self.context.getPipe(p);
                if (pipe.capture_writer.written().len > 0) {
                    try w.writeAll(pipe.capture_writer.written());
                } else {
                    try w.writeAll(pipe.buffer_writer.written());
                }
            },
            .null => try w.writeAll("null"),
            .void, .strct, .thread, .closeable, .fn_ref => {},
        }
    }

    fn materializePipelineInput(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        value: ir.Value,
        w: *std.Io.Writer,
    ) MaterializeStringError!void {
        if (value == .addr) {
            if (try self.maybeHeapSequenceLen(value.addr)) |seq| {
                try self.materializeHeapSequence(thread, seq.heap_addr, seq.len, ' ', w);
                return;
            }
        }

        try self.materializeString(thread, value, w);
    }
};

fn initTestEvaluator(
    allocator: Allocator,
) !struct {
    tracer: Tracer,
    stdin_stream: *ReaderWriterStream,
    stdout_stream: *ReaderWriterStream,
    stderr_stream: *ReaderWriterStream,
    context: ir.context.IRProgramContext,
} {
    var tracer = Tracer.init(allocator, .{ .echo_to_stdout = false });
    const stdin_stream = try Stream(u8).initReaderWriter(allocator, "test-stdin", .{}, &tracer);
    const stdout_stream = try Stream(u8).initReaderWriter(allocator, "test-stdout", .{}, &tracer);
    const stderr_stream = try Stream(u8).initReaderWriter(allocator, "test-stderr", .{}, &tracer);
    const context = ir.context.IRProgramContext.init(allocator, .{
        .data = &.{},
        .instructions = &.{},
        .labels = .init(),
        .struct_types = &.{},
        .current_heap_addr = 0,
    });

    return .{
        .tracer = tracer,
        .stdin_stream = stdin_stream,
        .stdout_stream = stdout_stream,
        .stderr_stream = stderr_stream,
        .context = context,
    };
}

test "evaluator step reports missing current thread" {
    const allocator = std.testing.allocator;
    var fixture = try initTestEvaluator(allocator);
    defer fixture.context.deinit();
    defer fixture.stdin_stream.deinitParent();
    defer fixture.stdout_stream.deinitParent();
    defer fixture.stderr_stream.deinitParent();
    defer fixture.tracer.deinit();

    var evaluator = IREvaluator.init(allocator, .{
        .verbose = false,
        .stdin = fixture.stdin_stream,
        .stdout = fixture.stdout_stream,
        .stderr = fixture.stderr_stream,
        .tracer = &fixture.tracer,
    }, &fixture.context);

    try std.testing.expectError(Error.MissingCurrentThread, evaluator.step());
}

test "evaluator materializeString reports missing pipe handle" {
    const allocator = std.testing.allocator;
    var fixture = try initTestEvaluator(allocator);
    defer fixture.context.deinit();
    defer fixture.stdin_stream.deinitParent();
    defer fixture.stdout_stream.deinitParent();
    defer fixture.stderr_stream.deinitParent();
    defer fixture.tracer.deinit();

    var evaluator = IREvaluator.init(allocator, .{
        .verbose = false,
        .stdin = fixture.stdin_stream,
        .stdout = fixture.stdout_stream,
        .stderr = fixture.stderr_stream,
        .tracer = &fixture.tracer,
    }, &fixture.context);

    try fixture.context.addMainThread(null);
    const thread = fixture.context.getCurrentThread().?;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    try std.testing.expectError(
        ir.context.Error.MissingPipeHandle,
        evaluator.materializeString(thread, .{ .pipe = 999 }, &writer.writer),
    );
}

test "evaluator fast arithmetic path updates ref destination" {
    const allocator = std.testing.allocator;
    var fixture = try initTestEvaluator(allocator);
    defer fixture.context.deinit();
    defer fixture.stdin_stream.deinitParent();
    defer fixture.stdout_stream.deinitParent();
    defer fixture.stderr_stream.deinitParent();
    defer fixture.tracer.deinit();

    var evaluator = IREvaluator.init(allocator, .{
        .verbose = false,
        .stdin = fixture.stdin_stream,
        .stdout = fixture.stdout_stream,
        .stderr = fixture.stderr_stream,
        .tracer = &fixture.tracer,
    }, &fixture.context);

    try fixture.context.addMainThread(null);
    const thread = fixture.context.getCurrentThread().?;
    try thread.private.stack.appendSlice(allocator, &.{ .{ .uinteger = 4 }, .{ .uinteger = 7 } });

    const left = ir.Location.initAbs(.{ .ref = .{ .name = "left", .rel_stack_addr = 0 } }, .{});
    const right = ir.Location.initAbs(.{ .ref = .{ .name = "right", .rel_stack_addr = 1 } }, .{});
    const result = ir.Location.initAbs(.{ .ref = .{ .name = "result", .rel_stack_addr = 1 } }, .{});

    switch (try evaluator.runInstruction(thread, .init(null, .{ .ath = .{
        .op = .add,
        .a = .fromLocation(left.dereference()),
        .b = .fromLocation(right.dereference()),
        .result = result,
    } }))) {
        .cont => {},
        else => unreachable,
    }

    try std.testing.expectEqual(@as(u64, 11), thread.private.stack.items[1].uinteger);
}

test "evaluator fast arithmetic path updates closure destination" {
    const allocator = std.testing.allocator;
    var fixture = try initTestEvaluator(allocator);
    defer fixture.context.deinit();
    defer fixture.stdin_stream.deinitParent();
    defer fixture.stdout_stream.deinitParent();
    defer fixture.stderr_stream.deinitParent();
    defer fixture.tracer.deinit();

    var evaluator = IREvaluator.init(allocator, .{
        .verbose = false,
        .stdin = fixture.stdin_stream,
        .stdout = fixture.stdout_stream,
        .stderr = fixture.stderr_stream,
        .tracer = &fixture.tracer,
    }, &fixture.context);

    try fixture.context.addMainThread(null);
    const thread = fixture.context.getCurrentThread().?;
    try thread.private.stack.resize(allocator, 4);
    @memset(thread.private.stack.items, .void);

    const closure_base = try fixture.context.shared.alloc(allocator, 1);
    const closure_addr = switch (closure_base) {
        .addr => |addr| addr,
        else => unreachable,
    };
    thread.private.stack.items[3] = .fromAddr(closure_addr);
    fixture.context.shared.heapGetPtr(closure_addr).?.* = .{ .uinteger = 4 };

    const closure_slot = ir.Location.initAbs(.closure, .{}).dereference();

    switch (try evaluator.runInstruction(thread, .init(null, .{ .ath = .{
        .op = .add,
        .a = .fromLocation(closure_slot),
        .b = .fromValue(.{ .uinteger = 7 }),
        .result = closure_slot,
    } }))) {
        .cont => {},
        else => unreachable,
    }

    try std.testing.expectEqual(@as(u64, 11), fixture.context.shared.heapGet(closure_addr).?.uinteger);
}

test "evaluator fast compare path updates register destination" {
    const allocator = std.testing.allocator;
    var fixture = try initTestEvaluator(allocator);
    defer fixture.context.deinit();
    defer fixture.stdin_stream.deinitParent();
    defer fixture.stdout_stream.deinitParent();
    defer fixture.stderr_stream.deinitParent();
    defer fixture.tracer.deinit();

    var evaluator = IREvaluator.init(allocator, .{
        .verbose = false,
        .stdin = fixture.stdin_stream,
        .stdout = fixture.stdout_stream,
        .stderr = fixture.stderr_stream,
        .tracer = &fixture.tracer,
    }, &fixture.context);

    try fixture.context.addMainThread(null);
    const thread = fixture.context.getCurrentThread().?;
    thread.private.result_register = .{ .uinteger = 9 };
    thread.private.result_register_2 = .{ .uinteger = 3 };

    switch (try evaluator.runInstruction(thread, .init(null, .{ .cmp = .{
        .op = .gt,
        .a = .fromLocation(.initRegister(.r)),
        .b = .fromLocation(.initRegister(.r2)),
        .result = .initRegister(.r2),
    } }))) {
        .cont => {},
        else => unreachable,
    }

    try std.testing.expect(switch (thread.private.result_register_2) {
        .exit_code => |exit_code| exit_code.toBoolean(),
        else => false,
    });
}

test "evaluator fast compare path reads dereferenced refs" {
    const allocator = std.testing.allocator;
    var fixture = try initTestEvaluator(allocator);
    defer fixture.context.deinit();
    defer fixture.stdin_stream.deinitParent();
    defer fixture.stdout_stream.deinitParent();
    defer fixture.stderr_stream.deinitParent();
    defer fixture.tracer.deinit();

    var evaluator = IREvaluator.init(allocator, .{
        .verbose = false,
        .stdin = fixture.stdin_stream,
        .stdout = fixture.stdout_stream,
        .stderr = fixture.stderr_stream,
        .tracer = &fixture.tracer,
    }, &fixture.context);

    try fixture.context.addMainThread(null);
    const thread = fixture.context.getCurrentThread().?;
    try thread.private.stack.appendSlice(allocator, &.{ .{ .uinteger = 3 }, .{ .uinteger = 7 } });

    const left = ir.Location.initAbs(.{ .ref = .{ .name = "left", .rel_stack_addr = 0 } }, .{});
    const right = ir.Location.initAbs(.{ .ref = .{ .name = "right", .rel_stack_addr = 1 } }, .{});

    switch (try evaluator.runInstruction(thread, .init(null, .{ .cmp = .{
        .op = .lt,
        .a = .fromLocation(left.dereference()),
        .b = .fromLocation(right.dereference()),
        .result = .initRegister(.r2),
    } }))) {
        .cont => {},
        else => unreachable,
    }

    try std.testing.expect(switch (thread.private.result_register_2) {
        .exit_code => |exit_code| exit_code.toBoolean(),
        else => false,
    });
}

test "evaluator fused range loop updates accumulator and exits loop" {
    const allocator = std.testing.allocator;
    var fixture = try initTestEvaluator(allocator);
    defer fixture.context.deinit();
    defer fixture.stdin_stream.deinitParent();
    defer fixture.stdout_stream.deinitParent();
    defer fixture.stderr_stream.deinitParent();
    defer fixture.tracer.deinit();

    var evaluator = IREvaluator.init(allocator, .{
        .verbose = false,
        .stdin = fixture.stdin_stream,
        .stdout = fixture.stdout_stream,
        .stderr = fixture.stderr_stream,
        .tracer = &fixture.tracer,
    }, &fixture.context);

    try fixture.context.addMainThread(null);
    const thread = fixture.context.getCurrentThreadPtr().?;
    try thread.private.stack.appendSlice(allocator, &.{
        .{ .uinteger = 0 },
        .{ .uinteger = 5 },
        .{ .uinteger = 0 },
    });

    const total = ir.Location.initAbs(.{ .ref = .{ .name = "total", .rel_stack_addr = 0 } }, .{});
    const len = ir.Location.initAbs(.{ .ref = .{ .name = "len", .rel_stack_addr = 1 } }, .{});
    const counter = ir.Location.initAbs(.{ .ref = .{ .name = "counter", .rel_stack_addr = 2 } }, .{});

    const instructions = [_]ir.Instruction{
        .init(null, .{ .cmp = .{
            .op = .lt,
            .a = .fromLocation(counter.dereference()),
            .b = .fromLocation(len.dereference()),
            .result = .initRegister(.r2),
        } }),
        .init(null, .{ .jmp = .{
            .cond = .fromLocation(.initRegister(.r2)),
            .jump_if = false,
            .dest = .init(0, .{ .abs = 5 }),
        } }),
        .init(null, .{ .ath = .{
            .op = .add,
            .a = .fromLocation(total.dereference()),
            .b = .fromLocation(counter.dereference()),
            .result = total.dereference(),
        } }),
        .init(null, .{ .ath = .{
            .op = .add,
            .a = .fromLocation(counter.dereference()),
            .b = .fromValue(.{ .uinteger = 1 }),
            .result = counter,
        } }),
        .init(null, .{ .jmp = .{
            .cond = null,
            .jump_if = false,
            .dest = .init(0, .{ .abs = 0 }),
        } }),
    };

    try std.testing.expect(try evaluator.tryRunFastRangeLoop(thread, &instructions));
    try std.testing.expectEqual(@as(u64, 10), thread.private.stack.items[0].uinteger);
    try std.testing.expectEqual(@as(u64, 5), thread.private.stack.items[2].uinteger);
    try std.testing.expectEqual(@as(usize, 5), thread.getCurrentInstructionAddr().local_addr);
    try std.testing.expect(switch (thread.private.result_register_2) {
        .exit_code => |exit_code| !exit_code.toBoolean(),
        else => false,
    });
}
