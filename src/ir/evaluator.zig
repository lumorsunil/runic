const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir.zig");
const runic = @import("runic");
const CloseableProcessIo = runic.process.CloseableProcessIo;
const FileSink = runic.process.FileSink;
const ReaderWriterStream = runic.stream.ReaderWriterStream;
const ExitCode = runic.command_runner.ExitCode;
const Stream = runic.stream.Stream;
const Tracer = runic.trace.Tracer;
const compiler = runic.ir.compiler;

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

        const thread = self.context.getCurrentThread() orelse return Error.MissingCurrentThread;

        const instruction = thread.currentInstruction() orelse {
            self.log("{}: no more instructions", .{thread.id});
            try self.context.closeThread(thread.id, null);
            return try self.advanceThreadCounter();
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

        return try self.advanceThreadCounter();
    }

    fn tempCloseStdIoCheck(self: *IREvaluator) void {
        const is_main_thread_done = self.context.isThreadClosed(0);
        if (is_main_thread_done) {
            for (self.context.threads.items) |thread| {
                if (thread.id == 0) continue;
                if (self.context.isThreadClosed(thread.id)) continue;
                self.context.closeThread(thread.id, .success) catch |err| {
                    self.log("failed to close lingering thread {}: {}", .{ thread.id, err });
                };
            }
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
        return self.context.shared.heap.get(heap_index) orelse Error.MalformedExecutionResult;
    }

    fn executionHandlesFieldValue(
        self: *IREvaluator,
        thread: ir.context.IRThreadContext,
        location: ir.Location,
        field: compiler.ExecutionHandlesField,
    ) Error!ir.Value {
        const base_addr = try self.resolveStructBaseAddr(thread, location);
        const heap_index = base_addr + compiler.executionHandlesFieldOffset(field);
        return self.context.shared.heap.get(heap_index) orelse Error.MalformedExecutionHandles;
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
                const ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                child.env_map = ctx.env;
                child.stdin_behavior = .Pipe;
                child.stdout_behavior = .Pipe;
                child.stderr_behavior = .Pipe;
                child.cwd = ctx.cwd;
                try child.spawn();
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
            .set_env => |set_env| {
                const ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                const env_map = try self.getOrCreateSubshellEnv(ctx);
                const value = try self.resolveValueSource(thread, set_env.value);
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
                const child_ctx_handle = switch (fork.subshell) {
                    .inherit => thread.private.subshell_context,
                    .new => blk: {
                        const parent_ctx = self.context.getSubshellContextPtr(thread.private.subshell_context);
                        break :blk try self.context.addSubshellContext(try parent_ctx.clone(self.allocator));
                    },
                };
                const new_thread_handle = try self.context.spawnThread(child_ctx_handle);
                const new_thread = self.context.getThreadContext(new_thread_handle) orelse {
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
                } else if (try self.evaluateStringCompare(thread, cmp.op, left, right)) |result| {
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
        return self.context.shared.heap.get(addr) orelse MaterializeStringError.MissingHeapValue;
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
