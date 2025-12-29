const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir.zig");
const runic = @import("runic");

pub const Error =
    Allocator.Error ||
    std.process.Child.SpawnError ||
    std.process.Child.WaitError ||
    std.Io.Reader.Error ||
    ir.Value.DeserializeError ||
    IREvaluator.MaterializeStringError ||
    error{
        UnsupportedInstruction,
        SetImmutableLocation,
        SetInstructionLocation,
        RefNotFound,
        DuplicateRef,
    };

pub const Result = union(enum) {
    cont,
    exit: runic.command_runner.ExitCode,
};

pub const IREvaluator = struct {
    allocator: Allocator,
    config: Config,
    context: *ir.context.IRProgramContext,

    pub const Config = struct {
        verbose: bool,
    };

    pub fn init(
        allocator: Allocator,
        config: Config,
        context: *ir.context.IRProgramContext,
    ) @This() {
        return .{ .allocator = allocator, .config = config, .context = context };
    }

    fn log(self: IREvaluator, comptime fmt: []const u8, args: anytype) void {
        if (!self.config.verbose) return;
        std.log.debug("[IREvaluator]: " ++ fmt, args);
    }

    pub fn step(self: *IREvaluator) Error!?Result {
        const thread = self.context.getCurrentThread();

        const instruction = thread.currentInstruction() orelse {
            self.log("{}: no more instructions", .{thread.id});
            try self.context.closeThread(thread.id, null);
            return self.advanceThreadCounter();
        };

        const result = try self.runInstruction(thread, instruction);

        switch (result) {
            .exit => |exit_code| try self.context.closeThread(thread.id, exit_code),
            .cont => {},
        }

        thread.private.instruction_counter += 1;

        return self.advanceThreadCounter();
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
    ) usize {
        return switch (addr) {
            .abs => |abs| abs,
            .rel => |rel| @intCast(
                @as(isize, @intCast(thread.private.instruction_counter)) + rel,
            ),
            .label => |label| self.context.labels().get(label).?,
        };
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
                .ref => |ref| thread.private.refs.get(ref.addr).?,
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
        self.log("[{}] {f}", .{ thread.private.instruction_counter, instruction });

        return switch (instruction.type) {
            .push => |push| {
                try thread.private.stack.append(self.allocator, push);
                return .cont;
            },
            .pop => {
                _ = thread.private.stack.pop();
                return .cont;
            },
            .call => {
                // const argv_len = self.context.stack.pop().?.uinteger + 1;
                const context_loc = thread.private.stack.pop().?;
                const context = (try self.resolveValue(thread, context_loc)).strct;
                const argv_value = context.fields[0];
                const argv_value_slice = try self.getSlice(argv_value.slice);

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

                var child = std.process.Child.init(argv.items, self.allocator);
                try child.spawn();
                try child.waitForSpawn();

                const child_thread_id = try self.context.spawnThread();
                const child_thread = self.context.getThreadContext(child_thread_id).?;
                child_thread.private.instruction_counter = self.context.shared.instructions.len;
                child_thread.private.process = child;
                thread.waitFor(child_thread_id);

                return .cont;
            },
            .jmp => |jmp| {
                const dest = self.resolveAddr(thread, jmp.dest) - 1;
                const cond = jmp.cond orelse {
                    thread.private.instruction_counter = dest;
                    return .cont;
                };

                const cond_value = try self.resolveValue(thread, cond);

                if (cond_value.exit_code.toBoolean() == jmp.jump_if) {
                    thread.private.instruction_counter = dest;
                }

                return .cont;
            },
            .ref => |ref| {
                const entry = try thread.private.refs.getOrPut(self.allocator, ref.addr);

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
                        const ref_ptr = thread.private.refs.getPtr(ref.addr) orelse return Error.RefNotFound;
                        ref_ptr.* = set.value;

                        return .cont;
                    },
                    .stack => |stack| {
                        thread.private.stack.items[stack] = set.value;
                        return .cont;
                    },
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
            .void, .strct => {},
        }
    }
};
