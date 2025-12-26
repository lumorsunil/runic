const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("ir.zig");
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
    context: *ir.IRContext,

    pub const Config = struct {
        verbose: bool,
    };

    pub fn init(
        allocator: Allocator,
        config: Config,
        context: *ir.IRContext,
    ) @This() {
        return .{ .allocator = allocator, .config = config, .context = context };
    }

    fn log(self: IREvaluator, comptime fmt: []const u8, args: anytype) void {
        if (!self.config.verbose) return;
        std.log.debug("[IREvaluator]: " ++ fmt, args);
    }

    fn currentInstruction(self: IREvaluator) ?ir.Instruction {
        if (self.context.read_only.instructions.len < self.context.instruction_counter) {
            return null;
        }
        return self.context.read_only.instructions[self.context.instruction_counter];
    }

    pub fn step(self: *IREvaluator) Error!?Result {
        const instruction = self.currentInstruction() orelse {
            self.log("no more instructions", .{});
            return null;
        };
        const result = try self.runInstruction(instruction);
        self.context.instruction_counter += 1;
        return result;
    }

    pub const GetSliceError = error{UnsupportedSliceLocation};

    fn getSlice(self: IREvaluator, slice: ir.Value.Slice) GetSliceError![]const u8 {
        self.log("getSlice: {any}", .{slice});
        return switch (self.context.mapAddr(slice.addr)) {
            .data => |data| data.get(
                slice.len * slice.element_size,
                self.context.read_only.data,
            ),
            else => GetSliceError.UnsupportedSliceLocation,
        };
    }

    fn resolveAddr(self: IREvaluator, addr: ir.InstructionAddr) usize {
        return switch (addr) {
            .abs => |abs| abs,
            .rel => |rel| @intCast(
                @as(isize, @intCast(self.context.instruction_counter)) + rel,
            ),
            .label => |label| self.context.labels.get(label).?,
        };
    }

    pub const ResolveValueError = error{UnsupportedResolveValueType};

    fn resolveValue(self: IREvaluator, value: ir.Value) ResolveValueError!ir.Value {
        return switch (value) {
            .addr => |addr| switch (self.context.mapAddr(addr)) {
                .data, .scope => ResolveValueError.UnsupportedResolveValueType,
                .ref => |ref| self.context.refs.get(ref.addr).?,
                .stack => |stack| self.context.stack.items[stack],
                .instruction => value,
            },
            else => value,
        };
    }

    fn runInstruction(self: *IREvaluator, instruction: ir.Instruction) Error!?Result {
        self.log("[{}] {f}", .{ self.context.instruction_counter, instruction });

        return switch (instruction.type) {
            .push => |push| {
                try self.context.stack.append(self.allocator, push);
                return .cont;
            },
            .pop => {
                _ = self.context.stack.pop();
                return .cont;
            },
            .call => {
                // const argv_len = self.context.stack.pop().?.uinteger + 1;
                const context_loc = self.context.stack.pop().?;
                const context = (try self.resolveValue(context_loc)).strct;
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
                    try self.materializeString(arg_stream, &arg_writer.writer);
                    std.log.debug("arg {}: {s}", .{ i, arg_writer.written() });
                    argv.appendAssumeCapacity(try arg_writer.toOwnedSlice());
                }

                // for (0..argv_len) |_| {
                //     const arg = self.context.stack.pop().?;
                //     const slice = try self.getSlice(arg.slice);
                //     argv.appendAssumeCapacity(slice);
                // }

                var child = std.process.Child.init(argv.items, self.allocator);
                try child.spawn();
                _ = try child.wait();

                return .cont;
            },
            .jmp => |jmp| {
                const dest = self.resolveAddr(jmp.dest) - 1;
                const cond = jmp.cond orelse {
                    self.context.instruction_counter = dest;
                    return .cont;
                };

                const cond_value = try self.resolveValue(cond);

                if (cond_value.exit_code.toBoolean() == jmp.jump_if) {
                    self.context.instruction_counter = dest;
                }

                return .cont;
            },
            .ref => |ref| {
                const entry = try self.context.refs.getOrPut(self.allocator, ref.addr);

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
                        const ref_ptr = self.context.refs.getPtr(ref.addr) orelse return Error.RefNotFound;
                        ref_ptr.* = set.value;

                        return .cont;
                    },
                    .stack => |stack| {
                        self.context.stack.items[stack] = set.value;
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
        value: ir.Value,
        w: *std.Io.Writer,
    ) MaterializeStringError!void {
        std.log.debug("materializeString (before resolve): <{t}> {f}", .{ value, value });
        const resolvedValue = try self.resolveValue(value);
        std.log.debug("materializeString (after resolve): <{t}> {f}", .{ resolvedValue, resolvedValue });

        switch (resolvedValue) {
            .stream => |stream| for (stream) |s| try self.materializeString(s, w),
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
