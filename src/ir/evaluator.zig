const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("ir.zig");
const runic = @import("runic");

const log_enabled = true;

pub const Error =
    Allocator.Error ||
    std.process.Child.SpawnError ||
    std.process.Child.WaitError ||
    error{
        UnsupportedInstruction,
        UnsupportedSliceLocation,
    };

pub const Result = union(enum) {
    cont,
    exit: runic.command_runner.ExitCode,
};

pub const IREvaluator = struct {
    allocator: Allocator,
    context: *ir.IRContext,

    pub fn init(allocator: Allocator, context: *ir.IRContext) @This() {
        return .{ .allocator = allocator, .context = context };
    }

    fn log(_: IREvaluator, comptime fmt: []const u8, args: anytype) void {
        if (!log_enabled) return;
        std.log.debug("[IREvaluator]: " ++ fmt, args);
    }

    fn currentInstruction(self: IREvaluator) ?ir.Instruction {
        if (self.context.read_only.instructions.len < self.context.instruction_counter) {
            return null;
        }
        return self.context.read_only.instructions[self.context.instruction_counter];
    }

    pub fn step(self: *IREvaluator) Error!?Result {
        self.log("ic: {} / {}", .{
            self.context.instruction_counter,
            self.context.read_only.instructions.len,
        });
        const instruction = self.currentInstruction() orelse {
            self.log("no more instructions", .{});
            return null;
        };
        const result = try self.runInstruction(instruction);
        self.context.instruction_counter += 1;
        return result;
    }

    fn getSlice(self: IREvaluator, slice: ir.Value.Slice) Error![]const u8 {
        return switch (slice.location) {
            .data => |data| data.get(slice.len, self.context.read_only.data),
            else => Error.UnsupportedSliceLocation,
        };
    }

    fn runInstruction(self: *IREvaluator, instruction: ir.Instruction) Error!?Result {
        self.log("{f}", .{instruction});

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
                const argv_len = self.context.stack.pop().?.uinteger + 1;
                var argv = try std.ArrayList([]const u8).initCapacity(
                    self.allocator,
                    argv_len,
                );
                defer argv.deinit(self.allocator);
                for (0..argv_len) |_| {
                    const slice = try self.getSlice(self.context.stack.pop().?.slice);
                    argv.appendAssumeCapacity(slice);
                }

                var child = std.process.Child.init(argv.items, self.allocator);
                try child.spawn();
                _ = try child.wait();

                return .cont;
            },
            .exit => |exit_code| return .{ .exit = exit_code },
            else => Error.UnsupportedInstruction,
        };
    }
};
