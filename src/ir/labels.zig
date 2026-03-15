const std = @import("std");
const Allocator = std.mem.Allocator;
const InstructionAddr = @import("instruction-addr.zig").InstructionAddr;
const ResolvedInstructionAddr = @import("instruction-addr.zig").ResolvedInstructionAddr;

pub const Labels = struct {
    map: std.ArrayHashMapUnmanaged(
        InstructionAddr.LabelKey,
        ?ResolvedInstructionAddr,
        HashContext,
        true,
    ) = .empty,

    const HashContext = struct {
        pub fn hash(_: @This(), key: InstructionAddr.LabelKey) u32 {
            var hasher = std.hash.Wyhash.init(0);
            std.hash.autoHash(&hasher, key.counter);
            return @truncate(hasher.final());
        }

        pub fn eql(
            _: @This(),
            a: InstructionAddr.LabelKey,
            b: InstructionAddr.LabelKey,
            _: usize,
        ) bool {
            return a.counter == b.counter;
        }
    };

    pub fn init() @This() {
        return .{};
    }

    pub fn new(
        self: *Labels,
        allocator: Allocator,
        name: []const u8,
        addr: ?ResolvedInstructionAddr,
    ) Allocator.Error!InstructionAddr.LabelKey {
        const label: InstructionAddr.LabelKey = .{ .counter = self.map.count(), .name = name };
        try self.set(allocator, label, addr);
        return label;
    }

    pub fn set(
        self: *Labels,
        allocator: Allocator,
        label: InstructionAddr.LabelKey,
        addr: ?ResolvedInstructionAddr,
    ) Allocator.Error!void {
        try self.map.put(allocator, label, addr);
    }

    pub fn get(
        self: Labels,
        label: InstructionAddr.LabelKey,
    ) ?ResolvedInstructionAddr {
        return self.map.get(label).?;
    }

    pub fn sort(
        self: *Labels,
    ) void {
        return self.map.sort(self);
    }

    pub fn lessThan(ctx: *@This(), a_index: usize, b_index: usize) bool {
        const left = ctx.map.values()[a_index].?;
        const right = ctx.map.values()[b_index].?;
        return left.instr_set < right.instr_set or (left.instr_set == right.instr_set and left.local_addr < right.local_addr);
    }
};

pub const Label = struct {
    key: InstructionAddr.LabelKey,
    addr: usize,

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("{f}:", .{self.key});
    }
};
