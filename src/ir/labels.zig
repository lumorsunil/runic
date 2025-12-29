const std = @import("std");
const Allocator = std.mem.Allocator;
const InstructionAddr = @import("instruction-addr.zig").InstructionAddr;

pub const Labels = struct {
    map: std.ArrayHashMapUnmanaged(
        InstructionAddr.LabelKey,
        ?usize,
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
        addr: ?usize,
    ) Allocator.Error!InstructionAddr.LabelKey {
        const label: InstructionAddr.LabelKey = .{ .counter = self.map.count(), .name = name };
        try self.set(allocator, label, addr);
        return label;
    }

    pub fn set(
        self: *Labels,
        allocator: Allocator,
        label: InstructionAddr.LabelKey,
        addr: ?usize,
    ) Allocator.Error!void {
        try self.map.put(allocator, label, addr);
    }

    pub fn get(
        self: Labels,
        label: InstructionAddr.LabelKey,
    ) ?usize {
        return self.map.get(label).?;
    }

    pub fn sort(
        self: *Labels,
    ) void {
        return self.map.sort(self);
    }

    pub fn lessThan(ctx: *@This(), a_index: usize, b_index: usize) bool {
        return ctx.map.values()[a_index].? < ctx.map.values()[b_index].?;
    }
};

pub const Label = struct {
    key: InstructionAddr.LabelKey,
    addr: usize,

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("{f}:", .{self.key});
    }
};
