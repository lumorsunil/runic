const std = @import("std");

pub const InstructionAddr = union(enum) {
    rel: isize,
    abs: usize,
    label: LabelKey,

    pub const LabelKey = struct {
        counter: usize,
        name: []const u8,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("{s}_{}", .{ self.name, self.counter });
        }
    };

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .rel => |a| try w.print("<+{x}>", .{a}),
            .abs => |a| try w.print("<{x}>", .{a}),
            .label => |a| try w.print(":{f}", .{a}),
        }
    }
};
