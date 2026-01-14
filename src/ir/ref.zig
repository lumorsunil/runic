const std = @import("std");

pub const Ref = struct {
    rel_stack_addr: usize,
    name: []const u8 = "<unknown>",

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("@@{s}(%sf+{x})", .{ self.name, self.rel_stack_addr });
    }
};
