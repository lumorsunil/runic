const std = @import("std");

pub const Ref = struct {
    addr: usize,
    name: []const u8 = "<unknown>",

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("@@{s}({x})", .{ self.name, self.addr });
    }
};
