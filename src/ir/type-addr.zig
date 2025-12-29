const std = @import("std");
const Value = @import("value.zig").Value;
const endian = @import("constants.zig").endian;

pub const TypeAddr = union(enum) {
    value: std.meta.Tag(Value),
    /// Element size
    slice: usize,
    struct_type: usize,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .struct_type => |struct_type| try writer.writeInt(
                usize,
                struct_type,
                endian,
            ),
            .slice => |s| try writer.print("{t}({})", .{ self, s }),
            .value => |t| try writer.print("{t}", .{t}),
        }
    }
};
