const std = @import("std");
const Ref = @import("ref.zig").Ref;
const InstructionAddr = @import("instruction-addr.zig").InstructionAddr;
const ir = @import("../ir.zig");
const page_size = ir.context.page_size;
const stack_start = ir.context.stack_start;

pub const Location = union(enum) {
    ref: Ref,
    scope: ScopeLocation,
    stack: usize,
    instruction: InstructionAddr,
    data: struct {
        page: usize,
        addr: usize,

        pub const zero = @This(){ .page = 0, .addr = 0 };

        pub fn init(page: usize, addr: usize) @This() {
            return .{ .page = page, .addr = addr };
        }

        pub fn fromAddr(addr: usize) @This() {
            const page = @divFloor(addr, page_size);
            return .init(page, @mod(addr, page_size));
        }

        pub fn get(self: @This(), len: usize, data: []const []const u8) []const u8 {
            const end = self.addr + len;
            return data[self.page][self.addr..end];
        }
    },

    pub const Error = error{UnsupportedAddrType};

    pub fn toAddr(self: Location) Error!usize {
        return switch (self) {
            .data => |data| data.page * page_size + data.addr,
            .ref => |ref| ref.addr,
            .scope => Error.UnsupportedAddrType,
            .stack => |stack| stack_start + stack,
            .instruction => Error.UnsupportedAddrType,
        };
    }

    pub fn size() usize {
        return @sizeOf(Location);
    }

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .ref => |r| try w.print("{f}", .{r}),
            .scope => |s| try w.print("@{s}", .{s}),
            .data => |d| try w.print("#{}:{}", .{ d.page, d.addr }),
            .stack => |s| try w.print("S{}", .{s}),
            .instruction => |i| try w.print("I{f}", .{i}),
        }
    }
};

pub const ScopeLocation = []const u8;
