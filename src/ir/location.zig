const std = @import("std");
const Ref = @import("ref.zig").Ref;
const InstructionAddr = @import("instruction-addr.zig").InstructionAddr;
const ir = @import("../ir.zig");
const page_size = ir.context.page_size;
const stack_start = ir.context.stack_start;

pub const Location = struct {
    abs: LocationAbs,
    mod: ?LocationMod = null,

    pub const Error = error{UnsupportedAddrType};

    pub fn init(abs: LocationAbs, mod: ?LocationMod) @This() {
        return .{ .abs = abs, .mod = mod };
    }

    pub fn initAbs(abs: LocationAbs) @This() {
        return .init(abs, null);
    }

    pub fn initAdd(abs: LocationAbs, x: usize) @This() {
        return .init(abs, .{ .add = x });
    }

    pub fn initSub(abs: LocationAbs, x: usize) @This() {
        return .init(abs, .{ .sub = x });
    }

    pub fn size() usize {
        return @sizeOf(@This());
    }

    pub fn toAddr(self: @This()) Error!usize {
        const abs = try self.abs.toAddr();
        return self.applyMod(abs);
    }

    pub fn applyMod(self: @This(), x: usize) usize {
        return if (self.mod) |mod| mod.apply(x) else x;
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (self.mod) |mod| {
            try writer.print("[{f} {f}]", .{ self.abs, mod });
        } else {
            try writer.print("{f}", .{self.abs});
        }
    }
};

pub const LocationMod = union(enum) {
    add: usize,
    sub: usize,

    pub fn apply(self: @This(), x: usize) usize {
        return switch (self) {
            .add => |y| x + y,
            .sub => |y| x - y,
        };
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .add => |x| try writer.print("+ {}", .{x}),
            .sub => |x| try writer.print("- {}", .{x}),
        }
    }
};

pub const RegisterAbs = enum { r, ic, sf, sc };

pub const Register = struct {
    abs: RegisterAbs,
    mod: ?LocationMod = null,

    pub fn init(abs: RegisterAbs, mod: ?LocationMod) @This() {
        return .{ .abs = abs, .mod = mod };
    }

    pub fn initAbs(abs: RegisterAbs) @This() {
        return .init(abs, null);
    }

    pub fn initAdd(abs: RegisterAbs, x: usize) @This() {
        return .init(abs, .{ .add = x });
    }

    pub fn initSub(abs: RegisterAbs, x: usize) @This() {
        return .init(abs, .{ .sub = x });
    }

    pub fn applyMod(self: @This(), x: usize) usize {
        return if (self.mod) |mod| mod.apply(x) else x;
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (self.mod) |mod| {
            try writer.print("[%{t} {f}]", .{ self.abs, mod });
        } else {
            try writer.print("%{t}", .{self.abs});
        }
    }
};

pub const LocationAbs = union(enum) {
    ref: Ref,
    scope: ScopeLocation,
    stack: usize,
    register: RegisterAbs,
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

    pub fn toAddr(self: @This()) Location.Error!usize {
        return switch (self) {
            .data => |data| data.page * page_size + data.addr,
            .stack => |stack| stack_start + stack,
            .ref, .instruction, .scope, .register => Location.Error.UnsupportedAddrType,
        };
    }

    pub fn fromRef(name: []const u8, rel_stack_addr: usize) @This() {
        return .{ .ref = .{ .name = name, .rel_stack_addr = rel_stack_addr } };
    }

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .ref => |r| try w.print("{f}", .{r}),
            .scope => |s| try w.print("@{s}", .{s}),
            .data => |d| try w.print("#{}:{}", .{ d.page, d.addr }),
            .stack => |s| try w.print("S{}", .{s}),
            .instruction => |i| try w.print("I{f}", .{i}),
            .register => |r| try w.print("%{t}", .{r}),
        }
    }
};

pub const ScopeLocation = []const u8;
