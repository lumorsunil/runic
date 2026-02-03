const std = @import("std");
const Ref = @import("ref.zig").Ref;
const InstructionAddr = @import("instruction-addr.zig").InstructionAddr;
const ir = @import("../ir.zig");
const page_size = ir.context.page_size;
const stack_start = ir.context.stack_start;

pub const Location = struct {
    abs: LocationAbs,
    mod: ?LocationMod = null,
    options: Options,

    pub const Options = struct {
        dereference: bool = false,
    };

    pub const Error = error{UnsupportedAddrType};

    pub const noll: @This() = .initAbs(.{ .heap = 0 }, .{});

    pub fn init(abs: LocationAbs, mod: ?LocationMod, options: Options) @This() {
        return .{
            .abs = abs,
            .mod = mod,
            .options = options,
        };
    }

    pub fn initRegister(reg: RegisterAbs) @This() {
        return .init(.{ .register = reg }, null, .{});
    }

    pub fn initAbs(abs: LocationAbs, options: Options) @This() {
        return .init(abs, null, options);
    }

    pub fn initAdd(abs: LocationAbs, x: usize, options: Options) @This() {
        return .init(abs, .{ .add = x }, options);
    }

    pub fn initSub(abs: LocationAbs, x: usize, options: Options) @This() {
        return .init(abs, .{ .sub = x }, options);
    }

    pub fn size() usize {
        return @sizeOf(@This());
    }

    pub fn isNoll(self: @This()) bool {
        return self.abs == .heap and (self.toAddr() catch unreachable) == 0;
    }

    pub fn toAddr(self: @This()) Error!usize {
        const abs = try self.abs.toAddr();
        return self.applyMod(abs);
    }

    pub fn toAddrWithContext(
        self: @This(),
        stack_frame: usize,
        closure_addr: usize,
    ) Error!usize {
        const abs = try self.abs.toAddrWithContext(stack_frame, closure_addr);
        return self.applyMod(abs);
    }

    pub fn applyMod(self: @This(), x: usize) usize {
        return if (self.mod) |mod| mod.apply(x) else x;
    }

    pub fn dereference(self: @This()) @This() {
        var clone = self;
        clone.options.dereference = true;
        return clone;
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (self.options.dereference) try writer.writeByte('[');
        if (self.mod) |mod| {
            try writer.print("{f}{f}", .{ self.abs, mod });
        } else {
            try writer.print("{f}", .{self.abs});
        }
        if (self.options.dereference) try writer.writeByte(']');
    }
};

pub const LocationMod = union(enum) {
    add: usize,
    sub: usize,

    pub const empty = LocationMod{ .add = 0 };

    pub fn apply(self: @This(), x: usize) usize {
        return switch (self) {
            .add => |y| x + y,
            .sub => |y| x - y,
        };
    }

    pub fn applyMaybe(self: ?@This(), x: usize) usize {
        const mod = self orelse return x;
        return mod.apply(x);
    }

    pub fn merge(self: ?@This(), other: ?@This()) @This() {
        const o = other orelse return self;

        return switch (self) {
            .add => |x| switch (o) {
                .add => |y| .{ .add = x + y },
                .sub => |y| if (x > y) .{ .add = x - y } else .{ .sub = y - x },
            },
            .sub => |x| switch (o) {
                .sub => |y| .{ .sub = x + y },
                .add => |y| if (x < y) .{ .add = y - x } else .{ .sub = x - y },
            },
        };
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .add => |x| try writer.print("+{}", .{x}),
            .sub => |x| try writer.print("-{}", .{x}),
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
    stack: usize,
    heap: usize,
    closure,
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
            .heap => |heap| heap,
            .ref, .instruction, .register, .closure => Location.Error.UnsupportedAddrType,
        };
    }

    pub fn toAddrWithContext(
        self: @This(),
        stack_frame: usize,
        closure_addr: usize,
    ) Location.Error!usize {
        return switch (self) {
            .ref => |ref| ref.rel_stack_addr + stack_frame,
            .closure => closure_addr,
            else => self.toAddr(),
        };
    }

    pub fn fromRef(name: []const u8, rel_stack_addr: usize) @This() {
        return .{ .ref = .{ .name = name, .rel_stack_addr = rel_stack_addr } };
    }

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .ref => |r| try w.print("{f}", .{r}),
            .data => |d| try w.print("#{}:{}", .{ d.page, d.addr }),
            .stack => |s| try w.print("S{}", .{s}),
            .heap => |s| try w.print("H{}", .{s}),
            .instruction => |i| try w.print("I{f}", .{i}),
            .register => |r| try w.print("%{t}", .{r}),
            .closure => try w.print("C", .{}),
        }
    }
};

pub const ScopeLocation = []const u8;
