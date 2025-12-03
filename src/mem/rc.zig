const std = @import("std");
const Allocator = std.mem.Allocator;

pub const RCError = Allocator.Error || error{ NotAllocated, InvalidRef };

const log_enabled = false;

pub fn RC(comptime T: type) type {
    return struct {
        allocator: Allocator,
        ptr: ?*T = null,
        refs: usize = 0,

        pub const Ref = struct {
            rc: ?*RC(T),

            pub fn init(allocator: Allocator, value: T) RCError!Ref {
                const rc = try RC(T).init(allocator, value);
                return try rc.ref();
            }

            pub fn dupe(allocator: Allocator, value: T) RCError!Ref {
                const rc = try RC(T).dupe(allocator, value);
                return try rc.ref();
            }

            pub fn print(
                allocator: Allocator,
                comptime fmt: []const u8,
                args: anytype,
            ) RCError!Ref {
                const rc = try RC(T).print(allocator, fmt, args);
                return try rc.ref();
            }

            pub fn get(self: Ref) RCError!T {
                if (self.rc) |rc| return rc.get();
                return RCError.InvalidRef;
            }

            pub fn getPtr(self: Ref) RCError!*T {
                if (self.rc) |rc| return rc.getPtr();
                return RCError.InvalidRef;
            }

            pub fn ref(self: Ref) RCError!Ref {
                if (self.rc) |rc| return rc.ref();
                return RCError.InvalidRef;
            }

            pub fn release(self: *Ref) void {
                if (self.rc) |rc| {
                    rc.release();
                } else {
                    std.log.warn("RC: called release on already released RC.Ref", .{});
                    return;
                }
                self.rc = null;
            }
        };

        pub fn init(allocator: Allocator, value: T) RCError!*RC(T) {
            const rcPtr = try allocator.create(RC(T));
            if (log_enabled) std.log.debug("rc init: {*}", .{rcPtr});
            rcPtr.* = try .initInner(allocator, value);
            return rcPtr;
        }

        pub fn dupe(allocator: Allocator, value: T) RCError!*RC(T) {
            if (!isSlice) @compileError("Cannot dupe non-slice types");
            const duped = try allocator.dupe(@typeInfo(T).pointer.child, value);
            return try .init(allocator, duped);
        }

        pub fn print(
            allocator: Allocator,
            comptime fmt: []const u8,
            args: anytype,
        ) RCError!*RC(T) {
            if (!isSlice) @compileError("Cannot print non-slice types");
            const Child = @typeInfo(T).pointer.child;
            if (Child != u8) @compileError("Cannot print non-byte slices");
            const buffer = try std.fmt.allocPrint(allocator, fmt, args);
            return try .init(allocator, buffer);
        }

        fn initInner(allocator: Allocator, value: T) RCError!RC(T) {
            const ptr = try allocator.create(T);
            ptr.* = value;

            return .{
                .allocator = allocator,
                .ptr = ptr,
            };
        }

        const isSlice = brk: {
            switch (@typeInfo(T)) {
                .pointer => |p| if (p.size == .slice) break :brk true,
                else => {},
            }

            break :brk false;
        };

        const isPtrOne = brk: {
            switch (@typeInfo(T)) {
                .pointer => |p| if (p.size == .one) break :brk true,
                else => {},
            }

            break :brk false;
        };

        fn deinit(self: *RC(T)) void {
            if (self.isAllocated()) |ptr| {
                if (std.meta.hasFn(T, "deinit")) {
                    ptr.deinit(self.allocator);
                } else if (isSlice) {
                    self.allocator.free(ptr.*);
                } else if (isPtrOne) {
                    self.allocator.destroy(ptr.*);
                }
                ptr.* = undefined;
                self.allocator.destroy(ptr);
            }
            self.ptr = null;
            self.refs = 0;
            self.allocator.destroy(self);
        }

        fn get(self: RC(T)) RCError!T {
            if (self.isPtrAllocated()) |ptr| return ptr.*;
            return RCError.NotAllocated;
        }

        fn getPtr(self: RC(T)) RCError!*T {
            if (self.isPtrAllocated()) |ptr| return ptr;
            return RCError.NotAllocated;
        }

        fn release(self: *RC(T)) void {
            if (self.isAllocated() == null) {
                std.log.warn("RC: called release on non-allocated released RC ({*})", .{self});
                return;
            }
            if (self.refs <= 1) {
                if (log_enabled) std.log.debug("{*}: deinit", .{self});
                self.deinit();
            } else {
                if (log_enabled) std.log.debug("{*}: releasing ref ({})", .{ self, self.refs });
                self.refs -= 1;
            }
        }

        fn isAllocated(self: *RC(T)) ?*T {
            return self.isPtrAllocated();
        }

        fn isPtrAllocated(self: RC(T)) ?*T {
            return self.ptr;
        }

        pub fn ref(self: *RC(T)) RCError!Ref {
            if (self.isAllocated() == null) return RCError.NotAllocated;
            self.refs += 1;
            return .{
                .rc = self,
            };
        }
    };
}

test "RC ref get" {
    const testing = @import("std").testing;
    const rc = try RC([]const u8).init(testing.allocator, "the string");
    try testing.expectEqual(0, rc.refs);
    var ref = try rc.ref();
    defer ref.release();
    try testing.expectEqual(1, rc.refs);
    var ref2 = try rc.ref();
    defer ref2.release();
    try testing.expectEqual(2, rc.refs);
    const refGet = try ref.getPtr();
    const ref2Get = try ref2.getPtr();
    try testing.expectEqual(2, rc.refs);
    try testing.expectEqual(rc.ptr, refGet);
    try testing.expectEqual(refGet, ref2Get);
    try testing.expectEqualStrings("the string", refGet.*);
}

test "RC ref release" {
    const testing = @import("std").testing;
    const rc = try RC([]const u8).init(testing.allocator, "the string");
    var ref = try rc.ref();
    ref.release();
}

test "RC ref release multiple" {
    const testing = @import("std").testing;
    const rc = try RC([]const u8).init(testing.allocator, "the string");
    var ref = try rc.ref();
    var ref2 = try rc.ref();
    try testing.expectEqual(2, rc.refs);
    ref.release();
    try testing.expectEqual(null, ref.rc);
    try testing.expectEqual(1, rc.refs);
    try testing.expect(rc.isAllocated() != null);
    ref2.release();
    try testing.expectEqual(null, ref2.rc);
}
