const std = @import("std");
const Allocator = std.mem.Allocator;

pub const RCError = Allocator.Error || error{ NotAllocated, DoubleRelease, InvalidRef };

pub fn RC(comptime T: type) type {
    return struct {
        allocator: Allocator,
        ptr: ?*T = null,
        refs: usize = 0,

        pub const Ref = struct {
            rc: ?*RC(T),

            pub fn get(self: Ref) RCError!*T {
                if (self.rc) |rc| return rc.get();
                return RCError.InvalidRef;
            }

            pub fn release(self: *Ref) RCError!void {
                if (self.rc) |rc| {
                    try rc.release();
                } else {
                    return RCError.DoubleRelease;
                }
                self.rc = null;
            }
        };

        pub fn init(allocator: Allocator, value: T) !*RC(T) {
            const rcPtr = try allocator.create(RC(T));
            rcPtr.* = try .initInner(allocator, value);
            return rcPtr;
        }

        fn initInner(allocator: Allocator, value: T) RCError!RC(T) {
            const ptr = try allocator.create(T);
            ptr.* = value;

            return .{
                .allocator = allocator,
                .ptr = ptr,
            };
        }

        fn deinit(self: *RC(T)) void {
            if (self.isAllocated()) |ptr| {
                ptr.* = undefined;
                self.allocator.destroy(ptr);
            }
            self.ptr = null;
            self.refs = 0;
            self.allocator.destroy(self);
        }

        fn get(self: RC(T)) RCError!*T {
            if (self.isPtrAllocated()) |ptr| return ptr;
            return RCError.NotAllocated;
        }

        fn release(self: *RC(T)) RCError!void {
            if (self.isAllocated() == null) return RCError.NotAllocated;
            if (self.refs <= 1) {
                self.deinit();
            } else {
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
    defer rc.deinit();
    try testing.expectEqual(0, rc.refs);
    const ref = try rc.ref();
    try testing.expectEqual(1, rc.refs);
    const ref2 = try rc.ref();
    try testing.expectEqual(2, rc.refs);
    const refGet = try ref.get();
    const ref2Get = try ref2.get();
    try testing.expectEqual(2, rc.refs);
    try testing.expectEqual(rc.ptr, refGet);
    try testing.expectEqual(refGet, ref2Get);
    try testing.expectEqualStrings("the string", refGet.*);
}

test "RC ref release" {
    const testing = @import("std").testing;
    const rc = try RC([]const u8).init(testing.allocator, "the string");
    var ref = try rc.ref();
    try ref.release();
}

test "RC ref release multiple" {
    const testing = @import("std").testing;
    const rc = try RC([]const u8).init(testing.allocator, "the string");
    var ref = try rc.ref();
    var ref2 = try rc.ref();
    try testing.expectEqual(2, rc.refs);
    try ref.release();
    try testing.expectEqual(1, rc.refs);
    try testing.expect(rc.isAllocated() != null);
    try ref2.release();
}

test "RC ref release error" {
    const testing = @import("std").testing;
    var rc = try RC([]const u8).init(testing.allocator, "the string");
    var ref = try rc.ref();
    try ref.release();
    try testing.expectError(RCError.DoubleRelease, ref.release());
}
