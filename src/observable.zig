const std = @import("std");
const Stream = @import("stream.zig").Stream;

pub fn Observable(comptime T: type) type {
    return struct {
        vtable: *const VTable,

        pub const VTable = struct {
            subscribe: *const fn (*Observable(T)) *Stream(T),
        };

        pub fn subscribe(self: *Observable(T)) *Stream(T) {
            return self.vtable.subscribe(self);
        }
    };
}

pub fn BufferObservable(comptime T: type) type {
    return struct {
        observable: Observable(T) = .{ .vtable = &vtable },
        buffer: []T,

        const vtable = Observable(T).VTable{
            .subscribe = subscribe,
        };

        fn getParent(self: *Observable(T)) *@This() {
            return @fieldParentPtr("observable", self);
        }

        pub fn subscribe(
            self: *Observable(T),
            allocator: std.mem.Allocator,
        ) *Stream(T) {
            return Stream(T).initBuffer(allocator, getParent(self).buffer);
        }
    };
}
