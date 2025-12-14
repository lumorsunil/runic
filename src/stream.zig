const std = @import("std");
const Value = @import("interpreter/value.zig").Value;
const Transformer = @import("transformer.zig").Transformer;
const RC = @import("mem/root.zig").RC;
const RCError = @import("mem/root.zig").RCError;

fn ReturnType(f: anytype) type {
    return @typeInfo(@TypeOf(f)).@"fn".return_type orelse void;
}

fn JoinError(comptime T: type) type {
    return @typeInfo(T).error_union.payload;
}

fn JoinOptional(comptime T: type) type {
    return @typeInfo(T).optional.child;
}

fn MapFnResult(f: anytype) type {
    return JoinOptional(JoinError(ReturnType(f)));
}

pub fn StreamEvent(comptime T: type) type {
    return union(enum) {
        next: T,
        completed,
    };
}

pub const StreamError = RCError || error{
    InvalidSource,
    InitFailed,
};

fn BufferedStream(comptime T: type) type {
    return struct {
        stream: Stream(T) = .init(&vtable),
        ref: RC(@This()).Ref = undefined,
        buffer: []const T,
        index: ?usize = 0,

        const vtable = Stream(T).VTable{
            .next = next,
            .deinit = deinit,
            .getAllocator = getAllocator,
            .newRef = newRef,
        };

        pub fn init(buffer: []const T) @This() {
            return .{ .buffer = buffer };
        }

        fn getParent(self: *Stream(T)) *@This() {
            return @fieldParentPtr("stream", self);
        }

        pub fn newRef(self: *Stream(T)) RCError!void {
            _ = try getParent(self).ref.ref(.{});
        }

        pub fn next(self: *Stream(T)) StreamError!?StreamEvent(T) {
            return getParent(self).nextInner();
        }

        fn nextInner(self: *@This()) StreamError!?StreamEvent(T) {
            const index = self.index orelse return .completed;
            if (index >= self.buffer.len) {
                self.index = null;
                return .completed;
            }
            defer self.index = index + 1;
            return .{ .next = self.buffer[index] };
        }

        pub fn deinit(self: *Stream(T)) void {
            getParent(self).ref.deinit(.{ .refresh_ref = true });
        }

        pub fn getAllocator(self: *Stream(T)) RCError!std.mem.Allocator {
            return getParent(self).ref.allocator();
        }
    };
}

fn SingleStream(comptime T: type) type {
    return struct {
        stream: Stream(T) = .init(&vtable),
        ref: RC(@This()).Ref = undefined,
        value: T,
        consumed: bool = false,

        const vtable = Stream(T).VTable{
            .next = next,
            .deinit = deinit,
            .getAllocator = getAllocator,
            .newRef = newRef,
        };

        pub fn init(value: T) @This() {
            return .{
                .value = value,
            };
        }

        fn getParent(self: *Stream(T)) *@This() {
            return @fieldParentPtr("stream", self);
        }

        pub fn newRef(self: *Stream(T)) RCError!void {
            _ = try getParent(self).ref.ref(.{});
        }

        pub fn next(self: *Stream(T)) StreamError!?StreamEvent(T) {
            return getParent(self).nextInner();
        }

        fn nextInner(self: *@This()) StreamError!?StreamEvent(T) {
            if (self.consumed) return .completed;
            self.consumed = true;
            return .{ .next = self.value };
        }

        pub fn deinit(self: *Stream(T)) void {
            getParent(self).ref.deinit(.{ .refresh_ref = true });
        }

        pub fn getAllocator(self: *Stream(T)) RCError!std.mem.Allocator {
            return getParent(self).ref.allocator();
        }
    };
}

fn MappedStream(comptime In: type, comptime Out: type) type {
    const MapFn = *const fn (std.mem.Allocator, In) StreamError!?Out;

    return struct {
        stream: Stream(Out) = .init(&vtable),
        ref: RC(@This()).Ref = undefined,
        source: *Stream(In),
        mapFn: MapFn,

        const vtable = Stream(Out).VTable{
            .next = next,
            .deinit = deinit,
            .getAllocator = getAllocator,
            .newRef = newRef,
        };

        pub fn init(source: *Stream(In), mapFn: MapFn) RCError!@This() {
            try source.newRef();

            return .{
                .source = source,
                .mapFn = mapFn,
            };
        }

        fn getParent(self: *Stream(Out)) *@This() {
            return @fieldParentPtr("stream", self);
        }

        pub fn newRef(self: *Stream(Out)) RCError!void {
            _ = try getParent(self).ref.ref(.{});
        }

        pub fn next(self: *Stream(Out)) StreamError!?StreamEvent(Out) {
            return getParent(self).nextInner();
        }

        pub fn nextInner(self: *@This()) StreamError!?StreamEvent(Out) {
            const source_next = try self.source.next() orelse return null;
            return switch (source_next) {
                .completed => .completed,
                .next => |n| {
                    const result = self.mapFn(try self.ref.allocator(), n) catch |err| {
                        self.stream.err = err;
                        return err;
                    } orelse return null;

                    return .{ .next = result };
                },
            };
        }

        pub fn deinit(self: *Stream(Out)) void {
            const parent = getParent(self);
            parent.source.deinit();
            parent.ref.deinit(.{ .refresh_ref = true });
        }

        pub fn getAllocator(self: *Stream(Out)) RCError!std.mem.Allocator {
            return getParent(self).ref.allocator();
        }
    };
}

fn FailedStream(comptime T: type) type {
    return struct {
        stream: Stream(T) = .init(&vtable),
        ref: RC(@This()).Ref = undefined,

        const vtable = Stream(T).VTable{
            .next = next,
            .deinit = deinit,
            .getAllocator = getAllocator,
            .newRef = newRef,
        };

        pub fn init(err: StreamError) @This() {
            return .{
                .ref = undefined,
                .stream = .{ .vtable = &vtable, .err = err },
            };
        }

        fn getParent(self: *Stream(T)) *@This() {
            return @fieldParentPtr("stream", self);
        }

        pub fn newRef(self: *Stream(T)) RCError!void {
            _ = try getParent(self).ref.ref(.{});
        }

        pub fn next(self: *Stream(T)) StreamError!?StreamEvent(T) {
            return self.err.?;
        }

        pub fn deinit(self: *Stream(T)) void {
            getParent(self).ref.deinit(.{ .refresh_ref = true });
        }

        pub fn getAllocator(self: *Stream(T)) RCError!std.mem.Allocator {
            return getParent(self).ref.allocator();
        }
    };
}

pub fn Stream(comptime T: type) type {
    return struct {
        completed: bool = false,
        err: ?StreamError = null,
        vtable: *const VTable,

        pub const VTable = struct {
            next: *const fn (*Stream(T)) StreamError!?StreamEvent(T),
            deinit: *const fn (*Stream(T)) void,
            getAllocator: *const fn (*Stream(T)) RCError!std.mem.Allocator,
            newRef: *const fn (*Stream(T)) RCError!void,
        };

        fn init(vtable: *const VTable) Stream(T) {
            return .{ .vtable = vtable };
        }

        pub fn initFailed(allocator: std.mem.Allocator, err: StreamError) RCError!*Stream(T) {
            const failed_stream_ref = try RC(FailedStream(T)).Ref.init(
                allocator,
                .init(err),
                .{},
            );
            var failed_stream = try failed_stream_ref.getPtr();
            failed_stream.ref = failed_stream_ref;
            return &failed_stream.stream;
        }

        pub fn initSingle(allocator: std.mem.Allocator, item: T) RCError!*Stream(T) {
            const single_stream_ref = try RC(SingleStream(T)).Ref.init(
                allocator,
                .init(item),
                .{},
            );
            var single_stream = try single_stream_ref.getPtr();
            single_stream.ref = single_stream_ref;
            return &single_stream.stream;
        }

        pub fn initBuffer(allocator: std.mem.Allocator, buffer: []const T) RCError!*Stream(T) {
            const buffered_stream_ref = try RC(BufferedStream(T)).Ref.init(
                allocator,
                .init(buffer),
                .{},
            );
            var buffered_stream = try buffered_stream_ref.getPtr();
            buffered_stream.ref = buffered_stream_ref;
            return &buffered_stream.stream;
        }

        pub fn fromValue(allocator: std.mem.Allocator, value: Value) RCError!*Stream(T) {
            return Transformer(*@This()).transform(
                allocator,
                value,
            ) catch try @This().initFailed(
                allocator,
                StreamError.InitFailed,
            ) orelse try @This().initFailed(
                allocator,
                StreamError.InvalidSource,
            );
        }

        pub fn deinit(self: *@This()) void {
            return self.vtable.deinit(self);
        }

        pub fn newRef(self: *@This()) RCError!void {
            return self.vtable.newRef(self);
        }

        pub fn next(self: *@This()) StreamError!?StreamEvent(T) {
            if (self.err) |err| return err;
            return self.vtable.next(self);
        }

        pub fn getAllocator(self: *@This()) RCError!std.mem.Allocator {
            return self.vtable.getAllocator(self);
        }

        pub fn clone(self: *@This()) RCError!*@This() {
            try self.newRef();
            return self;
        }

        pub fn map(
            self: *@This(),
            mapFn: anytype,
        ) RCError!*Stream(MapFnResult(mapFn)) {
            const mapped_stream_ref = try RC(MappedStream(T, MapFnResult(mapFn))).Ref.init(
                try self.getAllocator(),
                try .init(self, mapFn),
                .{},
            );
            var mapped_stream = try mapped_stream_ref.getPtr();
            mapped_stream.ref = mapped_stream_ref;
            return &mapped_stream.stream;
        }

        pub const TakeAllResult = struct {
            list: []T = &.{},
            err: ?StreamError = null,
        };

        /// Is blocking until underlying stream has ended or failed.
        pub fn takeAll(self: *@This()) RCError!TakeAllResult {
            const allocator = try self.getAllocator();
            var list = std.ArrayList(T).empty;

            while (true) {
                if (self.next() catch |err| return .{
                    .err = err,
                    .list = try list.toOwnedSlice(allocator),
                }) |e| switch (e) {
                    .completed => break,
                    .next => |n| try list.append(allocator, n),
                };
            }

            return .{ .list = try list.toOwnedSlice(allocator) };
        }
    };
}
