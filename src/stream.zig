const std = @import("std");
const Value = @import("interpreter/value.zig").Value;
const Transformer = @import("transformer.zig").Transformer;
const RC = @import("mem/root.zig").RC;
const RCError = @import("mem/root.zig").RCError;
const Closeable = @import("closeable.zig").Closeable;
const CloseableReader = @import("closeable.zig").CloseableReader;
const CloseableWriter = @import("closeable.zig").CloseableWriter;
const NeverCloses = @import("closeable.zig").NeverCloses;
const ExitCode = @import("runtime/command_runner.zig").ExitCode;
const rainbow = @import("rainbow.zig");
const TraceWriter = @import("trace-writer.zig").TraceWriter;

const log_enabled = false;
const log_writer_enabled = true;

const prefix_color = rainbow.beginColor(.indigo);
const source_color = rainbow.beginColor(.green);
const destination_color = rainbow.beginColor(.orange);
const end_color = rainbow.endColor();

fn log(self: *ReaderWriterStream, comptime fmt: []const u8, args: anytype) void {
    if (!log_enabled) return;
    std.log.debug("[{s}{*}:\"{s}\"{s}]", .{ prefix_color, self, self.label, end_color });
    if (self.source) |source| if (self.destination) |destination| {
        std.log.debug("{s}{s}:{*}{s} >>> {s}{s}:{*}{s}", .{
            source_color,
            source.getLabel(),
            source.reader,
            end_color,
            destination_color,
            destination.getLabel(),
            destination.writer,
            end_color,
        });
    };
    std.log.debug(fmt, args);
}

fn log_writer(self: *ReaderWriterStream, comptime fmt: []const u8, args: anytype) void {
    if (!log_writer_enabled) return;
    log(self, fmt, args);
}

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
    return JoinError(ReturnType(f));
}

pub fn StreamEvent(comptime T: type) type {
    return union(enum) {
        next: struct {
            payload: []T,
            allocator: std.mem.Allocator,
        },
        completed,

        pub const empty = initNext(
            std.heap.page_allocator,
            &.{},
        ) catch unreachable;

        pub fn initNext(
            allocator: std.mem.Allocator,
            payload: []T,
        ) std.mem.Allocator.Error!@This() {
            return .{
                .next = .{
                    .payload = try allocator.dupe(T, payload),
                    .allocator = allocator,
                },
            };
        }

        pub fn deinit(self: @This()) void {
            switch (self) {
                .next => |n| n.allocator.free(n.payload),
                .completed => {},
            }
        }
    };
}

pub const StreamError = RCError || error{
    InvalidSource,
    InitFailed,
    ReadFailed,
    WriteFailed,
};

fn FixedStream(comptime T: type) type {
    return struct {
        stream: Stream(T) = .init(&vtable),
        ref: RC(@This()).Ref = undefined,
        buffer: []T,
        index: ?usize = 0,

        const vtable = Stream(T).VTable{
            .next = next,
            .deinit = deinit,
            .getAllocator = getAllocator,
            .newRef = newRef,
        };

        pub fn init(
            allocator: std.mem.Allocator,
            buffer: []const T,
        ) std.mem.Allocator.Error!@This() {
            return .{ .buffer = try allocator.dupe(T, buffer) };
        }

        fn getParent(self: *Stream(T)) *@This() {
            return @fieldParentPtr("stream", self);
        }

        pub fn newRef(self: *Stream(T)) RCError!void {
            _ = try getParent(self).ref.ref(.{});
        }

        pub fn next(self: *Stream(T), limit: std.Io.Limit) StreamError!StreamEvent(T) {
            return getParent(self).nextInner(limit);
        }

        fn nextInner(self: *@This(), limit: std.Io.Limit) StreamError!StreamEvent(T) {
            const index = self.index orelse return .completed;
            if (index >= self.buffer.len) {
                self.index = null;
                return .completed;
            }

            const end = limit.minInt(self.buffer.len - index);
            const result = self.buffer[index..end];
            self.index = end;

            return .initNext(try self.stream.getAllocator(), result);
        }

        pub fn deinit(self: *Stream(T)) void {
            const allocator = self.getAllocator() catch null;
            const parent = getParent(self);
            if (allocator) |a| a.free(parent.buffer);
            parent.ref.deinit(.{ .refresh_ref = true });
        }

        pub fn getAllocator(self: *Stream(T)) RCError!std.mem.Allocator {
            return getParent(self).ref.allocator();
        }
    };
}

/// Consumer needs to free the return strings.
const IoReaderByteStream = struct {
    stream: Stream(u8) = .init(&vtable),
    ref: RC(@This()).Ref = undefined,
    source: *std.Io.Reader,
    is_completed: bool = false,

    const vtable = Stream(u8).VTable{
        .next = next,
        .deinit = deinit,
        .getAllocator = getAllocator,
        .newRef = newRef,
    };

    pub fn init(source: *std.Io.Reader) @This() {
        return .{
            .source = source,
        };
    }

    fn getParent(self: *Stream(u8)) *@This() {
        return @fieldParentPtr("stream", self);
    }

    pub fn newRef(self: *Stream(u8)) RCError!void {
        _ = try getParent(self).ref.ref(.{});
    }

    pub fn next(self: *Stream(u8), limit: std.Io.Limit) StreamError!StreamEvent(u8) {
        return getParent(self).nextInner(limit);
    }

    pub fn nextInner(self: *@This(), limit: std.Io.Limit) StreamError!StreamEvent(u8) {
        if (self.is_completed) return .completed;

        const allocator = try self.stream.getAllocator();
        var alloc_writer = std.Io.Writer.Allocating.init(allocator);
        defer alloc_writer.deinit();

        _ = self.source.stream(&alloc_writer.writer, limit) catch |err| switch (err) {
            std.Io.Reader.StreamError.EndOfStream => return .completed,
            std.Io.Reader.StreamError.ReadFailed => return StreamError.ReadFailed,
            std.Io.Reader.StreamError.WriteFailed => return StreamError.WriteFailed,
        };

        return .{ .next = try alloc_writer.toOwnedSlice() };
    }

    pub fn deinit(self: *Stream(u8)) void {
        const parent = getParent(self);
        parent.ref.deinit(.{ .refresh_ref = true });
    }

    pub fn getAllocator(self: *Stream(u8)) RCError!std.mem.Allocator {
        return getParent(self).ref.allocator();
    }
};

/// Consumer needs to free the return strings.
const IoWriterByteStream = struct {
    stream: Stream(u8) = .init(&vtable),
    ref: RC(@This()).Ref = undefined,
    destination: *std.Io.Writer,
    is_completed: bool = false,

    const vtable = Stream(u8).VTable{
        .next = next,
        .deinit = deinit,
        .getAllocator = getAllocator,
        .newRef = newRef,
    };

    pub fn init(source: *std.Io.Reader) @This() {
        return .{
            .source = source,
        };
    }

    fn getParent(self: *Stream(u8)) *@This() {
        return @fieldParentPtr("stream", self);
    }

    pub fn newRef(self: *Stream(u8)) RCError!void {
        _ = try getParent(self).ref.ref(.{});
    }

    pub fn next(self: *Stream(u8), limit: std.Io.Limit) StreamError!StreamEvent(u8) {
        return getParent(self).nextInner(limit);
    }

    pub fn nextInner(self: *@This(), limit: std.Io.Limit) StreamError!StreamEvent(u8) {
        if (self.is_completed) return .completed;

        const allocator = try self.stream.getAllocator();
        var alloc_writer = std.Io.Writer.Allocating.init(allocator);
        defer alloc_writer.deinit();

        _ = self.source.stream(&alloc_writer.writer, limit) catch |err| switch (err) {
            std.Io.Reader.StreamError.EndOfStream => return .completed,
            std.Io.Reader.StreamError.ReadFailed => return StreamError.ReadFailed,
            std.Io.Reader.StreamError.WriteFailed => return StreamError.WriteFailed,
        };

        return .{ .next = try alloc_writer.toOwnedSlice() };
    }

    pub fn deinit(self: *Stream(u8)) void {
        const parent = getParent(self);
        parent.ref.deinit(.{ .refresh_ref = true });
    }

    pub fn getAllocator(self: *Stream(u8)) RCError!std.mem.Allocator {
        return getParent(self).ref.allocator();
    }
};

pub const ReaderWriterStream = struct {
    stream: Stream(u8) = .init(&vtable),
    ref: RC(@This()).Ref = undefined,
    writer: std.Io.Writer,
    trace_writer: TraceWriter = undefined,
    buffer_writer: std.Io.Writer.Allocating,
    closeable: Closeable(ExitCode) = .{ .vtable = &closeable_vtable },
    source: ?CloseableReader(ExitCode) = null,
    destination: ?CloseableWriter(ExitCode) = null,
    is_completed: bool = false,
    label: []const u8,

    const vtable = Stream(u8).VTable{
        .next = next,
        .deinit = deinit,
        .getAllocator = getAllocator,
        .newRef = newRef,
    };

    const writer_vtable = std.Io.Writer.VTable{
        .drain = writer_drain,
        .sendFile = writer_sendFile,
        .flush = writer_flush,
        .rebase = writer_rebase,
    };

    const closeable_vtable = Closeable(ExitCode).VTable{
        .close = NeverCloses(ExitCode).close,
        .getResult = NeverCloses(ExitCode).getResult,
        .getLabel = closeable_getLabel,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        label: []const u8,
    ) std.mem.Allocator.Error!@This() {
        return .{
            .buffer_writer = .init(allocator),
            .label = label,
            .writer = .{
                .vtable = &writer_vtable,
                .buffer = try allocator.alloc(u8, 1024),
                //.buffer = &.{},
            },
        };
    }

    fn getParent(self: *Stream(u8)) *@This() {
        return @fieldParentPtr("stream", self);
    }

    pub fn connectSource(
        self: *@This(),
        source: CloseableReader(ExitCode),
    ) StreamError!void {
        log(
            self,
            @src().fn_name ++ ": ({s}) {*} + {*}",
            .{ source.getLabel(), source.reader, source.closeable },
        );

        self.source = source;
    }

    pub fn connectDestination(
        self: *@This(),
        destination: CloseableWriter(ExitCode),
    ) StreamError!void {
        log(
            self,
            @src().fn_name ++ ": ({s}) {*} + {*}",
            .{ destination.getLabel(), destination.writer, destination.closeable },
        );

        try self.writer.flush();
        self.destination = destination;
        self.trace_writer = .init(
            try self.buffer_writer.allocator.alloc(u8, 1024),
            destination.writer,
            self.label,
        );
        // self.writer.buffer = destination.writer.buffer;
        const buffered = try self.buffer_writer.toOwnedSlice();
        try self.writer.writeAll(buffered);
        try self.writer.flush();
        log(self, "bytes forwarded: {}", .{buffered.len});
        if (self.source) |source| if (source.isClosed()) {
            _ = destination.close();
        };
    }

    pub fn newRef(self: *Stream(u8)) RCError!void {
        const parent = getParent(self);
        log(parent, @src().fn_name, .{});

        _ = try parent.ref.ref(.{});
    }

    pub fn next(self: *Stream(u8), limit: std.Io.Limit) StreamError!StreamEvent(u8) {
        return getParent(self).nextInner(limit);
    }

    pub fn nextInner(_: *@This(), _: std.Io.Limit) StreamError!StreamEvent(u8) {
        return .completed;
    }

    pub fn deinit(self: *Stream(u8)) void {
        getParent(self).deinitParent();
    }

    pub fn deinitParent(self: *@This()) void {
        log(self, @src().fn_name, .{});

        self.buffer_writer.allocator.free(self.writer.buffer);
        if (self.destination) |_| self.buffer_writer.allocator.free(self.trace_writer.writer.buffer);
        self.buffer_writer.deinit();
        self.ref.deinit(.{ .refresh_ref = true });
    }

    pub fn getAllocator(self: *Stream(u8)) RCError!std.mem.Allocator {
        return getParent(self).ref.allocator();
    }

    fn getDestinationWriter(self: *@This()) *std.Io.Writer {
        const destination = self.destination orelse return &self.buffer_writer.writer;
        _ = destination;
        return &self.trace_writer.writer;
        //return destination.writer;
    }

    fn writer_getParent(self: *std.Io.Writer) *@This() {
        return @fieldParentPtr("writer", self);
    }

    fn writer_getDestination(self: *std.Io.Writer) *std.Io.Writer {
        return writer_getParent(self).getDestinationWriter();
    }

    fn writer_drain(
        self: *std.Io.Writer,
        data: []const []const u8,
        splat: usize,
    ) std.Io.Writer.Error!usize {
        const destination = writer_getDestination(self);
        log_writer(
            writer_getParent(self),
            "." ++ @src().fn_name ++ " >>> {*} ({})",
            .{ destination, destination.buffered().len },
        );
        const buffer_written = try destination.writeSplat(&.{self.buffered()}, 1);
        self.end -= buffer_written;

        var written: usize = 0;

        if (self.end == 0) {
            written = try destination.writeSplat(data, splat);
        }

        try destination.flush();
        return written;
    }

    fn writer_sendFile(
        self: *std.Io.Writer,
        file_reader: *std.fs.File.Reader,
        limit: std.Io.Limit,
    ) std.Io.Writer.FileError!usize {
        const destination = writer_getDestination(self);
        log_writer(
            writer_getParent(self),
            "." ++ @src().fn_name ++ " >>> {*} ({})",
            .{ destination, destination.buffered().len },
        );
        const written = try destination.sendFile(file_reader, limit);
        try destination.flush();
        return written;
    }

    fn writer_flush(self: *std.Io.Writer) std.Io.Writer.Error!void {
        const destination = writer_getDestination(self);
        log_writer(
            writer_getParent(self),
            "." ++ @src().fn_name ++ " >>> {*} ({*}:{})",
            .{ destination, destination.buffer, destination.buffered().len },
        );
        return self.defaultFlush();
    }

    fn writer_rebase(
        self: *std.Io.Writer,
        preserve: usize,
        capacity: usize,
    ) std.Io.Writer.Error!void {
        const destination = writer_getDestination(self);
        log_writer(
            writer_getParent(self),
            "." ++ @src().fn_name ++ " >>> {*} ({})",
            .{ destination, destination.buffered().len },
        );
        return self.defaultRebase(preserve, capacity);
    }

    pub fn closeable_getLabel(self: *Closeable(ExitCode)) []const u8 {
        const parent: *@This() = @fieldParentPtr("closeable", self);
        return parent.label;
    }

    pub fn closeableWriter(self: *@This()) CloseableWriter(ExitCode) {
        return .{
            .writer = &self.writer,
            .closeable = &self.closeable,
        };
    }

    pub const ForwardEvent = enum { no_source, closed, not_done };

    /// Returns true if source is completed.
    pub fn forward(self: *@This(), limit: std.Io.Limit) std.Io.Reader.StreamError!ForwardEvent {
        log(self, @src().fn_name, .{});

        const source = self.source orelse {
            log(self, "no source connected", .{});
            return .no_source;
        };
        // const destination_writer = self.getDestinationWriter();

        log(self, "checking isClosed: {?f}", .{source.getResult()});
        if (source.isClosed()) {
            return .closed;
        }

        const bytes_forwarded = source.reader.stream(
            &self.writer,
            limit,
        ) catch |err| switch (err) {
            error.EndOfStream => {
                log(self, "ended stream", .{});
                _ = self.source.?.close();
                if (self.destination) |d| _ = d.close();
                return .closed;
            },
            else => {
                _ = self.source.?.close();
                if (self.destination) |d| _ = d.close();
                return .closed;
            },
        };
        try self.writer.flush();
        // try destination_writer.flush();

        log(self, "bytes forwarded: {}", .{bytes_forwarded});

        return .not_done;
    }

    pub fn disconnectSource(self: *ReaderWriterStream) void {
        self.source = null;
    }
};

// fn ByteTransformerStream(comptime T: type) type {
//     return struct {
//         source: *Stream([]const u8),
//         stream: Stream(T) = .init(&vtable),
//         /// Buffer that is going to hold the data of the next item.
//         buffer: [@sizeOf(T)]u8 = undefined,
//         /// Cursor into buffer where we have written up until now for the current item being read.
//         cursor: usize = 0,
//
//         const vtable = Stream(T).VTable{
//             .next = next,
//             .deinit = deinit,
//             .getAllocator = getAllocator,
//             .newRef = newRef,
//         };
//
//         pub fn init(source: *Stream([]const u8)) RCError!@This() {
//             try source.newRef();
//
//             return .{
//                 .source = source,
//             };
//         }
//
//         fn getParent(self: *Stream(T)) *@This() {
//             return @fieldParentPtr("stream", self);
//         }
//
//         pub fn newRef(self: *Stream(T)) RCError!void {
//             _ = try getParent(self).ref.ref(.{});
//         }
//
//         pub fn next(self: *Stream(T)) StreamError!?StreamEvent([]const u8) {
//             return getParent(self).nextInner();
//         }
//
//         pub fn nextInner(self: *@This()) StreamError!?StreamEvent([]const u8) {
//             const source_next = try self.source.next() orelse return null;
//
//             switch (source_next) {
//                 .completed => {
//                     if (self.cursor != 0) {
//                         return StreamError.ReadAborted;
//                     }
//
//                     return .completed;
//                 },
//                 .next => |n| return self.produce(n),
//             }
//         }
//
//         fn produce(self: *@This(), n: []const u8) StreamEvent(T) {
//             // If we can fill up the buffer, transform it from a string of bytes to T and return it, fill up buffer with eventual rest bytes
//             // If we can't just fill up until the source is empty and set cursor to new position
//             //
//
//             var n_cursor: usize = 0;
//
//             while(true) {
//                 const bytes_needed = self.buffer.len - self.cursor;
//                 const bytes_source_left = n.len - n_cursor;
//
//                 if (bytes_source_left >= bytes_needed) {
//
//                 }
//             }
//         }
//
//         pub fn deinit(self: *Stream(T)) void {
//             const parent = getParent(self);
//             parent.source.deinit();
//             parent.ref.deinit(.{ .refresh_ref = true });
//         }
//
//         pub fn getAllocator(self: *Stream(T)) RCError!std.mem.Allocator {
//             return getParent(self).ref.allocator();
//         }
//     };
// }

fn MappedStream(comptime In: type, comptime Out: type) type {
    const MapFn = *const fn (std.mem.Allocator, In) StreamError!Out;

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

        pub fn next(self: *Stream(Out), limit: std.Io.Limit) StreamError!StreamEvent(Out) {
            return getParent(self).nextInner(limit);
        }

        pub fn nextInner(self: *@This(), limit: std.Io.Limit) StreamError!StreamEvent(Out) {
            const source_next = try self.source.next(limit);

            return switch (source_next) {
                .completed => .completed,
                .next => |n| {
                    defer source_next.deinit();
                    const allocator = try self.stream.getAllocator();
                    const result = try allocator.alloc(Out, n.payload.len);
                    defer allocator.free(result);

                    for (result, n.payload) |*out, in| out.* = self.mapFn(allocator, in) catch |err| {
                        self.stream.err = err;
                        return err;
                    };

                    return .initNext(allocator, result);
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

        pub fn next(self: *Stream(T), _: std.Io.Limit) StreamError!StreamEvent(T) {
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
            next: *const fn (*Stream(T), std.Io.Limit) StreamError!StreamEvent(T),
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
            const single_stream_ref = try RC(FixedStream(T)).Ref.init(
                allocator,
                try .init(allocator, &.{item}),
                .{},
            );
            var single_stream = try single_stream_ref.getPtr();
            single_stream.ref = single_stream_ref;
            return &single_stream.stream;
        }

        pub fn initFixed(allocator: std.mem.Allocator, buffer: []const T) RCError!*Stream(T) {
            const buffered_stream_ref = try RC(FixedStream(T)).Ref.init(
                allocator,
                try .init(allocator, buffer),
                .{},
            );
            var buffered_stream = try buffered_stream_ref.getPtr();
            buffered_stream.ref = buffered_stream_ref;
            return &buffered_stream.stream;
        }

        pub fn initReaderWriter(
            allocator: std.mem.Allocator,
            label: []const u8,
        ) RCError!*ReaderWriterStream {
            if (T != u8) @compileError(
                @src().fn_name ++ " is only available on Stream(u8), actual: Stream(" ++ @typeName(T) ++ ")",
            );

            const reader_writer_stream_ref = try RC(ReaderWriterStream).Ref.init(
                allocator,
                try .init(allocator, label),
                .{},
            );
            var reader_writer_stream = try reader_writer_stream_ref.getPtr();
            reader_writer_stream.ref = reader_writer_stream_ref;

            log(reader_writer_stream, @src().fn_name, .{});
            log(reader_writer_stream, "{*}", .{&reader_writer_stream.writer});

            return reader_writer_stream;
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

        /// Stream owns the returned slice if StreamEvent is next.
        pub fn next(
            self: *@This(),
            limit: std.Io.Limit,
        ) StreamError!StreamEvent(T) {
            if (self.err) |err| return err;
            return self.vtable.next(self, limit);
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
                const stream_next = self.next(.unlimited) catch |err| return .{
                    .err = err,
                    .list = try list.toOwnedSlice(allocator),
                };
                defer stream_next.deinit();

                switch (stream_next) {
                    .completed => break,
                    .next => |n| try list.appendSlice(allocator, n.payload),
                }
            }

            return .{ .list = try list.toOwnedSlice(allocator) };
        }

        /// Caller needs to deinit returned value.
        pub fn stream(
            self: *@This(),
            w: *std.Io.Writer,
            limit: std.Io.Limit,
        ) StreamError!StreamEvent(T) {
            if (T != u8) @compileError(
                @src().fn_name ++ " is only available on Stream(u8), actual: Stream(" ++ @typeName(T) ++ ")",
            );

            const e = try self.next(limit);

            switch (e) {
                .completed => {},
                .next => |n| try w.writeAll(n.payload),
            }

            return e;
        }
    };
}
