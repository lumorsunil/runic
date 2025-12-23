const std = @import("std");

pub fn Closeable(comptime T: type) type {
    return struct {
        vtable: *const VTable,

        const Self = @This();

        pub const VTable = struct {
            close: *const fn (*Self) T,
            getResult: *const fn (*Self) ?T,
            getLabel: *const fn (*Self) []const u8,
        };

        pub fn close(self: *@This()) T {
            return self.vtable.close(self);
        }

        pub fn getResult(self: *@This()) ?T {
            return self.vtable.getResult(self);
        }

        pub fn getLabel(self: *@This()) []const u8 {
            return self.vtable.getLabel(self);
        }

        pub fn isClosed(self: *@This()) bool {
            return self.getResult() != null;
        }
    };
}

pub fn ManualCloseable(comptime T: type) type {
    return struct {
        result: ?T = null,
        closeable: Closeable(T) = .{ .vtable = &vtable },

        const vtable = Closeable(T).VTable{
            .close = close,
            .getResult = getResult,
            .getLabel = getLabel,
        };

        fn close(self: *Closeable(T)) T {
            const parent: *@This() = @fieldParentPtr("closeable", self);
            parent.result = parent.result orelse .{ .Exited = 0 };
            return parent.result.?;
        }

        fn getResult(self: *Closeable(T)) ?T {
            const parent: *@This() = @fieldParentPtr("closeable", self);
            return parent.result;
        }

        pub fn setResult(self: *@This(), result: T) void {
            self.result = result;
            _ = self.closeable.close();
        }

        pub fn getLabel(self: *Closeable(T)) []const u8 {
            const parent: *@This() = @fieldParentPtr("closeable", self);
            return parent.label;
        }
    };
}

pub fn CloseableReader(comptime T: type) type {
    return struct {
        reader: *std.Io.Reader,
        closeable: *Closeable(T),

        pub fn init(reader: *std.Io.Reader, closeable: *Closeable(T)) @This() {
            return .{
                .reader = reader,
                .closeable = closeable,
            };
        }

        pub fn close(self: @This()) T {
            return self.closeable.close();
        }

        pub fn isClosed(self: @This()) bool {
            return self.closeable.isClosed();
        }

        pub fn getResult(self: @This()) ?T {
            return self.closeable.getResult();
        }

        pub fn getLabel(self: @This()) []const u8 {
            return self.closeable.getLabel();
        }
    };
}

pub fn CloseableWriter(comptime T: type) type {
    return struct {
        writer: *std.Io.Writer,
        closeable: *Closeable(T),

        pub fn init(writer: *std.Io.Writer, closeable: *Closeable(T)) @This() {
            return .{
                .writer = writer,
                .closeable = closeable,
            };
        }

        pub fn close(self: @This()) T {
            return self.closeable.close();
        }

        pub fn isClosed(self: @This()) bool {
            return self.closeable.isClosed();
        }

        pub fn getLabel(self: @This()) []const u8 {
            return self.closeable.getLabel();
        }
    };
}

pub fn NeverCloses(comptime T: type) type {
    return struct {
        closeable: Closeable(T) = .{ .vtable = &vtable },
        label: []const u8,

        pub const vtable = Closeable(T).VTable{
            .close = close,
            .getResult = getResult,
            .getLabel = getLabel,
        };

        pub fn close(_: *Closeable(T)) T {
            return undefined;
        }

        pub fn getResult(_: *Closeable(T)) ?T {
            return null;
        }

        pub fn getLabel(self: *Closeable(T)) []const u8 {
            const parent: *@This() = @fieldParentPtr("closeable", self);
            return parent.label;
        }
    };
}
