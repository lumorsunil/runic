const std = @import("std");

pub const DelimiterType = enum { scalar };
fn DelimiterFn(comptime Ctx: type, comptime T: type) type {
    return *const fn (Ctx, T) bool;
}

pub fn splitScalarByFn(
    comptime T: type,
    ctx: anytype,
    haystack: []const T,
    needle: DelimiterFn(@TypeOf(ctx), T),
) SplitIteratorByFn(@TypeOf(ctx), T, .scalar) {
    return .{
        .buffer = haystack,
        .delimiter = needle,
        .ctx = ctx,
    };
}

pub fn SplitIteratorByFn(
    comptime Ctx: type,
    comptime T: type,
    comptime delimiter_type: DelimiterType,
) type {
    return struct {
        buffer: []const T,
        index: ?usize = 0,
        delimiter: switch (delimiter_type) {
            .scalar => DelimiterFn(Ctx, T),
        },
        ctx: Ctx,

        const Self = @This();

        /// Returns a slice of the first field.
        /// Call this only to get the first field and then use `next` to get all subsequent fields.
        /// Asserts that iteration has not begun.
        pub fn first(self: *Self) []const T {
            std.debug.assert(self.index.? == 0);
            return self.next().?;
        }

        /// Returns a slice of the next field, or null if splitting is complete.
        pub fn next(self: *Self) ?[]const T {
            const start = self.index orelse return null;
            const end = if (switch (delimiter_type) {
                .scalar => indexOfScalarPosByFn(T, self.ctx, self.buffer, start, self.delimiter),
            }) |delim_start| blk: {
                self.index = delim_start + switch (delimiter_type) {
                    .scalar => 1,
                };
                break :blk delim_start;
            } else blk: {
                self.index = null;
                break :blk self.buffer.len;
            };
            return self.buffer[start..end];
        }

        /// Returns a slice of the next field, or null if splitting is complete.
        /// This method does not alter self.index.
        pub fn peek(self: *Self) ?[]const T {
            const start = self.index orelse return null;
            const end = if (switch (delimiter_type) {
                .scalar => indexOfScalarPosByFn(T, self.ctx, self.buffer, start, self.delimiter),
            }) |delim_start| delim_start else self.buffer.len;
            return self.buffer[start..end];
        }

        /// Returns a slice of the remaining bytes. Does not affect iterator state.
        pub fn rest(self: Self) []const T {
            const end = self.buffer.len;
            const start = self.index orelse end;
            return self.buffer[start..end];
        }

        /// Resets the iterator to the initial slice.
        pub fn reset(self: *Self) void {
            self.index = 0;
        }
    };
}

pub fn indexOfScalarPosByFn(
    comptime T: type,
    ctx: anytype,
    haystack: []const T,
    start_index: usize,
    needle: DelimiterFn(@TypeOf(ctx), T),
) ?usize {
    if (start_index >= haystack.len) return null;
    for (start_index..haystack.len) |i| if (needle(ctx, haystack[i])) return i;
    return null;
}

// TESTING

const UE = union(enum) {
    one: usize,
    two: f32,
    three: bool,
};

const DF = struct {
    matcher: bool,

    pub fn isDelimiter(self: DF, ue: UE) bool {
        return switch (ue) {
            .three => |three| three == self.matcher,
            else => false,
        };
    }
};

test "test" {
    const testing = @import("std").testing;

    const delimiter = DF{
        .matcher = false,
    };

    const list = [_]UE{ .{ .one = 1 }, .{ .two = 2.2 }, .{ .three = false }, .{ .two = 4.4 }, .{ .one = 5 } };

    var it = splitScalarByFn(UE, delimiter, &list, DF.isDelimiter);

    const first = it.next().?;
    const second = it.next().?;

    try testing.expectEqualSlices(UE, &.{ .{ .one = 1 }, .{ .two = 2.2 } }, first);
    try testing.expectEqualSlices(UE, &.{ .{ .two = 4.4 }, .{ .one = 5 } }, second);
}

test "test 2" {
    const testing = @import("std").testing;

    const delimiter = DF{
        .matcher = false,
    };

    const list = [_]UE{ .{ .one = 1 }, .{ .two = 2.2 }, .{ .three = true }, .{ .two = 4.4 }, .{ .one = 5 } };

    var it = splitScalarByFn(UE, delimiter, &list, DF.isDelimiter);

    const first = it.next().?;

    try testing.expectEqualSlices(UE, &.{ .{ .one = 1 }, .{ .two = 2.2 }, .{ .three = true }, .{ .two = 4.4 }, .{ .one = 5 } }, first);
}
