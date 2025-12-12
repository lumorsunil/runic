const std = @import("std");
const Stream = @import("stream.zig").Stream;
const Value = @import("interpreter/value.zig").Value;
const RC = @import("mem/root.zig").RC;
const RCError = @import("mem/root.zig").RCError;

pub fn Transformer(comptime T: type) type {
    return struct {
        pub fn transform(
            allocator: std.mem.Allocator,
            value: Value,
        ) RCError!?T {
            if (T == []const u8) {
                switch (value) {
                    .string => |string_ref| {
                        const string = string_ref.get() catch return null;
                        return string;
                    },
                    else => return null,
                }
            } else if (T == *Stream([]const u8)) {
                switch (value) {
                    .string => |string_ref| {
                        const string = string_ref.get() catch return null;
                        return Stream([]const u8).initSingle(allocator, string);
                    },
                    .array => |array_ref| {
                        const array = array_ref.get() catch return null;
                        const source = try Stream(Value).initBuffer(allocator, array.items);
                        defer source.deinit();
                        return source.map(Transformer([]const u8).transform);
                    },
                    else => return null,
                }
            }

            @compileError(@typeName(T) ++ " is an unsupported transformer type");
        }
    };
}
