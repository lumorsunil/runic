const std = @import("std");
const Stream = @import("stream.zig").Stream;
const Value = @import("interpreter/value.zig").Value;
const RC = @import("mem/root.zig").RC;
const RCError = @import("mem/root.zig").RCError;
const ExitCode = @import("runtime/command_runner.zig").ExitCode;
const CloseableReader = @import("closeable.zig").CloseableReader;

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
            } else if (T == CloseableReader(ExitCode)) {
                switch (value) {
                    .string => |string_ref| {
                        const string = string_ref.get() catch return null;
                        const duped = try allocator.dupe(u8, string);
                        return Stream(u8).initFixed(allocator, duped);
                    },
                    // .array => |array_ref| {
                    //     const array = array_ref.get() catch return null;
                    //     const source = try Stream(Value).initFixed(allocator, array.items);
                    //     defer source.deinit();
                    //     return source.map(Transformer(u8).transform);
                    // },
                    else => return null,
                }
            } else if (T == *Stream(u8)) {
                switch (value) {
                    .string => |string_ref| {
                        const string = string_ref.get() catch return null;
                        const duped = try allocator.dupe(u8, string);
                        return Stream(u8).initFixed(allocator, duped);
                    },
                    // .array => |array_ref| {
                    //     const array = array_ref.get() catch return null;
                    //     const source = try Stream(Value).initFixed(allocator, array.items);
                    //     defer source.deinit();
                    //     return source.map(Transformer(u8).transform);
                    // },
                    else => return null,
                }
            }

            @compileError(@typeName(T) ++ " is an unsupported transformer type");
        }
    };
}
