const std = @import("std");

pub const RainbowColor = enum {
    black,
    red,
    orange,
    yellow,
    green,
    blue,
    indigo,
    violet,
};

const csi = "\x1b[";

const ansi_reset = csi ++ "0m";

fn ansiPrefix(color: RainbowColor) []const u8 {
    return csi ++ switch (color) {
        .black => "30m",
        .red => "31m",
        .orange => "33m",
        .yellow => "93m",
        .green => "32m",
        .blue => "34m",
        .indigo => "94m",
        .violet => "35m",
    };
}

fn ansiBackgroundPrefix(comptime color: RainbowColor) []const u8 {
    return csi ++ switch (color) {
        .black => "40m",
        .red => "41m",
        .orange => "43m",
        .yellow => "103m",
        .green => "42m",
        .blue => "44m",
        .indigo => "104m",
        .violet => "45m",
    };
}

pub fn beginColor(color: RainbowColor) []const u8 {
    return ansiPrefix(color);
}

pub fn beginBgColor(comptime color: RainbowColor) []const u8 {
    return ansiBackgroundPrefix(color);
}

pub fn endColor() []const u8 {
    return ansi_reset;
}

/// Writes `value` wrapped in ANSI escape sequences that colorize the output.
pub fn colorize(color: RainbowColor, value: anytype) type {
    const Formatter = struct {
        color: RainbowColor,
        value: @TypeOf(value),

        pub fn format(self: @This(), writer: *std.Io.Writer) !void {
            try writer.writeAll(beginColor(self.color));
            try writer.print("{}", .{self.value});
            try writer.writeAll(endColor());
        }
    };

    return Formatter{ .color = color, .value = value };
}
