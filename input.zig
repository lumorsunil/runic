const std = @import("std");

pub fn main() !void {
    const stdin_file = std.fs.File.stdin();
    var stdin_buffer: [1024]u8 = undefined;
    var stdin_file_reader = stdin_file.reader(&stdin_buffer);
    var input = try stdin_file_reader.interface.takeDelimiterExclusive('\n');

    var number: ?u32 = std.fmt.parseInt(u32, input, 10) catch null;

    if (number) |n| {
        std.log.debug("You inputted: {}, which is a number!", .{n});
    } else {
        std.log.debug("You inputted: {s}", .{input});
    }

    input = try stdin_file_reader.interface.takeDelimiterExclusive('\n');

    number = std.fmt.parseInt(u32, input, 10) catch null;

    if (number) |n| {
        std.log.debug("You inputted: {}, which is a number!", .{n});
    } else {
        std.log.debug("You inputted: {s}", .{input});
    }
}
