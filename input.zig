const std = @import("std");

pub fn main() !void {
    const stdin_file = std.fs.File.stdin();
    var stdin_buffer: [1024]u8 = undefined;
    var stdin_file_reader = stdin_file.reader(&stdin_buffer);
    const input = try stdin_file_reader.interface.takeDelimiterExclusive('\n');

    std.log.debug("You inputted: {s}", .{input});
}
