const std = @import("std");

pub fn main() !void {
    const stdout_file = std.fs.File.stdout();
    const stderr_file = std.fs.File.stderr();
    var stdout_file_writer = stdout_file.writer(&.{});
    var stderr_file_writer = stderr_file.writer(&.{});
    const stdout_writer = &stdout_file_writer.interface;
    const stderr_writer = &stderr_file_writer.interface;

    try stdout_writer.writeAll("stdout");
    try stdout_writer.flush();

    try stderr_writer.writeAll("stderr\n");
    try stderr_writer.flush();

    try stdout_writer.writeAll("stdout");
    try stdout_writer.flush();

    try stderr_writer.writeAll("stderr\n");
    try stderr_writer.flush();

    try stdout_writer.writeAll("stdout\n");
    try stdout_writer.flush();

    try stderr_writer.writeAll("stderr\n");
    try stderr_writer.flush();
}
