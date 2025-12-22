const std = @import("std");

pub fn main() !void {
    const stdout_file = std.fs.File.stdout();
    const stderr_file = std.fs.File.stderr();
    var stdout_file_writer = stdout_file.writer(&.{});
    var stderr_file_writer = stderr_file.writer(&.{});
    const stdout_writer = &stdout_file_writer.interface;
    const stderr_writer = &stderr_file_writer.interface;

    // try stderr_writer.writeAll("from stderr\n");
    // try stderr_writer.flush();
    // try stderr_writer.writeAll("from stderr\n");
    // try stderr_writer.flush();
    // try stderr_writer.writeAll("from stderr\n");
    // try stderr_writer.flush();
    // try stderr_writer.writeAll("from stderr\n");
    // try stderr_writer.flush();
    // try stderr_writer.writeAll("from stderr\n");
    // try stderr_writer.flush();
    // try stderr_writer.writeAll("from stderr\n");
    // try stderr_writer.flush();
    //

    std.log.err("from log.err", .{});
    std.log.err("from log.err", .{});
    std.log.err("from log.err", .{});
    try stdout_writer.writeAll("from stdout!!!!!\n");
    try stdout_writer.flush();
    try stdout_writer.writeAll("from stdout!!!!!\n");
    try stdout_writer.flush();
    std.log.err("from log.err", .{});
    std.log.err("from log.err", .{});

    try stdout_writer.writeAll("from stdout!!!!!\n");
    try stdout_writer.flush();
    try stdout_writer.writeAll("from stdout!!!!!\n");
    try stdout_writer.flush();

    try send(stdout_writer, stderr_writer);
    try send(stdout_writer, stderr_writer);
    try send(stdout_writer, stderr_writer);
    try send(stdout_writer, stderr_writer);
    try send(stdout_writer, stderr_writer);
    try send(stdout_writer, stderr_writer);
    try send(stdout_writer, stderr_writer);
}

fn send(stdout: *std.Io.Writer, stderr: *std.Io.Writer) !void {
    try stdout.writeAll("from stdout!!!!!\n");
    try stdout.flush();

    try stderr.writeAll("from stderr!!!!!\n");
    try stderr.flush();
}
