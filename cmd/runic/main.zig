const std = @import("std");
const utils = @import("main-utils.zig");
const dispatch = @import("dispatch.zig").dispatch;
const runic = @import("runic");
const PipeReader = runic.process.PipeReader;
const signals = runic.signals;

pub const std_options = std.Options{
    .logFn = log,
};

pub fn main() !void {
    signals.init(std.heap.page_allocator);
    const exit_code = mainImpl() catch |err| {
        std.log.err("runic exited with error: {t}", .{err});
        std.process.exit(1);
    };
    if (exit_code != .success) {
        std.process.exit(exit_code.getErrorCode());
    }
}

fn mainImpl() !runic.command_runner.ExitCode {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 24 }){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) std.log.err("runic CLI leaked memory", .{});
    }

    const allocator = gpa.allocator();

    const orig_termios = try enableRawMode();
    defer if (orig_termios) |t| restoreTermios(t) catch |err| {
        std.log.err("Couldn't restore terminal attributes: {}", .{err});
    };

    var tracer = runic.trace.Tracer.init(allocator, .{ .echo_to_stdout = false });
    defer tracer.deinit();

    var stdin_buffer: [1024]u8 = undefined;
    var stdout_buffer: [1024]u8 = undefined;
    var stderr_buffer: [1024]u8 = undefined;
    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    // var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    var stdin_reader = PipeReader.init(std.fs.File.stdin(), &stdin_buffer, &tracer);
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stdin = &stdin_reader.reader;
    const stdout = &stdout_writer.interface;
    const stderr = &stderr_writer.interface;
    defer stdout.flush() catch {};
    defer stderr.flush() catch {};

    const result = try utils.parseCommandLine(allocator, argv);
    switch (result) {
        .show_help => {
            try utils.printUsage(stdout);
            return .success;
        },
        .usage_error => |message| {
            defer allocator.free(message);
            try stderr.print("error: {s}\n\n", .{message});
            try utils.printUsage(stderr);
            try stderr.flush();
            return .fromByte(2);
        },
        .ready => |config| {
            defer {
                var cfg = config;
                cfg.deinit(allocator);
            }
            return try dispatch(
                allocator,
                config,
                stdin,
                stdout,
                stderr,
                &tracer,
            );
        },
    }

    return .success;
}

fn enableRawMode() !?std.posix.termios {
    const stdin = std.fs.File.stdin();
    const is_tty = stdin.isTty();
    if (!is_tty) return null;
    if (@import("builtin").os.tag != .linux) return;
    var raw = try std.posix.tcgetattr(stdin.handle);
    const orig = raw;
    raw.iflag.BRKINT = true;
    raw.iflag.ICRNL = true;
    raw.iflag.INPCK = false;
    raw.iflag.ISTRIP = false;
    raw.iflag.IXON = false;
    raw.oflag.OPOST = true;
    raw.oflag.ONLCR = true;
    raw.oflag.OCRNL = false;
    raw.oflag.ONLRET = false;
    raw.cflag.CSIZE = .CS8;
    raw.cflag.CREAD = true;
    raw.cflag.CLOCAL = true;
    raw.lflag.ECHO = false;
    raw.lflag.ECHONL = false;
    raw.lflag.ICANON = false;
    raw.lflag.IEXTEN = false;
    raw.lflag.ISIG = true;
    try std.posix.tcsetattr(stdin.handle, .FLUSH, raw);
    return orig;
}

fn restoreTermios(termios: std.posix.termios) !void {
    const stdin = std.fs.File.stdin();
    const is_tty = stdin.isTty();
    if (!is_tty) return;
    if (@import("builtin").os.tag != .linux) return;
    try std.posix.tcsetattr(stdin.handle, .FLUSH, termios);
}

pub fn log(
    comptime _: std.log.Level,
    comptime _: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    var buffer: [64]u8 = undefined;
    const stderr = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();
    nosuspend stderr.print(format ++ "\n", args) catch return;
}
