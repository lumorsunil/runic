const std = @import("std");
const utils = @import("main-utils.zig");
const build_options = @import("build_options");
const dispatch = @import("dispatch.zig").dispatch;
const runic = @import("runic");
const PipeReader = runic.process.PipeReader;
const signals = runic.signals;

pub const std_options = std.Options{
    .logFn = log,
};

pub fn main(init: std.process.Init) !void {
    signals.init(std.heap.page_allocator);
    const exit_code = mainImpl(init) catch |err| {
        // Errors that already printed a precise, user-facing message at the
        // point of failure (e.g. InvalidInt/InvalidFloat name the offending
        // input and location) skip the generic footer so the user sees one
        // clean line.
        if (err != error.InvalidInt and err != error.InvalidFloat) {
            std.log.err("runic exited with error: {t}", .{err});
        }
        std.process.exit(1);
    };
    if (exit_code != .success) {
        std.process.exit(exit_code.getErrorCode());
    }
}

fn mainImpl(init: std.process.Init) !runic.ExitCode {
    const io = init.io;
    const allocator = init.gpa;
    const env_map = init.environ_map;

    var tracer = runic.trace.Tracer.init(io, allocator, .{ .echo_to_stdout = false });
    defer tracer.deinit();

    var stdin_buffer: [1024]u8 = undefined;
    var stdout_buffer: [1024]u8 = undefined;
    var stderr_buffer: [1024]u8 = undefined;
    const args = init.minimal.args;

    // var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    var stdin_reader = PipeReader.init(io, std.Io.File.stdin(), &stdin_buffer, &tracer);
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    var stderr_writer = std.Io.File.stderr().writer(io, &stderr_buffer);
    const stdin = &stdin_reader.reader;
    const stdout = &stdout_writer.interface;
    const stderr = &stderr_writer.interface;
    defer stdout.flush() catch {};
    defer stderr.flush() catch {};

    const result = try utils.parseCommandLine(allocator, args);
    switch (result) {
        .show_help => {
            try utils.printUsage(stdout);
            return .success;
        },
        .show_version => {
            try stdout.print("{s}\n", .{build_options.version});
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

            const orig_termios = if (config.debug_ir) try enableRawMode(io) else null;
            defer if (orig_termios) |t| restoreTermios(io, t) catch |err| {
                std.log.err("Couldn't restore terminal attributes: {}", .{err});
            };

            return try dispatch(
                io,
                allocator,
                env_map,
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

fn enableRawMode(io: std.Io) !?std.posix.termios {
    const stdin = std.Io.File.stdin();
    const is_tty = try stdin.isTty(io);
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

fn restoreTermios(io: std.Io, termios: std.posix.termios) !void {
    const stdin = std.Io.File.stdin();
    const is_tty = try stdin.isTty(io);
    if (!is_tty) return;
    if (@import("builtin").os.tag != .linux) return;
    try std.posix.tcsetattr(stdin.handle, .FLUSH, termios);
}

pub fn log(
    comptime _: std.log.Level,
    comptime _: @EnumLiteral(),
    comptime format: []const u8,
    args: anytype,
) void {
    const io = std.Options.debug_io;
    const prev = io.swapCancelProtection(.blocked);
    defer _ = io.swapCancelProtection(prev);
    var buffer: [64]u8 = undefined;
    const stderr = std.debug.lockStderr(&buffer).terminal();
    defer std.debug.unlockStderr();
    // return defaultLogFileTerminal(level, scope, format, args, stderr) catch {};
    stderr.writer.print(format ++ "\n", args) catch {};
}
