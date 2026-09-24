const std = @import("std");
const lsp = @import("runic_lsp");
const build_options = @import("build_options");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.gpa;

    const args = init.minimal.args;

    const Mode = enum { stdio, tcp };
    var mode: Mode = .stdio;
    var tcp_port: ?u16 = null;

    var args_it = try args.iterateAllocator(allocator);
    defer args_it.deinit();
    _ = args_it.next();
    while (args_it.next()) |arg| {
        if (std.mem.eql(u8, arg, "--version")) {
            try printVersion(io);
            return;
        }
        if (std.mem.eql(u8, arg, "--stdio")) {
            std.log.err("stdio mode", .{});
            mode = .stdio;
            continue;
        }
        if (std.mem.eql(u8, arg, "--tcp")) {
            const port_arg = args_it.next() orelse return error.MissingTcpPort;
            tcp_port = try std.fmt.parseInt(u16, port_arg, 10);
            mode = .tcp;
            continue;
        }
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try printUsage(io);
            return;
        }
        var stderr = std.Io.File.stderr().writer(io, &.{});
        try stderr.interface.print("unknown flag: {s}\n", .{arg});
        try stderr.interface.flush();
        return error.InvalidArguments;
    }

    const stdin = std.Io.File.stdin();
    // var oldattr: std.c.termios = undefined;

    mode_init: switch (mode) {
        .stdio => {
            switch (@import("builtin").os.tag) {
                .linux => {
                    break :mode_init;
                    // var attr = std.posix.tcgetattr(stdin.handle) catch |err| switch (err) {
                    //     error.NotATerminal => break :mode_init,
                    //     else => return err,
                    // };
                    // oldattr = attr;
                    // attr.lflag.ICANON = false;
                    // attr.lflag.ECHO = false;
                    // attr.cc[@intFromEnum(std.posix.V.MIN)] = 1;
                    // attr.cc[@intFromEnum(std.posix.V.TIME)] = 0;
                    // try std.posix.tcsetattr(stdin.handle, .NOW, attr);
                },
                else => {},
            }
        },
        .tcp => {
            // _ = tcp_port; // reserved for future debugging sessions.
            var stderr = std.Io.File.stderr().writer(io, &.{});
            try stderr.interface.print("--tcp transport is not implemented yet; use --stdio\n", .{});
            try stderr.interface.flush();
            return error.TcpTransportUnavailable;
        },
    }

    defer switch (mode) {
        .stdio => switch (@import("builtin").os.tag) {
            .linux => {
                // std.posix.tcsetattr(stdin.handle, .NOW, oldattr) catch {};
            },
            else => {},
        },
        else => {},
    };

    var server = try lsp.server.Server.init(
        io,
        allocator,
        init.environ_map,
        stdin,
        std.Io.File.stdout(),
        std.Io.File.stderr(),
    );
    server.initInterface();
    defer server.deinit();

    try server.run();
}

fn printUsage(io: std.Io) !void {
    var writer = std.Io.File.stdout().writer(io, &.{});
    try writer.interface.writeAll(
        "Runic language server\n" ++
            "\n" ++
            "USAGE:\n" ++
            "  runic-lsp [--stdio]\n" ++
            "  runic-lsp --tcp <port>  # reserved for upcoming protocol inspectors\n",
    );
    try writer.interface.flush();
}

fn printVersion(io: std.Io) !void {
    var writer = std.Io.File.stdout().writer(io, &.{});
    try writer.interface.print("{s}\n", .{build_options.version});
    try writer.interface.flush();
}
