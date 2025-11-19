const std = @import("std");
const lsp = @import("runic_lsp");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const Mode = enum { stdio, tcp };
    var mode: Mode = .stdio;
    var tcp_port: ?u16 = null;

    var i: usize = 1;
    while (i < args.len) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--stdio")) {
            mode = .stdio;
            i += 1;
            continue;
        }
        if (std.mem.eql(u8, arg, "--tcp")) {
            if (i + 1 >= args.len) return error.MissingTcpPort;
            tcp_port = try std.fmt.parseInt(u16, args[i + 1], 10);
            mode = .tcp;
            i += 2;
            continue;
        }
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            try printUsage();
            return;
        }
        var stderr = std.fs.File.stderr().writer(&.{});
        try stderr.interface.print("unknown flag: {s}\n", .{arg});
        return error.InvalidArguments;
    }

    switch (mode) {
        .stdio => {},
        .tcp => {
            // _ = tcp_port; // reserved for future debugging sessions.
            var stderr = std.fs.File.stderr().writer(&.{});
            try stderr.interface.print("--tcp transport is not implemented yet; use --stdio\n", .{});
            return error.TcpTransportUnavailable;
        },
    }

    var server = try lsp.server.Server.init(allocator, std.fs.File.stdin(), std.fs.File.stdout(), std.fs.File.stderr());
    defer server.deinit();

    try server.run();
}

fn printUsage() !void {
    var writer = std.fs.File.stdout().writer(&.{});
    try writer.interface.writeAll(
        "Runic language server\n" ++
            "\n" ++
            "USAGE:\n" ++
            "  runic-lsp [--stdio]\n" ++
            "  runic-lsp --tcp <port>  # reserved for upcoming protocol inspectors\n",
    );
}
