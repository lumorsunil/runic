const std = @import("std");
const workspace = @import("workspace.zig");
const symbols = @import("symbols.zig");

test "symbol extractor identifies modules, functions, and values" {
    var list = std.ArrayList(symbols.Symbol){};
    defer {
        for (list.items) |*entry| entry.deinit(std.testing.allocator);
        list.deinit(std.testing.allocator);
    }

    const source =
        \\module sample.core
        \\pub fn greet(name: Str) {}
        \\let version = "1.0"
        \\mut flag = true
        \\# comments inside the buffer should not affect extraction
    ;

    try symbols.collectSymbols(
        std.testing.allocator,
        "src/app/main.rn",
        source,
        &list,
    );

    try std.testing.expectEqual(@as(usize, 4), list.items.len);
    try std.testing.expectEqual(symbols.SymbolKind.module, list.items[0].kind);
    try std.testing.expectEqualStrings("sample", list.items[0].name);
    try std.testing.expectEqual(symbols.SymbolKind.function, list.items[1].kind);
    try std.testing.expectEqualStrings("greet", list.items[1].name);
    try std.testing.expectEqual(symbols.SymbolKind.variable, list.items[2].kind);
    try std.testing.expectEqualStrings("version", list.items[2].name);
    try std.testing.expectEqual(symbols.SymbolKind.variable, list.items[3].kind);
    try std.testing.expectEqualStrings("flag", list.items[3].name);
}

test "workspace indexes runic files under known roots" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.makePath("src/pkg");
    {
        var file = try tmp.dir.createFile("src/pkg/runtime.rn", .{});
        defer file.close();
        const sample =
            \\pub fn greet() {}
        ;
        try file.writeAll(sample);
    }

    var ws = workspace.Workspace.init(std.testing.allocator);
    defer ws.deinit();

    const root = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(root);

    try ws.resetRoots(&.{root});
    try ws.refresh();

    const symbols_slice = ws.symbolSlice();
    try std.testing.expect(symbols_slice.len >= 1);
    try std.testing.expect(hasSymbol(symbols_slice, "greet"));
}

fn hasSymbol(list: []const symbols.Symbol, needle: []const u8) bool {
    for (list) |entry| {
        if (std.mem.eql(u8, entry.name, needle)) return true;
    }
    return false;
}
