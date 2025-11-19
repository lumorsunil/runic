const std = @import("std");
const symbols = @import("symbols.zig");

const Allocator = std.mem.Allocator;

pub const MatchList = struct {
    allocator: Allocator,
    items: std.ArrayList(Match),

    pub fn init(allocator: Allocator) MatchList {
        return .{ .allocator = allocator, .items = .{} };
    }

    pub fn deinit(self: *MatchList) void {
        self.items.deinit(self.allocator);
    }
};

pub const Match = struct {
    symbol: *const symbols.Symbol,
    source: Source,
};

pub const Source = enum { document, workspace };

pub fn collectMatches(
    allocator: Allocator,
    prefix: []const u8,
    doc_symbols: []const symbols.Symbol,
    workspace_symbols: []const symbols.Symbol,
) !MatchList {
    const mode = determineMode(prefix, doc_symbols, workspace_symbols);
    var matches = MatchList.init(allocator);
    try appendMatches(&matches, .document, doc_symbols, prefix, mode);
    try appendMatches(&matches, .workspace, workspace_symbols, prefix, mode);
    return matches;
}

fn appendMatches(
    matches: *MatchList,
    source: Source,
    symbol_list: []const symbols.Symbol,
    prefix: []const u8,
    mode: FilterMode,
) !void {
    for (symbol_list) |*entry| {
        if (!symbolMatches(entry.name, prefix, mode)) continue;
        try matches.items.append(matches.allocator, .{ .symbol = entry, .source = source });
    }
}

pub const FilterMode = enum { prefix, substring };

fn determineMode(prefix: []const u8, doc_symbols: []const symbols.Symbol, workspace_symbols: []const symbols.Symbol) FilterMode {
    if (prefix.len == 0) return .prefix;
    if (hasPrefixMatch(prefix, doc_symbols)) return .prefix;
    if (hasPrefixMatch(prefix, workspace_symbols)) return .prefix;
    return .substring;
}

fn hasPrefixMatch(prefix: []const u8, list: []const symbols.Symbol) bool {
    for (list) |entry| {
        if (std.mem.startsWith(u8, entry.name, prefix)) return true;
    }
    return false;
}

fn symbolMatches(name: []const u8, prefix: []const u8, mode: FilterMode) bool {
    if (prefix.len == 0) return true;
    return switch (mode) {
        .prefix => std.mem.startsWith(u8, name, prefix),
        .substring => std.mem.indexOf(u8, name, prefix) != null,
    };
}
