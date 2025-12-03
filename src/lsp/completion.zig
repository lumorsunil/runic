const std = @import("std");
const symbols = @import("symbols.zig");
const types = @import("types.zig");
const runic = @import("runic");
const Parser = @import("parser.zig").Parser;

const Allocator = std.mem.Allocator;

pub const MatchList = struct {
    allocator: Allocator,
    items: std.ArrayList(Match),

    pub fn empty(allocator: Allocator) MatchList {
        return .{ .allocator = allocator, .items = .empty };
    }

    pub fn init(allocator: Allocator) MatchList {
        return .{ .allocator = allocator, .items = .empty };
    }

    pub fn deinit(self: *MatchList) void {
        for (self.items.items) |item| item.deinit(self.allocator);
        self.items.deinit(self.allocator);
    }
};

fn OwnedOrNot(comptime T: type) type {
    return union(enum) {
        owned: *T,
        notOwned: *const T,

        pub fn deinit(self: @This(), allocator: Allocator) void {
            switch (self) {
                .owned => |ptr| {
                    if (std.meta.hasFn(T, "deinit")) {
                        ptr.deinit(allocator);
                    }
                    allocator.destroy(ptr);
                    ptr.* = undefined;
                },
                .notOwned => {},
            }
        }

        pub fn get(self: @This()) T {
            return self.getPtr().*;
        }

        pub fn getPtr(self: @This()) *const T {
            return switch (self) {
                inline else => |ptr| ptr,
            };
        }
    };
}

pub const Match = struct {
    symbol: OwnedOrNot(symbols.Symbol),
    source: Source,

    pub fn deinit(self: Match, allocator: Allocator) void {
        self.symbol.deinit(allocator);
    }
};

pub const Source = enum { document, workspace };

const CompletionKind = enum {
    module,
    symbol,
    string,
    none,
};

pub const CollectMatchesContext = struct {
    allocator: Allocator,
    file: []const u8,
    text_slice: []const u8,
    line_index: usize,
    char_index: usize,
    doc_symbols: []const symbols.Symbol,
    workspace_symbols: []const symbols.Symbol,
};

pub fn collectMatches(
    context: CollectMatchesContext,
) !MatchList {
    return switch (try determineCompletionKind(context)) {
        .symbol => collectSymbolMatches(context),
        .module => collectModuleMatches(context),
        .string => .empty(context.allocator),
        .none => .empty(context.allocator),
    };
}

fn determineCompletionKind(context: CollectMatchesContext) !CompletionKind {
    const line_index = 1;
    const char_index = context.char_index + 1;
    const line_slice = getLine(context);
    var lexer = try runic.lexer.Stream.init(context.allocator, context.file, line_slice);
    defer lexer.deinit();
    lexer.lexer.logging_enabled = true;

    while (true) {
        const next = lexer.next() catch return .symbol;

        if (next.tag == .kw_import) {
            const string_start = lexer.next() catch return .none;
            if (string_start.tag != .string_start) return .none;
            if (string_start.span.contains(line_index, char_index)) return .module;
            const string_text = lexer.next() catch return .none;
            if (string_text.span.contains(line_index, char_index)) return .module;
        }

        if (next.span.contains(line_index, char_index)) {
            switch (lexer.lexer.currentContext()) {
                .root, .string_interp => return .symbol,
                .string => return .string,
            }
        }

        if (next.tag == .eof) return .symbol;
    }
}

fn collectSymbolMatches(context: CollectMatchesContext) !MatchList {
    const prefix = extractPrefix(context, symbols.isIdentifierChar);
    const all_symbols: []const []const symbols.Symbol = &.{
        context.doc_symbols,
        context.workspace_symbols,
    };
    const mode = determineMode(prefix, all_symbols);
    var matches = MatchList.init(context.allocator);
    for (all_symbols) |symbols_| {
        try appendMatches(&matches, .document, symbols_, prefix, mode);
    }
    return matches;
}

fn isModulePathChar(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or std.mem.indexOfScalar(u8, "/_-.", ch) != null;
}

fn isModuleFileChar(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or std.mem.indexOfScalar(u8, "_-.", ch) != null;
}

fn collectModuleMatches(context: CollectMatchesContext) !MatchList {
    const script_dirname = std.fs.path.dirname(context.file) orelse return .empty(context.allocator);
    var module_path_prefix = extractPrefix(context, isModulePathChar);
    const basename_prefix = extractPrefix(context, isModuleFileChar);
    module_path_prefix = module_path_prefix[0 .. module_path_prefix.len - basename_prefix.len];

    const dirname = try std.fs.path.join(context.allocator, &.{ script_dirname, module_path_prefix });
    defer context.allocator.free(dirname);

    std.log.err("collecting module matches in: {s}", .{dirname});
    var dir = try std.fs.openDirAbsolute(dirname, .{ .iterate = true });
    defer dir.close();
    var it = dir.iterate();

    var matches = MatchList.init(context.allocator);

    while (try it.next()) |entry| {
        switch (entry.kind) {
            .file, .directory => {
                if (entry.kind == .file and !std.mem.endsWith(u8, entry.name, ".rn")) continue;
                if (symbolMatches(entry.name, basename_prefix, .fromPrefix(basename_prefix))) {
                    const symbol = try context.allocator.create(symbols.Symbol);
                    symbol.* = .{
                        .name = try context.allocator.dupe(u8, entry.name),
                        .kind = .module,
                        .detail = try context.allocator.dupe(u8, entry.name),
                        .documentation = &.{},
                    };

                    try matches.items.append(context.allocator, .{
                        .symbol = .{ .owned = symbol },
                        .source = .workspace,
                    });
                }
            },
            // TODO: support .sym_link
            else => {},
        }
    }

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
        std.log.err("Trying to match \"{s}\" against \"{s}\"", .{ prefix, entry.name });
        if (!symbolMatches(entry.name, prefix, mode)) continue;
        try matches.items.append(matches.allocator, .{ .symbol = .{ .notOwned = entry }, .source = source });
    }
}

pub const FilterMode = enum {
    prefix,
    substring,

    pub fn fromPrefix(prefix: []const u8) FilterMode {
        return if (prefix.len == 0) .prefix else .substring;
    }
};

fn determineMode(
    prefix: []const u8,
    symbols_lists: []const []const symbols.Symbol,
) FilterMode {
    if (prefix.len == 0) return .prefix;
    for (symbols_lists) |symbols_| {
        if (hasPrefixMatch(prefix, symbols_)) return .prefix;
    }
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

fn extractPrefix(context: CollectMatchesContext, charMatcher: *const fn (u8) bool) []const u8 {
    const text = context.text_slice;
    const line = context.line_index;
    const character = context.char_index;
    var offset: usize = 0;
    var current_line: usize = 0;
    while (offset < text.len and current_line < line) {
        if (text[offset] == '\n') current_line += 1;
        offset += 1;
    }
    var cursor = offset;
    var consumed: usize = 0;
    while (cursor < text.len and consumed < character) {
        if (text[cursor] == '\n') break;
        cursor += 1;
        consumed += 1;
    }
    var start = cursor;
    while (start > offset) {
        const ch = text[start - 1];
        // if (symbols.isIdentifierChar(ch)) {
        if (charMatcher(ch)) {
            start -= 1;
        } else break;
    }
    return text[start..cursor];
}

fn getLine(context: CollectMatchesContext) []const u8 {
    var it = std.mem.splitScalar(u8, context.text_slice, '\n');
    var i: usize = 0;

    while (it.next()) |line| : (i += 1) {
        if (i == context.line_index) {
            return line;
        }
    }

    return "";
}

test "getLine" {
    const text = "asdf\nhello\nawewefwfewef\nwefwaegf\nwfefa";
    var ctx: CollectMatchesContext = undefined;
    ctx.text_slice = text;
    ctx.line_index = 1;
    ctx.char_index = 2;
    const result = getLine(ctx);

    try @import("std").testing.expectEqualStrings("hello", result);
}
