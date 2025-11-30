const std = @import("std");
const runic = @import("runic");
const ast = runic.ast;

const Allocator = std.mem.Allocator;

pub const SymbolKind = enum {
    module,
    function,
    variable,
};

pub const Symbol = struct {
    name: []const u8,
    detail: []const u8,
    documentation: []const u8 = &[_]u8{},
    kind: SymbolKind,

    pub fn deinit(self: *Symbol, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.detail);
        if (self.documentation.len > 0) {
            allocator.free(self.documentation);
        }
        self.* = undefined;
    }
};

pub fn collectSymbols(
    allocator: Allocator,
    detail: []const u8,
    script: ast.Script,
    list: *std.ArrayList(Symbol),
) !void {
    for (script.statements) |statement| {
        switch (statement.*) {
            .bash_block, .error_decl, .for_stmt, .while_stmt => {
                // Not Yet Implemented,
            },
            .return_stmt, .expression => {
                // Does not produce symbols
            },
            .fn_decl => |fn_decl| {
                try appendSymbol(allocator, list, .function, fn_decl.name.name, detail);
            },
            .binding_decl => |binding_decl| {
                switch (binding_decl.pattern.*) {
                    .discard => {},
                    .record, .tuple => {
                        // Not Yet Implemented
                    },
                    .identifier => |identifier| {
                        const name = identifier.name;
                        // const initializer = binding_decl.initializer.span().sliceFrom(contents);

                        try appendSymbol(allocator, list, .variable, name, detail);
                    },
                }
            },
        }
    }

    // var cursor = TokenCursor{ .source = contents };
    // while (true) {
    //     cursor.skipTrivia();
    //     if (cursor.done()) break;
    //     if (cursor.skipStringLiteral()) continue;
    //     if (cursor.matchKeyword("pub")) continue;
    //     if (cursor.matchKeyword("module")) {
    //         if (cursor.readIdentifier()) |name| {
    //             try appendSymbol(allocator, list, .module, name, detail);
    //         }
    //         continue;
    //     }
    //     if (cursor.matchKeyword("fn")) {
    //         if (cursor.readIdentifier()) |name| {
    //             try appendSymbol(allocator, list, .function, name, detail);
    //         }
    //         continue;
    //     }
    //     if (cursor.matchKeyword("const") or cursor.matchKeyword("var")) {
    //         if (cursor.readIdentifier()) |name| {
    //             try appendSymbol(allocator, list, .variable, name, detail);
    //         }
    //         continue;
    //     }
    //     cursor.advance();
    // }
}

fn appendSymbol(
    allocator: Allocator,
    list: *std.ArrayList(Symbol),
    kind: SymbolKind,
    name: []const u8,
    detail: []const u8,
) !void {
    var entry = Symbol{
        .name = try allocator.dupe(u8, name),
        .detail = try allocator.dupe(u8, detail),
        .documentation = try std.fmt.allocPrint(allocator, "`{s}`", .{@tagName(kind)}),
        .kind = kind,
    };
    errdefer entry.deinit(allocator);
    try list.append(allocator, entry);
}

pub fn isIdentifierChar(ch: u8) bool {
    return std.ascii.isAlphanumeric(ch) or ch == '_' or ch == '-';
}

fn isIdentifierStart(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_' or ch == '-';
}

const TokenCursor = struct {
    source: []const u8,
    index: usize = 0,

    fn done(self: *TokenCursor) bool {
        return self.index >= self.source.len;
    }

    fn advance(self: *TokenCursor) void {
        if (!self.done()) self.index += 1;
    }

    fn skipTrivia(self: *TokenCursor) void {
        while (true) {
            self.skipWhitespace();
            if (self.skipLineComment()) continue;
            if (self.skipBlockComment()) continue;
            break;
        }
    }

    fn skipWhitespace(self: *TokenCursor) void {
        while (self.index < self.source.len) {
            const ch = self.source[self.index];
            switch (ch) {
                ' ', '\t', '\r', '\n' => self.index += 1,
                else => return,
            }
        }
    }

    fn skipLineComment(self: *TokenCursor) bool {
        if (self.index >= self.source.len) return false;
        if (self.source[self.index] == '#') {
            self.consumeLine();
            return true;
        }
        if (self.matchSequence("//")) {
            self.consumeLine();
            return true;
        }
        return false;
    }

    fn consumeLine(self: *TokenCursor) void {
        while (self.index < self.source.len) : (self.index += 1) {
            if (self.source[self.index] == '\n') {
                self.index += 1;
                break;
            }
        }
    }

    fn skipBlockComment(self: *TokenCursor) bool {
        if (!self.matchSequence("/*")) return false;
        while (self.index < self.source.len) {
            if (self.matchSequence("*/")) break;
            self.index += 1;
        }
        return true;
    }

    fn skipStringLiteral(self: *TokenCursor) bool {
        if (self.index >= self.source.len) return false;
        const quote = self.source[self.index];
        if (quote != '"' and quote != '\'') return false;
        self.index += 1;
        while (self.index < self.source.len) {
            const ch = self.source[self.index];
            self.index += 1;
            if (ch == '\\' and self.index < self.source.len) {
                self.index += 1;
                continue;
            }
            if (ch == quote) break;
        }
        return true;
    }

    fn matchSequence(self: *TokenCursor, seq: []const u8) bool {
        if (self.index + seq.len > self.source.len) return false;
        if (!std.mem.eql(u8, self.source[self.index .. self.index + seq.len], seq)) return false;
        self.index += seq.len;
        return true;
    }

    fn matchKeyword(self: *TokenCursor, keyword: []const u8) bool {
        if (self.index + keyword.len > self.source.len) return false;
        if (!std.mem.eql(u8, self.source[self.index .. self.index + keyword.len], keyword)) return false;
        if (self.index > 0) {
            const prev = self.source[self.index - 1];
            if (isIdentifierChar(prev)) return false;
        }
        if (self.index + keyword.len < self.source.len) {
            const next = self.source[self.index + keyword.len];
            if (isIdentifierChar(next)) return false;
        }
        self.index += keyword.len;
        return true;
    }

    fn readIdentifier(self: *TokenCursor) ?[]const u8 {
        self.skipWhitespace();
        if (self.index >= self.source.len) return null;
        if (!isIdentifierStart(self.source[self.index])) return null;
        const start = self.index;
        self.index += 1;
        while (self.index < self.source.len) {
            if (!isIdentifierChar(self.source[self.index])) break;
            self.index += 1;
        }
        return self.source[start..self.index];
    }
};
