const std = @import("std");
const runic = @import("runic");
const ast = runic.ast;

const Allocator = std.mem.Allocator;

pub const SymbolKind = enum {
    module,
    function,
    method,
    variable,
    field,
    keyword,
    @"struct",
};

pub const Symbol = struct {
    name: []const u8,
    detail: []const u8,
    documentation: []const u8 = &[_]u8{},
    /// LSP snippet insert text (tab stops like `${1:name}`). Empty means the
    /// completion inserts its label verbatim rather than a snippet.
    snippet: []const u8 = &[_]u8{},
    kind: SymbolKind,
    /// The symbol's selection span — the identifier itself. Used by
    /// references/rename/definition and as the document-symbol selectionRange.
    span: ast.Span,
    /// The full enclosing span (e.g. a whole struct or function declaration),
    /// used as the document-symbol range so nested children fall inside it.
    /// Falls back to `span` when null.
    range_span: ?ast.Span = null,
    /// Nested symbols (struct fields, function parameters). Only the outline
    /// (documentSymbol) descends into these; the flat top-level list is what
    /// completion and workspace search iterate.
    children: []Symbol = &.{},

    pub fn deinit(self: *Symbol, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.detail);
        if (self.documentation.len > 0) {
            allocator.free(self.documentation);
        }
        if (self.snippet.len > 0) {
            allocator.free(self.snippet);
        }
        for (self.children) |*child| child.deinit(allocator);
        if (self.children.len > 0) {
            allocator.free(self.children);
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
            .bash_block, .while_stmt => {
                // Not Yet Implemented,
            },
            .exit_stmt, .yield_stmt => {
                // Does not produce symbols
            },
            .expression => |expr_stmt| {
                // A top-level named function declaration parses as an expression
                // statement wrapping a `fn_decl`. Surface it as a symbol so it
                // appears in the outline, workspace search, and completions, with
                // its parameters nested as children.
                switch (expr_stmt.expression.*) {
                    .fn_decl => |fn_decl| {
                        if (fn_decl.name) |name| {
                            const children = try fnParamChildren(allocator, detail, fn_decl);
                            errdefer freeChildren(allocator, children);
                            try appendSymbolFull(allocator, list, .function, name.name, detail, name.span, fn_decl.span, children);
                        }
                    },
                    else => {},
                }
            },
            // A type binding to a struct (`const Point = struct { … }`) surfaces
            // as a Struct symbol with its fields and declarations as children.
            .type_binding_decl => |type_binding_decl| {
                switch (type_binding_decl.type_expr.*) {
                    .struct_type => |struct_type| {
                        const children = try structChildren(allocator, detail, struct_type);
                        errdefer freeChildren(allocator, children);
                        try appendSymbolFull(allocator, list, .@"struct", type_binding_decl.identifier.name, detail, type_binding_decl.identifier.span, type_binding_decl.span, children);
                    },
                    else => {},
                }
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

                        try appendSymbol(allocator, list, .variable, name, detail, identifier.span);
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
    span: ast.Span,
) !void {
    var entry = Symbol{
        .name = try allocator.dupe(u8, name),
        .detail = try allocator.dupe(u8, detail),
        .documentation = try std.fmt.allocPrint(allocator, "`{s}`", .{@tagName(kind)}),
        .kind = kind,
        .span = span,
    };
    errdefer entry.deinit(allocator);
    try list.append(allocator, entry);
}

/// Appends a symbol carrying an enclosing `range_span` and nested `children`
/// (which it takes ownership of).
fn appendSymbolFull(
    allocator: Allocator,
    list: *std.ArrayList(Symbol),
    kind: SymbolKind,
    name: []const u8,
    detail: []const u8,
    span: ast.Span,
    range_span: ast.Span,
    children: []Symbol,
) !void {
    var entry = Symbol{
        .name = try allocator.dupe(u8, name),
        .detail = try allocator.dupe(u8, detail),
        .documentation = try std.fmt.allocPrint(allocator, "`{s}`", .{@tagName(kind)}),
        .kind = kind,
        .span = span,
        .range_span = range_span,
        .children = children,
    };
    errdefer entry.deinit(allocator);
    try list.append(allocator, entry);
}

fn freeChildren(allocator: Allocator, children: []Symbol) void {
    for (children) |*child| child.deinit(allocator);
    if (children.len > 0) allocator.free(children);
}

/// Builds child symbols for a struct type's fields and declarations.
fn structChildren(
    allocator: Allocator,
    detail: []const u8,
    struct_type: ast.TypeExpr.StructType,
) ![]Symbol {
    var children = std.ArrayList(Symbol).empty;
    errdefer {
        for (children.items) |*child| child.deinit(allocator);
        children.deinit(allocator);
    }

    for (struct_type.fields) |field| {
        try children.append(allocator, try leafSymbol(allocator, .field, field.name.name, detail, field.name.span, field.span));
    }
    for (struct_type.decls) |decl| {
        const kind: SymbolKind = switch (decl.decl_source) {
            .fn_decl => .method,
            .binding_decl => .field,
        };
        try children.append(allocator, try leafSymbol(allocator, kind, decl.name.name, detail, decl.name.span, decl.span));
    }

    return children.toOwnedSlice(allocator);
}

/// Builds child symbols for a function's parameters.
fn fnParamChildren(
    allocator: Allocator,
    detail: []const u8,
    fn_decl: ast.FunctionDecl,
) ![]Symbol {
    var children = std.ArrayList(Symbol).empty;
    errdefer {
        for (children.items) |*child| child.deinit(allocator);
        children.deinit(allocator);
    }

    const params = switch (fn_decl.params) {
        ._non_variadic => |ps| ps,
        ._variadic => |p| @as([]const *ast.Parameter, &.{p}),
    };
    for (params) |param| {
        switch (param.pattern.*) {
            .identifier => |identifier| {
                try children.append(allocator, try leafSymbol(allocator, .variable, identifier.name, detail, identifier.span, param.span));
            },
            else => {},
        }
    }

    return children.toOwnedSlice(allocator);
}

fn leafSymbol(
    allocator: Allocator,
    kind: SymbolKind,
    name: []const u8,
    detail: []const u8,
    span: ast.Span,
    range_span: ast.Span,
) !Symbol {
    var entry = Symbol{
        .name = try allocator.dupe(u8, name),
        .detail = try allocator.dupe(u8, detail),
        .documentation = try std.fmt.allocPrint(allocator, "`{s}`", .{@tagName(kind)}),
        .kind = kind,
        .span = span,
        .range_span = range_span,
    };
    errdefer entry.deinit(allocator);
    return entry;
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
