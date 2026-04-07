const std = @import("std");
const symbols = @import("symbols.zig");
const types = @import("types.zig");
const runic = @import("runic");
const Parser = @import("parser.zig").Parser;
const ast = runic.ast;

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
    member,
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
    scope: ?*runic.semantic.Scope = null,
    type_checker: ?*runic.semantic.TypeChecker = null,
};

pub fn collectMatches(
    context: CollectMatchesContext,
) !MatchList {
    return switch (try determineCompletionKind(context)) {
        .member => collectMemberMatches(context),
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
                .root, .string_interp => return if (extractMemberContext(context) != null) .member else .symbol,
                .string => return .string,
            }
        }

        if (next.tag == .eof) {
            return if (extractMemberContext(context) != null) .member else .symbol;
        }
    }
}

const MemberContext = struct {
    object_name: []const u8,
    prefix: []const u8,
};

fn collectMemberMatches(context: CollectMatchesContext) !MatchList {
    const member_ctx = extractMemberContext(context) orelse return .empty(context.allocator);
    const type_checker = context.type_checker orelse return .empty(context.allocator);

    var candidates = MatchList.init(context.allocator);
    errdefer candidates.deinit();
    if (context.scope) |scope| {
        if (scope.lookup(member_ctx.object_name)) |binding| {
            if (binding.type_expr) |binding_type| {
                try appendMembersForType(&candidates, context, type_checker, binding_type, member_ctx.object_name);
            }
        }
    }
    if (candidates.items.items.len == 0) {
        if (type_checker.modules.get(context.file)) |module_scope| {
            if (module_scope.lookup(member_ctx.object_name)) |binding| {
                if (binding.type_expr) |binding_type| {
                    try appendMembersForType(&candidates, context, type_checker, binding_type, member_ctx.object_name);
                }
            }
        }
    }
    if (candidates.items.items.len == 0) {
        try appendImportedModuleMembersFromText(&candidates, context, type_checker, member_ctx.object_name);
    }

    const owned_symbols = try ownedSymbolsSlice(context.allocator, candidates.items.items);
    defer context.allocator.free(owned_symbols);

    const mode = determineMode(member_ctx.prefix, &.{owned_symbols});
    var filtered = MatchList.init(context.allocator);
    errdefer filtered.deinit();
    for (candidates.items.items) |candidate| {
        const symbol = candidate.symbol.getPtr().*;
        if (!symbolMatches(symbol.name, member_ctx.prefix, mode)) continue;
        try filtered.items.append(context.allocator, candidate);
    }
    candidates.items.clearRetainingCapacity();
    candidates.items.deinit(context.allocator);
    return filtered;
}

fn appendImportedModuleMembersFromText(
    matches: *MatchList,
    context: CollectMatchesContext,
    _: *runic.semantic.TypeChecker,
    object_name: []const u8,
) !void {
    var lexer = try runic.lexer.Lexer.init(context.allocator, context.file, context.text_slice);
    defer lexer.deinit();

    while (true) {
        const tok = try lexer.next();
        switch (tok.tag) {
            .kw_const, .kw_var => {
                const identifier = try lexer.next();
                if (identifier.tag != .identifier or !std.mem.eql(u8, identifier.lexeme, object_name)) continue;
                const assign = try lexer.next();
                if (assign.tag != .assign) continue;
                const import_kw = try lexer.next();
                if (import_kw.tag != .kw_import) continue;

                var next = try lexer.next();
                if (next.tag == .l_paren) {
                    next = try lexer.next();
                }
                if (next.tag != .string_start) continue;
                const string_text = try lexer.next();
                if (string_text.tag != .string_text) continue;

                const module_path = runic.document.resolveModulePath(
                    context.allocator,
                    context.file,
                    string_text.lexeme,
                ) catch continue;
                defer context.allocator.free(module_path);

                const module_type = ast.TypeExpr.ModuleType{
                    .path = module_path,
                    .span = string_text.span,
                };
                _ = module_type;
                try appendPubModuleDeclsFromFile(matches, context.allocator, module_path);
                try appendExecutionMembers(matches, context.allocator, object_name);
                return;
            },
            .identifier => {
                const next = lexer.next() catch return;
                if (isLikelyExecutableInitializer(tok, next)) {
                    try appendExecutionMembers(matches, context.allocator, object_name);
                    return;
                }
            },
            .eof => return,
            else => {},
        }
    }
}

fn isLikelyExecutableInitializer(first: runic.token.Token, second: runic.token.Token) bool {
    if (first.tag != .identifier) return false;

    return switch (second.tag) {
        .l_paren,
        .assign,
        .equal_equal,
        .bang_equal,
        .plus,
        .minus,
        .star,
        .slash,
        .percent,
        .dot,
        .question,
        .colon,
        .newline,
        .semicolon,
        .eof,
        => false,
        else => true,
    };
}

fn appendPubModuleDeclsFromFile(
    matches: *MatchList,
    allocator: Allocator,
    module_path: []const u8,
) !void {
    const file = std.fs.openFileAbsolute(module_path, .{}) catch return;
    defer file.close();
    const source = file.readToEndAlloc(allocator, 4 * 1024 * 1024) catch return;
    defer allocator.free(source);

    var lexer = try runic.lexer.Lexer.init(allocator, module_path, source);
    defer lexer.deinit();

    while (true) {
        const tok = try lexer.next();
        if (tok.tag == .eof) return;
        if (tok.tag != .kw_pub) continue;

        const decl_tok = try lexer.next();
        switch (decl_tok.tag) {
            .kw_const, .kw_var => {
                const identifier = try lexer.next();
                if (identifier.tag != .identifier) continue;
                try appendOwnedMatch(matches, allocator, .variable, identifier.lexeme, module_path, identifier.span);
            },
            .kw_fn => {
                const identifier = try lexer.next();
                if (identifier.tag != .identifier) continue;
                try appendOwnedMatch(matches, allocator, .function, identifier.lexeme, module_path, identifier.span);
            },
            else => {},
        }
    }
}

fn appendMembersForType(
    matches: *MatchList,
    context: CollectMatchesContext,
    type_checker: *runic.semantic.TypeChecker,
    type_expr: *const ast.TypeExpr,
    detail: []const u8,
) !void {
    switch (type_expr.*) {
        .alias => |alias_type| try appendMembersForType(matches, context, type_checker, type_checker.resolveAliasType(&alias_type), detail),
        .module => |module_type| {
            if (try type_checker.resolveModuleScopeForMemberCompletion(module_type)) |module_scope| {
                var it = module_scope.bindings.iterator();
                while (it.next()) |entry| {
                    const binding = entry.value_ptr.*;
                    if (!binding.is_pub) continue;
                    const binding_type = binding.type_expr orelse &ast.TypeExpr.executableType;
                    const kind: symbols.SymbolKind = switch (binding_type.*) {
                        .function, .fn_ref_type => .function,
                        else => .variable,
                    };
                    try appendOwnedMatch(matches, context.allocator, kind, binding.identifier.name, module_type.path, binding.identifier.span);
                }
            }
            try appendExecutionMembers(matches, context.allocator, detail);
        },
        .execution => try appendExecutionMembers(matches, context.allocator, detail),
        .thread => try appendOwnedMatch(matches, context.allocator, .method, "wait", detail, .global),
        .array => try appendOwnedMatch(matches, context.allocator, .field, "len", detail, .global),
        .struct_type => |struct_type| {
            for (struct_type.fields) |field| {
                try appendOwnedMatch(matches, context.allocator, .field, field.name.name, detail, field.name.span);
            }
            for (struct_type.decls) |decl| {
                const kind: symbols.SymbolKind = switch (decl.decl_source) {
                    .fn_decl => .method,
                    .binding_decl => .field,
                };
                try appendOwnedMatch(matches, context.allocator, kind, decl.name.name, detail, decl.span);
            }
        },
        else => {},
    }
}

fn appendExecutionMembers(
    matches: *MatchList,
    allocator: Allocator,
    detail: []const u8,
) !void {
    for ([_][]const u8{ "exit_code", "stdout", "stderr" }) |name| {
        try appendOwnedMatch(matches, allocator, .field, name, detail, .global);
    }
    try appendOwnedMatch(matches, allocator, .method, "wait", detail, .global);
}

fn appendOwnedMatch(
    matches: *MatchList,
    allocator: Allocator,
    kind: symbols.SymbolKind,
    name: []const u8,
    detail: []const u8,
    span: ast.Span,
) !void {
    for (matches.items.items) |existing| {
        if (std.mem.eql(u8, existing.symbol.getPtr().name, name)) return;
    }

    const symbol = try allocator.create(symbols.Symbol);
    errdefer allocator.destroy(symbol);
    symbol.* = .{
        .name = try allocator.dupe(u8, name),
        .detail = try allocator.dupe(u8, detail),
        .documentation = &.{},
        .kind = kind,
        .span = span,
    };

    try matches.items.append(allocator, .{
        .symbol = .{ .owned = symbol },
        .source = .document,
    });
}

fn ownedSymbolsSlice(allocator: Allocator, matches: []const Match) ![]symbols.Symbol {
    var list = try allocator.alloc(symbols.Symbol, matches.len);
    for (matches, 0..) |match, i| {
        list[i] = match.symbol.getPtr().*;
    }
    return list;
}

fn extractMemberContext(context: CollectMatchesContext) ?MemberContext {
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

    var member_start = cursor;
    while (member_start > offset and runic.lexer.isIdentifierContinue(text[member_start - 1])) {
        member_start -= 1;
    }

    if (member_start == offset or text[member_start - 1] != '.') return null;
    const object_end = member_start - 1;
    if (object_end == offset) return null;

    var object_start = object_end;
    while (object_start > offset and runic.lexer.isIdentifierContinue(text[object_start - 1])) {
        object_start -= 1;
    }

    const object_name = text[object_start..object_end];
    if (object_name.len == 0 or !runic.lexer.isIdentifierStart(object_name[0])) return null;

    return .{
        .object_name = object_name,
        .prefix = text[member_start..cursor],
    };
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
                        .span = .global,
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

test "extractMemberContext finds member access before cursor" {
    var ctx: CollectMatchesContext = undefined;
    ctx.text_slice =
        \\const m = import "./module.rn"
        \\echo m.v
        \\
    ;
    ctx.line_index = 1;
    ctx.char_index = 7;

    const member_ctx = extractMemberContext(ctx).?;
    try std.testing.expectEqualStrings("m", member_ctx.object_name);
    try std.testing.expectEqualStrings("", member_ctx.prefix);
}
