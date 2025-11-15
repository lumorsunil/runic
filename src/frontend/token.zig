const std = @import("std");

/// Location describes a byte offset inside the original source text alongside
/// the 1-based line/column pair so diagnostics can point at the exact token.
pub const Location = struct {
    line: usize,
    column: usize,
    offset: usize,
};

/// Span represents the start and end location of a token. The end location is
/// exclusive so slicing works naturally.
pub const Span = struct {
    start: Location,
    end: Location,
};

/// Token is the basic unit emitted by the lexer.
pub const Token = struct {
    tag: Tag,
    /// Slice of the original source backing this token.
    lexeme: []const u8,
    span: Span,

    pub fn isKeyword(self: Token) bool {
        return self.tag.toKeyword() != null;
    }
};

/// Tag enumerates every lexical category Runic currently understands.
pub const Tag = enum {
    eof,
    newline,

    identifier,
    int_literal,
    float_literal,
    string_literal,

    // Keywords
    kw_let,
    kw_mut,
    kw_fn,
    kw_error,
    kw_enum,
    kw_union,
    kw_async,
    kw_await,
    kw_if,
    kw_else,
    kw_for,
    kw_while,
    kw_match,
    kw_return,
    kw_import,
    kw_from,
    kw_bash,
    kw_try,
    kw_catch,
    kw_true,
    kw_false,
    kw_null,

    // Operators / punctuation
    plus,
    minus,
    star,
    slash,
    percent,
    caret,
    amp,
    amp_amp,
    pipe,
    pipe_pipe,
    bang,
    question,
    colon,
    semicolon,
    comma,
    dot,
    range, // ".."
    ellipsis, // "..."
    assign,
    equal_equal,
    bang_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    arrow, // "->"
    fat_arrow, // "=>"
    l_paren,
    r_paren,
    l_brace,
    r_brace,
    l_bracket,
    r_bracket,

    pub fn toKeyword(tag: Tag) ?[]const u8 {
        return switch (tag) {
            .kw_let => "let",
            .kw_mut => "mut",
            .kw_fn => "fn",
            .kw_error => "error",
            .kw_enum => "enum",
            .kw_union => "union",
            .kw_async => "async",
            .kw_await => "await",
            .kw_if => "if",
            .kw_else => "else",
            .kw_for => "for",
            .kw_while => "while",
            .kw_match => "match",
            .kw_return => "return",
            .kw_import => "import",
            .kw_from => "from",
            .kw_bash => "bash",
            .kw_try => "try",
            .kw_catch => "catch",
            .kw_true => "true",
            .kw_false => "false",
            .kw_null => "null",
            else => null,
        };
    }
};

/// Returns the keyword tag for the given identifier text.
pub fn keywordFor(name: []const u8) ?Tag {
    return keyword_map.get(name);
}

/// Identifiers that match keywords adopt the keyword tag, otherwise they remain
/// as generic identifiers.
pub fn identifierTag(name: []const u8) Tag {
    if (keywordFor(name)) |kw| return kw;
    return .identifier;
}

const keyword_map = std.ComptimeStringMap(Tag, .{
    .{ "let", .kw_let },
    .{ "mut", .kw_mut },
    .{ "fn", .kw_fn },
    .{ "error", .kw_error },
    .{ "enum", .kw_enum },
    .{ "union", .kw_union },
    .{ "async", .kw_async },
    .{ "await", .kw_await },
    .{ "if", .kw_if },
    .{ "else", .kw_else },
    .{ "for", .kw_for },
    .{ "while", .kw_while },
    .{ "match", .kw_match },
    .{ "return", .kw_return },
    .{ "import", .kw_import },
    .{ "from", .kw_from },
    .{ "bash", .kw_bash },
    .{ "try", .kw_try },
    .{ "catch", .kw_catch },
    .{ "true", .kw_true },
    .{ "false", .kw_false },
    .{ "null", .kw_null },
});
