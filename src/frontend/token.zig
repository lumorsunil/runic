const std = @import("std");

/// Location describes a byte offset inside the original source text alongside
/// the 1-based line/column pair so diagnostics can point at the exact token.
pub const Location = struct {
    file: []const u8,
    line: usize,
    column: usize,
    offset: usize,

    pub const global = Location{
        .file = "<global scope>",
        .column = 0,
        .line = 0,
        .offset = 0,
    };

    pub fn isGlobal(self: Location) bool {
        return self.column == 0 and self.line == 0 and self.offset == 0;
    }
};

/// Span represents the start and end location of a token. The end location is
/// exclusive so slicing works naturally.
pub const Span = struct {
    start: Location,
    end: Location,

    pub const global: Span = .{ .start = .global, .end = .global };

    pub fn fromLocs(start: Location, end: Location) Span {
        return .{ .start = start, .end = end };
    }

    pub fn fromTo(start: Span, end: Span) Span {
        return .{ .start = start.start, .end = end.end };
    }

    pub fn startAt(self: Span, start: Span) Span {
        var new = self;
        new.start = start.start;
        return new;
    }

    pub fn endAt(self: Span, end: Span) Span {
        var new = self;
        new.end = end.end;
        return new;
    }

    pub fn containsLoc(self: Span, loc: Location) bool {
        return std.mem.eql(u8, loc.file, self.start.file) and self.contains(loc.line, loc.column);
    }

    pub fn contains(self: Span, line: usize, column: usize) bool {
        if (self.start.line == line and self.end.line == line) {
            return self.start.column <= column and self.end.column >= column;
        }

        if (self.start.line == line) {
            return self.start.column <= column;
        }

        if (self.end.line == line) {
            return self.end.column >= column;
        }

        return self.start.line <= line and self.end.line >= line;
    }

    pub fn sliceFrom(self: Span, source: []const u8) []const u8 {
        return source[self.start.offset..self.end.offset];
    }

    pub fn isGlobal(self: Span) bool {
        return self.start.isGlobal();
    }
};

pub fn Spanned(comptime T: type) type {
    return struct {
        payload: T,
        span: Span,

        pub fn fromToken(tok: Token) ?Spanned(T) {
            return .{
                .payload = T.fromToken(tok) orelse return null,
                .span = tok.span,
            };
        }
    };
}

/// Token is the basic unit emitted by the lexer.
pub const Token = struct {
    tag: Tag,
    /// Slice of the original source backing this token.
    lexeme: []const u8,
    span: Span,

    pub fn isKeyword(self: Token) bool {
        return self.tag.toKeyword() != null;
    }

    pub fn format(self: Token, writer: *std.Io.Writer) !void {
        try writer.writeAll(@tagName(self.tag));
    }
};

/// Tag enumerates every lexical category Runic currently understands.
pub const Tag = enum {
    eof,
    newline,

    /// Bare words that double as both command segments and expression
    /// identifiers depending on context.
    identifier,

    // Literal tokens
    int_literal,
    float_literal,

    // String tokens
    string_start,
    string_end,
    string_interp_start,
    string_interp_end,
    string_text,

    // Declaration keywords
    /// Immutable variable declarations and destructuring.
    kw_const,
    /// Explicitly mutable variable declarations.
    kw_var,
    /// Function declarations/expressions.
    kw_fn,

    // Error/type keywords
    /// Starts error set declarations.
    kw_error,
    /// Plain error enum variant blocks.
    kw_enum,
    /// Tagged union error variant blocks.
    kw_union,
    /// Struct.
    kw_struct,

    // Async/promise keywords
    /// Async blocks/functions returning ^T.
    kw_async,
    /// Await expression keyword used with ^T.
    kw_await,

    // Flow control keywords
    kw_if,
    kw_else,
    kw_for,
    kw_while,
    kw_match,
    kw_return,

    // Module/interop keywords
    /// Module import keyword.
    kw_import,

    // Error-handling keywords
    kw_try,
    kw_catch,

    // Literal keywords
    kw_true,
    kw_false,
    kw_null,

    // Operators / punctuation
    plus,
    minus,
    star,
    slash,
    dollar,
    percent,
    /// Promise prefix "^".
    caret,
    amp,
    amp_amp,
    kw_and,
    /// Stage separator for pipelines.
    pipe,
    pipe_pipe,
    kw_or,
    bang,
    /// Optional prefix "?".
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
    /// Scope/map literal opener.
    l_brace,
    /// Scope/map literal closer.
    r_brace,
    l_bracket,
    r_bracket,
    dot_l_brace,

    pub fn toKeyword(tag: Tag) ?[]const u8 {
        return switch (tag) {
            .kw_const => "const",
            .kw_var => "var",
            .kw_fn => "fn",
            .kw_error => "error",
            .kw_enum => "enum",
            .kw_union => "union",
            .kw_struct => "struct",
            .kw_async => "async",
            .kw_await => "await",
            .kw_if => "if",
            .kw_else => "else",
            .kw_for => "for",
            .kw_while => "while",
            .kw_match => "match",
            .kw_return => "return",
            .kw_import => "import",
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

const keyword_map = std.StaticStringMap(Tag).initComptime(.{
    .{ "const", .kw_const },
    .{ "var", .kw_var },
    .{ "fn", .kw_fn },
    .{ "error", .kw_error },
    .{ "enum", .kw_enum },
    .{ "union", .kw_union },
    .{ "struct", .kw_struct },
    .{ "async", .kw_async },
    .{ "await", .kw_await },
    .{ "if", .kw_if },
    .{ "else", .kw_else },
    .{ "for", .kw_for },
    .{ "while", .kw_while },
    .{ "match", .kw_match },
    .{ "return", .kw_return },
    .{ "import", .kw_import },
    .{ "try", .kw_try },
    .{ "catch", .kw_catch },
    .{ "true", .kw_true },
    .{ "false", .kw_false },
    .{ "null", .kw_null },
    .{ "and", .kw_and },
    .{ "or", .kw_or },
});
