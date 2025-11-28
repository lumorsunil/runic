const std = @import("std");
const token = @import("token.zig");

const max_token_consumption_depth: usize = 1024;
const MAX_CONTEXT_STACK: usize = 100;

pub const Error = std.mem.Allocator.Error || std.Io.Writer.Error || error{
    UnexpectedCharacter,
    UnterminatedString,
    UnterminatedBlockComment,
    TokenConsumptionDepthExceeded,
    ContextStackUnderflow,
    IndexDidNotIncrease,
};

pub const Lexer = struct {
    source: []const u8,
    index: usize = 0,
    line: usize = 1,
    column: usize = 1,
    needs_trailing_newline: bool,
    emitted_trailing_newline: bool = false,
    active_next_depth: usize = 0,
    last_index: ?usize = null,
    last_context_len: ?usize = null,
    context_stack: std.ArrayList(Context),
    arena: std.heap.ArenaAllocator,
    logging_enabled: bool = false,

    const Context = union(enum) {
        root,
        string,
        string_interp: StringInterp,

        pub const StringInterp = struct {
            paren_counter: usize = 0,
            bracket_counter: usize = 0,
            brace_counter: usize = 0,

            pub const CounterType = enum {
                paren,
                bracket,
                brace,
            };

            pub fn field(self: *@This(), comptime ctType: CounterType) *usize {
                return &@field(self, @tagName(ctType) ++ "_counter");
            }

            pub fn inc(self: *@This(), comptime ctType: CounterType) void {
                self.field(ctType).* += 1;
            }

            pub fn dec(self: *@This(), comptime ctType: CounterType) void {
                self.field(ctType).* -= 1;
            }

            pub fn canEnd(self: @This()) bool {
                return self.paren_counter | self.brace_counter | self.brace_counter == 0;
            }
        };

        pub fn interp() Context {
            return .{ .string_interp = .{} };
        }
    };

    pub fn init(allocator_: std.mem.Allocator, source: []const u8) !Lexer {
        return .{
            .arena = .init(allocator_),
            .source = source,
            .context_stack = try .initCapacity(allocator_, MAX_CONTEXT_STACK),
            .needs_trailing_newline = source.len > 0 and !endsWithNewline(source),
        };
    }

    pub fn deinit(self: Lexer) void {
        self.arena.deinit();
    }

    pub fn allocator(self: *Lexer) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn currentContext(self: Lexer) Context {
        return self.context_stack.getLastOrNull() orelse .root;
    }

    pub fn currentContextPtr(self: Lexer) ?*Context {
        if (self.context_stack.items.len == 0) return null;
        return &self.context_stack.items[self.context_stack.items.len - 1];
    }

    pub fn pushContext(self: *Lexer, context: Context) !void {
        try self.context_stack.appendBounded(context);
    }

    pub fn popContext(self: *Lexer) !void {
        _ = self.context_stack.pop() orelse return Error.ContextStackUnderflow;
    }

    pub fn incCounter(self: *Lexer, comptime ctType: Context.StringInterp.CounterType) void {
        if (self.currentContextPtr()) |ctx| {
            switch (ctx.*) {
                .string_interp => |*interp| interp.inc(ctType),
                else => {},
            }
        }
    }

    pub fn decCounter(self: *Lexer, comptime ctType: Context.StringInterp.CounterType) void {
        if (self.currentContextPtr()) |ctx| {
            switch (ctx.*) {
                .string_interp => |*interp| interp.dec(ctType),
                else => {},
            }
        }
    }

    pub fn snapshot(self: Lexer) Lexer {
        return self;
    }

    pub fn restore(self: *Lexer, s: Lexer) void {
        self.* = s;
    }

    pub fn log(self: Lexer, comptime fmt: []const u8, args: anytype) !void {
        if (!self.logging_enabled) return;
        var stdout = std.fs.File.stderr().writer(&.{});
        try stdout.interface.print("[lexer:{}:{}:{}]: ", .{ self.index, self.line, self.column });
        try stdout.interface.print(fmt ++ "\n", args);
    }

    fn nextInner(self: *Lexer) Error!token.Token {
        if (self.currentContext() == .string) return self.lexStringText();

        if (self.lexStartInterp()) |start_interp_token| return start_interp_token;

        try self.skipWhitespaceAndComments();

        if (self.isAtEnd()) {
            if (self.needs_trailing_newline and !self.emitted_trailing_newline) {
                return try self.synthesizeTrailingNewline();
            }
            const loc = self.location();
            defer self.index += 1;
            return .{
                .tag = .eof,
                .lexeme = "",
                .span = .{ .start = loc, .end = loc },
            };
        }

        const ch = self.peek().?;

        if (ch == '}') switch (self.currentContext()) {
            .string_interp => |ctx| if (ctx.canEnd()) {
                const start = self.mark();
                _ = self.advance();
                try self.popContext();
                return self.finish(.startAt(start), .string_interp_end);
            },
            else => {},
        };

        if (ch == '\n' or ch == '\r') {
            return self.lexNewline();
        }

        if (isIdentifierStart(ch)) {
            return self.lexIdentifier();
        }

        if (std.ascii.isDigit(ch)) {
            return self.lexNumber();
        }

        return switch (ch) {
            '"' => self.lexString(),
            '(' => brk: {
                self.incCounter(.paren);
                break :brk self.singleCharToken(.l_paren);
            },
            ')' => brk: {
                self.decCounter(.paren);
                break :brk self.singleCharToken(.r_paren);
            },
            '{' => brk: {
                self.incCounter(.brace);
                break :brk self.singleCharToken(.l_brace);
            },
            '}' => brk: {
                self.decCounter(.brace);
                break :brk self.singleCharToken(.r_brace);
            },
            '[' => brk: {
                self.incCounter(.bracket);
                break :brk self.singleCharToken(.l_bracket);
            },
            ']' => brk: {
                self.decCounter(.bracket);
                break :brk self.singleCharToken(.r_bracket);
            },
            ',' => self.singleCharToken(.comma),
            ':' => self.singleCharToken(.colon),
            ';' => self.singleCharToken(.semicolon),
            '+' => self.singleCharToken(.plus),
            '-' => self.lexMinus(),
            '*' => self.singleCharToken(.star),
            '%' => self.singleCharToken(.percent),
            '^' => self.singleCharToken(.caret),
            '?' => self.singleCharToken(.question),
            '!' => self.lexBang(),
            '=' => self.lexEquals(),
            '>' => self.lexGreater(),
            '<' => self.lexLess(),
            '.' => self.lexDotLike(),
            '|' => self.lexPipe(),
            '&' => self.lexAmp(),
            '$' => self.singleCharToken(.dollar),
            '/' => self.singleCharToken(.slash),
            else => Error.UnexpectedCharacter,
        };
    }

    pub fn next(self: *Lexer) Error!token.Token {
        try self.log("next", .{});

        if (self.active_next_depth >= max_token_consumption_depth) {
            return Error.TokenConsumptionDepthExceeded;
        }
        self.active_next_depth += 1;
        defer self.active_next_depth -= 1;

        const next_token = try self.nextInner();

        const didIndexIncrease = self.last_index == self.index;
        const didContextChange = self.last_context_len == self.context_stack.items.len;
        if (didIndexIncrease and didContextChange) return Error.IndexDidNotIncrease;

        self.last_index = self.index;
        self.last_context_len = self.context_stack.items.len;

        return next_token;
    }

    fn lexIdentifier(self: *Lexer) token.Token {
        const start = self.location();
        const start_index = self.index;
        _ = self.advance(); // first character already known as identifier start
        while (!self.isAtEnd()) {
            const ch = self.peek().?;
            if (!isIdentifierContinue(ch)) break;
            _ = self.advance();
        }
        const lexeme = self.source[start_index..self.index];
        const tag = token.identifierTag(lexeme);
        return .{
            .tag = tag,
            .lexeme = lexeme,
            .span = .{ .start = start, .end = self.location() },
        };
    }

    fn lexStartInterp(self: *Lexer) ?token.Token {
        if (self.currentContextPtr()) |ctx| {
            switch (ctx.*) {
                .string_interp => {
                    if (self.peekSliceIs("${")) {
                        const start = self.mark();
                        _ = self.advance();
                        _ = self.advance();
                        return self.finish(.startAt(start), .string_interp_start);
                    }
                },
                else => {},
            }
        }

        return null;
    }

    fn lexNumber(self: *Lexer) token.Token {
        const start = self.location();
        const start_index = self.index;
        var has_dot = false;
        var has_exponent = false;

        self.consumeDigits();
        if (self.peekIs('.')) {
            if (!self.peekNextIs('.')) {
                has_dot = true;
                _ = self.advance(); // consume '.'
                self.consumeDigits();
            }
        }

        if (self.peekIs('e') or self.peekIs('E')) {
            has_exponent = true;
            _ = self.advance();
            if (self.peekIs('+') or self.peekIs('-')) _ = self.advance();
            self.consumeDigits();
        }

        const lexeme = self.source[start_index..self.index];
        return .{
            .tag = if (has_dot or has_exponent) .float_literal else .int_literal,
            .lexeme = lexeme,
            .span = .{ .start = start, .end = self.location() },
        };
    }

    fn lexString(self: *Lexer) Error!token.Token {
        const start = self.mark();
        try self.pushContext(.string);
        _ = self.advance(); // consume opening quote
        return self.finish(.startAt(start), .string_start);
    }

    fn lexStringText(self: *Lexer) Error!token.Token {
        const start = self.mark();

        {
            const ch = self.peek().?;
            if (ch == '"') {
                try self.popContext();
                _ = self.advance();
                return self.finish(.startAt(start), .string_end);
            }
        }

        while (!self.isAtEnd()) {
            const ch = self.peek().?;
            if (ch == '"') {
                return self.finish(.startAt(start), .string_text);
            }
            if (ch == '\\') {
                _ = self.advance();
                if (self.isAtEnd()) break;
                _ = self.advance();
                continue;
            }
            if (ch == '$') {
                if (self.peekSliceIs("${")) {
                    try self.pushContext(.interp());
                    return self.finish(.startAt(start), .string_text);
                }
                continue;
            }
            if (ch == '\n' or ch == '\r') break;
            _ = self.advance();
        }
        return Error.UnterminatedString;
    }

    fn lexNewline(self: *Lexer) Error!token.Token {
        try self.log("newline", .{});

        const start = self.mark();
        const consumed = self.advance().?;
        if (consumed == '\r') {
            if (self.match('\n')) {
                // newline already accounted for by '\n'
            } else {
                self.line += 1;
                self.column = 1;
            }
        } else if (consumed == '\n') {
            // already handled in advance()
        } else {
            // Should be unreachable, but we fall back to emitting newline anyway.
        }
        return self.finish(.startAt(start), .newline);
    }

    fn synthesizeTrailingNewline(self: *Lexer) Error!token.Token {
        try self.log("synthesizeTrailingNewline", .{});
        const start = self.location();
        self.emitted_trailing_newline = true;
        defer self.index += 1;
        self.line += 1;
        self.column = 1;
        const end = self.location();
        return .{
            .tag = .newline,
            .lexeme = self.source[self.index..self.index],
            .span = .{ .start = start, .end = end },
        };
    }

    fn lexMinus(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('>')) {
            return self.finish(.startAt(start), .arrow);
        }
        return self.finish(.startAt(start), .minus);
    }

    fn lexBang(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('=')) {
            return self.finish(.startAt(start), .bang_equal);
        }
        return self.finish(.startAt(start), .bang);
    }

    fn lexEquals(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('>')) {
            return self.finish(.startAt(start), .fat_arrow);
        }
        if (self.match('=')) {
            return self.finish(.startAt(start), .equal_equal);
        }
        return self.finish(.startAt(start), .assign);
    }

    fn lexGreater(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('=')) {
            return self.finish(.startAt(start), .greater_equal);
        }
        return self.finish(.startAt(start), .greater);
    }

    fn lexLess(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('=')) {
            return self.finish(.startAt(start), .less_equal);
        }
        return self.finish(.startAt(start), .less);
    }

    fn lexDotLike(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('.')) {
            if (self.match('.')) {
                return self.finish(.startAt(start), .ellipsis);
            }
            return self.finish(.startAt(start), .range);
        }
        return self.finish(.startAt(start), .dot);
    }

    fn lexPipe(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('|')) {
            return self.finish(.startAt(start), .pipe_pipe);
        }
        return self.finish(.startAt(start), .pipe);
    }

    fn lexAmp(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('&')) {
            return self.finish(.startAt(start), .amp_amp);
        }
        return self.finish(.startAt(start), .amp);
    }

    fn singleCharToken(self: *Lexer, tag: token.Tag) token.Token {
        const start = self.mark();
        _ = self.advance();
        return self.finish(.startAt(start), tag);
    }

    fn skipWhitespaceAndComments(self: *Lexer) Error!void {
        try self.log("Skipping whitespace", .{});
        while (!self.isAtEnd()) {
            const ch = self.peek().?;
            switch (ch) {
                ' ', '\t', '\x0b', '\x0c' => {
                    _ = self.advance();
                    continue;
                },
                '\n', '\r' => return,
                '#' => {
                    self.skipLineComment();
                    continue;
                },
                '/' => {
                    if (self.peekNextIs('/')) {
                        self.skipLineComment();
                        continue;
                    } else if (self.peekNextIs('*')) {
                        try self.skipBlockComment();
                        continue;
                    }
                    return;
                },
                else => return,
            }
            if (self.isAtEnd()) return;
        }
    }

    fn skipLineComment(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const ch = self.peek().?;
            if (ch == '\n') break;
            _ = self.advance();
        }
    }

    fn skipBlockComment(self: *Lexer) Error!void {
        // consume "/*"
        _ = self.advance();
        _ = self.advance();
        var depth: usize = 1;
        while (depth > 0) {
            if (self.isAtEnd()) return Error.UnterminatedBlockComment;
            const ch = self.advance().?;
            if (ch == '/' and self.peekIs('*')) {
                _ = self.advance();
                depth += 1;
            } else if (ch == '*' and self.peekIs('/')) {
                _ = self.advance();
                depth -= 1;
            }
        }
    }

    fn consumeDigits(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const ch = self.peek().?;
            if (!std.ascii.isDigit(ch)) break;
            _ = self.advance();
        }
    }

    fn peek(self: *Lexer) ?u8 {
        if (self.index >= self.source.len) return null;
        return self.source[self.index];
    }

    fn peekSlice(self: *Lexer, len: usize) ?[]const u8 {
        if (self.index + len - 1 >= self.source.len) return null;
        return self.source[self.index .. self.index + len];
    }

    fn peekSliceIs(self: *Lexer, comptime s: []const u8) bool {
        if (self.index + s.len - 1 >= self.source.len) return false;
        const src = self.source[self.index .. self.index + s.len];
        return std.mem.eql(u8, src, s);
    }

    fn peekIs(self: *Lexer, needle: u8) bool {
        if (self.isAtEnd()) return false;
        return self.source[self.index] == needle;
    }

    fn peekNextIs(self: *Lexer, needle: u8) bool {
        const next_index = self.index + 1;
        if (next_index >= self.source.len) return false;
        return self.source[next_index] == needle;
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.index] != expected) return false;
        _ = self.advance();
        return true;
    }

    fn advance(self: *Lexer) ?u8 {
        if (self.index >= self.source.len) return null;
        const ch = self.source[self.index];
        self.index += 1;
        if (ch == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        return ch;
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.index >= self.source.len;
    }

    fn location(self: *Lexer) token.Location {
        return .{
            .line = self.line,
            .column = self.column,
            .offset = self.index,
        };
    }

    const TokenMark = struct {
        loc: token.Location,
        index: usize,
    };

    fn mark(self: *Lexer) TokenMark {
        return .{ .loc = self.location(), .index = self.index };
    }

    const TokenFinishType = union(enum) {
        start: TokenMark,
        start_end: struct { TokenMark, TokenMark },

        pub fn startAt(marker: TokenMark) TokenFinishType {
            return .{ .start = marker };
        }

        pub fn startEnd(start: TokenMark, end: TokenMark) TokenFinishType {
            return .{ .start_end = .{ start, end } };
        }
    };

    fn finish(
        self: *Lexer,
        finishType: TokenFinishType,
        tag: token.Tag,
    ) token.Token {
        const span: token.Span = switch (finishType) {
            .start => |marker| .fromLocs(marker.loc, self.location()),
            .start_end => |markers| .fromLocs(markers.@"0".loc, markers.@"1".loc),
        };

        return .{
            .tag = tag,
            .lexeme = span.sliceFrom(self.source),
            .span = span,
        };
    }
};

/// Stream provides a higher-level, pull-based view over the lexer so the
/// parser can peek ahead without materializing the entire token list.
pub const Stream = struct {
    lexer: Lexer,
    current: ?token.Token = null,
    peeked_tokens_buffer: std.ArrayList(token.Token) = .empty,
    active_next_depth: usize = 0,

    pub const max_guard_depth: usize = max_token_consumption_depth;

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Stream {
        return .{ .lexer = try .init(allocator, source) };
    }

    pub fn deinit(self: Stream) void {
        self.lexer.deinit();
    }

    pub fn snapshot(self: Stream) Stream {
        return self;
    }

    /// Invalidates the buffer in the snapshot
    pub fn restore(self: *Stream, s: Stream) void {
        const clone = try s.peeked_tokens_buffer.clone(self.lexer.allocator());
        s.peeked_tokens_buffer.deinit(self.lexer.allocator());
        self.peeked_tokens_buffer.deinit(self.lexer.allocator());
        self.* = s;
        self.peeked_tokens_buffer = clone;
    }

    pub fn next(self: *Stream) Error!token.Token {
        if (self.peeked_tokens_buffer.items.len > 0) {
            const tok = self.peeked_tokens_buffer.orderedRemove(0);
            self.current = tok;
            return tok;
        }
        return self.guardedNext();
    }

    pub fn peek(self: *Stream) Error!token.Token {
        if (self.peeked_tokens_buffer.items.len == 0) {
            try self.peeked_tokens_buffer.append(self.lexer.allocator(), try self.guardedNext());
        }
        return self.peeked_tokens_buffer.items[0];
    }

    pub fn peekSlice(self: *Stream, len: usize) Error![]const token.Token {
        const len_needed = len -| self.peeked_tokens_buffer.items.len;

        if (len_needed > 0) for (0..len_needed) |_| {
            try self.peeked_tokens_buffer.append(self.lexer.allocator(), try self.guardedNext());
        };

        return self.peeked_tokens_buffer.items[0..len];
    }

    pub fn consumeIf(self: *Stream, tag: token.Tag) Error!bool {
        const tok = try self.peek();
        if (tok.tag == tag) {
            _ = try self.next();
            return true;
        }
        return false;
    }

    fn guardedNext(self: *Stream) Error!token.Token {
        if (self.active_next_depth >= max_guard_depth) {
            return Error.TokenConsumptionDepthExceeded;
        }
        self.active_next_depth += 1;
        defer self.active_next_depth -= 1;
        const next_token = try self.lexer.next();
        try self.lexer.log("next token: {}", .{next_token.tag});
        self.current = next_token;
        return next_token;
    }
};

fn isIdentifierStart(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isIdentifierContinue(ch: u8) bool {
    return isIdentifierStart(ch) or std.ascii.isDigit(ch);
}

fn endsWithNewline(source: []const u8) bool {
    if (source.len == 0) return false;
    const last = source[source.len - 1];
    return last == '\n' or last == '\r';
}

const LexFixture = struct {
    name: []const u8,
    source: []const u8,
    tokens: []const token.Tag,
};

fn runLexFixture(fixture: LexFixture) !void {
    var lx = Lexer.init(fixture.source);
    var index: usize = 0;
    errdefer std.debug.print("lexer fixture `{s}` failed around token #{d}\n", .{ fixture.name, index });
    const maxTokens = 1000000;
    var i: usize = 0;
    while (i < maxTokens) : (i += 1) {
        try std.testing.expect(index < fixture.tokens.len);
        const pre_index = lx.index;
        const pre_line = lx.line;
        const pre_column = lx.column;
        const tok = try lx.next();
        if (pre_index == lx.index) {
            if (tok.tag == .newline) {
                try std.testing.expect(lx.emitted_trailing_newline);
            } else {
                try std.testing.expect(tok.tag == .eof);
            }
        } else {
            try std.testing.expect(pre_index < lx.index);
            try std.testing.expect(pre_line < lx.line or pre_column < lx.column);
        }
        const expected = fixture.tokens[index];
        index += 1;
        try std.testing.expectEqual(expected, tok.tag);
        if (tok.tag == .eof) break;
    }
    try std.testing.expectEqual(fixture.tokens.len, index);
}

test "lexer fixtures cover feature surfaces" {
    const fixtures = [_]LexFixture{
        .{
            .name = "commands_and_pipelines",
            .source =
            \\echo "hi" | upper && lower || fail & sleep 1
            \\cmd2; cmd3
            ,
            .tokens = &[_]token.Tag{
                .identifier, .string_literal, .pipe,       .identifier, .amp_amp,     .identifier,
                .pipe_pipe,  .identifier,     .amp,        .identifier, .int_literal, .newline,
                .identifier, .semicolon,      .identifier, .newline,    .eof,
            },
        },
        .{
            .name = "declarations_and_types",
            .source =
            \\let greeting: Str = "hi"
            \\mut count = 2
            \\fn add(a: Int, b: Int) -> Int {
            \\    return a + b
            \\}
            ,
            .tokens = &[_]token.Tag{
                .kw_const,   .identifier, .colon,      .identifier,  .assign,  .string_literal, .newline,
                .kw_var,     .identifier, .assign,     .int_literal, .newline, .kw_fn,          .identifier,
                .l_paren,    .identifier, .colon,      .identifier,  .comma,   .identifier,     .colon,
                .identifier, .r_paren,    .arrow,      .identifier,  .l_brace, .newline,        .kw_return,
                .identifier, .plus,       .identifier, .newline,     .r_brace, .newline,        .eof,
            },
        },
        .{
            .name = "literal_data_structures",
            .source =
            \\let array = [1, 2.5, true]
            \\let mapping = { key: null, nested: [false] }
            ,
            .tokens = &[_]token.Tag{
                .kw_const,  .identifier, .assign,   .l_bracket,  .int_literal, .comma,     .float_literal, .comma,   .kw_true,
                .r_bracket, .newline,    .kw_const, .identifier, .assign,      .l_brace,   .identifier,    .colon,   .kw_null,
                .comma,     .identifier, .colon,    .l_bracket,  .kw_false,    .r_bracket, .r_brace,       .newline, .eof,
            },
        },
        .{
            .name = "errors_and_match",
            .source =
            \\error NetworkError = enum { Timeout, ConnectionLost }
            \\error FileError = union { NotFound: { path: Str }, PermissionDenied }
            \\try bootstrap() catch |err| match err { _ => fail }
            ,
            .tokens = &[_]token.Tag{
                .kw_error,   .identifier, .assign,     .kw_enum,    .l_brace,  .identifier, .comma,   .identifier, .r_brace,    .newline,
                .kw_error,   .identifier, .assign,     .kw_union,   .l_brace,  .identifier, .colon,   .l_brace,    .identifier, .colon,
                .identifier, .r_brace,    .comma,      .identifier, .r_brace,  .newline,    .kw_try,  .identifier, .l_paren,    .r_paren,
                .kw_catch,   .pipe,       .identifier, .pipe,       .kw_match, .identifier, .l_brace, .identifier, .fat_arrow,  .identifier,
                .r_brace,    .newline,    .eof,
            },
        },
        .{
            .name = "async_loops_and_bash",
            .source =
            \\async fn fetch(): ^Result {
            \\    let maybe: ?Int = await job catch |err| { return err }
            \\    for (items, range) |item, idx| { while idx < 3 { if (idx > 0) { bash { echo "inner" } } } }
            \\}
            ,
            .tokens = &[_]token.Tag{
                .kw_async,   .kw_fn,      .identifier,  .l_paren,    .r_paren,        .colon,   .caret,      .identifier, .l_brace,     .newline,
                .kw_const,   .identifier, .colon,       .question,   .identifier,     .assign,  .kw_await,   .identifier, .kw_catch,    .pipe,
                .identifier, .pipe,       .l_brace,     .kw_return,  .identifier,     .r_brace, .newline,    .kw_for,     .l_paren,     .identifier,
                .comma,      .identifier, .r_paren,     .pipe,       .identifier,     .comma,   .identifier, .pipe,       .l_brace,     .kw_while,
                .identifier, .less,       .int_literal, .l_brace,    .kw_if,          .l_paren, .identifier, .greater,    .int_literal, .r_paren,
                .l_brace,    .kw_bash,    .l_brace,     .identifier, .string_literal, .r_brace, .r_brace,    .r_brace,    .r_brace,     .newline,
                .r_brace,    .newline,    .eof,
            },
        },
        .{
            .name = "modules_and_access",
            .source =
            \\let http = import("net/http")
            \\let resp = http.client.get
            ,
            .tokens = &[_]token.Tag{
                .kw_const, .identifier, .assign, .kw_import,  .l_paren, .string_literal, .r_paren, .newline,
                .kw_const, .identifier, .assign, .identifier, .dot,     .identifier,     .dot,     .identifier,
                .newline,  .eof,
            },
        },
        .{
            .name = "operators_and_ranges",
            .source =
            \\if (value >= 10) { value = value * 2 - 1 } else { value = value / 2 }
            \\let slice = items[1..3]
            \\let spread = 1...3
            \\while (count != 0 && !done) { count = count % 2 / 2 }
            \\if (limit <= 5) { match item { _ => item } }
            ,
            .tokens = &[_]token.Tag{
                .kw_if,       .l_paren,     .identifier,  .greater_equal, .int_literal, .r_paren,    .l_brace,
                .identifier,  .assign,      .identifier,  .star,          .int_literal, .minus,      .int_literal,
                .r_brace,     .kw_else,     .l_brace,     .identifier,    .assign,      .identifier, .slash,
                .int_literal, .r_brace,     .newline,     .kw_const,      .identifier,  .assign,     .identifier,
                .l_bracket,   .int_literal, .range,       .int_literal,   .r_bracket,   .newline,    .kw_const,
                .identifier,  .assign,      .int_literal, .ellipsis,      .int_literal, .newline,    .kw_while,
                .l_paren,     .identifier,  .bang_equal,  .int_literal,   .amp_amp,     .bang,       .identifier,
                .r_paren,     .l_brace,     .identifier,  .assign,        .identifier,  .percent,    .int_literal,
                .slash,       .int_literal, .r_brace,     .newline,       .kw_if,       .l_paren,    .identifier,
                .less_equal,  .int_literal, .r_paren,     .l_brace,       .kw_match,    .identifier, .l_brace,
                .identifier,  .fat_arrow,   .identifier,  .r_brace,       .r_brace,     .newline,    .eof,
            },
        },
    };

    for (fixtures) |fixture| {
        try runLexFixture(fixture);
    }
}

const LexErrorFixture = struct {
    name: []const u8,
    source: []const u8,
    err: Error,
};

test "lexer failure fixtures report precise errors" {
    const fixtures = [_]LexErrorFixture{
        .{ .name = "unexpected_character", .source = "@", .err = Error.UnexpectedCharacter },
        .{ .name = "unterminated_string", .source = "\"missing", .err = Error.UnterminatedString },
        .{ .name = "unterminated_block_comment", .source = "/* block", .err = Error.UnterminatedBlockComment },
    };

    for (fixtures) |fixture| {
        var lx = Lexer.init(fixture.source);
        errdefer std.debug.print("lexer error fixture `{s}` did not raise expected error\n", .{fixture.name});
        try std.testing.expectError(fixture.err, lx.next());
    }
}

test "lexer emits tokens with spans" {
    const src =
        \\let greeting = "hello"
        \\mut count = 2
        \\echo greeting | upper
    ;

    var lx = Lexer.init(src);
    const expected_tags = [_]token.Tag{
        .kw_const,   .identifier, .assign, .string_literal, .newline,
        .kw_var,     .identifier, .assign, .int_literal,    .newline,
        .identifier, .identifier, .pipe,   .identifier,     .newline,
        .eof,
    };

    for (expected_tags) |tag| {
        const tok = try lx.next();
        try std.testing.expectEqual(tag, tok.tag);
        try std.testing.expect(tok.span.end.line >= tok.span.start.line);
    }
}

test "lexer spans report precise lines and columns" {
    const src =
        \\let foo = 1
        \\upper
        \\mut bar = foo
    ;

    var lx = Lexer.init(src);
    const let_tok = try lx.next();
    try std.testing.expectEqual(@as(usize, 1), let_tok.span.start.line);
    try std.testing.expectEqual(@as(usize, 1), let_tok.span.start.column);

    _ = try lx.next(); // foo
    _ = try lx.next(); // =
    _ = try lx.next(); // 1
    _ = try lx.next(); // newline

    const upper_tok = try lx.next();
    try std.testing.expectEqual(@as(usize, 2), upper_tok.span.start.line);
    try std.testing.expectEqual(@as(usize, 1), upper_tok.span.start.column);

    _ = try lx.next(); // newline

    const mut_tok = try lx.next();
    try std.testing.expectEqual(@as(usize, 3), mut_tok.span.start.line);
    try std.testing.expectEqual(@as(usize, 1), mut_tok.span.start.column);
}

test "lexer stream supports peeking and conditional consumption" {
    const src =
        \\let name = "hi"
        \\upper
    ;

    var stream = Stream.init(src);

    const peek_let = try stream.peek();
    try std.testing.expectEqual(token.Tag.kw_const, peek_let.tag);

    const let_tok = try stream.next();
    try std.testing.expectEqual(token.Tag.kw_const, let_tok.tag);

    const ident_tok = try stream.peek();
    try std.testing.expectEqualSlices(u8, "name", ident_tok.lexeme);
    try std.testing.expectEqual(token.Tag.identifier, ident_tok.tag);

    _ = try stream.next(); // consume identifier
    _ = try stream.next(); // consume '='
    _ = try stream.next(); // consume "hi"

    try std.testing.expect(try stream.consumeIf(.newline));

    const command_tok = try stream.peek();
    try std.testing.expectEqual(token.Tag.identifier, command_tok.tag);
    try std.testing.expectEqualStrings("upper", command_tok.lexeme);

    try std.testing.expect(!(try stream.consumeIf(.newline)));
}

test "stream guard detects runaway token consumption" {
    var stream = Stream.init("let value = 1");
    stream.active_next_depth = Stream.max_guard_depth;
    try std.testing.expectError(Error.TokenConsumptionDepthExceeded, stream.next());

    var peek_stream = Stream.init("upper");
    peek_stream.active_next_depth = Stream.max_guard_depth;
    try std.testing.expectError(Error.TokenConsumptionDepthExceeded, peek_stream.peek());
}

test "lexer guard detects runaway token consumption" {
    var lx = Lexer.init("identifier");
    lx.active_next_depth = max_token_consumption_depth;
    try std.testing.expectError(Error.TokenConsumptionDepthExceeded, lx.next());
}
