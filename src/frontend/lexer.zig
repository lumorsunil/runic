const std = @import("std");
const token = @import("token.zig");

pub const Error = error{
    UnexpectedCharacter,
    UnterminatedString,
    UnterminatedBlockComment,
};

pub const Lexer = struct {
    source: []const u8,
    index: usize = 0,
    line: usize = 1,
    column: usize = 1,

    pub fn init(source: []const u8) Lexer {
        return .{ .source = source };
    }

    pub fn next(self: *Lexer) Error!token.Token {
        try self.skipWhitespaceAndComments();

        if (self.isAtEnd()) {
            const loc = self.location();
            return .{
                .tag = .eof,
                .lexeme = self.source[self.index..self.index],
                .span = .{ .start = loc, .end = loc },
            };
        }

        const ch = self.peek().?;
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
            '(' => self.singleCharToken(.l_paren),
            ')' => self.singleCharToken(.r_paren),
            '{' => self.singleCharToken(.l_brace),
            '}' => self.singleCharToken(.r_brace),
            '[' => self.singleCharToken(.l_bracket),
            ']' => self.singleCharToken(.r_bracket),
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
            '/' => self.singleCharToken(.slash),
            else => Error.UnexpectedCharacter,
        };
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
        _ = self.advance(); // consume opening quote
        while (!self.isAtEnd()) {
            const ch = self.advance().?;
            if (ch == '"') {
                return self.finish(start, .string_literal);
            }
            if (ch == '\\') {
                if (self.isAtEnd()) break;
                _ = self.advance();
                continue;
            }
            if (ch == '\n' or ch == '\r') break;
        }
        return Error.UnterminatedString;
    }

    fn lexNewline(self: *Lexer) token.Token {
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
        return self.finish(start, .newline);
    }

    fn lexMinus(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('>')) {
            return self.finish(start, .arrow);
        }
        return self.finish(start, .minus);
    }

    fn lexBang(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('=')) {
            return self.finish(start, .bang_equal);
        }
        return self.finish(start, .bang);
    }

    fn lexEquals(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('>')) {
            return self.finish(start, .fat_arrow);
        }
        if (self.match('=')) {
            return self.finish(start, .equal_equal);
        }
        return self.finish(start, .assign);
    }

    fn lexGreater(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('=')) {
            return self.finish(start, .greater_equal);
        }
        return self.finish(start, .greater);
    }

    fn lexLess(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('=')) {
            return self.finish(start, .less_equal);
        }
        return self.finish(start, .less);
    }

    fn lexDotLike(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('.')) {
            if (self.match('.')) {
                return self.finish(start, .ellipsis);
            }
            return self.finish(start, .range);
        }
        return self.finish(start, .dot);
    }

    fn lexPipe(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('|')) {
            return self.finish(start, .pipe_pipe);
        }
        return self.finish(start, .pipe);
    }

    fn lexAmp(self: *Lexer) token.Token {
        const start = self.mark();
        _ = self.advance();
        if (self.match('&')) {
            return self.finish(start, .amp_amp);
        }
        return self.finish(start, .amp);
    }

    fn singleCharToken(self: *Lexer, tag: token.Tag) token.Token {
        const start = self.mark();
        _ = self.advance();
        return self.finish(start, tag);
    }

    fn skipWhitespaceAndComments(self: *Lexer) Error!void {
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

    const TokenStart = struct {
        loc: token.Location,
        index: usize,
    };

    fn mark(self: *Lexer) TokenStart {
        return .{ .loc = self.location(), .index = self.index };
    }

    fn finish(self: *Lexer, start: TokenStart, tag: token.Tag) token.Token {
        return .{
            .tag = tag,
            .lexeme = self.source[start.index..self.index],
            .span = .{
                .start = start.loc,
                .end = self.location(),
            },
        };
    }
};

fn isIdentifierStart(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isIdentifierContinue(ch: u8) bool {
    return isIdentifierStart(ch) or std.ascii.isDigit(ch);
}

test "lexer emits tokens with spans" {
    const src =
        \\let greeting = "hello"
        \\mut count = 2
        \\echo greeting | upper
    ;

    var lx = Lexer.init(src);
    const expected_tags = [_]token.Tag{
        .kw_let,     .identifier, .assign, .string_literal, .newline,
        .kw_mut,     .identifier, .assign, .int_literal,    .newline,
        .identifier, .identifier, .pipe,   .identifier,     .newline,
        .eof,
    };

    for (expected_tags) |tag| {
        const tok = try lx.next();
        try std.testing.expectEqual(tag, tok.tag);
        try std.testing.expect(tok.span.end.line >= tok.span.start.line);
    }
}
