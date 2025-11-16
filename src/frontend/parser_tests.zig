const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const ParseError = error{
    UnexpectedToken,
    UnexpectedEOF,
};

const ParserError = ParseError || lexer.Error;
const ParseFn = *const fn (*lexer.Stream) ParserError!void;

const ParserFixture = struct {
    name: []const u8,
    source: []const u8,
    parser: ParseFn,
    expect_error: bool = false,
    expected_error: ParseError = ParseError.UnexpectedToken,
};

test "parser fixtures exercise stream-driven parsing success and failure" {
    const fixtures = [_]ParserFixture{
        .{
            .name = "let_binding_with_annotation",
            .source = "let greeting: Str = \"hi\"\n",
            .parser = parseLetOrMut,
        },
        .{
            .name = "mut_binding_without_annotation",
            .source = "mut count = 2\n",
            .parser = parseLetOrMut,
        },
        .{
            .name = "let_binding_missing_identifier",
            .source = "let = 1\n",
            .parser = parseLetOrMut,
            .expect_error = true,
            .expected_error = ParseError.UnexpectedToken,
        },
        .{
            .name = "function_declaration_with_params",
            .source = "fn add(a: Int, b: Int) -> Int { return a + b }\n",
            .parser = parseFnDecl,
        },
        .{
            .name = "function_declaration_missing_paren",
            .source = "fn broken(a: Int, b: Int { return 0 }\n",
            .parser = parseFnDecl,
            .expect_error = true,
            .expected_error = ParseError.UnexpectedToken,
        },
        .{
            .name = "pipeline_chain",
            .source = "echo \"hi\" | upper | lower\n",
            .parser = parsePipeline,
        },
        .{
            .name = "pipeline_trailing_pipe",
            .source = "echo value |\n",
            .parser = parsePipeline,
            .expect_error = true,
            .expected_error = ParseError.UnexpectedEOF,
        },
        .{
            .name = "import_statement",
            .source = "import http from \"net/http\"\n",
            .parser = parseImport,
        },
        .{
            .name = "import_missing_path",
            .source = "import http from \n",
            .parser = parseImport,
            .expect_error = true,
            .expected_error = ParseError.UnexpectedToken,
        },
        .{
            .name = "error_enum_declaration",
            .source = "error NetworkError = enum { Timeout, ConnectionLost }\n",
            .parser = parseErrorDecl,
        },
        .{
            .name = "error_union_missing_brace",
            .source = "error FileError = union { NotFound: { path: Str }\n",
            .parser = parseErrorDecl,
            .expect_error = true,
            .expected_error = ParseError.UnexpectedEOF,
        },
        .{
            .name = "await_with_catch_clause",
            .source = "await job catch |err| { return err }\n",
            .parser = parseAwaitClause,
        },
        .{
            .name = "await_missing_capture_pipe",
            .source = "await job catch err| { return err }\n",
            .parser = parseAwaitClause,
            .expect_error = true,
            .expected_error = ParseError.UnexpectedToken,
        },
        .{
            .name = "bash_block_balanced",
            .source = "bash { echo \"hi\" }\n",
            .parser = parseBashBlock,
        },
        .{
            .name = "bash_block_unterminated",
            .source = "bash { echo \"hi\"\n",
            .parser = parseBashBlock,
            .expect_error = true,
            .expected_error = ParseError.UnexpectedEOF,
        },
    };

    for (fixtures) |fixture| {
        errdefer std.debug.print("parser fixture `{s}` failed\n", .{fixture.name});
        var stream = lexer.Stream.init(fixture.source);
        if (fixture.expect_error) {
            try std.testing.expectError(fixture.expected_error, fixture.parser(&stream));
        } else {
            try fixture.parser(&stream);
        }
    }
}

fn parseLetOrMut(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    const first = try stream.next();
    if (first.tag != .kw_let and first.tag != .kw_mut) return ParseError.UnexpectedToken;
    try expectIdentifier(stream);
    try skipNewlines(stream);
    if (try stream.consumeIf(.colon)) {
        try parseTypeAnnotation(stream);
    }
    try skipNewlines(stream);
    _ = try expectToken(stream, .assign);
    try parseLiteralOrIdentifier(stream);
    try consumeStatementTerminator(stream);
}

fn parseFnDecl(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    _ = try expectToken(stream, .kw_fn);
    try expectIdentifier(stream);
    try parseParamList(stream);
    try skipNewlines(stream);
    if (try stream.consumeIf(.arrow)) {
        try parseTypeAnnotation(stream);
    }
    try consumeBlock(stream);
    try consumeStatementTerminator(stream);
}

fn parseParamList(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    _ = try expectToken(stream, .l_paren);
    try skipNewlines(stream);
    if (try stream.consumeIf(.r_paren)) return;
    while (true) {
        try expectIdentifier(stream);
        if (try stream.consumeIf(.colon)) {
            try parseTypeAnnotation(stream);
        }
        try skipNewlines(stream);
        if (try stream.consumeIf(.comma)) {
            try skipNewlines(stream);
            continue;
        }
        break;
    }
    _ = try expectToken(stream, .r_paren);
}

fn parsePipeline(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    try expectCommandStage(stream);
    while (true) {
        try skipNewlines(stream);
        const tok = try stream.peek();
        if (tok.tag == .pipe or tok.tag == .pipe_pipe) {
            _ = try stream.next();
            try expectCommandStage(stream);
            continue;
        }
        break;
    }
    try consumeStatementTerminator(stream);
}

fn expectCommandStage(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    const stage = try stream.next();
    if (stage.tag == .eof) return ParseError.UnexpectedEOF;
    if (stage.tag != .identifier and stage.tag != .kw_await and stage.tag != .kw_try) return ParseError.UnexpectedToken;
    // Allow immediate string/int literals for argument coverage.
    try parseOptionalStageArgument(stream);
}

fn parseOptionalStageArgument(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    const tok = try stream.peek();
    switch (tok.tag) {
        .string_literal,
        .int_literal,
        .float_literal,
        .identifier,
        => {
            _ = try stream.next();
        },
        else => {},
    }
}

fn parseImport(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    _ = try expectToken(stream, .kw_import);
    try expectIdentifier(stream);
    _ = try expectToken(stream, .kw_from);
    try skipNewlines(stream);
    const path = try stream.next();
    if (path.tag != .string_literal) return ParseError.UnexpectedToken;
    try consumeStatementTerminator(stream);
}

fn parseErrorDecl(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    _ = try expectToken(stream, .kw_error);
    try expectIdentifier(stream);
    try skipNewlines(stream);
    _ = try expectToken(stream, .assign);
    try skipNewlines(stream);
    const body_kind = try stream.next();
    if (body_kind.tag != .kw_enum and body_kind.tag != .kw_union) return ParseError.UnexpectedToken;
    try consumeBlock(stream);
    try consumeStatementTerminator(stream);
}

fn parseAwaitClause(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    _ = try expectToken(stream, .kw_await);
    try expectIdentifier(stream);
    try skipNewlines(stream);
    _ = try expectToken(stream, .kw_catch);
    try consumeCapture(stream);
    try consumeBlock(stream);
    try consumeStatementTerminator(stream);
}

fn parseBashBlock(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    _ = try expectToken(stream, .kw_bash);
    try consumeBlock(stream);
    try consumeStatementTerminator(stream);
}

fn consumeBlock(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    _ = try expectToken(stream, .l_brace);
    var depth: usize = 1;
    while (depth > 0) {
        const tok = try stream.next();
        switch (tok.tag) {
            .l_brace => depth += 1,
            .r_brace => {
                depth -= 1;
                if (depth == 0) break;
            },
            .eof => return ParseError.UnexpectedEOF,
            else => {},
        }
    }
}

fn consumeCapture(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    _ = try expectToken(stream, .pipe);
    try skipNewlines(stream);
    try expectIdentifier(stream);
    while (true) {
        try skipNewlines(stream);
        if (try stream.consumeIf(.comma)) {
            try expectIdentifier(stream);
            continue;
        }
        break;
    }
    try skipNewlines(stream);
    _ = try expectToken(stream, .pipe);
}

fn parseTypeAnnotation(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    while (true) {
        const tok = try stream.peek();
        if (tok.tag == .question or tok.tag == .caret) {
            _ = try stream.next();
            continue;
        }
        break;
    }
    try expectIdentifier(stream);
}

fn parseLiteralOrIdentifier(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    const tok = try stream.next();
    switch (tok.tag) {
        .identifier,
        .string_literal,
        .int_literal,
        .float_literal,
        .kw_true,
        .kw_false,
        .kw_null,
        => {},
        .l_bracket => try consumeDelimited(stream, .l_bracket, .r_bracket),
        .l_brace => try consumeDelimited(stream, .l_brace, .r_brace),
        else => return ParseError.UnexpectedToken,
    }
}

fn consumeDelimited(stream: *lexer.Stream, open: token.Tag, close: token.Tag) ParserError!void {
    var depth: usize = 1;
    while (depth > 0) {
        const tok = try stream.next();
        if (tok.tag == open) {
            depth += 1;
        } else if (tok.tag == close) {
            depth -= 1;
        } else if (tok.tag == .eof) {
            return ParseError.UnexpectedEOF;
        }
    }
}

fn consumeStatementTerminator(stream: *lexer.Stream) ParserError!void {
    while (true) {
        const tok = try stream.peek();
        if (tok.tag == .newline or tok.tag == .semicolon) {
            _ = try stream.next();
            continue;
        }
        break;
    }
}

fn skipNewlines(stream: *lexer.Stream) ParserError!void {
    while (true) {
        const tok = try stream.peek();
        if (tok.tag == .newline) {
            _ = try stream.next();
            continue;
        }
        break;
    }
}

fn expectToken(stream: *lexer.Stream, tag: token.Tag) ParserError!token.Token {
    const tok = try stream.next();
    if (tok.tag != tag) {
        if (tok.tag == .eof) return ParseError.UnexpectedEOF;
        return ParseError.UnexpectedToken;
    }
    return tok;
}

fn expectIdentifier(stream: *lexer.Stream) ParserError!void {
    try skipNewlines(stream);
    const tok = try stream.next();
    if (tok.tag != .identifier) return if (tok.tag == .eof) ParseError.UnexpectedEOF else ParseError.UnexpectedToken;
}
