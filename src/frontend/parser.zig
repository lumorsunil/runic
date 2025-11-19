const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

pub const ModuleParseError = std.mem.Allocator.Error || error{
    MissingFunctionName,
    MissingParameterList,
    UnterminatedParameterList,
    MissingFunctionBody,
    UnterminatedFunctionBody,
    InvalidParameterName,
    MissingReturnStatement,
    MissingReturnCommand,
    MissingValueName,
    MissingValueInitializer,
    InvalidValueExpression,
    MissingManifestBody,
    UnterminatedManifestBody,
    InvalidSyntax,
};

pub const SourceRange = struct {
    start: usize,
    end: usize,
};

pub const ModuleDocument = struct {
    declarations: []const ModuleDeclaration,
};

pub const ModuleDeclaration = union(enum) {
    function: *ModuleFunction,
    value: *ModuleValue,
    manifest: *ModuleManifest,
};

pub const ModuleFunction = struct {
    is_async: bool,
    name: []const u8,
    name_span: token.Span,
    params: []const ModuleParam,
    return_type_span: ?token.Span,
    body_range: SourceRange,
};

pub const ModuleParam = struct {
    name: []const u8,
    span: token.Span,
};

pub const ModuleValue = struct {
    name: []const u8,
    name_span: token.Span,
    initializer_range: SourceRange,
};

pub const ModuleManifest = struct {
    body_range: SourceRange,
};

pub const ParseError = error{
    UnexpectedToken,
    UnexpectedEOF,
} || std.mem.Allocator.Error || lexer.Error;

/// Parser consumes tokens from the streaming lexer and produces AST nodes.
/// It currently implements enough grammar to cover optional-aware `if`
/// expressions with capture clauses and braced blocks.
pub const Parser = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    stream: lexer.Stream,
    source: []const u8,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        return .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .stream = lexer.Stream.init(source),
            .source = source,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    pub fn parseExpression(self: *Parser) ParseError!*ast.Expression {
        return self.parseIfExpressionOrPrimary();
    }

    pub fn expectEnd(self: *Parser) ParseError!void {
        self.skipNewlines();
        const tok = try self.stream.next();
        if (tok.tag != .eof) return ParseError.UnexpectedToken;
    }

    fn parseIfExpressionOrPrimary(self: *Parser) ParseError!*ast.Expression {
        const next = try self.stream.peek();
        if (next.tag == .kw_if) {
            return self.parseIfExpression();
        }
        return self.parsePrimaryExpression();
    }

    fn parseIfExpression(self: *Parser) ParseError!*ast.Expression {
        const if_tok = try self.expect(.kw_if);
        _ = try self.expect(.l_paren);
        const condition = try self.parseIfExpressionOrPrimary();
        _ = try self.expect(.r_paren);
        const capture = try self.parseOptionalCaptureClause();
        const then_block = try self.parseBlock();

        var else_branch: ?ast.IfExpr.ElseBranch = null;
        var span_end = then_block.span.end;

        const maybe_else = try self.stream.peek();
        if (maybe_else.tag == .kw_else) {
            _ = try self.stream.next();
            const after_else = try self.stream.peek();
            if (after_else.tag == .kw_if) {
                const nested = try self.parseIfExpression();
                const nested_if = switch (nested.*) {
                    .if_expr => |*payload| payload,
                    else => return ParseError.UnexpectedToken,
                };
                span_end = nested_if.span.end;
                else_branch = .{ .if_expr = nested_if };
            } else {
                const else_block = try self.parseBlock();
                span_end = else_block.span.end;
                else_branch = .{ .block = else_block };
            }
        }

        return self.allocExpression(.{
            .if_expr = .{
                .condition = condition,
                .capture = capture,
                .then_block = then_block,
                .else_branch = else_branch,
                .span = .{ .start = if_tok.span.start, .end = span_end },
            },
        });
    }

    fn parseOptionalCaptureClause(self: *Parser) ParseError!?ast.CaptureClause {
        const next = try self.stream.peek();
        if (next.tag != .pipe) return null;

        _ = try self.stream.next(); // consume opening '|'
        const capture_start = next.span.start;

        var bindings = std.ArrayList(*ast.BindingPattern).empty;
        defer bindings.deinit(self.allocator);

        while (true) {
            const pattern = try self.parseBindingPattern();
            try bindings.append(self.allocator, pattern);

            const maybe_comma = try self.stream.peek();
            if (maybe_comma.tag == .comma) {
                _ = try self.stream.next();
                continue;
            }
            break;
        }

        const close_tok = try self.expect(.pipe);
        const owned = try self.copyToArena(*ast.BindingPattern, bindings.items);
        return ast.CaptureClause{
            .bindings = owned,
            .span = .{ .start = capture_start, .end = close_tok.span.end },
        };
    }

    fn parseBindingPattern(self: *Parser) ParseError!*ast.BindingPattern {
        const tok = try self.stream.next();
        if (tok.tag != .identifier) return ParseError.UnexpectedToken;

        const pattern = try self.arena.allocator().create(ast.BindingPattern);
        if (tok.lexeme.len == 1 and tok.lexeme[0] == '_') {
            pattern.* = .{
                .discard = tok.span,
            };
        } else {
            pattern.* = .{
                .identifier = .{ .name = tok.lexeme, .span = tok.span },
            };
        }
        return pattern;
    }

    fn parseBlock(self: *Parser) ParseError!ast.Block {
        const open = try self.expect(.l_brace);
        var statements = std.ArrayList(*ast.Statement).empty;
        defer statements.deinit(self.allocator);

        while (true) {
            self.skipNewlines();
            const next = try self.stream.peek();
            switch (next.tag) {
                .r_brace => {
                    _ = try self.stream.next();
                    const owned = try self.copyToArena(*ast.Statement, statements.items);
                    return .{
                        .statements = owned,
                        .span = .{ .start = open.span.start, .end = next.span.end },
                    };
                },
                .eof => return ParseError.UnexpectedEOF,
                else => {},
            }

            if (try self.parseStatement()) |stmt| {
                try statements.append(self.allocator, stmt);
            }
        }
    }

    fn parseStatement(self: *Parser) ParseError!?*ast.Statement {
        self.skipNewlines();
        const expr = try self.parseIfExpressionOrPrimary();
        const stmt = try self.arena.allocator().create(ast.Statement);
        const expr_span = expr.span();
        stmt.* = .{
            .expression = .{
                .expression = expr,
                .span = expr_span,
            },
        };
        try self.consumeStatementTerminator();
        return stmt;
    }

    fn consumeStatementTerminator(self: *Parser) ParseError!void {
        while (true) {
            const tok = try self.stream.peek();
            switch (tok.tag) {
                .newline, .semicolon => {
                    _ = try self.stream.next();
                    continue;
                },
                else => return,
            }
        }
    }

    fn parsePrimaryExpression(self: *Parser) ParseError!*ast.Expression {
        const tok = try self.stream.next();
        return switch (tok.tag) {
            .identifier => self.allocExpression(.{
                .identifier = .{ .name = tok.lexeme, .span = tok.span },
            }),
            .kw_null => self.allocExpression(.{
                .literal = .{ .null = .{ .span = tok.span } },
            }),
            .kw_true => self.allocExpression(.{
                .literal = .{ .bool = .{ .value = true, .span = tok.span } },
            }),
            .kw_false => self.allocExpression(.{
                .literal = .{ .bool = .{ .value = false, .span = tok.span } },
            }),
            .int_literal => self.allocExpression(.{
                .literal = .{ .integer = .{ .text = tok.lexeme, .span = tok.span } },
            }),
            .float_literal => self.allocExpression(.{
                .literal = .{ .float = .{ .text = tok.lexeme, .span = tok.span } },
            }),
            .string_literal => self.parseStringLiteral(tok),
            .l_paren => blk: {
                const expr = try self.parseExpression();
                _ = try self.expect(.r_paren);
                break :blk expr;
            },
            else => ParseError.UnexpectedToken,
        };
    }

    fn skipNewlines(self: *Parser) void {
        while (true) {
            const tok = self.stream.peek() catch return;
            if (tok.tag != .newline) break;
            _ = self.stream.next() catch return;
        }
    }

    fn expect(self: *Parser, tag: token.Tag) ParseError!token.Token {
        const tok = try self.stream.next();
        if (tok.tag != tag) return ParseError.UnexpectedToken;
        return tok;
    }

    fn allocExpression(self: *Parser, value: ast.Expression) ParseError!*ast.Expression {
        const node = try self.arena.allocator().create(ast.Expression);
        node.* = value;
        return node;
    }

    fn parseStringLiteral(self: *Parser, tok: token.Token) ParseError!*ast.Expression {
        const inner =
            if (tok.lexeme.len >= 2)
                tok.lexeme[1 .. tok.lexeme.len - 1]
            else
                tok.lexeme[0..0];

        const segments = try self.arena.allocator().alloc(ast.StringLiteral.Segment, 1);
        segments[0] = .{ .text = inner };

        return self.allocExpression(.{
            .literal = .{
                .string = .{
                    .segments = segments,
                    .span = tok.span,
                },
            },
        });
    }

    fn copyToArena(self: *Parser, comptime T: type, values: []const T) ParseError![]const T {
        if (values.len == 0) return &[0]T{};
        const buffer = try self.arena.allocator().alloc(T, values.len);
        std.mem.copyForwards(T, buffer, values);
        return buffer;
    }

    fn copyModuleSlice(self: *Parser, comptime T: type, values: []const T) std.mem.Allocator.Error![]const T {
        if (values.len == 0) return &[0]T{};
        const buffer = try self.arena.allocator().alloc(T, values.len);
        std.mem.copyForwards(T, buffer, values);
        return buffer;
    }

    pub fn parseModuleDocument(self: *Parser) (ModuleParseError || std.mem.Allocator.Error)!ModuleDocument {
        var declarations = std.ArrayList(ModuleDeclaration).empty;
        defer declarations.deinit(self.allocator);

        var scope_depth: usize = 0;
        while (true) {
            try self.skipModuleSeparators();
            const tok = try self.peekToken();
            if (tok.tag == .eof) break;

            if (scope_depth > 0) {
                switch (tok.tag) {
                    .l_brace => scope_depth += 1,
                    .r_brace => {
                        if (scope_depth > 0) scope_depth -= 1;
                    },
                    else => {},
                }
                _ = try self.nextToken();
                continue;
            }

            switch (tok.tag) {
                .kw_async => {
                    if (try self.parseMaybeAsyncFunction()) |decl| {
                        try declarations.append(self.allocator, decl);
                    }
                    continue;
                },
                .kw_fn => {
                    const decl = try self.parseModuleFunctionDecl(false);
                    try declarations.append(self.allocator, decl);
                    continue;
                },
                .kw_let => {
                    const decl = try self.parseModuleValueDecl();
                    try declarations.append(self.allocator, decl);
                    continue;
                },
                .identifier => {
                    if (std.mem.eql(u8, tok.lexeme, "manifest")) {
                        const decl = try self.parseManifestDecl();
                        try declarations.append(self.allocator, decl);
                        continue;
                    }
                },
                .l_brace => {
                    _ = try self.nextToken();
                    scope_depth += 1;
                    continue;
                },
                .r_brace => {
                    _ = try self.nextToken();
                    continue;
                },
                else => {},
            }

            _ = try self.nextToken();
        }

        return ModuleDocument{
            .declarations = try self.copyModuleSlice(ModuleDeclaration, declarations.items),
        };
    }

    pub fn sliceForSpan(self: *Parser, span: token.Span) []const u8 {
        return self.source[span.start.offset..span.end.offset];
    }

    pub fn sliceForRange(self: *Parser, range: SourceRange) []const u8 {
        return self.source[range.start..range.end];
    }

    fn parseMaybeAsyncFunction(self: *Parser) ModuleParseError!?ModuleDeclaration {
        _ = try self.nextToken();
        try self.skipModuleSeparators();
        const next = try self.peekToken();
        if (next.tag != .kw_fn) return null;
        return try self.parseModuleFunctionDecl(true);
    }

    fn parseModuleFunctionDecl(self: *Parser, is_async: bool) ModuleParseError!ModuleDeclaration {
        _ = try self.expectTokenTag(.kw_fn, ModuleParseError.InvalidSyntax);
        try self.skipModuleSeparators();
        const name_tok = try self.expectIdentifierWithError(ModuleParseError.MissingFunctionName);
        try self.skipModuleSeparators();
        const params = try self.parseModuleParamList();
        const return_span = try self.parseOptionalReturnTypeSpan();
        const body_range = try self.parseBlockRange(
            ModuleParseError.MissingFunctionBody,
            ModuleParseError.UnterminatedFunctionBody,
        );

        const storage = try self.arena.allocator().create(ModuleFunction);
        storage.* = .{
            .is_async = is_async,
            .name = name_tok.lexeme,
            .name_span = name_tok.span,
            .params = params,
            .return_type_span = return_span,
            .body_range = body_range,
        };
        return .{ .function = storage };
    }

    fn parseModuleParamList(self: *Parser) (ModuleParseError || std.mem.Allocator.Error)![]const ModuleParam {
        _ = try self.expectTokenTag(.l_paren, ModuleParseError.MissingParameterList);

        var params = std.ArrayList(ModuleParam).empty;
        defer params.deinit(self.allocator);

        try self.skipParamWhitespace();
        var maybe_close = try self.peekToken();
        if (maybe_close.tag == .r_paren) {
            _ = try self.nextToken();
            return try self.copyModuleSlice(ModuleParam, params.items);
        }

        while (true) {
            const param = try self.parseModuleParam();
            try params.append(self.allocator, param);
            try self.skipParamWhitespace();
            maybe_close = try self.peekToken();
            if (maybe_close.tag == .comma) {
                _ = try self.nextToken();
                try self.skipParamWhitespace();
                continue;
            }
            if (maybe_close.tag == .r_paren) {
                _ = try self.nextToken();
                break;
            }
            if (maybe_close.tag == .eof) return ModuleParseError.UnterminatedParameterList;
            return ModuleParseError.UnterminatedParameterList;
        }

        return try self.copyModuleSlice(ModuleParam, params.items);
    }

    fn parseModuleParam(self: *Parser) ModuleParseError!ModuleParam {
        try self.skipParamWhitespace();
        const tok = try self.peekToken();
        if (tok.tag == .kw_mut) {
            _ = try self.nextToken();
            try self.skipParamWhitespace();
        }

        const name_tok = try self.expectIdentifierWithError(ModuleParseError.InvalidParameterName);
        try self.skipParamWhitespace();
        try self.skipParameterTail();
        return .{
            .name = name_tok.lexeme,
            .span = name_tok.span,
        };
    }

    fn skipParameterTail(self: *Parser) ModuleParseError!void {
        var tok = try self.peekToken();
        if (tok.tag == .colon) {
            _ = try self.nextToken();
            try self.skipTypeExpression();
            try self.skipParamWhitespace();
            tok = try self.peekToken();
        }

        if (tok.tag == .assign) {
            _ = try self.nextToken();
            try self.skipInitializerExpression();
        }
    }

    fn skipTypeExpression(self: *Parser) ModuleParseError!void {
        while (true) {
            try self.skipParamWhitespace();
            const tok = try self.peekToken();
            switch (tok.tag) {
                .comma, .r_paren, .assign => return,
                inline .l_paren, .l_brace, .l_bracket => |tag| {
                    _ = try self.nextToken();
                    try self.skipNestedStructure(tag, ModuleParseError.UnterminatedParameterList);
                    continue;
                },
                .eof => return ModuleParseError.UnterminatedParameterList,
                else => {
                    _ = try self.nextToken();
                },
            }
        }
    }

    fn skipInitializerExpression(self: *Parser) ModuleParseError!void {
        while (true) {
            try self.skipParamWhitespace();
            const tok = try self.peekToken();
            switch (tok.tag) {
                .comma, .r_paren => return,
                inline .l_paren, .l_brace, .l_bracket => |tag| {
                    _ = try self.nextToken();
                    try self.skipNestedStructure(tag, ModuleParseError.UnterminatedParameterList);
                    continue;
                },
                .eof => return ModuleParseError.UnterminatedParameterList,
                else => {
                    _ = try self.nextToken();
                },
            }
        }
    }

    fn skipNestedStructure(self: *Parser, comptime open: token.Tag, err: ModuleParseError) ModuleParseError!void {
        const close = switch (open) {
            .l_paren => token.Tag.r_paren,
            .l_brace => token.Tag.r_brace,
            .l_bracket => token.Tag.r_bracket,
            else => return,
        };
        var depth: usize = 1;
        while (depth > 0) {
            const tok = try self.nextToken();
            switch (tok.tag) {
                .eof => return err,
                open => depth += 1,
                close => depth -= 1,
                else => {},
            }
        }
    }

    fn parseOptionalReturnTypeSpan(self: *Parser) ModuleParseError!?token.Span {
        try self.skipModuleSeparators();
        const first = try self.peekToken();
        if (first.tag == .l_brace) return null;

        const start = first.span.start;
        var end = first.span.end;
        var consumed = false;
        while (true) {
            const tok = try self.peekToken();
            if (tok.tag == .l_brace) break;
            if (tok.tag == .eof) return ModuleParseError.MissingFunctionBody;
            consumed = true;
            end = tok.span.end;
            _ = try self.nextToken();
            try self.skipModuleSeparators();
        }
        if (!consumed) return null;
        return token.Span{ .start = start, .end = end };
    }

    fn parseBlockRange(
        self: *Parser,
        missing_error: ModuleParseError,
        unterminated_error: ModuleParseError,
    ) ModuleParseError!SourceRange {
        try self.skipModuleSeparators();
        const next = try self.peekToken();
        if (next.tag != .l_brace) return missing_error;
        const open = try self.nextToken();
        var depth: usize = 1;
        var close_tok: ?token.Token = null;
        while (depth > 0) {
            const tok = try self.nextToken();
            switch (tok.tag) {
                .l_brace => depth += 1,
                .r_brace => {
                    depth -= 1;
                    close_tok = tok;
                    if (depth == 0) break;
                },
                .eof => return unterminated_error,
                else => {},
            }
        }
        const close = close_tok.?;
        return .{
            .start = open.span.end.offset,
            .end = close.span.start.offset,
        };
    }

    fn parseModuleValueDecl(self: *Parser) ModuleParseError!ModuleDeclaration {
        _ = try self.expectTokenTag(.kw_let, ModuleParseError.InvalidSyntax);
        try self.skipModuleSeparators();
        const name_tok = try self.expectIdentifierWithError(ModuleParseError.MissingValueName);
        try self.skipModuleSeparators();
        const assign_tok = try self.expectTokenTag(.assign, ModuleParseError.MissingValueInitializer);
        const range = try self.captureInitializerRange(assign_tok.span.end.offset);

        const storage = try self.arena.allocator().create(ModuleValue);
        storage.* = .{
            .name = name_tok.lexeme,
            .name_span = name_tok.span,
            .initializer_range = range,
        };
        return .{ .value = storage };
    }

    fn parseManifestDecl(self: *Parser) ModuleParseError!ModuleDeclaration {
        _ = try self.nextToken();
        const body_range = try self.parseBlockRange(
            ModuleParseError.MissingManifestBody,
            ModuleParseError.UnterminatedManifestBody,
        );
        const storage = try self.arena.allocator().create(ModuleManifest);
        storage.* = .{ .body_range = body_range };
        return .{ .manifest = storage };
    }

    fn captureInitializerRange(self: *Parser, start: usize) ModuleParseError!SourceRange {
        var end = start;
        while (true) {
            const tok = try self.peekToken();
            switch (tok.tag) {
                .newline, .semicolon, .eof => break,
                else => {
                    end = tok.span.end.offset;
                    _ = try self.nextToken();
                },
            }
        }
        if (end == start) return ModuleParseError.InvalidValueExpression;
        return .{ .start = start, .end = end };
    }

    fn skipModuleSeparators(self: *Parser) ModuleParseError!void {
        while (true) {
            const tok = try self.peekToken();
            switch (tok.tag) {
                .newline, .semicolon => {
                    _ = try self.nextToken();
                    continue;
                },
                else => return,
            }
        }
    }

    fn skipParamWhitespace(self: *Parser) ModuleParseError!void {
        while (true) {
            const tok = try self.peekToken();
            if (tok.tag == .newline) {
                _ = try self.nextToken();
                continue;
            }
            break;
        }
    }

    fn nextToken(self: *Parser) ModuleParseError!token.Token {
        return self.stream.next() catch |err| switch (err) {
            else => ModuleParseError.InvalidSyntax,
        };
    }

    fn peekToken(self: *Parser) ModuleParseError!token.Token {
        return self.stream.peek() catch |err| switch (err) {
            else => ModuleParseError.InvalidSyntax,
        };
    }

    fn expectIdentifierWithError(self: *Parser, err: ModuleParseError) ModuleParseError!token.Token {
        const tok = try self.nextToken();
        if (tok.tag != .identifier) return err;
        return tok;
    }

    fn expectTokenTag(self: *Parser, tag: token.Tag, err: ModuleParseError) ModuleParseError!token.Token {
        const tok = try self.nextToken();
        if (tok.tag != tag) return err;
        return tok;
    }
};
