const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const rainbow = @import("../rainbow.zig");
const DocumentStore = @import("document_store.zig").DocumentStore;
const Document = @import("document_store.zig").Document;

pub const module_parser = @import("module_parser_deprecated.zig");

pub const ParseError = error{
    UnexpectedToken,
    UnexpectedEOF,
    StringInterpNotAllowed,
    DuplicateStructDecl,
} || std.mem.Allocator.Error || lexer.Error;

/// Parser consumes tokens from the streaming lexer and produces AST nodes.
/// It currently implements enough grammar to cover optional-aware `if`
/// expressions with capture clauses and braced blocks.
const max_expected_tokens = 8;
const max_breadcrumbs = 32;

pub fn Parser(
    comptime Ctx: type,
    comptime getCachedAst: anytype,
    comptime putCachedAst: anytype,
    comptime getSource: anytype,
) type {
    return struct {
        allocator: std.mem.Allocator,
        arena: std.heap.ArenaAllocator,
        ctx: *Ctx,
        path: []const u8 = undefined,
        source: []const u8 = undefined,
        stream: lexer.Stream = undefined,
        // currentDocument: *Document = undefined,
        // documents: DocumentStore = .empty,
        // getCachedAst: *const fn (path: []const u8) ?ast.Script,
        // getSource: *const fn (path: []const u8) []const u8,
        unexpected_token: ?token.Tag = null,
        expected_token_buffer: [max_expected_tokens]token.Tag = undefined,
        expected_token_count: usize = 0,
        breadcrumb_stack: [max_breadcrumbs][]const u8 = undefined,
        breadcrumb_depth: usize = 0,
        breadcrumb_snapshot: [max_breadcrumbs][]const u8 = undefined,
        breadcrumb_snapshot_depth: usize = 0,
        interp_counter: usize = 0,
        logging_enabled: bool = false,

        const Self = @This();

        // pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        pub fn init(
            allocator: std.mem.Allocator,
            ctx: *Ctx,
            // getCachedAst: *const fn (path: []const u8) ?ast.Script,
            // getSource: *const fn (path: []const u8) []const u8,
        ) Self {
            return .{
                .allocator = allocator,
                .ctx = ctx,
                .arena = std.heap.ArenaAllocator.init(allocator),
                // .getCachedAst = getCachedAst,
                // .getSource = getSource,
                // .stream = lexer.Stream.init(allocator, source),
                // .source = source,
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
            // self.stream.deinit();
        }

        pub fn expectedTokens(self: *const Self) []const token.Tag {
            return self.expected_token_buffer[0..self.expected_token_count];
        }

        pub fn writeExpectedTokens(self: *const Self, writer: *std.Io.Writer) !bool {
            const expected = self.expectedTokens();
            if (expected.len == 0) return false;
            try writer.writeAll("expected ");
            try writeTokenList(writer, expected);
            if (self.unexpected_token) |ut| try writer.print(" got: {s}", .{tokenDescription(ut)});
            return true;
        }

        pub fn log(self: Self, comptime fmt: []const u8, args: anytype) !void {
            if (!self.logging_enabled) return;
            var stdout = std.fs.File.stderr().writer(&.{});
            try stdout.interface.print("[parser]:", .{});
            for (0..self.breadcrumb_depth) |_| {
                try stdout.interface.writeByte(' ');
            }
            const maxColors = std.meta.tags(rainbow.RainbowColor).len;
            const color: rainbow.RainbowColor = @enumFromInt(@max(1, @mod(self.breadcrumb_depth, maxColors)));
            try stdout.interface.writeAll(rainbow.beginColor(color));
            try stdout.interface.print(fmt, args);
            try stdout.interface.writeAll(rainbow.endColor());
            try stdout.interface.writeByte('\n');
            try self.logCurrentTokenSourceLine(&stdout.interface);
            try stdout.interface.flush();
        }

        fn logCurrentTokenSourceLine(self: Self, writer: *std.Io.Writer) !void {
            const tok = self.currentToken() orelse return;
            if (tok.span.start.line != tok.span.end.line) return;
            const peeked = self.peekedToken();
            const line = tok.span.start.line;
            const col = tok.span.start.column;
            const start = tok.span.start.column - 1;
            const end = tok.span.end.column - 1;
            const line_slice = self.lineSlice(line) orelse return;

            try writer.print("[parser]:", .{});
            for (0..self.breadcrumb_depth) |_| {
                try writer.writeByte(' ');
            }
            const pre_token_slice = line_slice[0..start];
            const token_slice = line_slice[start..end];
            const post_token_slice = line_slice[end..];
            try writer.print("L{d}C{d}: ", .{ line, col });
            try writer.writeAll(pre_token_slice);
            try writer.writeAll(rainbow.beginColor(.black));
            try writer.writeAll(rainbow.beginBgColor(.orange));
            try writer.writeAll(token_slice);
            try writer.writeAll(rainbow.endColor());
            if (peeked) |p| {
                if (p.span.start.offset != tok.span.start.offset and p.span.start.line == line) {
                    try writer.writeAll(rainbow.beginColor(.black));
                    try writer.writeAll(rainbow.beginBgColor(.violet));
                    try writer.writeAll(line_slice[end .. p.span.start.column - 1]);
                    try writer.writeAll(rainbow.endColor());
                    try writer.writeAll(line_slice[p.span.start.column - 1 ..]);
                } else {
                    try writer.writeAll(post_token_slice);
                }
            } else {
                try writer.writeAll(post_token_slice);
            }
            try writer.writeByte('\n');
        }

        fn nextToken(self: *Self) !token.Token {
            return try self.stream.next();
        }

        fn peekToken(self: *Self) !token.Token {
            return try self.stream.peek();
        }

        fn peekSlice(self: *Self, len: usize) ![]const token.Token {
            return try self.stream.peekSlice(len);
        }

        fn currentToken(self: Self) ?token.Token {
            return self.stream.current;
        }

        fn peekedToken(self: Self) ?token.Token {
            if (self.stream.peeked_tokens_buffer.items.len == 0) return null;
            return self.stream.peeked_tokens_buffer.items[0];
        }

        fn takeSnapshot(self: *Self) lexer.Stream {
            return self.stream.snapshot();
        }

        fn restoreSnapshot(self: *Self, snapshot: lexer.Stream) void {
            return self.stream.restore(snapshot);
        }

        fn currentTokenLocation(self: Self) ?token.Location {
            if (self.currentToken()) |tok| return tok.span.start;
            return .{
                .line = self.stream.lexer.line,
                .column = self.stream.lexer.column,
                .offset = self.stream.lexer.index,
            };
        }

        fn lineSlice(self: Self, target_line: usize) ?[]const u8 {
            if (target_line == 0) return null;

            var current_line: usize = 1;
            var line_start: usize = 0;
            var idx: usize = 0;
            const source = self.source;
            while (idx < source.len) : (idx += 1) {
                if (source[idx] == '\n') {
                    if (current_line == target_line) {
                        return std.mem.trimRight(u8, source[line_start..idx], "\r");
                    }
                    current_line += 1;
                    line_start = idx + 1;
                }
            }

            if (current_line == target_line and line_start <= source.len) {
                return std.mem.trimRight(u8, source[line_start..source.len], "\r");
            }
            return null;
        }

        pub fn parseScript(self: *Self, path: []const u8) !ast.Script {
            const breadcrumb = try self.createBreadcrumb("parseScript");
            defer breadcrumb.end();

            if (try getCachedAst(self.ctx, path)) |script| return script;
            self.clearExpectedTokens();
            self.path = path;
            self.source = try getSource(self.ctx, path);

            self.stream = try .init(self.arena.allocator(), path, self.source);
            const statements = try self.parseStatementsUntil(.eof);

            const script = ast.Script{
                .span = statements.span,
                .statements = statements.payload,
            };

            try putCachedAst(self.ctx, path, script);

            return script;
        }

        pub fn parseSource(self: *Self, source: []const u8) !ast.Script {
            const breadcrumb = try self.createBreadcrumb("parseSource");
            defer breadcrumb.end();

            self.clearExpectedTokens();
            self.source = source;
            self.stream = try .init(self.arena.allocator(), "<source>", source);
            // self.currentDocument = try self.arena.allocator().create(Document);
            // self.currentDocument.* = .{
            //     .source = source,
            //     .ast = null,
            //     .lexer = .init(self.arena.allocator(), source),
            // };
            const statements = try self.parseStatementsUntil(.eof);

            const script = ast.Script{
                .span = statements.span,
                .statements = statements.payload,
            };

            return script;
        }

        fn parseExpression(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parseExpression");
            defer breadcrumb.end();

            self.clearExpectedTokens();

            // (a.b).c
            //
            // return switch (try self.oneOf(.{
            //     parseMemberAccessExpression,
            //     parseImportExpression,
            //     parseIdentifierExpression,
            //     parseIfExpression,
            //     parsePrimaryExpression,
            // })) {
            //     inline else => |r| r,
            // };

            const next = try self.peekToken();

            return switch (next.tag) {
                .kw_import => self.parseImportExpression(),
                .identifier => self.parseIdentifierExpression(),
                .kw_if => self.parseIfExpression(),
                else => self.parsePrimaryExpression(),
            };
        }

        // .{ parseInt, parseString } == .{ .@"0" = parseInt, .@"1" = parseString }
        // union enum { @"0": i64, @"1": []const u8 }
        fn OneOf(comptime Parsers: type) type {
            var fields: []const std.builtin.Type.UnionField = &.{};

            for (std.meta.fields(Parsers)) |field| {
                const ReturnType = @typeInfo(field.type).@"fn".return_type orelse void;
                const Payload = @typeInfo(ReturnType).error_union.payload;
                fields = fields ++ [_]std.builtin.Type.UnionField{
                    .{
                        .type = Payload,
                        .name = field.name,
                        .alignment = @alignOf(Payload),
                    },
                };
            }

            return @Type(.{
                .@"union" = .{
                    .layout = .auto,
                    .tag_type = std.meta.FieldEnum(Parsers),
                    .fields = fields,
                    .decls = &.{},
                },
            });
        }

        // TODO: error handling
        fn oneOf(self: *Self, parsers: anytype) ParseError!OneOf(@TypeOf(parsers)) {
            var the_error: ParseError = undefined;

            inline for (std.meta.fields(@TypeOf(parsers))) |field| {
                const parser = @field(parsers, field.name);
                const snapshot = self.takeSnapshot();

                if (parser(self)) |result| {
                    return @unionInit(OneOf(@TypeOf(parsers)), field.name, result);
                } else |err| {
                    the_error = err;
                    self.restoreSnapshot(snapshot);
                }
            }

            return the_error;
        }

        fn parseMemberAccessExpression(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parseMemberAccessExpression");
            defer breadcrumb.end();
        }

        fn parseIdentifierExpression(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parseIdentifierExpression");
            defer breadcrumb.end();

            const p = try self.peekSlice(2);
            const identifier = p[0];
            const next_token = p[1];

            if (isExprTerminator(next_token.tag)) {
                return try self.parsePipeline();
            }

            // TODO: Support assignments and other expressions that start with an identifier
            switch (next_token.tag) {
                .dot => {
                    _ = try self.nextToken();
                    _ = try self.nextToken();
                    const accessor = try self.expectTokenTag(.identifier);
                    const object = try self.allocExpression(.{
                        .identifier = .{ .name = identifier.lexeme, .span = identifier.span },
                    });

                    return try self.allocExpression(.{
                        .member = .{
                            .object = object,
                            .member = .{ .name = accessor.lexeme, .span = accessor.span },
                            .span = identifier.span.endAt(accessor.span),
                        },
                    });
                },
                else => return try self.parsePipeline(),
            }
        }

        fn parsePipeline(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parsePipeline");
            defer breadcrumb.end();

            var stages = std.ArrayList(ast.PipelineStage).empty;
            defer stages.deinit(self.allocator);

            const start_token = try self.peekToken();
            var last_token = start_token;
            var next_token: token.Token = start_token;

            while (true) : (last_token = next_token) {
                try stages.append(self.allocator, try self.parsePipelineStage());
                next_token = try self.peekToken();

                switch (next_token.tag) {
                    .pipe => {
                        _ = try self.nextToken();
                        continue;
                    },
                    else => |t| {
                        if (t == .string_interp_end) {
                            self.interp_counter -= 1;
                        }

                        if (stages.items.len == 1 and stages.items[0].role == .expression) {
                            return stages.items[0].payload.expression;
                        }

                        return try self.allocExpression(.{ .pipeline = .{
                            .stages = try self.copyToArena(ast.PipelineStage, stages.items),
                            .span = start_token.span.endAt(last_token.span),
                        } });
                    },
                }
            }
        }

        fn isExprTerminator(tag: token.Tag) bool {
            return switch (tag) {
                .r_paren, .r_bracket, .r_brace, .comma, .pipe, .pipe_pipe, .amp_amp, .amp, .string_interp_end, .newline => true,
                else => false,
            };
        }

        fn parsePipelineStage(self: *Self) ParseError!ast.PipelineStage {
            const breadcrumb = try self.createBreadcrumb("parsePipelineStage");
            defer breadcrumb.end();

            const identifier = try self.expectTokenTag(.identifier);

            var args = std.ArrayList(ast.CommandPart).empty;
            defer args.deinit(self.allocator);

            var last_token = identifier;

            while (true) {
                const next_token = try self.peekToken();

                if (isExprTerminator(next_token.tag)) {
                    return .{
                        .role = .command,
                        .payload = .{
                            .command = .{
                                .name = .{
                                    .word = .{
                                        .text = identifier.lexeme,
                                        .span = identifier.span,
                                    },
                                },
                                .args = try self.copyToArena(ast.CommandPart, args.items),
                                .background = false,
                                .capture = null,
                                .span = identifier.span.endAt(last_token.span),
                                .env_assignments = &.{},
                                .redirects = &.{},
                            },
                        },
                        .span = identifier.span.endAt(last_token.span),
                    };
                }

                switch (next_token.tag) {
                    .string_start => {
                        const string_literal = try self.parseStringLiteral();
                        try args.append(self.allocator, .{ .string = string_literal });
                    },
                    // .minus, .plus => {
                    //     _ = try self.nextToken();
                    //     try args.append(self.allocator, .{ .word = .fromToken(next_token) });
                    // },
                    else => return self.failExpectedToken(next_token.tag, .string_start),
                }

                last_token = next_token;
            }
        }

        fn parseImportExpression(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parseImportExpression");
            defer breadcrumb.end();

            const start = try self.expectTokenTag(.kw_import);
            // _ = try self.expectTokenTag(.l_paren);
            const module_name = try self.parseStringLiteralWithoutInterp();
            // const end = try self.expectTokenTag(.r_paren);

            return self.allocExpression(.{ .import_expr = .{
                .importer = self.path,
                .module_name = module_name.payload,
                .span = start.span.endAt(module_name.span),
            } });
        }

        pub fn expectEnd(self: *Self) ParseError!void {
            const breadcrumb = try self.createBreadcrumb("expectEnd");
            defer breadcrumb.end();

            self.skipNewlines();
            const tok = try self.nextToken();
            if (tok.tag != .eof) return self.failExpectedToken(tok.tag, .eof);
        }

        fn parseIfExpression(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parseIfExpression");
            defer breadcrumb.end();

            const if_tok = try self.expect(.kw_if);
            _ = try self.expect(.l_paren);
            const condition = try self.parseExpression();
            _ = try self.expect(.r_paren);
            const capture = try self.parseOptionalCaptureClause();
            const then_block = try self.parseBlock();

            var else_branch: ?ast.IfExpr.ElseBranch = null;
            var span_end = then_block.span.end;

            const maybe_else = try self.peekToken();
            if (maybe_else.tag == .kw_else) {
                _ = try self.nextToken();
                const after_else = try self.peekToken();
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

        fn parseOptionalCaptureClause(self: *Self) ParseError!?ast.CaptureClause {
            const breadcrumb = try self.createBreadcrumb("parseOptionalCaptureClause");
            defer breadcrumb.end();

            const next = try self.peekToken();
            if (next.tag != .pipe) return null;

            _ = try self.nextToken(); // consume opening '|'
            const capture_start = next.span.start;

            var bindings = std.ArrayList(*ast.BindingPattern).empty;
            defer bindings.deinit(self.allocator);

            while (true) {
                const pattern = try self.parseBindingPattern();
                try bindings.append(self.allocator, pattern);

                const maybe_comma = try self.peekToken();
                if (maybe_comma.tag == .comma) {
                    _ = try self.nextToken();
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

        fn parseBindingPattern(self: *Self) ParseError!*ast.BindingPattern {
            const breadcrumb = try self.createBreadcrumb("parseBindingPattern");
            defer breadcrumb.end();

            // TODO: Support deconstructions
            const tok = try self.nextToken();
            if (tok.tag != .identifier) return self.failExpectedToken(tok.tag, .identifier);

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

        fn ParserFn(comptime T: type) type {
            return *const fn (*Self) ParseError!T;
        }

        fn parseAndSkipLinesUntil(
            self: *Self,
            comptime T: type,
            comptime delimiter: token.Tag,
            parser: ParserFn(T),
        ) ParseError!ast.Spanned([]const T) {
            const breadcrumb = try self.createBreadcrumb("parseAndSkipLinesUntil");
            defer breadcrumb.end();

            const open = try self.peekToken();
            var parsed = std.ArrayList(T).empty;
            defer parsed.deinit(self.allocator);

            while (true) {
                self.skipNewlines();
                const next = try self.peekToken();

                if (next.tag == delimiter) {
                    _ = try self.nextToken();
                    return .{
                        .payload = try self.copyToArena(T, parsed.items),
                        .span = .{ .start = open.span.start, .end = next.span.end },
                    };
                } else if (next.tag == .eof) {
                    return ParseError.UnexpectedEOF;
                }

                const nextParsed = try parser(self);
                try parsed.append(self.allocator, nextParsed);
            }
        }

        fn parseStatementsUntil(
            self: *Self,
            comptime end: token.Tag,
        ) ParseError!ast.Spanned([]const *ast.Statement) {
            const breadcrumb = try self.createBreadcrumb("parseStatementsUntil");
            defer breadcrumb.end();

            return try self.parseAndSkipLinesUntil(
                *ast.Statement,
                end,
                parseStatement,
            );
        }

        fn parseBlock(self: *Self) ParseError!ast.Block {
            const breadcrumb = try self.createBreadcrumb("parseBlock");
            defer breadcrumb.end();

            const open = try self.expect(.l_brace);
            const statements = try self.parseStatementsUntil(.r_brace);

            return ast.Block{
                .statements = statements.payload,
                .span = open.span.endAt(statements.span),
            };
        }

        fn parseBlockExpression(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parseBlockExpression");
            defer breadcrumb.end();

            const open = try self.expect(.l_brace);
            const statements = try self.parseStatementsUntil(.r_brace);

            return ast.Block{
                .statements = statements.payload,
                .span = open.span.endAt(statements.span),
            };
        }

        fn parseStatement(self: *Self) ParseError!*ast.Statement {
            const breadcrumb = try self.createBreadcrumb("parseStatement");
            defer breadcrumb.end();

            self.skipNewlines();
            const stmt = try self.arena.allocator().create(ast.Statement);
            errdefer self.arena.allocator().destroy(stmt);

            const maybeBinding = try self.parseMaybeBinding();
            if (maybeBinding) |letDecl| {
                stmt.* = .{ .binding_decl = letDecl };
                return stmt;
            }

            const maybeFn = try self.parseMaybeFn();
            if (maybeFn) |fnDecl| {
                stmt.* = .{ .fn_decl = fnDecl };
                return stmt;
            }

            const expr = try self.parseExpression();
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

        fn consumeStatementTerminator(self: *Self) ParseError!void {
            const breadcrumb = try self.createBreadcrumb("consumeStatementTerminator");
            defer breadcrumb.end();

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

        fn parsePrimaryExpression(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parsePrimaryExpression");
            defer breadcrumb.end();

            const expected_primary_tokens = [_]token.Tag{
                .identifier,
                .kw_null,
                .kw_true,
                .kw_false,
                .int_literal,
                .float_literal,
                .string_start,
                .l_paren,
            };

            const p = self.peekToken() catch {
                return self.failExpectedTokens(null, &expected_primary_tokens);
            };

            if (p.tag == .string_start) return self.parseStringLiteralExpr();

            const tok = try self.nextToken();
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
                .l_paren => blk: {
                    const expr = try self.parseExpression();
                    _ = try self.expect(.r_paren);
                    break :blk expr;
                },
                else => self.failExpectedTokens(tok.tag, &expected_primary_tokens),
            };
        }

        fn skipNewlines(self: *Self) void {
            while (true) {
                const tok = self.peekToken() catch return;
                if (tok.tag != .newline) break;
                _ = self.nextToken() catch return;
            }
        }

        fn clearExpectedTokens(self: *Self) void {
            self.expected_token_count = 0;
            self.clearBreadcrumbSnapshot();
        }

        fn recordExpectedTokens(self: *Self, expected: []const token.Tag) void {
            self.expected_token_count = 0;
            for (expected) |tag| {
                if (self.expected_token_count >= self.expected_token_buffer.len) break;

                var duplicate = false;
                for (self.expected_token_buffer[0..self.expected_token_count]) |existing| {
                    if (existing == tag) {
                        duplicate = true;
                        break;
                    }
                }
                if (duplicate) continue;

                self.expected_token_buffer[self.expected_token_count] = tag;
                self.expected_token_count += 1;
            }
        }

        fn failExpectedTokens(self: *Self, unexpected: ?token.Tag, expected: []const token.Tag) ParseError {
            self.captureBreadcrumbSnapshot();
            self.recordExpectedTokens(expected);
            self.unexpected_token = unexpected;
            return ParseError.UnexpectedToken;
        }

        fn failExpectedToken(self: *Self, unexpected: ?token.Tag, tag: token.Tag) ParseError {
            const single = [_]token.Tag{tag};
            return self.failExpectedTokens(unexpected, &single);
        }

        fn writeTokenList(writer: *std.Io.Writer, tags: []const token.Tag) !void {
            for (tags, 0..) |tag, idx| {
                if (idx > 0) {
                    if (idx + 1 == tags.len) {
                        try writer.writeAll(" or ");
                    } else {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(tokenDescription(tag));
            }
        }

        fn tokenDescription(tag: token.Tag) []const u8 {
            return switch (tag) {
                .identifier => "identifier",
                .int_literal => "integer literal",
                .float_literal => "float literal",
                .string_start => "string start",
                .string_end => "string end",
                .string_interp_start => "string interp start",
                .string_interp_end => "string interp end",
                .string_text => "string text",
                .kw_true => "\"true\"",
                .kw_false => "\"false\"",
                .kw_null => "\"null\"",
                .kw_const => "\"let\"",
                .kw_var => "\"mut\"",
                .kw_if => "\"if\"",
                .kw_else => "\"else\"",
                .kw_import => "\"import\"",
                .l_paren => "\"(\"",
                .r_paren => "\")\"",
                .l_brace => "\"{\"",
                .r_brace => "\"}\"",
                .l_bracket => "\"[\"",
                .r_bracket => "\"]\"",
                .assign => "\"=\"",
                .comma => "\",\"",
                .newline => "newline",
                .pipe => "\"|\"",
                .semicolon => "\";\"",
                .eof => "end of file",
                else => blk: {
                    if (tag.toKeyword()) |kw| break :blk kw;
                    break :blk @tagName(tag);
                },
            };
        }

        pub fn writeBreadcrumbTrail(self: *const Self, writer: *std.Io.Writer) !void {
            try writer.writeAll(" while parsing ");
            const stack = if (self.breadcrumb_snapshot_depth > 0)
                self.breadcrumb_snapshot[0..self.breadcrumb_snapshot_depth]
            else
                self.breadcrumb_stack[0..self.breadcrumb_depth];
            if (stack.len == 0) {
                try writer.writeAll("<no breadcrumbs>");
                return;
            }
            for (stack, 0..) |crumb, idx| {
                if (idx > 0) try writer.writeAll(" -> ");
                try writer.writeAll(crumb);
            }
        }

        fn expect(self: *Self, tag: token.Tag) ParseError!token.Token {
            const tok = try self.nextToken();
            if (tok.tag != tag) return self.failExpectedToken(tok.tag, tag);
            return tok;
        }

        fn allocExpression(self: *Self, value: ast.Expression) ParseError!*ast.Expression {
            const node = try self.arena.allocator().create(ast.Expression);
            node.* = value;
            return node;
        }

        fn parseStringLiteralWithoutInterp(self: *Self) ParseError!ast.Spanned([]const u8) {
            const breadcrumb = try self.createBreadcrumb("parseStringLiteralWithoutInterp");
            defer breadcrumb.end();

            const stringLiteral = try self.parseStringLiteral();
            var result: std.ArrayList(u8) = .empty;
            defer result.deinit(self.allocator);
            for (stringLiteral.segments) |segment| {
                if (segment == .interpolation) {
                    return ParseError.StringInterpNotAllowed;
                }
                try result.appendSlice(self.allocator, segment.text.payload);
            }

            return .{
                .payload = try self.copyToArena(u8, result.items),
                .span = stringLiteral.span,
            };
        }

        fn parseStringLiteral(self: *Self) ParseError!ast.StringLiteral {
            const breadcrumb = try self.createBreadcrumb("parseStringLiteral");
            defer breadcrumb.end();

            const start_token = try self.expectTokenTag(.string_start);

            var segments = std.ArrayList(ast.StringLiteral.Segment).empty;
            defer segments.deinit(self.allocator);

            while (true) {
                const tok = try self.nextToken();
                try self.log("stok: {}:{s}", .{ tok.tag, tok.lexeme });

                switch (tok.tag) {
                    .string_text => {
                        try self.log("appending string text: \"{s}\"", .{tok.lexeme});
                        try segments.append(
                            self.allocator,
                            .{ .text = .{ .payload = tok.lexeme, .span = tok.span } },
                        );
                    },
                    .string_interp_start => {
                        self.interp_counter += 1;
                        try segments.append(
                            self.allocator,
                            .{ .interpolation = try self.parseExpression() },
                        );
                        _ = try self.expectTokenTag(.string_interp_end);
                    },
                    .string_end => break,
                    else => return self.failExpectedTokens(tok.tag, &.{ .string_text, .string_end, .string_interp_start }),
                }
            }

            const end = if (segments.getLastOrNull()) |s| s.span() else start_token.span;

            return .{
                .segments = try self.copyToArena(ast.StringLiteral.Segment, segments.items),
                .span = start_token.span.endAt(end),
            };
        }

        fn parseStringLiteralExpr(self: *Self) ParseError!*ast.Expression {
            const breadcrumb = try self.createBreadcrumb("parseStringLiteralExpr");
            defer breadcrumb.end();

            return self.allocExpression(.{
                .literal = .{ .string = try self.parseStringLiteral() },
            });
        }

        fn parseMaybeBinding(self: *Self) ParseError!?ast.BindingDecl {
            const breadcrumb = try self.createBreadcrumb("parseMaybeBinding");
            defer breadcrumb.end();

            const next = try self.peekToken();
            return switch (next.tag) {
                .kw_const, .kw_var => try self.parseBinding(),
                else => null,
            };
        }

        fn parseBinding(self: *Self) ParseError!ast.BindingDecl {
            const breadcrumb = try self.createBreadcrumb("parseBinding");
            defer breadcrumb.end();

            const constOrVar = try self.nextToken();
            switch (constOrVar.tag) {
                .kw_const, .kw_var => {},
                else => return self.failExpectedTokens(constOrVar.tag, &[_]token.Tag{ .kw_const, .kw_var }),
            }
            const pattern = try self.parseBindingPattern();
            const annotation: ?*ast.TypeExpr = try self.parseMaybeTypeAnnotation();
            _ = try self.expectTokenTag(.assign);
            const initializer = try self.parseExpression();

            return ast.BindingDecl{
                .is_mutable = constOrVar.tag == .kw_var,
                .span = constOrVar.span.endAt(initializer.span()),
                .annotation = annotation,
                .initializer = initializer,
                .pattern = pattern,
            };
        }

        fn parseMaybeTypeAnnotation(self: *Self) ParseError!?*ast.TypeExpr {
            const breadcrumb = try self.createBreadcrumb("parseMaybeTypeAnnotation");
            defer breadcrumb.end();

            // TODO: Implement an actual parser for type expressions
            const tok = try self.peekToken();
            switch (tok.tag) {
                .colon => {
                    _ = try self.nextToken();
                    return try self.parseMaybeTypeAnnotation();
                },
                else => {},
            }

            return null;
        }

        fn parseMaybeFn(self: *Self) ParseError!?ast.FunctionDecl {
            const breadcrumb = try self.createBreadcrumb("parseMaybeFn");
            defer breadcrumb.end();

            const next = try self.peekToken();
            return switch (next.tag) {
                .kw_fn => try self.parseFn(),
                else => null,
            };
        }

        fn parseFn(self: *Self) ParseError!ast.FunctionDecl {
            const breadcrumb = try self.createBreadcrumb("parseFn");
            defer breadcrumb.end();

            const start = try self.expectTokenTag(.kw_fn);
            const identifier = try self.expectTokenTag(.identifier);
            _ = try self.expectTokenTag(.l_paren);
            // TODO: parameters
            _ = try self.expectTokenTag(.r_paren);
            // TODO: types
            const returnType = try self.parseMaybeTypeExpr();
            // TODO: lambdas
            const block = try self.parseBlock();

            return .{
                .name = .{ .name = identifier.lexeme, .span = identifier.span },
                .is_async = false,
                .params = &.{}, // TODO: parameters
                .return_type = returnType,
                .body = .{ .block = block },
                .span = start.span.endAt(block.span),
            };
        }

        fn parseMaybeTypeExpr(self: *Self) ParseError!?*ast.TypeExpr {
            const breadcrumb = try self.createBreadcrumb("parseMaybeTypeExpr");
            defer breadcrumb.end();

            _ = try self.stream.consumeIf(.identifier);
            return null;
        }

        fn copyToArena(self: *Self, comptime T: type, values: []const T) ParseError![]const T {
            if (values.len == 0) return &[0]T{};
            const buffer = try self.arena.allocator().alloc(T, values.len);
            std.mem.copyForwards(T, buffer, values);
            return buffer;
        }

        fn skipTypeExpression(self: *Self) ParseError!void {
            const breadcrumb = try self.createBreadcrumb("skipTypeExpression");
            defer breadcrumb.end();

            while (true) {
                const tok = try self.peekToken();
                switch (tok.tag) {
                    .comma, .r_paren, .assign => return,
                    inline .l_paren, .l_brace, .l_bracket => |tag| {
                        _ = try self.nextToken();
                        try self.skipNestedStructure(tag);
                        continue;
                    },
                    .eof => return ParseError.UnexpectedEOF,
                    else => {
                        _ = try self.nextToken();
                    },
                }
            }
        }

        fn skipNestedStructure(self: *Self, comptime open: token.Tag) ParseError!void {
            const breadcrumb = try self.createBreadcrumb("skipNestedStructure");
            defer breadcrumb.end();

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
                    .eof => return ParseError.UnexpectedEOF,
                    open => depth += 1,
                    close => depth -= 1,
                    else => {},
                }
            }
        }

        /// Consumes the next token
        fn expectTokenTag(self: *Self, tag: token.Tag) ParseError!token.Token {
            const tok = try self.nextToken();
            if (tok.tag != tag) return self.failExpectedToken(tok.tag, tag);
            return tok;
        }

        fn createBreadcrumb(self: *Self, comptime name: []const u8) !BreadcrumbGuard {
            try self.log("{s}", .{name});
            return BreadcrumbGuard.init(self, name);
        }

        fn pushBreadcrumb(self: *Self, comptime name: []const u8) bool {
            if (self.breadcrumb_depth >= self.breadcrumb_stack.len) return false;
            self.breadcrumb_stack[self.breadcrumb_depth] = name;
            self.breadcrumb_depth += 1;
            return true;
        }

        fn popBreadcrumb(self: *Self) void {
            if (self.breadcrumb_depth == 0) return;
            self.breadcrumb_depth -= 1;
        }

        fn captureBreadcrumbSnapshot(self: *Self) void {
            const depth = if (self.breadcrumb_depth > self.breadcrumb_snapshot.len)
                self.breadcrumb_snapshot.len
            else
                self.breadcrumb_depth;
            self.breadcrumb_snapshot_depth = depth;
            if (depth == 0) return;
            std.mem.copyForwards([]const u8, self.breadcrumb_snapshot[0..depth], self.breadcrumb_stack[0..depth]);
        }

        fn clearBreadcrumbSnapshot(self: *Self) void {
            self.breadcrumb_snapshot_depth = 0;
        }

        const BreadcrumbGuard = struct {
            parser: *Self,
            active: bool,

            fn init(parser: *Self, comptime name: []const u8) BreadcrumbGuard {
                return .{
                    .parser = parser,
                    .active = parser.pushBreadcrumb(name),
                };
            }

            pub fn end(self: BreadcrumbGuard) void {
                if (self.active) self.parser.popBreadcrumb();
            }
        };

        pub fn structFromScript(
            self: *Self,
            script: ast.Script,
        ) ParseError!ast.TypeExpr.StructType {
            var fields: std.StringArrayHashMap(ast.TypeExpr.StructField) = .init(self.allocator);
            var decls: std.StringArrayHashMap(ast.TypeExpr.StructDecl) = .init(self.allocator);
            defer {
                fields.deinit();
                decls.deinit();
            }

            for (script.statements) |statement| {
                switch (statement.*) {
                    .binding_decl => |*s| {
                        switch (s.pattern.*) {
                            .identifier => |identifier| {
                                const entry = try decls.getOrPut(identifier.name);

                                if (entry.found_existing) {
                                    return ParseError.DuplicateStructDecl;
                                }

                                entry.value_ptr.* = .{
                                    .name = identifier.name,
                                    .type_expr = s.annotation,
                                    .decl_source = .{ .binding_decl = s },
                                    .span = s.span,
                                };
                            },
                        }
                    },
                    .fn_decl => |*s| {
                        const entry = try decls.getOrPut(s.name.name);

                        if (entry.found_existing) {
                            return ParseError.DuplicateStructDecl;
                        }

                        entry.value_ptr.* = .{
                            .name = s.name.name,
                            .type_expr = s.annotation,
                            .decl_source = .{ .fn_decl = s },
                            .span = s.span,
                        };
                    },
                }
            }

            return .{
                .fields = try self.copyToArena(ast.TypeExpr.StructField, fields.values()),
                .decls = try self.copyToArena(ast.TypeExpr.StructDecl, decls.values()),
            };
        }
    };
}

const TestCtx = struct {
    source: []const u8,
    cached: ?ast.Script = null,
};

fn testGetCachedAst(ctx: *TestCtx, path: []const u8) !?ast.Script {
    _ = path;
    return ctx.cached;
}

fn testPutCachedAst(ctx: *TestCtx, path: []const u8, script: ast.Script) !void {
    _ = path;
    ctx.cached = script;
}

fn testGetSource(ctx: *TestCtx, path: []const u8) ![]const u8 {
    _ = path;
    return ctx.source;
}

const TestParser = Parser(TestCtx, testGetCachedAst, testPutCachedAst, testGetSource);

test "parser preserves breadcrumb trail on unexpected token errors" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var ctx = TestCtx{ .source = "foo = 1" };
    var parser = TestParser.init(allocator, &ctx);
    defer parser.deinit();

    parser.source = ctx.source;
    parser.stream = try lexer.Stream.init(parser.arena.allocator(), "<test>", ctx.source);

    try std.testing.expectError(ParseError.UnexpectedToken, parser.parseExpression());

    var buffer: [256]u8 = undefined;
    var writer = std.Io.Writer.fixed(&buffer);
    const wrote = try parser.writeExpectedTokens(&writer);
    try std.testing.expect(wrote);
    const expected =
        "expected string literal while parsing parseExpression -> parseIdentifierExpression -> parsePipeline -> parsePipelineStage";
    try std.testing.expectEqualStrings(expected, writer.buffer[0..writer.end]);
}

test "parser builds command pipelines with string arguments" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var ctx = TestCtx{ .source = "echo \"hi\"" };
    var parser = TestParser.init(allocator, &ctx);
    defer parser.deinit();

    const script = try parser.parseSource(ctx.source);
    try std.testing.expectEqual(@as(usize, 1), script.statements.len);
    const stmt = script.statements[0].*;
    try std.testing.expect(std.meta.activeTag(stmt) == .expression);
    const expr_stmt = stmt.expression;
    const expr = expr_stmt.expression.*;
    try std.testing.expect(std.meta.activeTag(expr) == .pipeline);
    const pipeline = expr.pipeline;
    try std.testing.expectEqual(@as(usize, 1), pipeline.stages.len);
    const stage = pipeline.stages[0];
    try std.testing.expectEqual(ast.StageRole.command, stage.role);
    const command = stage.payload.command;
    try std.testing.expectEqualStrings("echo", command.name.word.text);
    try std.testing.expectEqual(@as(usize, 1), command.args.len);
    const arg = command.args[0];
    try std.testing.expect(std.meta.activeTag(arg) == .string);
    const literal = arg.string;
    try std.testing.expectEqual(@as(usize, 1), literal.segments.len);
    const segment = literal.segments[0];
    try std.testing.expect(std.meta.activeTag(segment) == .text);
    try std.testing.expectEqualStrings("hi", segment.text.payload);
}

test "parser rejects interpolation in import module names" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var ctx = TestCtx{ .source = "import \"foo${name}\"" };
    var parser = TestParser.init(allocator, &ctx);
    defer parser.deinit();

    try std.testing.expectError(ParseError.StringInterpNotAllowed, parser.parseSource(ctx.source));
}
