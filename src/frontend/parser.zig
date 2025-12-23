const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const rainbow = @import("../rainbow.zig");
const mem = @import("../mem/root.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;

pub const Error = error{
    UnexpectedToken,
    UnexpectedEOF,
    StringInterpNotAllowed,
    StringInArithmeticExpressionNotAllowed,
    DuplicateStructDecl,
    ForCapturesMustMatchSources,
    ExpectedTypeIdentifier,
    ExpectedTypeExpr,
} || std.mem.Allocator.Error || lexer.Error;

/// Parser consumes tokens from the streaming lexer and produces AST nodes.
/// It currently implements enough grammar to cover optional-aware `if`
/// expressions with capture clauses and braced blocks.
const max_expected_tokens = 8;
const max_breadcrumbs = 32;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    document_store: *DocumentStore,
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
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    logging_enabled: bool = false,

    const Self = @This();

    pub const Result = union(enum) {
        err: struct {
            _diagnostics: []const Diagnostic,
            script: ?ast.Script,

            pub fn diagnostics(self: @This()) []const Diagnostic {
                return self._diagnostics;
            }
        },
        success: ast.Script,

        pub fn fromScript(script: ?ast.Script) @This() {
            return if (script) |s| .{ .success = s } else .errNoDiagnostics(script);
        }

        pub fn errDiagnostics(_diagnostics: []const Diagnostic, script: ?ast.Script) @This() {
            return .{ .err = .{ ._diagnostics = _diagnostics, .script = script } };
        }

        pub fn errNoDiagnostics(script: ?ast.Script) @This() {
            return .{ .err = .{ ._diagnostics = &.{}, .script = script } };
        }
    };

    pub const Diagnostic = struct {
        span_: ast.Span,
        message: []const u8,
        err: Error,

        pub fn span(self: Diagnostic) ast.Span {
            return self.span_;
        }

        pub fn severity(_: Diagnostic) []const u8 {
            return "error";
        }
    };

    pub const Snapshot = struct {
        diagnostics: []const Diagnostic,
        stream_snapshot: lexer.StreamSnapshot,

        pub fn init(parser: *Self) !Snapshot {
            const stream_snapshot = try parser.stream.snapshot();
            const next = try parser.stream.peek();
            try parser.log(
                "took snapshot at {?f}({?}) : {f}({})",
                .{
                    stream_snapshot.stream.current,
                    if (stream_snapshot.stream.current) |c| c.span.start.offset else null,
                    next,
                    next.span.start.offset,
                },
            );

            return .{
                .diagnostics = try parser.arena.allocator().dupe(
                    Diagnostic,
                    parser.diagnostics.items,
                ),
                .stream_snapshot = stream_snapshot,
            };
        }

        pub fn deinit(self: *Snapshot) void {
            self.stream_snapshot.deinit();
        }

        pub fn restore(self: *Snapshot, parser: *Self) !void {
            try parser.stream.restore(&self.stream_snapshot);
            const next = try parser.stream.peek();
            parser.diagnostics.clearRetainingCapacity();
            try parser.diagnostics.appendSlice(parser.arena.allocator(), self.diagnostics);
            try parser.log("restored snapshot to {?f}({?}) : {f}({})", .{
                parser.stream.current,
                if (parser.stream.current) |c| c.span.start.offset else null,
                next,
                next.span.start.offset,
            });
        }
    };

    // pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
    pub fn init(
        allocator: std.mem.Allocator,
        document_store: *DocumentStore,
        // getCachedAst: *const fn (path: []const u8) ?ast.Script,
        // getSource: *const fn (path: []const u8) []const u8,
    ) Self {
        return .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .document_store = document_store,
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

    pub fn reportParseError(
        self: *Self,
        err: Error,
        span: ast.Span,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        try self.diagnostics.append(self.arena.allocator(), .{
            .err = err,
            .span_ = span,
            .message = try std.fmt.allocPrint(self.arena.allocator(), fmt, args),
        });
    }

    fn compileResult(self: *Self, script: ?ast.Script) Result {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        self.log(@src().fn_name, .{}) catch {};

        if (self.diagnostics.items.len > 0) {
            return .errDiagnostics(self.diagnostics.items, script);
        }

        return .fromScript(script);
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
        const writer = &stdout.interface;
        try self.writeLogPrefix(writer);
        const maxColors = std.meta.tags(rainbow.RainbowColor).len;
        const color: rainbow.RainbowColor = @enumFromInt(@max(1, @mod(self.breadcrumb_depth, maxColors)));
        try writer.writeAll(rainbow.beginColor(color));
        try writer.print(fmt, args);
        try writer.writeAll(rainbow.endColor());
        try writer.writeByte('\n');
        try self.logCurrentTokenSourceLine(writer);
        try writer.flush();
    }

    pub fn logWithoutPrefix(self: Self, comptime fmt: []const u8, args: anytype) !void {
        if (!self.logging_enabled) return;
        var stdout = std.fs.File.stderr().writer(&.{});
        const writer = &stdout.interface;
        try writer.print(fmt, args);
        try writer.flush();
    }

    fn writeLogPrefix(self: Self, writer: *std.Io.Writer) !void {
        try writer.print("[parser]:", .{});
        for (0..self.breadcrumb_depth) |_| {
            try writer.writeByte(' ');
        }
    }

    fn logNewlineToken(self: Self, writer: *std.Io.Writer) !void {
        const tok = self.currentToken() orelse return;
        const line = tok.span.start.line;
        const col = tok.span.start.column;
        const start = tok.span.start.column - 1;
        const line_slice = self.lineSlice(line) orelse return;

        try self.writeLogPrefix(writer);
        const pre_token_slice = line_slice[0..start];

        try writer.print("L{d}C{d}: ", .{ line, col });
        try writer.writeAll(pre_token_slice);
        try writer.writeAll(rainbow.beginColor(.black));
        try writer.writeAll(rainbow.beginBgColor(.orange));
        try writer.writeByte('$');
        try writer.writeAll(rainbow.endColor());
        try writer.writeByte('\n');
    }

    fn logCurrentTokenSourceLine(self: Self, writer: *std.Io.Writer) !void {
        const tok = self.currentToken() orelse return;
        if (tok.span.start.line != tok.span.end.line) {
            if (tok.lexeme.len == 1 and tok.lexeme[0] == '\n') {
                try self.logNewlineToken(writer);
                try self.logPeekedTokenSourceLine(writer);
                return;
            }
            try writer.print("skipping token ({}-{})[{}]{*}", .{
                tok.span.start.line,
                tok.span.end.line,
                tok.lexeme.len,
                tok.lexeme.ptr,
                    // if (std.ascii.isPrint(tok.lexeme[0])) tok.lexeme[0..32] else "<invalid>",
            });
            return;
        }
        if (tok.span.end.column <= tok.span.start.column) return;
        const peeked = self.peekedToken();
        const line = tok.span.start.line;
        const col = tok.span.start.column;
        const start = tok.span.start.column - 1;
        const end = tok.span.end.column - 1;
        const line_slice = self.lineSlice(line) orelse return;

        try self.writeLogPrefix(writer);
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
            if (p.span.start.offset > tok.span.start.offset and p.span.start.line == line) {
                try writer.writeAll(rainbow.beginColor(.black));
                try writer.writeAll(rainbow.beginBgColor(.violet));
                try writer.writeAll(line_slice[@min(end, line_slice.len)..@min(p.span.start.column, line_slice.len)]);
                try writer.writeAll(rainbow.endColor());
                try writer.writeAll(line_slice[@min(p.span.start.column, line_slice.len)..]);
            } else {
                try writer.writeAll(post_token_slice);
            }
        } else {
            try writer.writeAll(post_token_slice);
        }
        try writer.writeByte('\n');
        if (peeked) |p| {
            if (p.span.start.line != tok.span.end.line or p.span.end.line != tok.span.end.line) {
                try self.logPeekedTokenSourceLine(writer);
            }
        }
    }

    fn logPeekedTokenSourceLine(self: Self, writer: *std.Io.Writer) !void {
        const tok = self.peekedToken() orelse return;
        if (tok.span.start.line != tok.span.end.line) {
            if (tok.lexeme.len == 1 and tok.lexeme[0] == '\n') {
                return self.logNewlineToken(writer);
            }
            try writer.print("skipping token ({}-{}){s}", .{ tok.span.start.line, tok.span.end.line, tok.lexeme });
            return;
        }
        if (tok.span.end.column <= tok.span.start.column) return;
        const line = tok.span.start.line;
        const col = tok.span.start.column;
        const start = tok.span.start.column - 1;
        const end = tok.span.end.column - 1;
        const line_slice = self.lineSlice(line) orelse return;

        try self.writeLogPrefix(writer);
        const pre_token_slice = line_slice[0..start];
        const token_slice = line_slice[start..end];
        const post_token_slice = line_slice[end..];
        try writer.print("L{d}C{d}: ", .{ line, col });
        try writer.writeAll(pre_token_slice);
        try writer.writeAll(rainbow.beginColor(.black));
        try writer.writeAll(rainbow.beginBgColor(.violet));
        try writer.writeAll(token_slice);
        try writer.writeAll(rainbow.endColor());
        try writer.writeAll(post_token_slice);
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

    fn takeSnapshot(self: *Self) Error!Snapshot {
        return .init(self);
    }

    fn restoreSnapshot(self: *Self, snapshot: *Snapshot) Error!void {
        try snapshot.restore(self);
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

    pub fn parseScript(self: *Self, path: []const u8) Result {
        return self.compileResult(self.parseScriptInner(path) catch |err| brk: {
            self.log("error: {}", .{err}) catch {};
            break :brk null;
        });
    }

    fn parseScriptInner(self: *Self, path: []const u8) !ast.Script {
        const duped_path = try self.arena.allocator().dupe(u8, path);
        self.path = duped_path;
        self.source = try self.document_store.getSource(duped_path);
        self.stream = try .init(self.arena.allocator(), duped_path, self.source);

        const logging_enabled = std.process.getEnvVarOwned(self.allocator, "RUNIC_LOG_PARSER") catch |err| switch (err) {
            error.EnvironmentVariableNotFound => null,
            else => return err,
        };
        defer if (logging_enabled) |le| self.allocator.free(le);
        if (logging_enabled) |le| self.logging_enabled = std.mem.eql(u8, le, "1");

        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        if (try self.document_store.getAst(duped_path)) |script| {
            try self.logWithoutPrefix("returning cached ast", .{});
            return script;
        }
        self.clearExpectedTokens();

        const statements = try self.parseStatementsUntil(.eof);

        const script = ast.Script{
            .span = statements.span,
            .statements = statements.payload,
        };

        try self.document_store.putAst(duped_path, script);

        return script;
    }

    pub fn parseSource(self: *Self, source: []const u8) !ast.Script {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
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

    fn parseExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        self.clearExpectedTokens();

        const next = try self.peekToken();

        return switch (next.tag) {
            .kw_fn => self.parseFn(),
            .kw_import => self.parseImportExpression(),
            .identifier => self.parseIdentifierExpression(),
            .kw_if => self.parseIfExpression(),
            .kw_for => self.parseForExpression(),
            else => {
                if (try self.parseMaybeBinaryExpression()) |arith_expr| return arith_expr;
                return self.parsePrimaryExpression();
            },
        };
    }

    fn ParserPayload(comptime T: type) type {
        const ReturnType = @typeInfo(T).@"fn".return_type orelse void;
        const Payload = @typeInfo(ReturnType).error_union.payload;

        return Payload;
    }

    // .{ parseInt, parseString } == .{ .@"0" = parseInt, .@"1" = parseString }
    // union enum { @"0": i64, @"1": []const u8 }
    fn OneOf(comptime Parsers: type) type {
        var fields: []const std.builtin.Type.UnionField = &.{};

        for (std.meta.fields(Parsers)) |field| {
            const Payload = ParserPayload(field.type);
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
    fn oneOf(self: *Self, parsers: anytype) Error!OneOf(@TypeOf(parsers)) {
        var the_error: Error = undefined;

        inline for (std.meta.fields(@TypeOf(parsers))) |field| {
            const parser = @field(parsers, field.name);
            var snapshot = try self.takeSnapshot();
            defer snapshot.deinit();

            if (parser(self)) |result| {
                return @unionInit(OneOf(@TypeOf(parsers)), field.name, result);
            } else |err| {
                the_error = err;
                self.restoreSnapshot(snapshot);
            }
        }

        return the_error;
    }

    fn parseMemberAccessExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();
    }

    fn parseIdentifierExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        // TODO: Add member access and assign as binary expressions
        if (try self.parseMaybeBinaryExpression()) |expr| return expr;

        const p = try self.peekSlice(2);
        const identifier = p[0];
        const next_token = p[1];

        // if (isExprTerminator(next_token.tag)) {
        //     return try self.parsePipeline();
        // }

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
            .assign => {
                _ = try self.nextToken();
                _ = try self.nextToken();
                const expr = try self.parseExpression();

                return try self.allocExpression(.{
                    .assignment = .{
                        .expr = expr,
                        .identifier = .fromToken(identifier),
                        .span = identifier.span.endAt(expr.span()),
                    },
                });
            },
            else => return try self.parsePipeline(),
        }
    }

    const BinaryComponent = union(enum) {
        identifier: ast.Identifier,
        op: token.Spanned(ast.BinaryOp),
        literal: BinaryComponentLiteral,
        expr: *ast.Expression,

        pub const Chirality = enum { left, right };

        pub fn toExpression(
            self: BinaryComponent,
            parser: *Self,
            op: ?ast.BinaryOp,
            chirality: Chirality,
        ) Error!*ast.Expression {
            return try switch (self) {
                .expr => |expr| expr,
                .identifier => |identifier| self.toIdentifierExpression(parser, identifier, op, chirality),
                .literal => |literal| literal.toExpression(parser),
                .op => @panic("shouldn't happen :)"),
            };
        }

        fn toIdentifierExpression(
            _: BinaryComponent,
            parser: *Self,
            identifier: ast.Identifier,
            op: ?ast.BinaryOp,
            chirality: Chirality,
        ) Error!*ast.Expression {
            if (op == .member) {
                return parser.allocExpression(.{ .identifier = identifier });
            } else if (op != null and op.?.isAssignment() and chirality == .left) {
                return parser.allocExpression(.{ .identifier = identifier });
            } else {
                return parser.allocExpression(.{
                    .call = .{
                        .callee = try parser.allocExpression(.{
                            .identifier = identifier,
                        }),
                        .arguments = &.{},
                        .span = identifier.span,
                    },
                });
            }
        }

        pub fn span(self: BinaryComponent) token.Span {
            return switch (self) {
                .identifier => |identifier| identifier.span,
                .op => |op| op.span,
                .literal => |literal| literal.span(),
                .expr => |expr| expr.span(),
            };
        }

        pub fn format(self: BinaryComponent, writer: *std.Io.Writer) !void {
            try switch (self) {
                .op => |op| writer.writeAll(@tagName(op.payload)),
                .expr => |_| writer.writeAll("(expr)"),
                .identifier => |iden| writer.print("ident:{s}", .{iden.name}),
                .literal => |lit| switch (lit) {
                    inline .int, .float => |l| writer.writeAll(l.text),
                    .boolean => |l| writer.print("{}", .{l.value}),
                },
            };
        }
    };

    const BinaryComponentLiteral = union(enum) {
        int: ast.IntegerLiteral,
        float: ast.FloatLiteral,
        boolean: ast.BoolLiteral,
        string: ast.StringLiteral,

        pub fn fromToken(tok: token.Token) BinaryComponentLiteral {
            return switch (tok.tag) {
                .int_literal => .{ .int = .{ .text = tok.lexeme, .span = tok.span } },
                .float_literal => .{ .float = .{ .text = tok.lexeme, .span = tok.span } },
                .kw_true => .{ .boolean = .{ .value = true, .span = tok.span } },
                .kw_false => .{ .boolean = .{ .value = false, .span = tok.span } },
                else => @panic("shouldn't happen :)"),
            };
        }

        pub fn toExpression(self: BinaryComponentLiteral, parser: *Self) Error!*ast.Expression {
            return try switch (self) {
                .int => |int| parser.allocExpression(.{
                    .literal = .{ .integer = int },
                }),
                .float => |float| parser.allocExpression(.{
                    .literal = .{ .float = float },
                }),
                .boolean => |boolean| parser.allocExpression(.{
                    .literal = .{ .bool = boolean },
                }),
                .string => |string| parser.allocExpression(.{
                    .literal = .{ .string = string },
                }),
            };
        }

        pub fn span(self: BinaryComponentLiteral) token.Span {
            return switch (self) {
                inline else => |l| l.span,
            };
        }
    };

    fn parseFunctionCall(self: *Self) Error!*ast.Expression {
        const next = try self.peekToken();

        switch (next.tag) {
            .identifier => {
                _ = try self.nextToken();
                // hello "asdf" + "asdgf" 1234
                const args = try self.parseList(.comma, parseExpression, .{
                    .terminators = .function(isExprTerminator),
                });
                _ = args;
            },
            .l_paren => {},
        }
    }

    const BinaryState = enum {
        expr,
        op,

        pub fn advance(self: *BinaryState) void {
            switch (self.*) {
                .expr => self.* = .op,
                .op => self.* = .expr,
            }
        }
    };

    // a + b * 2 + 4
    //
    // components = [a, +, b, *, 2, +, 4]
    //
    // for each binary operator (+, *):
    //   1. find lowest precedence operator (LPO)
    //   2. group components between the LPO
    //         [(a), +, (b, *, 2), +, (4)]
    //   3. define mutable LHS: BinaryExpression
    //   parse binary expression:
    //     1. is length == 1 ? then return component
    //     2. is length == 3 ? then return binary expr
    //     3. take 3, store in LHS
    //         [((a), +, (b, *, 2)), +, (4)]

    fn parseMaybeBinaryExpression(self: *Self) Error!?*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        var snapshot = try self.takeSnapshot();
        defer snapshot.deinit();

        var components = std.ArrayList(BinaryComponent).empty;
        defer components.deinit(self.allocator);

        var state = BinaryState.expr;

        var next = try self.peekToken();

        const abort_initial_tokens = [_]token.Tag{
            .l_brace,
            .dot_l_brace,
        };

        for (abort_initial_tokens) |tag| if (next.tag == tag) return null;

        while (true) : (state.advance()) {
            next = try self.peekToken();

            switch (state) {
                .expr => {
                    switch (next.tag) {
                        .identifier => {
                            const breadcrumbInner = try self.createBreadcrumb("PBE:identifier");
                            defer breadcrumbInner.end();
                            try components.append(self.allocator, .{
                                .identifier = .fromToken(next),
                            });
                        },
                        .l_paren => {
                            _ = try self.nextToken();
                            try components.append(self.allocator, .{
                                .expr = try self.parseExpression(),
                            });
                            _ = try self.expectTokenTag(.r_paren);
                            continue;
                        },
                        .int_literal, .float_literal, .kw_true, .kw_false => {
                            const breadcrumbInner = try self.createBreadcrumb("PBE:literal");
                            defer breadcrumbInner.end();
                            try components.append(self.allocator, .{
                                .literal = .fromToken(next),
                            });
                        },
                        .string_start => {
                            const breadcrumbInner = try self.createBreadcrumb("PBE:string");
                            defer breadcrumbInner.end();
                            try components.append(self.allocator, .{
                                .literal = .{ .string = try self.parseStringLiteral() },
                            });
                            continue;
                        },
                        else => return Error.UnexpectedToken,
                    }
                },
                .op => {
                    switch (next.tag) {
                        .equal_equal, .bang_equal, .greater, .greater_equal, .less, .less_equal, .plus, .minus, .star, .slash, .percent, .kw_and, .kw_or, .pipe_pipe, .amp_amp, .pipe, .dot, .assign, .plus_assign, .minus_assign, .mul_assign, .div_assign, .rem_assign => {
                            const breadcrumbInner = try self.createBreadcrumb("PBE:op");
                            defer breadcrumbInner.end();
                            try components.append(self.allocator, .{
                                .op = token.Spanned(ast.BinaryOp).fromToken(next) orelse @panic("shouldn't happen :)"),
                            });
                        },
                        else => {
                            if (next.tag == .range) break;
                            if (isExprTerminator(next.tag)) break;

                            try components.append(self.allocator, .{
                                .op = token.Spanned(ast.BinaryOp){
                                    .payload = .apply,
                                    .span = next.span,
                                },
                            });

                            continue;

                            // try self.restoreSnapshot(&snapshot);
                            // return null;
                        },
                    }
                },
            }

            _ = try self.nextToken();
        }

        if (components.items.len == 0) {
            try self.restoreSnapshot(&snapshot);
            return null;
        }

        // if (components.items.len == 1) {
        //     if (components.items[0] == .identifier) {
        //         try self.restoreSnapshot(&snapshot);
        //         return null;
        //     } else {
        //         return components.items[0].toExpression(self);
        //     }
        // }

        return self.parseBinaryExpression(components.items, null, .left);
    }

    // f a b c ==> (((f a) b) c) ==> (f a b c)

    fn flattenBinaryExpression(self: *Self, binary: ast.Expression) Error!ast.Expression {
        const left = binary.binary.left;
        const right = binary.binary.right;

        return switch (binary.binary.op) {
            .apply => switch (left.*) {
                .call => |call| .{
                    .call = .{
                        .callee = call.callee,
                        .arguments = try std.mem.concat(
                            self.arena.allocator(),
                            *ast.Expression,
                            &.{ call.arguments, &.{right} },
                        ),
                        .span = call.span.endAt(right.span()),
                    },
                },
                else => .{
                    .call = .{
                        .callee = left,
                        .arguments = try self.arena.allocator().dupe(
                            *ast.Expression,
                            &.{right},
                        ),
                        .span = left.span().endAt(right.span()),
                    },
                },
            },
            .pipe => switch (left.*) {
                .pipeline => |pipeline| .{
                    .pipeline = .{
                        .stages = try std.mem.concat(
                            self.arena.allocator(),
                            *ast.Expression,
                            &.{ pipeline.stages, &.{right} },
                        ),
                        .span = binary.span(),
                    },
                },
                else => .{
                    .pipeline = .{
                        .stages = try self.copyToArena(*ast.Expression, &.{ left, right }),
                        .span = binary.span(),
                    },
                },
            },
            else => binary,
        };
    }

    fn parseBinaryExpression(
        self: *Self,
        components: []const BinaryComponent,
        op: ?ast.BinaryOp,
        chirality: BinaryComponent.Chirality,
    ) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        if (components.len == 1) return try components[0].toExpression(self, op, chirality);

        if (components.len == 3) {
            const op_ = components[1].op.payload;

            return try self.allocExpression(
                try self.flattenBinaryExpression(
                    try self.initBinaryExpression(
                        try components[0].toExpression(self, op_, .left),
                        op_,
                        try components[2].toExpression(self, op_, .right),
                        components[0].span().endAt(components[2].span()),
                    ),
                ),
            );
        }

        var lowest_precedence: usize = std.math.maxInt(usize);
        for (components) |item| {
            switch (item) {
                .op => |op_| lowest_precedence = @min(
                    op_.payload.precedence(),
                    lowest_precedence,
                ),
                else => continue,
            }
        }

        const DelimiterFn = struct {
            lowest_precedence: usize,

            pub fn isDelimiter(ctx: @This(), component: BinaryComponent) bool {
                return switch (component) {
                    .op => |op_| op_.payload.precedence() == ctx.lowest_precedence,
                    else => false,
                };
            }
        };

        const delimiter = DelimiterFn{ .lowest_precedence = lowest_precedence };

        var it = mem.splitScalarByFn(
            BinaryComponent,
            delimiter,
            components,
            DelimiterFn.isDelimiter,
        );
        const first_components = it.next().?;
        var i: usize = first_components.len + 1;
        const op_ = components[i - 1].op.payload;
        var lhs: *ast.Expression = try self.parseBinaryExpression(first_components, op_, .left);

        while (it.next()) |sub_components| {
            const right = try self.parseBinaryExpression(sub_components, op_, .right);

            lhs = try self.allocExpression(
                try self.flattenBinaryExpression(
                    try self.initBinaryExpression(
                        lhs,
                        op_,
                        right,
                        lhs.span().endAt(right.span()),
                    ),
                ),
            );

            i += sub_components.len + 1;
        }

        return lhs;

        // list algo:
        //
        // ex1:
        // 1 + 2 + 3 + 4        # equal precedence
        // -------
        // (1 + 2) + 3 + 4      # group first 2 terms -> equal precedence
        // -------------
        // ((1 + 2) + 3) + 4
        //
        // ex2:
        // 1 + 2 * 3 + 4   # higher precendece on the right op
        // -------
        // 1 + (2 * 3 + 4) # group until the end, lower precedence on the right op
        //      ---------
        // 1 + ((2 * 3) + 4) # group the left before the right op
        //
        // ex3:
        // 1 + 2 * 3 + 4 > 5 + 1 || true # ((1 + ((2 * 3) + 4)) > (5 + 1)) || true
        //
        // (1 + 2 * 3 + 4) > (5 + 1) || (true)
        //                 -
        // (((1 + 2 * 3 + 4) > (5 + 1)) || (true)) || (false)
    }

    pub fn initBinaryExpression(
        self: *Self,
        left: *ast.Expression,
        op: ast.BinaryOp,
        right: *ast.Expression,
        span: ast.Span,
    ) Error!ast.Expression {
        if (op == .member and right.* != .identifier) {
            try self.reportParseError(
                Error.UnexpectedToken,
                right.span(),
                "expected identifier after member access operator",
                .{},
            );
        } else if (op.isAssignment() and !left.isReference()) {
            try self.reportParseError(
                Error.UnexpectedToken,
                left.span(),
                "expected reference before assignment operator, actual: {t}",
                .{left.*},
            );
        }

        return .{ .binary = .{
            .left = left,
            .op = op,
            .right = right,
            .span = span,
        } };
    }

    fn parsePipeline(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        var stages = std.ArrayList(ast.PipelineStage).empty;
        defer stages.deinit(self.allocator);

        const start_token = try self.peekToken();
        var last_token = start_token;
        var next_token: token.Token = start_token;

        while (true) : (last_token = next_token) {
            const stage = try self.parsePipelineStage();
            try stages.append(self.allocator, stage);
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

                    return try self.allocExpression(.{ .pipeline_deprecated = .{
                        .stages = try self.copyToArena(ast.PipelineStage, stages.items),
                        .span = start_token.span.endAt(stage.span),
                    } });
                },
            }
        }
    }

    fn isExprTerminator(tag: token.Tag) bool {
        return switch (tag) {
            .r_paren, .r_bracket, .r_brace, .comma, .pipe, .pipe_pipe, .amp_amp, .amp, .string_interp_end, .newline, .l_brace, .range, .dot_l_brace, .kw_else => true,
            else => false,
        };
    }

    fn parsePipelineStage(self: *Self) Error!ast.PipelineStage {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
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

    pub fn parseImportExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
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

    pub fn expectEnd(self: *Self) Error!void {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        self.skipNewlines();
        const tok = try self.nextToken();
        if (tok.tag != .eof) return self.failExpectedToken(tok.tag, .eof);
    }

    fn parseIfExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const if_tok = try self.expect(.kw_if);
        _ = try self.expect(.l_paren);
        const condition = try self.parseExpression();
        _ = try self.expect(.r_paren);
        const capture = try self.parseOptionalCaptureClause();
        // const then_block = try self.parseBlock();
        const then_expr = try self.parseExpression();

        var else_branch: ?ast.IfExpr.ElseBranch = null;
        var end_span = then_expr.span();

        const maybe_else = try self.peekToken();
        if (maybe_else.tag == .kw_else) {
            _ = try self.nextToken();
            const after_else = try self.peekToken();
            if (after_else.tag == .kw_if) {
                const nested = try self.parseIfExpression();
                const nested_if = switch (nested.*) {
                    .if_expr => |*payload| payload,
                    else => return Error.UnexpectedToken,
                };
                end_span = nested_if.span;
                else_branch = .{ .if_expr = nested_if };
            } else {
                const else_expr = try self.parseExpression();
                end_span = else_expr.span();
                else_branch = .{ .expr = else_expr };
            }
        }

        return self.allocExpression(.{
            .if_expr = .{
                .condition = condition,
                .capture = capture,
                .then_expr = then_expr,
                .else_branch = else_branch,
                .span = if_tok.span.endAt(end_span),
            },
        });
    }

    // for (items) |item| {...}
    // for (items, 0..) |item, i| {...}
    fn parseForExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const for_tok = try self.expect(.kw_for);
        _ = try self.expect(.l_paren);
        const sources = try self.parseForSources();
        _ = try self.expect(.r_paren);
        const capture = try self.parseCaptureClause();

        if (capture.bindings.len != sources.len) return Error.ForCapturesMustMatchSources;

        const body = try self.parseExpression();

        return self.allocExpression(.{
            .for_expr = .{
                .sources = sources,
                .capture = capture,
                .body = body,
                .span = for_tok.span.endAt(body.span()),
            },
        });
    }

    fn parseForSources(self: *Self) Error![]const *ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const sources = try self.parseList(.comma, parseForSource, .{});
        return sources.payload;
    }

    // expr | 0.. | 1..4
    fn parseForSource(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        return try self.parseTry(parseRange) orelse self.parseExpression();
    }

    fn parseTry(
        self: *Self,
        parser: anytype,
    ) Error!?ParserPayload(@TypeOf(parser)) {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        var snapshot = try self.takeSnapshot();
        defer snapshot.deinit();

        return parser(self) catch {
            try self.restoreSnapshot(&snapshot);
            return null;
        };
    }

    // 0.. | 1..4
    fn parseRange(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const range_terminators = [_]token.Tag{
            .pipe,
            .l_brace,
            .r_paren,
        };

        const start = try self.parseExpression();
        const dots = try self.expectTokenTag(.range);
        const next = try self.peekToken();
        const end: ?*ast.Expression = for (range_terminators) |term| {
            if (next.tag == term) break null;
        } else try self.parseExpression();
        const end_span = if (end) |e| e.span() else dots.span;

        return self.allocExpression(.{
            .range = .{
                .start = start,
                .end = end,
                .inclusive_end = false,
                .span = start.span().endAt(end_span),
            },
        });
    }

    fn parseIntegerLiteral(self: *Self) Error!ast.IntegerLiteral {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const integer = try self.expectTokenTag(.int_literal);
        return .fromToken(integer);
    }

    fn parseArrayLiteralExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        _ = try self.expectTokenTag(.dot_l_brace);

        const elements = try self.parseList(.comma, parseExpression, .{ .skipNewLines = true });
        const array = try self.allocExpression(.{
            .array = .{
                .elements = elements.payload,
                .span = elements.span,
            },
        });

        _ = try self.expectTokenTag(.r_brace);

        return array;
    }

    fn parseCaptureClause(self: *Self) Error!ast.CaptureClause {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        return try self.parseOptionalCaptureClause() orelse return self.failExpectedToken(null, .pipe);
    }

    fn parseOptionalCaptureClause(self: *Self) Error!?ast.CaptureClause {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const next = try self.peekToken();
        if (next.tag != .pipe) return null;

        _ = try self.nextToken(); // consume opening '|'
        const capture_start = next.span.start;
        const bindings = try self.parseList(.comma, parseBindingPattern, .{});
        const close_tok = try self.expect(.pipe);

        return ast.CaptureClause{
            .bindings = bindings.payload,
            .span = .{ .start = capture_start, .end = close_tok.span.end },
        };
    }

    fn parseBindingPattern(self: *Self) Error!*ast.BindingPattern {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
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
        return *const fn (*Self) Error!T;
    }

    const ParseAndSkipUntilOptions = struct {
        skipNewLines: bool = false,
        terminators: ?union(enum) {
            _function: *const fn (token.Tag) bool,
            _list: []const token.Tag,

            pub fn list(_list: []const token.Tag) @This() {
                return .{ ._list = _list };
            }

            pub fn function(_function: *const fn (token.Tag) bool) @This() {
                return .{ ._function = _function };
            }

            pub fn isTerminator(self: @This(), tag: token.Tag) bool {
                return switch (self) {
                    ._list => |_list| std.mem.containsAtLeastScalar(token.Tag, _list, 1, tag),
                    ._function => |_function| _function(tag),
                };
            }
        } = null,
    };

    fn parseList(
        self: *Self,
        comptime delimiter: token.Tag,
        parser: anytype,
        options: ParseAndSkipUntilOptions,
    ) Error!ast.Spanned([]ParserPayload(@TypeOf(parser))) {
        const T = ParserPayload(@TypeOf(parser));

        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const start = try self.peekToken();

        if (options.terminators) |terminators| if (terminators.isTerminator(start.tag)) {
            return .{
                .payload = &.{},
                .span = start.span,
            };
        };

        var parsed = std.ArrayList(T).empty;
        defer parsed.deinit(self.allocator);

        while (true) {
            if (options.skipNewLines) self.skipNewlines();

            try parsed.append(self.allocator, try parser(self));

            const next = try self.peekToken();
            if (next.tag != delimiter) {
                const last_item = parsed.getLast();
                const last_item_span = brk: {
                    const Item = switch (@typeInfo(T)) {
                        .pointer => |pointer| pointer.child,
                        else => T,
                    };

                    if (std.meta.hasFn(Item, "span")) {
                        break :brk last_item.span();
                    } else {
                        break :brk last_item.span;
                    }
                };

                return .{
                    .payload = try self.copyToArena(T, parsed.items),
                    .span = start.span.endAt(last_item_span),
                };
            }
            _ = try self.nextToken();
        }
    }

    fn parseUntil(
        self: *Self,
        comptime delimiter: token.Tag,
        parser: anytype,
        options: ParseAndSkipUntilOptions,
    ) Error!ast.Spanned([]const ParserPayload(@TypeOf(parser))) {
        const T = ParserPayload(@TypeOf(parser));

        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const open = try self.peekToken();
        var parsed = std.ArrayList(T).empty;
        defer parsed.deinit(self.allocator);

        while (true) {
            if (options.skipNewLines) self.skipNewlines();
            const next = try self.peekToken();

            if (next.tag == delimiter) {
                _ = try self.nextToken();
                return .{
                    .payload = try self.copyToArena(T, parsed.items),
                    .span = .{ .start = open.span.start, .end = next.span.end },
                };
            } else if (next.tag == .eof) {
                return Error.UnexpectedEOF;
            }

            const nextParsed = try parser(self);
            try parsed.append(self.allocator, nextParsed);
        }
    }

    fn parseStatementsUntil(
        self: *Self,
        comptime end: token.Tag,
    ) Error!ast.Spanned([]const *ast.Statement) {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        return try self.parseUntil(
            end,
            parseStatement,
            .{ .skipNewLines = true },
        );
    }

    fn parseBlock(self: *Self) Error!ast.Block {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const open = try self.expect(.l_brace);
        const statements = try self.parseStatementsUntil(.r_brace);

        return ast.Block{
            .statements = statements.payload,
            .span = open.span.endAt(statements.span),
        };
    }

    fn parseBlockExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const open = try self.expect(.l_brace);
        const statements = try self.parseStatementsUntil(.r_brace);

        return self.allocExpression(.{ .block = ast.Block{
            .statements = statements.payload,
            .span = open.span.endAt(statements.span),
        } });
    }

    fn parseStatement(self: *Self) Error!*ast.Statement {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        self.skipNewlines();
        const stmt = try self.arena.allocator().create(ast.Statement);
        errdefer self.arena.allocator().destroy(stmt);

        if (try self.parseMaybeTypeBinding()) |type_binding_decl| {
            stmt.* = .{ .type_binding_decl = type_binding_decl };
            return stmt;
        }

        if (try self.parseMaybeBinding()) |binding_decl| {
            stmt.* = .{ .binding_decl = binding_decl };
            return stmt;
        }

        if (try self.parseMaybeReturn()) |return_stmt| {
            stmt.* = .{ .return_stmt = return_stmt };
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

    fn consumeStatementTerminator(self: *Self) Error!void {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
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

    fn parsePrimaryExpression(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
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
            .l_brace,
            .dot_l_brace,
        };

        const p = self.peekToken() catch {
            return self.failExpectedTokens(null, &expected_primary_tokens);
        };

        if (p.tag == .string_start) return self.parseStringLiteralExpr();

        var snapshot = try self.takeSnapshot();
        defer snapshot.deinit();
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
            .l_brace => blk: {
                try self.restoreSnapshot(&snapshot);
                break :blk try self.parseBlockExpression();
            },
            .dot_l_brace => blk: {
                try self.restoreSnapshot(&snapshot);
                break :blk try self.parseArrayLiteralExpression();
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

    fn failExpectedTokens(self: *Self, unexpected: ?token.Tag, expected: []const token.Tag) Error {
        self.captureBreadcrumbSnapshot();
        self.recordExpectedTokens(expected);
        self.unexpected_token = unexpected;
        return Error.UnexpectedToken;
    }

    fn failExpectedToken(self: *Self, unexpected: ?token.Tag, tag: token.Tag) Error {
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

    fn expect(self: *Self, tag: token.Tag) Error!token.Token {
        const tok = try self.nextToken();
        if (tok.tag != tag) return self.failExpectedToken(tok.tag, tag);
        return tok;
    }

    fn allocExpression(self: *Self, value: ast.Expression) Error!*ast.Expression {
        const node = try self.arena.allocator().create(ast.Expression);
        node.* = value;
        return node;
    }

    fn allocTypeExpression(self: *Self, value: ast.TypeExpr) Error!*const ast.TypeExpr {
        const node = try self.arena.allocator().create(ast.TypeExpr);
        node.* = value;
        return node;
    }

    fn parseStringLiteralWithoutInterp(self: *Self) Error!ast.Spanned([]const u8) {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const stringLiteral = try self.parseStringLiteral();
        var result: std.ArrayList(u8) = .empty;
        defer result.deinit(self.allocator);
        for (stringLiteral.segments) |segment| {
            if (segment == .interpolation) {
                return Error.StringInterpNotAllowed;
            }
            try result.appendSlice(self.allocator, segment.text.payload);
        }

        return .{
            .payload = try self.copyToArena(u8, result.items),
            .span = stringLiteral.span,
        };
    }

    fn parseStringLiteral(self: *Self) Error!ast.StringLiteral {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const start_token = try self.expectTokenTag(.string_start);

        var segments = std.ArrayList(ast.StringLiteral.Segment).empty;
        defer segments.deinit(self.allocator);

        const end = while (true) {
            const tok = try self.nextToken();
            try self.log("stok: {}:{s}", .{ tok.tag, tok.lexeme });

            switch (tok.tag) {
                .string_text => {
                    try self.log("appending string text: \"{s}\"", .{tok.lexeme});
                    try segments.append(
                        self.allocator,
                        .{ .text = .{
                            .payload = try self.decodeString(tok.lexeme),
                            .span = tok.span,
                        } },
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
                .string_end => break tok,
                else => return self.failExpectedTokens(tok.tag, &.{ .string_text, .string_end, .string_interp_start }),
            }
        };

        return .{
            .segments = try self.copyToArena(ast.StringLiteral.Segment, segments.items),
            .span = start_token.span.endAt(end.span),
        };
    }

    fn decodeString(self: *Self, string: []const u8) Error![]const u8 {
        return std.mem.replaceOwned(u8, self.arena.allocator(), string, "\\$", "$");
    }

    fn parseStringLiteralExpr(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        return self.allocExpression(.{
            .literal = .{ .string = try self.parseStringLiteral() },
        });
    }

    fn parseMaybeTypeBinding(self: *Self) Error!?ast.TypeBindingDecl {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const next = try self.peekToken();
        return switch (next.tag) {
            .kw_const => try self.parseTry(parseTypeBinding),
            else => null,
        };
    }

    fn parseTypeBinding(self: *Self) Error!ast.TypeBindingDecl {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const start = try self.expectTokenTag(.kw_const);
        const identifier = try self.parseTypeIdentifier();
        _ = try self.expectTokenTag(.assign);
        const type_expr = try self.parseTypeExpr();

        return .{
            .span = start.span.endAt(type_expr.span()),
            .identifier = identifier,
            .type_expr = type_expr,
        };
    }

    fn parseMaybeBinding(self: *Self) Error!?ast.BindingDecl {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const next = try self.peekToken();
        return switch (next.tag) {
            .kw_const, .kw_var => try self.parseBinding(),
            else => null,
        };
    }

    fn parseBinding(self: *Self) Error!ast.BindingDecl {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const constOrVar = try self.nextToken();
        switch (constOrVar.tag) {
            .kw_const, .kw_var => {},
            else => return self.failExpectedTokens(constOrVar.tag, &[_]token.Tag{ .kw_const, .kw_var }),
        }
        const pattern = try self.parseBindingPattern();
        const annotation = try self.parseMaybeTypeAnnotation();
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

    fn parseMaybeReturn(self: *Self) Error!?ast.ReturnStmt {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const next = try self.peekToken();
        return switch (next.tag) {
            .kw_return => try self.parseReturn(),
            else => null,
        };
    }

    fn parseReturn(self: *Self) Error!ast.ReturnStmt {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const start = try self.expectTokenTag(.kw_return);
        const next = try self.peekToken();

        if (isExprTerminator(next.tag)) {
            return .{
                .value = null,
                .span = start.span,
            };
        }

        const value = try self.parseExpression();

        return .{
            .value = value,
            .span = start.span.endAt(value.span()),
        };
    }

    fn parseMaybeTypeAnnotation(self: *Self) Error!?*const ast.TypeExpr {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const tok = try self.peekToken();
        switch (tok.tag) {
            .colon => {
                _ = try self.nextToken();
                return try self.parseTypeExpr();
            },
            else => {},
        }

        return null;
    }

    fn parseFn(self: *Self) Error!*ast.Expression {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const start = try self.expectTokenTag(.kw_fn);
        const stdinType = try self.parseMaybeTypeExpr();
        const identifier = try self.expectTokenTag(.identifier);
        _ = try self.expectTokenTag(.l_paren);
        const params = try self.parseList(.comma, parseParam, .{
            .terminators = .list(&.{.r_paren}),
        });
        _ = try self.expectTokenTag(.r_paren);
        const returnType = try self.parseMaybeTypeExpr();
        // TODO: lambdas
        // const block = try self.parseBlock();
        const body = try self.parseExpression();

        return self.allocExpression(.{ .fn_decl = .{
            .name = .{ .name = identifier.lexeme, .span = identifier.span },
            .params = .nonVariadic(params.payload),
            .stdin_type = stdinType,
            .return_type = returnType,
            .body = body,
            .span = start.span.endAt(body.span()),
        } });
    }

    fn parseParam(self: *Self) Error!*ast.Parameter {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        const pattern = try self.parseBindingPattern();
        const annotation = try self.parseMaybeTypeAnnotation();
        const default_value = try self.parseMaybeDefaultValue();

        const end = if (default_value) |e| e.span() else if (annotation) |e| e.span() else pattern.span();

        const param = try self.arena.allocator().create(ast.Parameter);
        param.* = .{
            .pattern = pattern,
            .type_annotation = annotation,
            .default_value = default_value,
            .is_mutable = false,
            .span = pattern.span().endAt(end),
        };

        return param;
    }

    fn parseMaybeDefaultValue(self: *Self) Error!?*ast.Expression {
        const peek = try self.peekToken();

        if (peek.tag == .assign) {
            _ = try self.nextToken();
            return try self.parseExpression();
        }

        return null;
    }

    fn parseMaybeTypeExpr(self: *Self) Error!?*const ast.TypeExpr {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
        defer breadcrumb.end();

        // TODO: implement

        const next = try self.peekToken();

        if (isTypeExprTerminator(next.tag)) return null;

        return switch (next.tag) {
            .identifier => self.parseIdentifierTypeExpr(),
            // .kw_enum => self.parseEnumTypeExpr(),
            // .kw_union => self.parseUnionTypeExpr(),
            // .kw_struct => self.parseStructTypeExpr(),
            // .kw_error => self.parseErrorTypeExpr(),
            .l_bracket => self.parseArrayTypeExpr(),
            // .caret => self.parsePromiseTypeExpr(),
            // .question => self.parseOptionalTypeExpr(),
            .l_paren => {
                _ = try self.nextToken();
                const type_expr = try self.parseTypeExpr();
                _ = try self.expectTokenTag(.r_paren);
                return type_expr;
            },
            else => null,
        };
    }

    fn parseTypeExpr(self: *Self) Error!*const ast.TypeExpr {
        const next = try self.peekToken();
        return try self.parseMaybeTypeExpr() orelse {
            try self.reportParseError(
                Error.ExpectedTypeExpr,
                next.span,
                "expected type, actual: {s}",
                .{next.lexeme},
            );

            return Error.ExpectedTypeExpr;
        };
    }

    fn parseTypeIdentifier(self: *Self) Error!ast.Identifier {
        const identifier = try self.expectTokenTag(.identifier);
        if (std.ascii.isLower(identifier.lexeme[0])) {
            const identifier_capitalized = try self.arena.allocator().dupe(
                u8,
                identifier.lexeme,
            );
            identifier_capitalized[0] = std.ascii.toUpper(identifier_capitalized[0]);
            try self.reportParseError(
                Error.ExpectedTypeIdentifier,
                identifier.span,
                "expected type identifier (did you mean {s}?)",
                .{identifier_capitalized},
            );
            return Error.ExpectedTypeIdentifier;
        }

        return .fromToken(identifier);
    }

    fn parseIdentifier(self: *Self) Error!ast.Identifier {
        const identifier = try self.expectTokenTag(.identifier);
        return .fromToken(identifier);
    }

    fn parseIdentifierTypeExpr(self: *Self) Error!*const ast.TypeExpr {
        const spanned_path: ast.Spanned([]ast.Identifier) = try self.parseList(
            .dot,
            parseIdentifier,
            .{},
        );
        const last_segment: ast.Identifier = brk: {
            if (spanned_path.payload.len > 0) {
                break :brk spanned_path.payload[spanned_path.payload.len - 1];
            }

            try self.reportParseError(
                Error.ExpectedTypeIdentifier,
                spanned_path.span,
                "expected type identifier",
                .{},
            );

            return Error.ExpectedTypeIdentifier;
        };

        if (std.ascii.isLower(last_segment.name[0])) {
            const identifier_capitalized = try self.arena.allocator().dupe(
                u8,
                last_segment.name,
            );
            identifier_capitalized[0] = std.ascii.toUpper(identifier_capitalized[0]);
            try self.reportParseError(
                Error.ExpectedTypeIdentifier,
                last_segment.span,
                "expected type identifier (did you mean {s}?)",
                .{identifier_capitalized},
            );

            return Error.ExpectedTypeIdentifier;
        }

        // TODO: implement generic function type expressions

        return self.allocTypeExpression(.{ .identifier = .{
            .path = .{ .segments = spanned_path.payload, .span = spanned_path.span },
            .span = spanned_path.span,
        } });
    }

    fn parseArrayTypeExpr(self: *Self) Error!*const ast.TypeExpr {
        const start = try self.expectTokenTag(.l_bracket);
        _ = try self.expectTokenTag(.r_bracket);

        const element = try self.parseTypeExpr();

        return try self.allocTypeExpression(.{
            .array = .{
                .element = element,
                .span = start.span.endAt(element.span()),
            },
        });
    }

    fn isTypeExprTerminator(tag: token.Tag) bool {
        return switch (tag) {
            .l_paren, .l_brace, .identifier, .star, .caret, .bang, .question, .kw_enum, .kw_error, .kw_union, .kw_struct => false,
            else => true,
        };
    }

    fn copyToArena(self: *Self, comptime T: type, values: []const T) Error![]T {
        if (values.len == 0) return &[0]T{};
        const buffer = try self.arena.allocator().alloc(T, values.len);
        std.mem.copyForwards(T, buffer, values);
        return buffer;
    }

    fn skipTypeExpression(self: *Self) Error!void {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
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
                .eof => return Error.UnexpectedEOF,
                else => {
                    _ = try self.nextToken();
                },
            }
        }
    }

    fn skipNestedStructure(self: *Self, comptime open: token.Tag) Error!void {
        const breadcrumb = try self.createBreadcrumb(@src().fn_name);
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
                .eof => return Error.UnexpectedEOF,
                open => depth += 1,
                close => depth -= 1,
                else => {},
            }
        }
    }

    /// Consumes the next token
    fn expectTokenTag(self: *Self, tag: token.Tag) Error!token.Token {
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
    ) Error!ast.TypeExpr.StructType {
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
                                return Error.DuplicateStructDecl;
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
                        return Error.DuplicateStructDecl;
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

    try std.testing.expectError(Error.UnexpectedToken, parser.parseExpression());

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

    try std.testing.expectError(Error.StringInterpNotAllowed, parser.parseSource(ctx.source));
}
