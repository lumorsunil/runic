// This file is deprecated, currently being replaced by src/frontend/parser.zig

const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

pub const SourceRange = struct {
    start: usize,
    end: usize,
};

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

/// Deprecated
pub const ModuleParser = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    stream: lexer.Stream,
    source: []const u8,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) ModuleParser {
        return .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .stream = lexer.Stream.init(allocator, source),
            .source = source,
        };
    }

    pub fn deinit(self: *ModuleParser) void {
        self.arena.deinit();
    }

    pub fn parseModuleDocument(self: *ModuleParser) (ModuleParseError || std.mem.Allocator.Error)!ModuleDocument {
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
                .kw_const => {
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

    fn copyModuleSlice(self: *ModuleParser, comptime T: type, values: []const T) std.mem.Allocator.Error![]const T {
        if (values.len == 0) return &[0]T{};
        const buffer = try self.arena.allocator().alloc(T, values.len);
        std.mem.copyForwards(T, buffer, values);
        return buffer;
    }

    fn parseMaybeAsyncFunction(self: *ModuleParser) ModuleParseError!?ModuleDeclaration {
        _ = try self.nextToken();
        try self.skipModuleSeparators();
        const next = try self.peekToken();
        if (next.tag != .kw_fn) return null;
        return try self.parseModuleFunctionDecl(true);
    }

    fn parseModuleFunctionDecl(self: *ModuleParser, is_async: bool) ModuleParseError!ModuleDeclaration {
        _ = try self.expectTokenTagModule(.kw_fn, ModuleParseError.InvalidSyntax);
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

    fn parseModuleParamList(self: *ModuleParser) (ModuleParseError || std.mem.Allocator.Error)![]const ModuleParam {
        _ = try self.expectTokenTagModule(.l_paren, ModuleParseError.MissingParameterList);

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

    fn parseModuleParam(self: *ModuleParser) ModuleParseError!ModuleParam {
        try self.skipParamWhitespace();
        const tok = try self.peekToken();
        if (tok.tag == .kw_var) {
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

    fn skipParameterTail(self: *ModuleParser) ModuleParseError!void {
        var tok = try self.peekToken();
        if (tok.tag == .colon) {
            _ = try self.nextToken();
            try self.skipTypeExpression_module();
            try self.skipParamWhitespace();
            tok = try self.peekToken();
        }

        if (tok.tag == .assign) {
            _ = try self.nextToken();
            try self.skipInitializerExpression();
        }
    }

    fn skipTypeExpression_module(self: *ModuleParser) ModuleParseError!void {
        while (true) {
            try self.skipParamWhitespace();
            const tok = try self.peekToken();
            switch (tok.tag) {
                .comma, .r_paren, .assign => return,
                inline .l_paren, .l_brace, .l_bracket => |tag| {
                    _ = try self.nextToken();
                    try self.skipNestedStructure_module(tag, ModuleParseError.UnterminatedParameterList);
                    continue;
                },
                .eof => return ModuleParseError.UnterminatedParameterList,
                else => {
                    _ = try self.nextToken();
                },
            }
        }
    }

    fn skipInitializerExpression(self: *ModuleParser) ModuleParseError!void {
        while (true) {
            try self.skipParamWhitespace();
            const tok = try self.peekToken();
            switch (tok.tag) {
                .comma, .r_paren => return,
                inline .l_paren, .l_brace, .l_bracket => |tag| {
                    _ = try self.nextToken();
                    try self.skipNestedStructure_module(tag, ModuleParseError.UnterminatedParameterList);
                    continue;
                },
                .eof => return ModuleParseError.UnterminatedParameterList,
                else => {
                    _ = try self.nextToken();
                },
            }
        }
    }

    fn skipNestedStructure_module(self: *ModuleParser, comptime open: token.Tag, err: ModuleParseError) ModuleParseError!void {
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

    fn parseOptionalReturnTypeSpan(self: *ModuleParser) ModuleParseError!?token.Span {
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
        self: *ModuleParser,
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

    fn parseModuleValueDecl(self: *ModuleParser) ModuleParseError!ModuleDeclaration {
        _ = try self.expectTokenTagModule(.kw_const, ModuleParseError.InvalidSyntax);
        try self.skipModuleSeparators();
        const name_tok = try self.expectIdentifierWithError(ModuleParseError.MissingValueName);
        try self.skipModuleSeparators();
        const assign_tok = try self.expectTokenTagModule(.assign, ModuleParseError.MissingValueInitializer);
        const range = try self.captureInitializerRange(assign_tok.span.end.offset);

        const storage = try self.arena.allocator().create(ModuleValue);
        storage.* = .{
            .name = name_tok.lexeme,
            .name_span = name_tok.span,
            .initializer_range = range,
        };
        return .{ .value = storage };
    }

    fn parseManifestDecl(self: *ModuleParser) ModuleParseError!ModuleDeclaration {
        _ = try self.nextToken();
        const body_range = try self.parseBlockRange(
            ModuleParseError.MissingManifestBody,
            ModuleParseError.UnterminatedManifestBody,
        );
        const storage = try self.arena.allocator().create(ModuleManifest);
        storage.* = .{ .body_range = body_range };
        return .{ .manifest = storage };
    }

    fn captureInitializerRange(self: *ModuleParser, start: usize) ModuleParseError!SourceRange {
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

    fn skipModuleSeparators(self: *ModuleParser) ModuleParseError!void {
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

    fn skipParamWhitespace(self: *ModuleParser) ModuleParseError!void {
        while (true) {
            const tok = try self.peekToken();
            if (tok.tag == .newline) {
                _ = try self.nextToken();
                continue;
            }
            break;
        }
    }

    fn nextToken(self: *ModuleParser) ModuleParseError!token.Token {
        return self.stream.next() catch |err| switch (err) {
            else => ModuleParseError.InvalidSyntax,
        };
    }

    fn peekToken(self: *ModuleParser) ModuleParseError!token.Token {
        return self.stream.peek() catch |err| switch (err) {
            else => ModuleParseError.InvalidSyntax,
        };
    }

    fn expectIdentifierWithError(self: *ModuleParser, err: ModuleParseError) ModuleParseError!token.Token {
        const tok = try self.nextToken();
        if (tok.tag != .identifier) return err;
        return tok;
    }

    fn expectTokenTagModule(self: *ModuleParser, tag: token.Tag, err: ModuleParseError) ModuleParseError!token.Token {
        const tok = try self.nextToken();
        if (tok.tag != tag) return err;
        return tok;
    }

    pub fn sliceForSpan(self: *ModuleParser, span: token.Span) []const u8 {
        return self.source[span.start.offset..span.end.offset];
    }

    pub fn sliceForRange(self: *ModuleParser, range: SourceRange) []const u8 {
        return self.source[range.start..range.end];
    }
};
