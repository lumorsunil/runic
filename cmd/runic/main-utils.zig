const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const ast = runic.ast;
const token_pkg = runic.token;
const lexer_pkg = runic.lexer;
const LexerError = lexer_pkg.Error;
const ManagedArrayList = std.array_list.Managed;

pub const CliConfig = struct {
    mode: Mode,
    trace_topics: [][]const u8,
    module_paths: [][]const u8,
    env_overrides: []EnvOverride,
    print_ast: bool = false,
    print_tokens: bool = false,
    type_check_only: bool = false,
    skip_type_check: bool = false,

    const Mode = union(enum) {
        script: ScriptInvocation,
        repl,
    };

    pub const ScriptInvocation = struct {
        path: []const u8,
        args: []const []const u8,
    };

    const EnvOverride = struct {
        key: []const u8,
        value: []const u8,
    };

    pub fn deinit(self: *CliConfig, allocator: Allocator) void {
        switch (self.mode) {
            .script => |script| {
                allocator.free(script.path);
                freeStringList(allocator, script.args);
            },
            .repl => {},
        }

        freeStringList(allocator, self.trace_topics);
        freeStringList(allocator, self.module_paths);

        freeEnvOverrides(allocator, self.env_overrides);
        self.* = undefined;
    }
};

const ParseResult = union(enum) {
    show_help,
    usage_error: []const u8,
    ready: CliConfig,
};

pub fn printScriptTokens(
    allocator: Allocator,
    stdout: *std.Io.Writer,
    script_path: []const u8,
    source: []const u8,
) (LexerError || std.Io.Writer.Error)!void {
    try stdout.print("Tokens {s}\n", .{script_path});
    var stream = try lexer_pkg.Stream.init(allocator, script_path, source);
    defer stream.deinit();
    var index: usize = 0;
    while (true) {
        const tok = try stream.next();
        try renderLexerToken(stdout, index, tok);
        index += 1;
        if (tok.tag == .eof) break;
    }
    try stdout.writeByte('\n');
}

fn renderLexerToken(writer: *std.Io.Writer, index: usize, tok: token_pkg.Token) !void {
    try writer.print("  {d}: {s} ", .{ index, @tagName(tok.tag) });
    try printTokenSpanInline(writer, tok.span);
    try writer.writeByte(' ');
    try writeEscapedLexeme(writer, tok.lexeme);
    try writer.writeByte('\n');
}

fn printTokenSpanInline(writer: *std.Io.Writer, span: token_pkg.Span) !void {
    try writer.print(
        "{d}:{d}-{d}:{d}",
        .{
            span.start.line,
            span.start.column,
            span.end.line,
            span.end.column,
        },
    );
}

pub fn writeEscapedLexeme(writer: *std.Io.Writer, lexeme: []const u8) !void {
    try writer.writeByte('"');
    for (lexeme) |byte| {
        switch (byte) {
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            '"' => try writer.writeAll("\\\""),
            else => {
                if (std.ascii.isPrint(byte)) {
                    try writer.writeByte(byte);
                } else {
                    const hex_digits = "0123456789ABCDEF";
                    try writer.writeAll("\\x");
                    try writer.writeByte(hex_digits[byte >> 4]);
                    try writer.writeByte(hex_digits[byte & 0x0F]);
                }
            },
        }
    }
    try writer.writeByte('"');
}

pub fn printScriptAst(
    stdout: *std.Io.Writer,
    script_path: []const u8,
    script: ast.Script,
) !void {
    try stdout.print("AST {s}\n", .{script_path});
    const block = ast.Block{
        .statements = script.statements,
        .span = script.span,
    };
    try renderBlockStatements(stdout, block, 1);
    try stdout.writeByte('\n');
}

const Colors = struct {
    pub const string = runic.rainbow.RainbowColor.yellow;
    pub const identifier = runic.rainbow.RainbowColor.blue;
};

const RenderExpressionError = std.Io.Writer.Error;

fn renderExpressionAst(writer: *std.Io.Writer, expr: *const ast.Expression, level: usize) RenderExpressionError!void {
    const node = expr.*;
    switch (node) {
        .identifier => |identifier| {
            try writer.writeAll(runic.rainbow.beginColor(Colors.identifier));
            defer writer.writeAll(runic.rainbow.endColor()) catch {};
            try writeIndent(writer, level);
            try writer.print("identifier {s} @ ", .{identifier.name});
            try printSpanInline(writer, identifier.span);
            try writer.writeByte('\n');
        },
        .literal => |literal| {
            try writeIndent(writer, level);
            try writer.print("literal @ ", .{});
            try printSpanInline(writer, literal.span());
            try writer.writeByte('\n');
            try renderLiteralAst(writer, literal, level + 1);
        },
        .block => |block_expr| {
            try writeIndent(writer, level);
            try writer.print("block @ ", .{});
            try printSpanInline(writer, block_expr.span);
            try writer.writeByte('\n');
            try renderBlockStatements(writer, block_expr, level + 1);
        },
        .if_expr => |if_expr| {
            try writeIndent(writer, level);
            try writer.print("if_expr @ ", .{});
            try printSpanInline(writer, if_expr.span);
            try writer.writeByte('\n');

            try writeIndent(writer, level + 1);
            try writer.print("condition:\n", .{});
            try renderExpressionAst(writer, if_expr.condition, level + 2);

            if (if_expr.capture) |capture| {
                try writeIndent(writer, level + 1);
                try writer.print("capture:\n", .{});
                try renderCaptureClauseAst(writer, capture, level + 2);
            }

            try writeIndent(writer, level + 1);
            try writer.print("then:\n", .{});
            try renderExpressionAst(writer, if_expr.then_expr, level + 2);

            if (if_expr.else_branch) |branch| {
                try writeIndent(writer, level + 1);
                try writer.print("else:\n", .{});
                switch (branch) {
                    .expr => |else_expr| try renderExpressionAst(writer, else_expr, level + 2),
                    .if_expr => |nested_if| try renderExpressionAst(writer, &.from(nested_if.*), level + 2),
                }
            }
        },
        .import_expr => |import_expr| {
            try writeIndent(writer, level);
            try writer.print("import @ ", .{});
            try printSpanInline(writer, import_expr.span);
            try writer.writeByte('\n');
            try writeIndent(writer, level + 1);
            try writer.print("from: {s}\n", .{import_expr.importer});
            try writeIndent(writer, level + 1);
            try writer.print("module: {s}", .{import_expr.module_name});
            try writer.writeByte('\n');
        },
        .member => |member_expr| {
            try writeIndent(writer, level);
            try writer.print("member @ ", .{});
            try printSpanInline(writer, member_expr.span);
            try writer.writeByte('\n');
            try renderExpressionAst(writer, member_expr.object, level + 1);
            try writeIndent(writer, level + 1);
            try writer.print(".{s}\n", .{member_expr.member.name});
        },
        .pipeline => |pipeline_expr| {
            try writeIndent(writer, level);
            try writer.print("pipeline @ ", .{});
            try printSpanInline(writer, pipeline_expr.span);
            try writer.writeByte('\n');
            for (pipeline_expr.stages) |stage| try renderPipelineStage(writer, stage, level + 1);
        },
        .binary => |binary| {
            try writeIndent(writer, level);
            try writer.writeAll("binary @ ");
            try printSpanInline(writer, binary.span);
            try writer.writeByte('\n');
            try writeIndent(writer, level + 1);
            try writer.print("op: {s}\n", .{@tagName(binary.op)});
            try writeIndent(writer, level + 1);
            try writer.writeAll("left: \n");
            try renderExpressionAst(writer, binary.left, level + 1);
            try writeIndent(writer, level + 1);
            try writer.writeAll("right: \n");
            try renderExpressionAst(writer, binary.right, level + 1);
        },
        .assignment => |assignment| {
            try writeIndent(writer, level);
            try writer.writeAll("assignment @ ");
            try printSpanInline(writer, assignment.span);
            try writer.writeByte('\n');
            try writeIndent(writer, level + 1);
            try writer.print("{s} =\n", .{assignment.identifier.name});
            try renderExpressionAst(writer, assignment.expr, level + 1);
        },
        else => {
            const tag_name = @tagName(std.meta.activeTag(node));
            try writeIndent(writer, level);
            try writer.print("{s} @ ", .{tag_name});
            try printSpanInline(writer, node.span());
            try writer.print(" (printing not implemented)\n", .{});
        },
    }
}

fn renderBlockStatements(writer: *std.Io.Writer, block: ast.Block, level: usize) !void {
    if (block.statements.len == 0) {
        try writeIndent(writer, level);
        try writer.print("(empty)\n", .{});
        return;
    }

    for (block.statements) |stmt| {
        try renderStatementAst(writer, stmt, level);
    }
}

fn renderStatementAst(writer: *std.Io.Writer, stmt: *const ast.Statement, level: usize) std.Io.Writer.Error!void {
    switch (stmt.*) {
        .expression => |expr_stmt| {
            try writeIndent(writer, level);
            try writer.print("expression_stmt @ ", .{});
            try printSpanInline(writer, expr_stmt.span);
            try writer.writeByte('\n');
            try renderExpressionAst(writer, expr_stmt.expression, level + 1);
        },
        .type_binding_decl => |type_binding_decl| {
            try writeIndent(writer, level);
            try writer.print("type_binding_decl @ ", .{});
            try printSpanInline(writer, type_binding_decl.span);
            try writer.writeByte('\n');
            try writeIndent(writer, level + 1);
            try writer.print("binding {s} @ ", .{type_binding_decl.identifier.name});
            try printSpanInline(writer, type_binding_decl.identifier.span);
            try writer.writeByte('\n');
            try renderTypeExpr(writer, type_binding_decl.type_expr, level + 1);
        },
        .binding_decl => |binding_decl| {
            try writeIndent(writer, level);
            try writer.print("binding_decl @ ", .{});
            try printSpanInline(writer, binding_decl.span);
            try writer.writeByte('\n');
            try writeIndent(writer, level + 1);
            try writer.print("is_mutable {}", .{binding_decl.is_mutable});
            try writer.writeByte('\n');
            try renderBindingPatternAst(writer, binding_decl.pattern, level + 1);
            try renderTypeExpr(writer, binding_decl.annotation, level + 1);
            try renderExpressionAst(writer, binding_decl.initializer, level + 1);
        },
        .fn_decl => |fn_decl| {
            try writeIndent(writer, level);
            try writer.print("fn_decl @ ", .{});
            try printSpanInline(writer, fn_decl.span);
            try writer.writeByte('\n');
            try writeIndent(writer, level + 1);
            try writer.writeAll(fn_decl.name.name);
            try writer.writeByte('\n');
            try writeIndent(writer, level + 1);
            switch (fn_decl.body) {
                .block => |block| {
                    try writer.print("# statements {}\n", .{block.statements.len});
                    try renderBlockStatements(writer, block, level + 1);
                },
                .expression => |expr| {
                    try writer.print("expression:\n", .{});
                    try renderExpressionAst(writer, expr, level + 1);
                },
            }
        },
        else => {
            const tag_name = @tagName(std.meta.activeTag(stmt.*));
            try writeIndent(writer, level);
            try writer.print("statement {s} @ ", .{tag_name});
            try printSpanInline(writer, stmt.span());
            try writer.print(" (printing not implemented)\n", .{});
        },
    }
}

fn renderCaptureClauseAst(writer: *std.Io.Writer, clause: ast.CaptureClause, level: usize) !void {
    try writeIndent(writer, level);
    try writer.print("capture @ ", .{});
    try printSpanInline(writer, clause.span);
    try writer.writeByte('\n');

    if (clause.bindings.len == 0) {
        try writeIndent(writer, level + 1);
        try writer.print("(no bindings)\n", .{});
        return;
    }

    for (clause.bindings) |binding| {
        try renderBindingPatternAst(writer, binding, level + 1);
    }
}

fn renderBindingPatternAst(
    writer: *std.Io.Writer,
    pattern: *const ast.BindingPattern,
    level: usize,
) !void {
    switch (pattern.*) {
        .identifier => |identifier| {
            try writeIndent(writer, level);
            try writer.print("binding {s} @ ", .{identifier.name});
            try printSpanInline(writer, identifier.span);
            try writer.writeByte('\n');
        },
        .discard => |span| {
            try writeIndent(writer, level);
            try writer.print("binding _ @ ", .{});
            try printSpanInline(writer, span);
            try writer.writeByte('\n');
        },
        .tuple => |tuple| {
            try writeIndent(writer, level);
            try writer.print("tuple_binding (has_rest={}) @ ", .{tuple.has_rest});
            try printSpanInline(writer, tuple.span);
            try writer.writeByte('\n');
            for (tuple.elements) |element| {
                try renderBindingPatternAst(writer, element, level + 1);
            }
        },
        .record => |record| {
            try writeIndent(writer, level);
            try writer.print("record_binding (has_rest={}) @ ", .{record.has_rest});
            try printSpanInline(writer, record.span);
            try writer.writeByte('\n');
            for (record.fields) |field| {
                try writeIndent(writer, level + 1);
                try writer.print("field {s} @ ", .{field.label.name});
                try printSpanInline(writer, field.span);
                try writer.writeByte('\n');
                if (field.binding) |binding| {
                    try renderBindingPatternAst(writer, binding, level + 2);
                }
            }
        },
    }
}

fn renderTypeExpr(
    writer: *std.Io.Writer,
    type_expr: ?*const ast.TypeExpr,
    level: usize,
) !void {
    try writeIndent(writer, level);
    try writer.print("type: {?f}\n", .{type_expr});
}

fn renderLiteralAst(writer: *std.Io.Writer, literal: ast.Literal, level: usize) !void {
    switch (literal) {
        .null => |payload| {
            try writeIndent(writer, level);
            try writer.print("null @ ", .{});
            try printSpanInline(writer, payload.span);
            try writer.writeByte('\n');
        },
        .bool => |payload| {
            try writeIndent(writer, level);
            try writer.print("bool {s} @ ", .{if (payload.value) "true" else "false"});
            try printSpanInline(writer, payload.span);
            try writer.writeByte('\n');
        },
        .integer => |payload| {
            try writeIndent(writer, level);
            try writer.print("integer \"{s}\" @ ", .{payload.text});
            try printSpanInline(writer, payload.span);
            try writer.writeByte('\n');
        },
        .float => |payload| {
            try writeIndent(writer, level);
            try writer.print("float \"{s}\" @ ", .{payload.text});
            try printSpanInline(writer, payload.span);
            try writer.writeByte('\n');
        },
        .string => |payload| {
            try writer.writeAll(runic.rainbow.beginColor(Colors.string));
            defer writer.writeAll(runic.rainbow.endColor()) catch {};
            try writeIndent(writer, level);
            try writer.print("string @ ", .{});
            try printSpanInline(writer, payload.span);
            try writer.writeByte('\n');
            try renderStringLiteralAst(writer, payload, level + 1);
        },
    }
}

fn renderStringLiteralAst(
    writer: *std.Io.Writer,
    literal: ast.StringLiteral,
    level: usize,
) !void {
    if (literal.segments.len == 0) {
        try writeIndent(writer, level);
        try writer.print("(empty string)\n", .{});
        return;
    }

    for (literal.segments, 0..) |segment, idx| {
        switch (segment) {
            .text => |text| {
                try writeIndent(writer, level);
                try writer.print("segment[{d}] text \"{s}\"\n", .{ idx, text.payload });
            },
            .interpolation => |expr| {
                try writeIndent(writer, level);
                try writer.print("segment[{d}] interpolation:\n", .{idx});
                try renderExpressionAst(writer, expr, level + 1);
            },
        }
    }
}

fn renderPipelineStage(writer: *std.Io.Writer, stage: ast.PipelineStage, level: usize) !void {
    try writeIndent(writer, level);
    try writer.print("[{}]\n", .{stage.role});
    switch (stage.payload) {
        .expression => |expr| try renderExpressionAst(writer, expr, level),
        .command => |c| try renderCommand(writer, c, level),
    }
}

fn renderCommand(writer: *std.Io.Writer, command: ast.CommandInvocation, level: usize) !void {
    try renderCommandPart(writer, command.name, level);
    try writer.writeByte('\n');
    for (command.args, 0..) |arg, i| {
        try writeIndent(writer, level + 1);
        try writer.print("arg{d}:\n", .{i});
        try renderCommandPart(writer, arg, level + 1);
    }
}

fn renderCommandPart(writer: *std.Io.Writer, commandPart: ast.CommandPart, level: usize) !void {
    switch (commandPart) {
        .string => |s| try renderStringLiteralAst(writer, s, level),
        .expr => |expr| try renderExpressionAst(writer, expr, level),
        .word => |word| {
            try writeIndent(writer, level);
            try writer.writeAll(word.text);
        },
    }
}

fn writeIndent(writer: *std.Io.Writer, level: usize) !void {
    var count: usize = 0;
    const spaces = level * 2;
    while (count < spaces) : (count += 1) {
        try writer.writeByte(' ');
    }
}

fn printSpanInline(writer: *std.Io.Writer, span: ast.Span) !void {
    try writer.print(
        "{d}:{d}-{d}:{d}",
        .{
            span.start.line,
            span.start.column,
            span.end.line,
            span.end.column,
        },
    );
}

pub fn applyEnvOverridesToMap(env_map: *std.process.EnvMap, overrides: []const CliConfig.EnvOverride) !void {
    for (overrides) |override| {
        try env_map.put(override.key, override.value);
    }
}

pub fn exposeScriptMetadata(env_map: *std.process.EnvMap, script: CliConfig.ScriptInvocation) !void {
    try env_map.put("RUNIC_SCRIPT_PATH", script.path);

    var count_buf: [32]u8 = undefined;
    const count_text = std.fmt.bufPrint(&count_buf, "{d}", .{script.args.len}) catch unreachable;
    try env_map.put("RUNIC_ARGC", count_text);

    for (script.args, 0..) |arg, idx| {
        var key_buf: [32]u8 = undefined;
        const key = std.fmt.bufPrint(&key_buf, "RUNIC_ARG_{d}", .{idx}) catch unreachable;
        try env_map.put(key, arg);
    }
}

pub fn computeScriptDirectory(allocator: Allocator, script_path: []const u8) ![]const u8 {
    var sep_index: ?usize = std.mem.lastIndexOfScalar(u8, script_path, '/');
    if (std.mem.lastIndexOfScalar(u8, script_path, '\\')) |backslash| {
        if (sep_index) |existing| {
            if (backslash > existing) sep_index = backslash;
        } else {
            sep_index = backslash;
        }
    }

    if (sep_index) |idx| {
        if (idx == 0) return allocator.dupe(u8, "/");
        return allocator.dupe(u8, script_path[0..idx]);
    }

    return allocator.dupe(u8, ".");
}

pub fn parseCommandLine(allocator: Allocator, argv: []const []const u8) !ParseResult {
    const trace_prefix = "--trace=";
    const module_prefix = "--module-path=";
    const env_prefix = "--env=";

    var trace_topics = ManagedArrayList([]const u8).init(allocator);
    var trace_cleanup = true;
    defer if (trace_cleanup) trace_topics.deinit();

    var module_paths = ManagedArrayList([]const u8).init(allocator);
    var module_cleanup = true;
    defer if (module_cleanup) module_paths.deinit();

    var env_overrides = ManagedArrayList(CliConfig.EnvOverride).init(allocator);
    var env_cleanup = true;
    defer if (env_cleanup) env_overrides.deinit();

    var script_args = ManagedArrayList([]const u8).init(allocator);
    var args_cleanup = true;
    defer if (args_cleanup) script_args.deinit();

    var script_path: ?[]const u8 = null;
    var repl_requested = false;
    var print_ast = false;
    var print_tokens = false;
    var type_check_only = false;
    var skip_type_check = false;
    var parsing_options = true;
    var idx: usize = 1;

    while (idx < argv.len) {
        const arg = argv[idx];
        idx += 1;

        if (parsing_options and std.mem.eql(u8, arg, "--")) {
            parsing_options = false;
            continue;
        }

        if (parsing_options and arg.len > 0 and arg[0] == '-') {
            if (argEqual(arg, "--help") or argEqual(arg, "-h")) {
                return ParseResult.show_help;
            }
            if (argEqual(arg, "--repl")) {
                repl_requested = true;
                continue;
            }
            if (argEqual(arg, "--print-ast")) {
                print_ast = true;
                continue;
            }
            if (argEqual(arg, "--print-tokens")) {
                print_tokens = true;
                continue;
            }
            if (argEqual(arg, "--type-check-only")) {
                type_check_only = true;
                continue;
            }
            if (argEqual(arg, "--skip-type-check")) {
                skip_type_check = true;
                continue;
            }
            if (std.mem.startsWith(u8, arg, trace_prefix)) {
                const value = arg[trace_prefix.len..];
                if (value.len == 0) return usageError(allocator, "--trace requires a topic name", .{});
                try trace_topics.append(try allocator.dupe(u8, value));
                continue;
            }
            if (argEqual(arg, "--trace")) {
                if (idx >= argv.len) return usageError(allocator, "--trace requires a topic name", .{});
                const value = argv[idx];
                idx += 1;
                if (value.len == 0) return usageError(allocator, "--trace requires a topic name", .{});
                try trace_topics.append(try allocator.dupe(u8, value));
                continue;
            }
            if (std.mem.startsWith(u8, arg, module_prefix)) {
                const value = arg[module_prefix.len..];
                if (value.len == 0) return usageError(allocator, "--module-path requires a directory", .{});
                try module_paths.append(try allocator.dupe(u8, value));
                continue;
            }
            if (argEqual(arg, "--module-path")) {
                if (idx >= argv.len) return usageError(allocator, "--module-path requires a directory", .{});
                const value = argv[idx];
                idx += 1;
                if (value.len == 0) return usageError(allocator, "--module-path requires a directory", .{});
                try module_paths.append(try allocator.dupe(u8, value));
                continue;
            }
            if (std.mem.startsWith(u8, arg, env_prefix)) {
                const raw = arg[env_prefix.len..];
                if (raw.len == 0) return usageError(allocator, "--env requires KEY=VALUE", .{});
                const override = parseEnvOverride(allocator, raw) catch {
                    return usageError(allocator, "Environment overrides must look like KEY=VALUE (got '{s}')", .{raw});
                };
                try env_overrides.append(override);
                continue;
            }
            if (argEqual(arg, "--env")) {
                if (idx >= argv.len) return usageError(allocator, "--env requires KEY=VALUE", .{});
                const raw = argv[idx];
                idx += 1;
                const override = parseEnvOverride(allocator, raw) catch {
                    return usageError(allocator, "Environment overrides must look like KEY=VALUE (got '{s}')", .{raw});
                };
                try env_overrides.append(override);
                continue;
            }

            return usageError(allocator, "Unknown option '{s}'", .{arg});
        }

        if (!parsing_options and script_path != null and argEqual(arg, "--") and script_args.items.len == 0) {
            continue;
        }

        if (script_path == null) {
            script_path = try allocator.dupe(u8, arg);
            parsing_options = false;
            continue;
        }

        try script_args.append(try allocator.dupe(u8, arg));
    }

    if (script_path != null and repl_requested) {
        return usageError(allocator, "Cannot combine --repl with a script path.", .{});
    }
    if (script_path == null and !repl_requested) {
        return usageError(allocator, "Provide a Runic script path or pass --repl for interactive mode.", .{});
    }
    if (print_ast and repl_requested) {
        return usageError(allocator, "--print-ast requires a script path.", .{});
    }
    if (print_ast and script_path == null) {
        return usageError(allocator, "--print-ast requires a script path.", .{});
    }
    if (print_tokens and repl_requested) {
        return usageError(allocator, "--print-tokens requires a script path.", .{});
    }
    if (print_tokens and script_path == null) {
        return usageError(allocator, "--print-tokens requires a script path.", .{});
    }
    if (type_check_only and repl_requested) {
        return usageError(allocator, "--type-check-only requires a script path.", .{});
    }
    if (type_check_only and script_path == null) {
        return usageError(allocator, "--type-check-only requires a script path.", .{});
    }
    if (skip_type_check and repl_requested) {
        return usageError(allocator, "--skip-type-check requires a script path.", .{});
    }
    if (skip_type_check and script_path == null) {
        return usageError(allocator, "--skip-type-check requires a script path.", .{});
    }

    const trace_slice = try finalizeList([]const u8, &trace_topics, &trace_cleanup);
    const module_slice = try finalizeList([]const u8, &module_paths, &module_cleanup);
    const env_slice = try finalizeList(CliConfig.EnvOverride, &env_overrides, &env_cleanup);
    const args_slice = try finalizeList([]const u8, &script_args, &args_cleanup);

    if (script_path) |path| {
        return ParseResult{
            .ready = .{
                .mode = .{ .script = .{ .path = path, .args = args_slice } },
                .trace_topics = trace_slice,
                .module_paths = module_slice,
                .env_overrides = env_slice,
                .print_ast = print_ast,
                .print_tokens = print_tokens,
                .type_check_only = type_check_only,
                .skip_type_check = skip_type_check,
            },
        };
    }

    freeStringList(allocator, args_slice);
    return ParseResult{
        .ready = .{
            .mode = .repl,
            .trace_topics = trace_slice,
            .module_paths = module_slice,
            .env_overrides = env_slice,
            .print_ast = print_ast,
            .print_tokens = print_tokens,
            .type_check_only = type_check_only,
            .skip_type_check = skip_type_check,
        },
    };
}

pub fn printUsage(writer: *std.Io.Writer) !void {
    try writer.print(
        \\Runic CLI
        \\Usage:
        \\  runic [options] path/to/script.rn [-- <script args>...]
        \\  runic --repl
        \\
        \\Options:
        \\  --help, -h           Display this help text.
        \\  --repl               Start the interactive REPL (history + multiline editing).
        \\  --trace <topic>      Enable detailed tracing of interpreter subsystems.
        \\  --module-path <dir>  Append an additional module lookup directory.
        \\  --env KEY=VALUE      Override an environment variable during execution.
        \\  --print-ast          Parse the script and emit its AST instead of executing.
        \\  --print-tokens       Dump the raw lexer tokens for the provided script path.
        \\  --type-check-only    Dry run with only type checking.
        \\
        \\Additional arguments after -- are forwarded to the script unchanged.
        \\Scripts honor the same tracing, module-path, and env override flags as the REPL.
        \\
    , .{});
    try writer.flush();
}

fn argEqual(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

fn parseEnvOverride(allocator: Allocator, raw: []const u8) !CliConfig.EnvOverride {
    const eq_index = std.mem.indexOfScalar(u8, raw, '=') orelse return error.InvalidEnvOverride;
    if (eq_index == 0) return error.InvalidEnvOverride;

    const key = try allocator.dupe(u8, raw[0..eq_index]);
    errdefer allocator.free(key);

    const value = try allocator.dupe(u8, raw[eq_index + 1 ..]);
    return .{ .key = key, .value = value };
}

fn usageError(allocator: Allocator, comptime message_fmt: []const u8, args: anytype) !ParseResult {
    const message = try std.fmt.allocPrint(allocator, message_fmt, args);
    return ParseResult{ .usage_error = message };
}

fn finalizeList(comptime T: type, list: *ManagedArrayList(T), cleanup_flag: *bool) ![]T {
    if (list.items.len == 0) {
        return &[_]T{};
    }
    const owned = try list.toOwnedSlice();
    cleanup_flag.* = false;
    return owned;
}

fn printStringList(writer: *std.Io.Writer, label: []const u8, values: [][]const u8) !void {
    if (values.len == 0) {
        try writer.print("{s}: (none)\n", .{label});
        try writer.flush();
        return;
    }
    try writer.print("{s}: ", .{label});
    for (values, 0..) |value, idx| {
        if (idx > 0) try writer.print(", ", .{});
        try writer.print("{s}", .{value});
    }
    try writer.writeByte('\n');
    try writer.flush();
}

fn printEnvOverrides(writer: *std.Io.Writer, overrides: []const CliConfig.EnvOverride) !void {
    if (overrides.len == 0) {
        try writer.print("Env overrides: (none)\n", .{});
        try writer.flush();
        return;
    }
    try writer.print("Env overrides:\n", .{});
    for (overrides) |override| {
        try writer.print("  {s}={s}\n", .{ override.key, override.value });
    }
    try writer.flush();
}

fn freeStringList(allocator: Allocator, values: []const []const u8) void {
    if (values.len == 0) return;
    for (values) |value| allocator.free(value);
    allocator.free(values);
}

fn freeEnvOverrides(allocator: Allocator, overrides: []CliConfig.EnvOverride) void {
    if (overrides.len == 0) return;
    for (overrides) |override| {
        allocator.free(override.key);
        allocator.free(override.value);
    }
    allocator.free(overrides);
}

pub fn ensureExecutableDirOnPath(allocator: Allocator, env_map: *std.process.EnvMap) !void {
    const exe_path = std.fs.selfExePathAlloc(allocator) catch return;
    defer allocator.free(exe_path);

    const exe_dir = computeScriptDirectory(allocator, exe_path) catch return;
    defer allocator.free(exe_dir);
    if (exe_dir.len == 0) return;

    const existing = env_map.get("PATH") orelse "";
    if (existing.len > 0 and pathHasEntry(existing, exe_dir)) return;

    var new_path: []u8 = undefined;
    if (existing.len == 0) {
        new_path = try allocator.dupe(u8, exe_dir);
    } else {
        new_path = try std.fmt.allocPrint(allocator, "{s}{c}{s}", .{
            exe_dir,
            std.fs.path.delimiter,
            existing,
        });
    }
    defer allocator.free(new_path);
    try env_map.put("PATH", new_path);
}

fn pathHasEntry(haystack: []const u8, needle: []const u8) bool {
    if (needle.len == 0) return false;
    var iterator = std.mem.splitScalar(u8, haystack, std.fs.path.delimiter);
    while (iterator.next()) |segment| {
        if (segment.len == 0) continue;
        if (std.mem.eql(u8, segment, needle)) return true;
    }
    return false;
}
