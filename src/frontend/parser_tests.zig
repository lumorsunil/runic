const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
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
            .name = "import_binding",
            .source = "let http = import(\"net/http\")\n",
            .parser = parseImport,
        },
        .{
            .name = "import_missing_path",
            .source = "let http = import(\n",
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

test "parser builds AST for optional-aware if expression" {
    const source =
        \\if (maybe_value) |value| {
        \\  value
        \\} else {
        \\  null
        \\}
    ;

    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const expr = try pr.parseExpression();
    try pr.expectEnd();

    try std.testing.expect(expr.* == .if_expr);
    const if_expr = expr.if_expr;
    const condition = switch (if_expr.condition.*) {
        .identifier => |id| id,
        else => return error.UnexpectedConditionShape,
    };
    try std.testing.expectEqualStrings("maybe_value", condition.name);

    try std.testing.expect(if_expr.capture != null);
    const capture = if_expr.capture.?;
    try std.testing.expectEqual(@as(usize, 1), capture.bindings.len);
    const binding = capture.bindings[0];
    switch (binding.*) {
        .identifier => |id| try std.testing.expectEqualStrings("value", id.name),
        else => return error.UnexpectedBindingPattern,
    }

    try std.testing.expectEqual(@as(usize, 1), if_expr.then_block.statements.len);
    const then_stmt = if_expr.then_block.statements[0];
    try std.testing.expect(then_stmt.* == .expression);
    try std.testing.expect(then_stmt.expression.expression.* == .identifier);

    const else_branch = if_expr.else_branch orelse return error.MissingElseBranch;
    switch (else_branch) {
        .block => |block| {
            try std.testing.expectEqual(@as(usize, 1), block.statements.len);
            const stmt = block.statements[0];
            try std.testing.expect(stmt.* == .expression);
            try std.testing.expect(stmt.expression.expression.* == .literal);
            try std.testing.expect(stmt.expression.expression.literal == .null);
        },
        else => return error.UnexpectedElseShape,
    }
}

test "parser splits string literals into text and interpolation segments" {
    const source = "\"prefix ${value.name} suffix\"";
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const expr = try pr.parseExpression();
    try pr.expectEnd();
    try std.testing.expect(expr.* == .literal);
    const literal = expr.literal.string;
    try std.testing.expectEqual(@as(usize, 3), literal.segments.len);

    switch (literal.segments[0]) {
        .text => |text| try std.testing.expectEqualStrings("prefix ", text),
        else => return error.MissingLeadingText,
    }

    const interpolation = literal.segments[1];
    switch (interpolation) {
        .interpolation => |expr_ptr| {
            try std.testing.expect(expr_ptr.* == .member);
            const member = expr_ptr.member;
            try std.testing.expectEqualStrings("name", member.member.name);
            try std.testing.expect(member.object.* == .identifier);
            try std.testing.expectEqualStrings("value", member.object.identifier.name);
        },
        else => return error.MissingInterpolationSegment,
    }

    switch (literal.segments[2]) {
        .text => |text| try std.testing.expectEqualStrings(" suffix", text),
        else => return error.MissingTrailingText,
    }
}

test "parser builds pipeline with command and expression stages" {
    const source = "ls ./src | lines()\n";
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const expr = try pr.parseExpression();
    try pr.expectEnd();
    try std.testing.expect(expr.* == .pipeline);
    const pipeline = expr.pipeline;
    try std.testing.expectEqual(@as(usize, 2), pipeline.stages.len);

    const first = pipeline.stages[0];
    try std.testing.expectEqual(ast.StageRole.command, first.role);
    const command = first.payload.command;
    try std.testing.expectEqualStrings("ls", command.name.word.text);
    try std.testing.expectEqual(@as(usize, 1), command.args.len);
    switch (command.args[0]) {
        .word => |word| try std.testing.expectEqualStrings("./src", word.text),
        else => return error.UnexpectedArgumentShape,
    }

    const second = pipeline.stages[1];
    try std.testing.expectEqual(ast.StageRole.expression, second.role);
    const stage_expr = second.payload.expression;
    try std.testing.expect(stage_expr.* == .call);
    const call = stage_expr.call;
    try std.testing.expect(call.callee.* == .identifier);
    try std.testing.expectEqualStrings("lines", call.callee.identifier.name);
    try std.testing.expectEqual(@as(usize, 0), call.arguments.len);
}

test "parser captures command env assignments interpolations redirects capture and background" {
    const source =
        \\FOO=config python api.py ${result.stdout} capture = { stdout: :stream, stderr: :buffer } 1>stdout_handle 2>>logs &
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const expr = try pr.parseExpression();
    try pr.expectEnd();
    try std.testing.expect(expr.* == .pipeline);
    const pipeline = expr.pipeline;
    try std.testing.expectEqual(@as(usize, 1), pipeline.stages.len);
    const stage = pipeline.stages[0];
    try std.testing.expectEqual(ast.StageRole.command, stage.role);
    const command = stage.payload.command;
    try std.testing.expect(command.background);
    try std.testing.expectEqual(@as(usize, 1), command.env_assignments.len);
    const env_assign = command.env_assignments[0];
    try std.testing.expectEqualStrings("FOO", env_assign.name.name);
    try std.testing.expect(env_assign.value.* == .identifier);
    try std.testing.expectEqualStrings("config", env_assign.value.identifier.name);

    try std.testing.expectEqualStrings("python", command.name.word.text);
    try std.testing.expectEqual(@as(usize, 2), command.args.len);
    switch (command.args[0]) {
        .word => |word| try std.testing.expectEqualStrings("api.py", word.text),
        else => return error.UnexpectedArgumentShape,
    }
    switch (command.args[1]) {
        .expr => |expr_ptr| {
            try std.testing.expect(expr_ptr.* == .member);
            const member = expr_ptr.member;
            try std.testing.expectEqualStrings("stdout", member.member.name);
            try std.testing.expect(member.object.* == .identifier);
            try std.testing.expectEqualStrings("result", member.object.identifier.name);
        },
        else => return error.MissingExpressionArgument,
    }

    const capture = command.capture orelse return error.MissingCaptureDirective;
    try std.testing.expectEqual(@as(usize, 2), capture.entries.len);
    const stdout_entry = capture.entries[0];
    try std.testing.expectEqual(ast.CaptureStream.stdout, stdout_entry.stream);
    try std.testing.expectEqual(ast.CaptureMode.stream, stdout_entry.mode);
    const stderr_entry = capture.entries[1];
    try std.testing.expectEqual(ast.CaptureStream.stderr, stderr_entry.stream);
    try std.testing.expectEqual(ast.CaptureMode.buffer, stderr_entry.mode);

    try std.testing.expectEqual(@as(usize, 2), command.redirects.len);
    const stdout_redirect = command.redirects[0];
    try std.testing.expectEqual(ast.StreamRef.stdout, stdout_redirect.stream);
    try std.testing.expectEqual(ast.RedirectionMode.truncate, stdout_redirect.mode);
    switch (stdout_redirect.target) {
        .variable => |ident| try std.testing.expectEqualStrings("stdout_handle", ident.name),
        else => return error.UnexpectedRedirectTarget,
    }
    const stderr_redirect = command.redirects[1];
    try std.testing.expectEqual(ast.StreamRef.stderr, stderr_redirect.stream);
    try std.testing.expectEqual(ast.RedirectionMode.append, stderr_redirect.mode);
    switch (stderr_redirect.target) {
        .variable => |ident| try std.testing.expectEqualStrings("logs", ident.name),
        else => return error.UnexpectedRedirectTarget,
    }
}

test "parser respects operator precedence for arithmetic expressions" {
    const source = "1 + 2 * 3";
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const expr = try pr.parseExpression();
    try pr.expectEnd();

    try std.testing.expect(expr.* == .binary);
    const add = expr.binary;
    try std.testing.expectEqual(ast.BinaryOp.add, add.op);
    try std.testing.expect(add.left.* == .literal);
    try std.testing.expectEqualStrings("1", add.left.literal.integer.text);
    try std.testing.expect(add.right.* == .binary);
    const mul = add.right.binary;
    try std.testing.expectEqual(ast.BinaryOp.multiply, mul.op);
    try std.testing.expectEqualStrings("2", mul.left.literal.integer.text);
    try std.testing.expectEqualStrings("3", mul.right.literal.integer.text);
}

test "parser builds function literal with parameters and expression body" {
    const source = "fn (value: Int = 1) => value + 4";
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const expr = try pr.parseExpression();
    try pr.expectEnd();

    try std.testing.expect(expr.* == .fn_literal);
    const fn_lit = expr.fn_literal;
    try std.testing.expectEqual(@as(usize, 1), fn_lit.params.len);
    const param = fn_lit.params[0];
    try std.testing.expectEqualStrings("value", param.name.name);
    try std.testing.expect(param.type_annotation != null);
    try std.testing.expect(param.default_value != null);
    const body_expr = switch (fn_lit.body) {
        .expression => |inner| inner,
        else => return error.ExpectedExpressionBody,
    };
    try std.testing.expect(body_expr.* == .binary);
    const add = body_expr.binary;
    try std.testing.expectEqual(ast.BinaryOp.add, add.op);
    try std.testing.expect(add.left.* == .identifier);
    try std.testing.expectEqualStrings("value", add.left.identifier.name);
    try std.testing.expect(add.right.* == .literal);
    try std.testing.expectEqualStrings("4", add.right.literal.integer.text);
}

test "script parser builds AST for let pipeline and expression statements" {
    const source =
        \\let greeting = "hello"
        \\print greeting | upper()
        \\greeting
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const script = try pr.parseScript();
    try std.testing.expectEqual(@as(usize, 3), script.statements.len);
    try std.testing.expectEqual(@as(usize, 0), script.span.start.offset);
    try std.testing.expect(script.span.end.offset > script.span.start.offset);

    const let_stmt = script.statements[0];
    try std.testing.expect(let_stmt.* == .let_decl);
    const let_decl = let_stmt.let_decl;
    try std.testing.expect(!let_decl.is_mutable);
    try std.testing.expect(let_decl.annotation == null);
    try std.testing.expect(let_decl.pattern.* == .identifier);
    try std.testing.expectEqualStrings("greeting", let_decl.pattern.identifier.name);
    try std.testing.expect(let_decl.initializer.* == .literal);
    const literal = let_decl.initializer.literal;
    const string_literal = switch (literal) {
        .string => |str| str,
        else => return error.ExpectedStringInitializer,
    };
    try std.testing.expectEqual(@as(usize, 1), string_literal.segments.len);
    switch (string_literal.segments[0]) {
        .text => |text| try std.testing.expectEqualStrings("hello", text),
        else => return error.ExpectedGreetingLiteral,
    }

    const pipeline_stmt = script.statements[1];
    try std.testing.expect(pipeline_stmt.* == .expression);
    const pipeline_expr_ptr = pipeline_stmt.expression.expression;
    try std.testing.expect(pipeline_expr_ptr.* == .pipeline);
    const pipeline = pipeline_expr_ptr.pipeline;
    try std.testing.expectEqual(@as(usize, 2), pipeline.stages.len);

    const first_stage = pipeline.stages[0];
    try std.testing.expectEqual(ast.StageRole.command, first_stage.role);
    const command = first_stage.payload.command;
    const command_name = switch (command.name) {
        .word => |word| word,
        else => return error.ExpectedCommandName,
    };
    try std.testing.expectEqualStrings("print", command_name.text);
    try std.testing.expectEqual(@as(usize, 1), command.args.len);
    switch (command.args[0]) {
        .word => |word| try std.testing.expectEqualStrings("greeting", word.text),
        else => return error.ExpectedPipelineArgument,
    }

    const second_stage = pipeline.stages[1];
    try std.testing.expectEqual(ast.StageRole.expression, second_stage.role);
    const call_expr_ptr = second_stage.payload.expression;
    try std.testing.expect(call_expr_ptr.* == .call);
    const call_expr = call_expr_ptr.call;
    try std.testing.expect(call_expr.callee.* == .identifier);
    try std.testing.expectEqualStrings("upper", call_expr.callee.identifier.name);
    try std.testing.expectEqual(@as(usize, 0), call_expr.arguments.len);

    const tail_stmt = script.statements[2];
    try std.testing.expect(tail_stmt.* == .expression);
    const tail_expr = tail_stmt.expression.expression;
    try std.testing.expect(tail_expr.* == .identifier);
    try std.testing.expectEqualStrings("greeting", tail_expr.identifier.name);
}

test "script parser handles async function declarations and return statements" {
    const source =
        \\async fn run(job: Job) -> Int {
        \\  return job()
        \\}
        \\return await run
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const script = try pr.parseScript();
    try std.testing.expectEqual(@as(usize, 2), script.statements.len);

    const fn_stmt = script.statements[0];
    try std.testing.expect(fn_stmt.* == .fn_decl);
    const fn_decl = fn_stmt.fn_decl;
    try std.testing.expect(fn_decl.is_async);
    try std.testing.expectEqualStrings("run", fn_decl.name.name);
    try std.testing.expectEqual(@as(usize, 1), fn_decl.params.len);
    const param = fn_decl.params[0];
    try std.testing.expectEqualStrings("job", param.name.name);
    try std.testing.expect(param.type_annotation != null);
    const param_type = param.type_annotation.?;
    try std.testing.expect(param_type.* == .identifier);
    try std.testing.expectEqual(@as(usize, 1), param_type.identifier.path.segments.len);
    try std.testing.expectEqualStrings("Job", param_type.identifier.path.segments[0].name);

    const return_type = fn_decl.return_type orelse return error.MissingReturnType;
    try std.testing.expect(return_type.* == .identifier);
    try std.testing.expectEqualStrings("Int", return_type.identifier.path.segments[0].name);

    const body_block = switch (fn_decl.body) {
        .block => |block| block,
        else => return error.ExpectedFunctionBlock,
    };
    try std.testing.expectEqual(@as(usize, 1), body_block.statements.len);
    const inner_stmt = body_block.statements[0];
    try std.testing.expect(inner_stmt.* == .return_stmt);

    const ret_stmt = script.statements[1];
    try std.testing.expect(ret_stmt.* == .return_stmt);
    const ret_value = ret_stmt.return_stmt.value orelse return error.MissingReturnValue;
    try std.testing.expect(ret_value.* == .await_expr);
    const await_expr = ret_value.await_expr;
    try std.testing.expect(await_expr.subject.* == .identifier);
    try std.testing.expectEqualStrings("run", await_expr.subject.identifier.name);
    try std.testing.expect(await_expr.success == null);
    try std.testing.expect(await_expr.failure == null);
}

test "script parser captures error enum and union declarations" {
    const source =
        \\error Network = enum { Timeout, ConnectionLost }
        \\error FileError = union { NotFound, PermissionDenied: Path }
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const script = try pr.parseScript();
    try std.testing.expectEqual(@as(usize, 2), script.statements.len);

    const enum_stmt = script.statements[0];
    try std.testing.expect(enum_stmt.* == .error_decl);
    const enum_decl = enum_stmt.error_decl;
    try std.testing.expectEqualStrings("Network", enum_decl.name.name);
    const enum_body = switch (enum_decl.definition) {
        .enumeration => |body| body,
        else => return error.ExpectedEnumBody,
    };
    try std.testing.expectEqual(@as(usize, 2), enum_body.variants.len);
    try std.testing.expectEqualStrings("Timeout", enum_body.variants[0].name.name);
    try std.testing.expectEqualStrings("ConnectionLost", enum_body.variants[1].name.name);

    const union_stmt = script.statements[1];
    try std.testing.expect(union_stmt.* == .error_decl);
    const union_decl = union_stmt.error_decl;
    try std.testing.expectEqualStrings("FileError", union_decl.name.name);
    const union_body = switch (union_decl.definition) {
        .union_type => |body| body,
        else => return error.ExpectedUnionBody,
    };
    try std.testing.expectEqual(@as(usize, 2), union_body.variants.len);
    const first_variant = union_body.variants[0];
    try std.testing.expectEqualStrings("NotFound", first_variant.name.name);
    try std.testing.expect(first_variant.payload == null);
    const second_variant = union_body.variants[1];
    try std.testing.expectEqualStrings("PermissionDenied", second_variant.name.name);
    const payload = second_variant.payload orelse return error.MissingUnionPayload;
    try std.testing.expect(payload.* == .identifier);
    try std.testing.expectEqualStrings("Path", payload.identifier.path.segments[0].name);
}

test "script parser detects import bindings before regular lets" {
    const source =
        \\let http = import("net/http")
        \\let version: Int = 1
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const script = try pr.parseScript();
    try std.testing.expectEqual(@as(usize, 2), script.statements.len);

    const import_stmt_ptr = script.statements[0];
    try std.testing.expect(import_stmt_ptr.* == .import_stmt);
    const import_stmt = import_stmt_ptr.import_stmt;
    try std.testing.expectEqualStrings("http", import_stmt.alias.name);
    const literal = import_stmt.path.literal;
    try std.testing.expectEqual(@as(usize, 1), literal.segments.len);
    switch (literal.segments[0]) {
        .text => |text| try std.testing.expectEqualStrings("net/http", text),
        else => return error.ExpectedImportPath,
    }

    const let_stmt = script.statements[1];
    try std.testing.expect(let_stmt.* == .let_decl);
    const let_decl = let_stmt.let_decl;
    try std.testing.expect(let_decl.annotation != null);
    const annotation = let_decl.annotation.?;
    try std.testing.expect(annotation.* == .identifier);
    try std.testing.expectEqualStrings("Int", annotation.identifier.path.segments[0].name);
    try std.testing.expect(let_decl.pattern.* == .identifier);
    try std.testing.expectEqualStrings("version", let_decl.pattern.identifier.name);
}

test "script parser handles for loop capture and while statements" {
    const source =
        \\for (jobs, configs) |job, config| {
        \\  process(job, config)
        \\}
        \\while ready() {
        \\  handle()
        \\}
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const script = try pr.parseScript();
    try std.testing.expectEqual(@as(usize, 2), script.statements.len);

    const for_stmt_ptr = script.statements[0];
    try std.testing.expect(for_stmt_ptr.* == .for_stmt);
    const for_stmt = for_stmt_ptr.for_stmt;
    try std.testing.expectEqual(@as(usize, 2), for_stmt.sources.len);
    try std.testing.expect(for_stmt.sources[0].* == .identifier);
    try std.testing.expect(for_stmt.sources[1].* == .identifier);
    try std.testing.expectEqualStrings("jobs", for_stmt.sources[0].identifier.name);
    try std.testing.expectEqualStrings("configs", for_stmt.sources[1].identifier.name);
    try std.testing.expectEqual(@as(usize, 2), for_stmt.capture.bindings.len);
    try std.testing.expect(for_stmt.capture.bindings[0].* == .identifier);
    try std.testing.expect(for_stmt.capture.bindings[1].* == .identifier);
    try std.testing.expectEqualStrings("job", for_stmt.capture.bindings[0].identifier.name);
    try std.testing.expectEqualStrings("config", for_stmt.capture.bindings[1].identifier.name);
    try std.testing.expectEqual(@as(usize, 1), for_stmt.body.statements.len);

    const while_stmt_ptr = script.statements[1];
    try std.testing.expect(while_stmt_ptr.* == .while_stmt);
    const while_stmt = while_stmt_ptr.while_stmt;
    try std.testing.expect(while_stmt.condition.* == .call);
    const condition_call = while_stmt.condition.call;
    try std.testing.expect(condition_call.callee.* == .identifier);
    try std.testing.expectEqualStrings("ready", condition_call.callee.identifier.name);
    try std.testing.expect(while_stmt.capture == null);
    try std.testing.expectEqual(@as(usize, 1), while_stmt.body.statements.len);
}

test "script parser extracts bash block bodies" {
    const source =
        \\bash {
        \\  echo "hi"
        \\  ls
        \\}
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const script = try pr.parseScript();
    try std.testing.expectEqual(@as(usize, 1), script.statements.len);
    const stmt = script.statements[0];
    try std.testing.expect(stmt.* == .bash_block);
    const bash_block = stmt.bash_block;
    const expected_body =
        \\
        \\  echo "hi"
        \\  ls
        \\
    ;
    try std.testing.expectEqualStrings(expected_body, bash_block.body);
}

test "script parser records pipelines with env assignments capture redirects and background" {
    const source =
        \\FOO=config python api.py ${result.stdout} capture = { stdout: :stream, stderr: :buffer } 1>stdout_handle 2>>logs &
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const script = try pr.parseScript();
    try std.testing.expectEqual(@as(usize, 1), script.statements.len);
    const stmt = script.statements[0];
    try std.testing.expect(stmt.* == .expression);
    const expr = stmt.expression.expression;
    try std.testing.expect(expr.* == .pipeline);
    const pipeline = expr.pipeline;
    try std.testing.expectEqual(@as(usize, 1), pipeline.stages.len);
    const stage = pipeline.stages[0];
    try std.testing.expectEqual(ast.StageRole.command, stage.role);
    const command = stage.payload.command;
    try std.testing.expect(command.background);
    try std.testing.expectEqual(@as(usize, 1), command.env_assignments.len);
    const env_assign = command.env_assignments[0];
    try std.testing.expectEqualStrings("FOO", env_assign.name.name);
    try std.testing.expect(env_assign.value.* == .identifier);
    try std.testing.expectEqualStrings("config", env_assign.value.identifier.name);

    const name_part = switch (command.name) {
        .word => |word| word,
        else => return error.ExpectedCommandName,
    };
    try std.testing.expectEqualStrings("python", name_part.text);
    try std.testing.expectEqual(@as(usize, 2), command.args.len);
    switch (command.args[0]) {
        .word => |word| try std.testing.expectEqualStrings("api.py", word.text),
        else => return error.ExpectedPipelineArgument,
    }
    switch (command.args[1]) {
        .expr => |expr_ptr| {
            try std.testing.expect(expr_ptr.* == .member);
            const member = expr_ptr.member;
            try std.testing.expectEqualStrings("stdout", member.member.name);
            try std.testing.expect(member.object.* == .identifier);
            try std.testing.expectEqualStrings("result", member.object.identifier.name);
        },
        else => return error.ExpectedInterpolationArgument,
    }

    const capture = command.capture orelse return error.MissingCaptureDirective;
    try std.testing.expectEqual(@as(usize, 2), capture.entries.len);
    try std.testing.expectEqual(ast.CaptureStream.stdout, capture.entries[0].stream);
    try std.testing.expectEqual(ast.CaptureMode.stream, capture.entries[0].mode);
    try std.testing.expectEqual(ast.CaptureStream.stderr, capture.entries[1].stream);
    try std.testing.expectEqual(ast.CaptureMode.buffer, capture.entries[1].mode);

    try std.testing.expectEqual(@as(usize, 2), command.redirects.len);
    const stdout_redirect = command.redirects[0];
    try std.testing.expectEqual(ast.StreamRef.stdout, stdout_redirect.stream);
    try std.testing.expectEqual(ast.RedirectionMode.truncate, stdout_redirect.mode);
    switch (stdout_redirect.target) {
        .variable => |ident| try std.testing.expectEqualStrings("stdout_handle", ident.name),
        else => return error.ExpectedRedirectVariable,
    }
    const stderr_redirect = command.redirects[1];
    try std.testing.expectEqual(ast.StreamRef.stderr, stderr_redirect.stream);
    try std.testing.expectEqual(ast.RedirectionMode.append, stderr_redirect.mode);
    switch (stderr_redirect.target) {
        .variable => |ident| try std.testing.expectEqualStrings("logs", ident.name),
        else => return error.ExpectedRedirectVariable,
    }
}

test "script parser reports error when for loop lacks sources" {
    const source =
        \\for () |value| {
        \\  value
        \\}
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();
    try std.testing.expectError(parser.ParseError.UnexpectedToken, pr.parseScript());
}

test "script parser reports error for unterminated bash block" {
    const source =
        \\bash {
        \\  echo "hi"
    ;
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();
    try std.testing.expectError(parser.ParseError.UnexpectedEOF, pr.parseScript());
}

test "module parser captures functions values and manifests" {
    const source =
        \\fn add(lhs: Int, rhs: Int) Int {
        \\  return echo hi
        \\}
        \\let tau = 6.28318
        \\manifest {
        \\  "exports": []
        \\}
    ;

    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();

    const document = try pr.parseModuleDocument();
    try std.testing.expectEqual(@as(usize, 3), document.declarations.len);

    const decl0 = document.declarations[0];
    switch (decl0) {
        .function => |func| {
            try std.testing.expectEqualStrings("add", func.name);
            try std.testing.expectEqual(@as(usize, 2), func.params.len);
            try std.testing.expectEqualStrings("lhs", func.params[0].name);
            try std.testing.expectEqualStrings("rhs", func.params[1].name);
            const body_slice = pr.sliceForRange(func.body_range);
            try std.testing.expect(std.mem.indexOfScalar(u8, body_slice, 'e') != null);
        },
        else => return error.ExpectedFunctionDecl,
    }

    const decl1 = document.declarations[1];
    switch (decl1) {
        .value => |value| {
            try std.testing.expectEqualStrings("tau", value.name);
            const literal = pr.sliceForRange(value.initializer_range);
            try std.testing.expectEqualStrings("6.28318", std.mem.trim(u8, literal, " \t\r\n"));
        },
        else => return error.ExpectedValueDecl,
    }

    const decl2 = document.declarations[2];
    switch (decl2) {
        .manifest => |manifest| {
            const manifest_slice = pr.sliceForRange(manifest.body_range);
            try std.testing.expect(std.mem.indexOfScalar(u8, manifest_slice, '[') != null);
        },
        else => return error.ExpectedManifestDecl,
    }
}

test "module parser reports missing function name" {
    const source = "fn (value: Int) Int { return value }";
    var pr = parser.Parser.init(std.testing.allocator, source);
    defer pr.deinit();
    try std.testing.expectError(parser.ModuleParseError.MissingFunctionName, pr.parseModuleDocument());
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
    _ = try expectToken(stream, .kw_let);
    try expectIdentifier(stream);
    try skipNewlines(stream);
    _ = try expectToken(stream, .assign);
    try skipNewlines(stream);
    _ = try expectToken(stream, .kw_import);
    try skipNewlines(stream);
    _ = try expectToken(stream, .l_paren);
    try skipNewlines(stream);
    const path = try stream.next();
    if (path.tag != .string_literal) return ParseError.UnexpectedToken;
    try skipNewlines(stream);
    _ = try expectToken(stream, .r_paren);
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
