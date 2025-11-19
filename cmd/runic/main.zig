const std = @import("std");
const Allocator = std.mem.Allocator;
const ManagedArrayList = std.array_list.Managed;
const runic = @import("runic");
const pipeline = @import("pipeline.zig");
const repl = @import("repl.zig");
const ScriptContext = @import("script_context.zig").ScriptContext;
const CommandRunner = runic.command_runner.CommandRunner;
const ProcessHandle = runic.command_runner.ProcessHandle;
const ProcessSnapshot = runic.command_runner.ProcessSnapshot;
const StackTrace = runic.stack_trace.StackTrace;
const Tracer = runic.tracing.Tracer;
const TokenList = pipeline.TokenList;
const Pipeline = pipeline.Pipeline;
const describeStage = pipeline.describeStage;
const parser_pkg = runic.parser;
const Parser = parser_pkg.Parser;
const ParserError = parser_pkg.ParseError;
const lexer_pkg = runic.lexer;
const LexerError = lexer_pkg.Error;
const ast = runic.ast;
const interpreter_pkg = runic.interpreter;
const Evaluator = interpreter_pkg.Evaluator;
const ScopeStack = interpreter_pkg.ScopeStack;
const RuntimeValue = interpreter_pkg.Value;
const ModuleParseError = parser_pkg.ModuleParseError;

const BindingProvider = struct {
    context: ?*const ScriptContext = null,
    overrides: []const Binding = &[_]Binding{},

    const Binding = struct {
        name: []const u8,
        value: []const u8,
    };

    fn get(self: BindingProvider, name: []const u8) ?[]const u8 {
        for (self.overrides) |binding| {
            if (std.mem.eql(u8, binding.name, name)) return binding.value;
        }
        if (self.context) |ctx| {
            if (ctx.getStringBinding(name)) |value| {
                return value;
            }
            if (parseInterpolatedObjectField(name)) |field| {
                if (ctx.getObjectBinding(field.object_name)) |object_binding| {
                    if (object_binding.getField(field.field_name)) |value| {
                        return value;
                    }
                }
            }
        }
        return null;
    }

    fn hasArrayBinding(self: BindingProvider, name: []const u8) bool {
        if (self.context) |ctx| {
            return ctx.getStringArrayBinding(name) != null;
        }
        return false;
    }
};

const ObjectFieldSpecifier = struct {
    object_name: []const u8,
    field_name: []const u8,
};

const AstSnippet = struct {
    source: []u8,
    parser: *Parser,
    root: *ast.Expression,
    script_path: []const u8,
    line_number: usize,

    fn deinit(self: *AstSnippet, allocator: Allocator) void {
        self.parser.deinit();
        allocator.destroy(self.parser);
        allocator.free(self.source);
        self.* = undefined;
    }
};

const InterpreterState = struct {
    evaluator: Evaluator,
    scopes: ScopeStack,

    fn init(allocator: Allocator, runner: *CommandRunner) !InterpreterState {
        var scopes = ScopeStack.init(allocator);
        errdefer scopes.deinit();
        try scopes.pushFrame();

        return .{
            .evaluator = Evaluator.initWithRunner(allocator, runner),
            .scopes = scopes,
        };
    }

    fn deinit(self: *InterpreterState) void {
        self.scopes.deinit();
        self.* = undefined;
    }

    fn executeSnippet(
        self: *InterpreterState,
        context: *const ScriptContext,
        snippet: *const AstSnippet,
    ) Evaluator.Error!void {
        try reseedInterpreterScopeWithContext(self.evaluator.allocator, &self.scopes, context);

        const expr_span = snippet.root.span();
        var stmt = ast.Statement{
            .expression = .{
                .expression = snippet.root,
                .span = expr_span,
            },
        };
        const statements = [_]*ast.Statement{&stmt};
        const block = ast.Block{
            .statements = &statements,
            .span = expr_span,
        };
        try self.evaluator.runBlock(&self.scopes, block);
    }
};

fn reseedInterpreterScopeWithContext(
    allocator: Allocator,
    scopes: *ScopeStack,
    context: *const ScriptContext,
) (Allocator.Error || ScopeStack.Error)!void {
    var fresh = ScopeStack.init(allocator);
    errdefer fresh.deinit();

    try fresh.pushFrame();

    for (context.bindings.items) |binding| {
        switch (binding.value) {
            .string => |literal| {
                var runtime_value = RuntimeValue{ .string = try allocator.dupe(u8, literal) };
                errdefer runtime_value.deinit(allocator);
                try fresh.declare(binding.name, &runtime_value, binding.is_mutable);
            },
            else => {},
        }
    }

    var previous = scopes.*;
    scopes.* = fresh;
    previous.deinit();
}

fn parseInterpolatedObjectField(name: []const u8) ?ObjectFieldSpecifier {
    if (name.len == 0) return null;
    const dot_index = std.mem.indexOfScalar(u8, name, '.') orelse return null;
    if (dot_index == 0 or dot_index + 1 >= name.len) return null;
    if (std.mem.indexOfScalar(u8, name[dot_index + 1 ..], '.')) |_| return null;

    const object_name = name[0..dot_index];
    const field_name = name[dot_index + 1 ..];
    if (!isValidIdentifier(object_name) or !isValidIdentifier(field_name)) return null;

    return .{
        .object_name = object_name,
        .field_name = field_name,
    };
}

fn isValidIdentifier(text: []const u8) bool {
    if (text.len == 0) return false;
    if (!isIdentifierStart(text[0])) return false;
    for (text[1..]) |ch| {
        if (!isIdentifierChar(ch)) return false;
    }
    return true;
}

const ModuleRegistry = struct {
    allocator: Allocator,
    modules: std.ArrayList(Module) = .empty,

    const Module = struct {
        alias: []const u8,
        spec: []const u8,
        path: []const u8,
        functions: std.ArrayList(Function),
        values: std.ArrayList(Value) = .empty,

        fn deinit(self: *Module, allocator: Allocator) void {
            allocator.free(self.alias);
            allocator.free(self.spec);
            allocator.free(self.path);
            for (self.functions.items) |function| {
                allocator.free(function.name);
                for (function.params) |param| allocator.free(param);
                allocator.free(function.params);
                allocator.free(function.command);
            }
            self.functions.deinit(allocator);
            for (self.values.items) |value| {
                allocator.free(value.name);
                allocator.free(value.literal);
            }
            self.values.deinit(allocator);
            self.* = undefined;
        }
    };

    const Function = struct {
        name: []const u8,
        params: [][]const u8,
        command: []const u8,
    };

    const Value = struct {
        name: []const u8,
        literal: []const u8,
        numeric: f64,
    };

    const Lookup = struct {
        module: *Module,
        module_path: []const u8,
        function: *const Function,
    };

    pub fn init(allocator: Allocator) ModuleRegistry {
        return .{ .allocator = allocator, .modules = .empty };
    }

    pub fn deinit(self: *ModuleRegistry) void {
        for (self.modules.items) |*module| {
            module.deinit(self.allocator);
        }
        self.modules.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn ensureImport(
        self: *ModuleRegistry,
        script_dir: []const u8,
        module_paths: [][]const u8,
        alias: []const u8,
        spec: []const u8,
    ) !void {
        if (self.findModule(alias)) |_| {
            return;
        }

        const alias_owned = try self.allocator.dupe(u8, alias);
        var alias_cleanup = true;
        defer if (alias_cleanup) self.allocator.free(alias_owned);

        const spec_owned = try self.allocator.dupe(u8, spec);
        var spec_cleanup = true;
        defer if (spec_cleanup) self.allocator.free(spec_owned);

        const resolved = try self.resolveModulePath(script_dir, module_paths, spec_owned);
        var path_cleanup = true;
        defer if (path_cleanup) self.allocator.free(resolved);

        var file = std.fs.cwd().openFile(resolved, .{}) catch |err| switch (err) {
            error.FileNotFound => return error.ModuleNotFound,
            else => return err,
        };
        defer file.close();

        const source = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
        defer self.allocator.free(source);

        var module = Module{
            .alias = alias_owned,
            .spec = spec_owned,
            .path = resolved,
            .functions = .empty,
        };
        alias_cleanup = false;
        spec_cleanup = false;
        path_cleanup = false;
        errdefer module.deinit(self.allocator);

        try self.parseModuleFunctions(&module, source);
        try self.parseModuleValues(&module, source);
        try self.modules.append(self.allocator, module);
    }

    pub fn registerInlineFunctions(
        self: *ModuleRegistry,
        alias: []const u8,
        script_path: []const u8,
        source: []const u8,
    ) (ModuleParseError || Allocator.Error)!?*Module {
        if (self.findModule(alias)) |existing| {
            return existing;
        }

        const alias_owned = try self.allocator.dupe(u8, alias);
        var alias_cleanup = true;
        defer if (alias_cleanup) self.allocator.free(alias_owned);

        const spec_owned = try self.allocator.dupe(u8, script_path);
        var spec_cleanup = true;
        defer if (spec_cleanup) self.allocator.free(spec_owned);

        const path_owned = try self.allocator.dupe(u8, script_path);
        var path_cleanup = true;
        defer if (path_cleanup) self.allocator.free(path_owned);

        var module = Module{
            .alias = alias_owned,
            .spec = spec_owned,
            .path = path_owned,
            .functions = .empty,
        };
        alias_cleanup = false;
        spec_cleanup = false;
        path_cleanup = false;
        errdefer module.deinit(self.allocator);

        try self.parseModuleFunctions(&module, source);
        if (module.functions.items.len == 0) {
            module.deinit(self.allocator);
            return null;
        }

        try self.modules.append(self.allocator, module);
        return &self.modules.items[self.modules.items.len - 1];
    }

    pub fn lookupFunction(self: *ModuleRegistry, alias: []const u8, name: []const u8) ?Lookup {
        const module = self.findModule(alias) orelse return null;
        for (module.functions.items) |*function| {
            if (std.mem.eql(u8, function.name, name)) {
                return Lookup{ .module = module, .module_path = module.path, .function = function };
            }
        }
        return null;
    }

    pub fn lookupValue(self: *const ModuleRegistry, alias: []const u8, name: []const u8) ?*const Value {
        const module = self.findModuleConst(alias) orelse return null;
        for (module.values.items) |*value| {
            if (std.mem.eql(u8, value.name, name)) return value;
        }
        return null;
    }

    fn evaluateFunctionLiteral(
        self: *ModuleRegistry,
        allocator: Allocator,
        module: *Module,
        function: *const Function,
        params: []const BindingProvider.Binding,
    ) !?[]u8 {
        _ = self;
        const trimmed = std.mem.trim(u8, function.command, " \t\r\n");
        if (trimmed.len == 0) return null;
        if (trimmed[0] == '"') {
            const literal = parseStringLiteral(allocator, trimmed) catch return null;
            var consumed = literal.consumed;
            skipWhitespaceAll(trimmed, &consumed);
            if (consumed == trimmed.len) {
                return literal.value;
            }
            allocator.free(literal.value);
        }
        const numeric = evaluateNumericExpression(module, trimmed, params) catch |err| switch (err) {
            ModuleExpressionError.InvalidToken, ModuleExpressionError.MissingOperand, ModuleExpressionError.UnknownIdentifier => return null,
            ModuleExpressionError.DivisionByZero => return null,
        };
        return try formatNumericLiteral(allocator, numeric);
    }

    fn findModule(self: *ModuleRegistry, alias: []const u8) ?*Module {
        for (self.modules.items) |*module| {
            if (std.mem.eql(u8, module.alias, alias)) return module;
        }
        return null;
    }

    fn findModuleConst(self: *const ModuleRegistry, alias: []const u8) ?Module {
        for (self.modules.items) |*module| {
            if (std.mem.eql(u8, module.alias, alias)) return module.*;
        }
        return null;
    }

    fn resolveModulePath(
        self: *ModuleRegistry,
        script_dir: []const u8,
        module_paths: [][]const u8,
        spec: []const u8,
    ) ![]u8 {
        var spec_with_ext = spec;
        var spec_buf: ?[]const u8 = null;
        defer if (spec_buf) |owned| self.allocator.free(owned);

        if (!std.mem.endsWith(u8, spec, ".rn")) {
            spec_with_ext = try std.fmt.allocPrint(self.allocator, "{s}.rn", .{spec});
            spec_buf = spec_with_ext;
        }

        if (isAbsolutePath(spec_with_ext)) {
            try ensureFileExists(spec_with_ext);
            return try self.allocator.dupe(u8, spec_with_ext);
        }

        if (try joinAndCheck(self.allocator, script_dir, spec_with_ext)) |path| return path;
        for (module_paths) |base| {
            if (try joinAndCheck(self.allocator, base, spec_with_ext)) |path| return path;
        }
        if (fileExists(spec_with_ext)) {
            return try self.allocator.dupe(u8, spec_with_ext);
        }

        return error.ModuleNotFound;
    }

    fn parseModuleFunctions(self: *ModuleRegistry, module: *Module, source: []const u8) !void {
        var pr = Parser.init(self.allocator, source);
        defer pr.deinit();

        const document = try pr.parseModuleDocument();
        for (document.declarations) |decl| {
            switch (decl) {
                .function => |func| {
                    const name_owned = try self.allocator.dupe(u8, func.name);
                    var name_cleanup = true;
                    defer if (name_cleanup) self.allocator.free(name_owned);

                    var params_owned = try self.allocator.alloc([]const u8, func.params.len);
                    var params_cleanup = true;
                    defer if (params_cleanup) freeStringList(self.allocator, params_owned);
                    for (func.params, 0..) |param, i| {
                        params_owned[i] = try self.allocator.dupe(u8, param.name);
                    }

                    const return_slice = if (func.return_type_span) |span|
                        pr.sliceForSpan(span)
                    else
                        source[0..0];
                    const has_void_return = functionReturnTypeIsVoid(return_slice);

                    const body_slice = pr.sliceForRange(func.body_range);
                    const command_text = try extractReturnCommand(self.allocator, body_slice, .{
                        .require_return = !has_void_return,
                        .allow_empty_command = has_void_return,
                    });

                    try module.functions.append(self.allocator, Function{
                        .name = name_owned,
                        .params = params_owned,
                        .command = command_text,
                    });
                    name_cleanup = false;
                    params_cleanup = false;
                },
                else => {},
            }
        }
    }

    fn parseModuleValues(self: *ModuleRegistry, module: *Module, source: []const u8) !void {
        var iterator = std.mem.splitScalar(u8, source, '\n');
        var brace_depth: usize = 0;
        while (iterator.next()) |raw_line| {
            const trimmed = std.mem.trim(u8, raw_line, " \t\r");
            if (trimmed.len == 0 or isCommentLine(trimmed)) {
                updateBraceDepth(raw_line, &brace_depth);
                continue;
            }

            if (brace_depth == 0 and std.mem.startsWith(u8, trimmed, "let")) {
                try self.parseModuleValueLine(module, trimmed);
                updateBraceDepth(raw_line, &brace_depth);
                continue;
            }

            updateBraceDepth(raw_line, &brace_depth);
        }
    }

    fn updateBraceDepth(line: []const u8, depth: *usize) void {
        const delta = braceBalanceDelta(line);
        if (delta > 0) {
            const increase: usize = @intCast(delta);
            depth.* += increase;
        } else if (delta < 0) {
            const magnitude: usize = @intCast(-delta);
            if (magnitude > depth.*) {
                depth.* = 0;
            } else {
                depth.* -= magnitude;
            }
        }
    }

    fn parseModuleValueLine(self: *ModuleRegistry, module: *Module, line: []const u8) !void {
        var idx: usize = 3;
        skipInlineWhitespace(line, &idx);
        if (idx >= line.len or !isIdentifierStart(line[idx])) {
            return ModuleParseError.MissingValueName;
        }
        const name_start = idx;
        idx += 1;
        while (idx < line.len and isIdentifierChar(line[idx])) : (idx += 1) {}
        const name_slice = line[name_start..idx];

        skipInlineWhitespace(line, &idx);
        if (idx >= line.len or line[idx] != '=') {
            return ModuleParseError.MissingValueInitializer;
        }
        idx += 1;

        const expr_slice = std.mem.trim(u8, line[idx..], " \t\r;");
        if (expr_slice.len == 0) return ModuleParseError.InvalidValueExpression;

        const numeric = evaluateNumericExpression(module, expr_slice, &[_]BindingProvider.Binding{}) catch |err| switch (err) {
            ModuleExpressionError.InvalidToken, ModuleExpressionError.MissingOperand, ModuleExpressionError.UnknownIdentifier => return ModuleParseError.InvalidValueExpression,
            ModuleExpressionError.DivisionByZero => return ModuleParseError.InvalidValueExpression,
        };

        const literal = try formatNumericLiteral(self.allocator, numeric);
        errdefer self.allocator.free(literal);

        const name_owned = try self.allocator.dupe(u8, name_slice);
        errdefer self.allocator.free(name_owned);

        try module.values.append(self.allocator, .{
            .name = name_owned,
            .literal = literal,
            .numeric = numeric,
        });
    }

    fn evaluateNumericExpression(
        module: *Module,
        expression: []const u8,
        params: []const BindingProvider.Binding,
    ) ModuleExpressionError!f64 {
        var parser = ModuleExpressionParser{
            .text = expression,
            .module = module,
            .params = params,
        };
        const value = try parser.parseExpression();
        parser.skipWhitespace();
        if (parser.idx != parser.text.len) return ModuleExpressionError.InvalidToken;
        return value;
    }

    fn formatNumericLiteral(allocator: Allocator, value: f64) ![]u8 {
        const epsilon = 1e-9;
        const rounded = std.math.round(value);
        if (@abs(value - rounded) < epsilon and rounded <= @as(f64, @floatFromInt(std.math.maxInt(i64))) and rounded >= @as(f64, @floatFromInt(std.math.minInt(i64)))) {
            const int_value = @as(i64, @intFromFloat(rounded));
            return try std.fmt.allocPrint(allocator, "{d}", .{int_value});
        }

        var literal = try std.fmt.allocPrint(allocator, "{d:.6}", .{value});
        errdefer allocator.free(literal);

        var end = literal.len;
        while (end > 0 and literal[end - 1] == '0') : (end -= 1) {}
        if (end > 0 and literal[end - 1] == '.') end -= 1;
        if (end == 0) end = 1;

        if (end == literal.len) {
            return literal;
        }

        const trimmed = literal[0..end];
        const owned = try allocator.dupe(u8, trimmed);
        allocator.free(literal);
        return owned;
    }

    const ModuleExpressionError = error{
        InvalidToken,
        MissingOperand,
        UnknownIdentifier,
        DivisionByZero,
    };

    const ModuleExpressionParser = struct {
        text: []const u8,
        idx: usize = 0,
        module: *Module,
        params: []const BindingProvider.Binding,

        fn parseExpression(self: *ModuleExpressionParser) ModuleExpressionError!f64 {
            var value = try self.parseTerm();
            while (true) {
                self.skipWhitespace();
                if (self.consume('+')) {
                    const rhs = try self.parseTerm();
                    value += rhs;
                    continue;
                }
                if (self.consume('-')) {
                    const rhs = try self.parseTerm();
                    value -= rhs;
                    continue;
                }
                break;
            }
            return value;
        }

        fn parseTerm(self: *ModuleExpressionParser) ModuleExpressionError!f64 {
            var value = try self.parseFactor();
            while (true) {
                self.skipWhitespace();
                if (self.consume('*')) {
                    const rhs = try self.parseFactor();
                    value *= rhs;
                    continue;
                }
                if (self.consume('/')) {
                    const rhs = try self.parseFactor();
                    if (rhs == 0) return ModuleExpressionError.DivisionByZero;
                    value /= rhs;
                    continue;
                }
                break;
            }
            return value;
        }

        fn parseFactor(self: *ModuleExpressionParser) ModuleExpressionError!f64 {
            self.skipWhitespace();
            if (self.consume('+')) {
                return try self.parseFactor();
            }
            if (self.consume('-')) {
                return -(try self.parseFactor());
            }
            if (self.consume('(')) {
                const value = try self.parseExpression();
                self.skipWhitespace();
                if (!self.consume(')')) return ModuleExpressionError.InvalidToken;
                return value;
            }
            return try self.parsePrimary();
        }

        fn parsePrimary(self: *ModuleExpressionParser) ModuleExpressionError!f64 {
            self.skipWhitespace();
            if (self.idx >= self.text.len) return ModuleExpressionError.MissingOperand;

            const ch = self.text[self.idx];
            if (std.ascii.isDigit(ch)) {
                return try self.parseNumber();
            }
            if (isIdentifierStart(ch)) {
                return try self.parseIdentifier();
            }
            return ModuleExpressionError.InvalidToken;
        }

        fn parseNumber(self: *ModuleExpressionParser) ModuleExpressionError!f64 {
            const start = self.idx;
            var seen_digit = false;
            var seen_dot = false;
            while (self.idx < self.text.len) : (self.idx += 1) {
                const ch = self.text[self.idx];
                if (std.ascii.isDigit(ch)) {
                    seen_digit = true;
                    continue;
                }
                if (ch == '.' and !seen_dot) {
                    seen_dot = true;
                    continue;
                }
                break;
            }
            if (!seen_digit) return ModuleExpressionError.InvalidToken;
            const literal = self.text[start..self.idx];
            return std.fmt.parseFloat(f64, literal) catch return ModuleExpressionError.InvalidToken;
        }

        fn parseIdentifier(self: *ModuleExpressionParser) ModuleExpressionError!f64 {
            const start = self.idx;
            self.idx += 1;
            while (self.idx < self.text.len and isIdentifierChar(self.text[self.idx])) : (self.idx += 1) {}
            const name = self.text[start..self.idx];
            if (self.resolveParam(name)) |value| return value;
            if (self.resolveModuleValue(name)) |value| return value;
            return ModuleExpressionError.UnknownIdentifier;
        }

        fn resolveParam(self: *ModuleExpressionParser, name: []const u8) ?f64 {
            for (self.params) |binding| {
                if (std.mem.eql(u8, binding.name, name)) {
                    return std.fmt.parseFloat(f64, binding.value) catch null;
                }
            }
            return null;
        }

        fn resolveModuleValue(self: *ModuleExpressionParser, name: []const u8) ?f64 {
            for (self.module.values.items) |value| {
                if (std.mem.eql(u8, value.name, name)) return value.numeric;
            }
            return null;
        }

        fn skipWhitespace(self: *ModuleExpressionParser) void {
            while (self.idx < self.text.len and std.ascii.isWhitespace(self.text[self.idx])) : (self.idx += 1) {}
        }

        fn consume(self: *ModuleExpressionParser, target: u8) bool {
            if (self.idx < self.text.len and self.text[self.idx] == target) {
                self.idx += 1;
                return true;
            }
            return false;
        }
    };
};

const ModuleInvocationError = error{
    MissingModuleName,
    MissingFunctionName,
    MissingArgumentList,
    MissingClosingParen,
    TrailingCharacters,
    UnsupportedCatchClause,
};

const ModuleInvocation = struct {
    alias: []const u8,
    function: []const u8,
    args: [][]const u8,

    fn deinit(self: *ModuleInvocation, allocator: Allocator) void {
        for (self.args) |arg| allocator.free(arg);
        if (self.args.len > 0) allocator.free(self.args);
        self.* = undefined;
    }
};

const CliConfig = struct {
    mode: Mode,
    trace_topics: [][]const u8,
    module_paths: [][]const u8,
    env_overrides: []EnvOverride,

    const Mode = union(enum) {
        script: ScriptInvocation,
        repl,
    };

    const ScriptInvocation = struct {
        path: []const u8,
        args: []const []const u8,
    };

    const EnvOverride = struct {
        key: []const u8,
        value: []const u8,
    };

    fn deinit(self: *CliConfig, allocator: Allocator) void {
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

pub fn main() !void {
    const exit_code = try mainImpl();
    if (exit_code != 0) {
        std.process.exit(exit_code);
    }
}

fn mainImpl() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const status = gpa.deinit();
        if (status == .leak) std.log.err("runic CLI leaked memory", .{});
    }

    const allocator = gpa.allocator();

    var stdout_buffer: [1024]u8 = undefined;
    var stderr_buffer: [1024]u8 = undefined;
    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    var stdoutWriter = std.fs.File.stdout().writer(&stdout_buffer);
    var stderrWriter = std.fs.File.stderr().writer(&stderr_buffer);
    const stdout = &stdoutWriter.interface;
    const stderr = &stderrWriter.interface;
    defer stdout.flush() catch {};
    defer stderr.flush() catch {};

    const result = try parseCommandLine(allocator, argv);
    switch (result) {
        .show_help => {
            try printUsage(stdout);
            return 0;
        },
        .usage_error => |message| {
            defer allocator.free(message);
            try stderr.print("error: {s}\n\n", .{message});
            try printUsage(stderr);
            try stderr.flush();
            return 2;
        },
        .ready => |config| {
            defer {
                var cfg = config;
                cfg.deinit(allocator);
            }
            return try dispatch(allocator, config, stdout, stderr);
        },
    }

    return 0;
}

fn dispatch(
    allocator: Allocator,
    config: CliConfig,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !u8 {
    switch (config.mode) {
        .script => |script| return try runScript(allocator, script, config, stdout, stderr),
        .repl => {
            try repl.run(allocator, .{
                .prompt = "runic> ",
                .continuation_prompt = "...> ",
                .history_limit = 256,
                .trace_topics = config.trace_topics,
                .module_paths = config.module_paths,
            });
            return 0;
        },
    }
}

fn runScript(
    allocator: Allocator,
    script: CliConfig.ScriptInvocation,
    config: CliConfig,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !u8 {
    var context = ScriptContext.init(allocator);
    defer context.deinit();

    var modules = ModuleRegistry.init(allocator);
    defer modules.deinit();

    var tracer = Tracer.init(config.trace_topics, null);
    if (tracer.anyTopicEnabled()) tracer.setSink(stderr);

    var env_map = std.process.getEnvMap(allocator) catch |err| {
        try stderr.print("error: unable to capture environment: {s}\n", .{@errorName(err)});
        return 1;
    };
    defer env_map.deinit();

    try applyEnvOverridesToMap(&env_map, config.env_overrides);
    try exposeScriptMetadata(&env_map, script);
    try ensureExecutableDirOnPath(allocator, &env_map);

    const script_dir = computeScriptDirectory(allocator, script.path) catch |err| {
        try stderr.print("error: unable to resolve script directory: {s}\n", .{@errorName(err)});
        return 1;
    };
    defer allocator.free(script_dir);

    const tracer_ptr = if (tracer.anyTopicEnabled()) &tracer else null;
    var runner = CommandRunner.initWithTracer(allocator, tracer_ptr);

    var file = std.fs.cwd().openFile(script.path, .{}) catch |err| {
        try stderr.print("error: failed to open script '{s}': {s}\n", .{ script.path, @errorName(err) });
        return 1;
    };
    defer file.close();

    const contents = file.readToEndAlloc(allocator, std.math.maxInt(usize)) catch |err| {
        try stderr.print("error: failed to read script '{s}': {s}\n", .{ script.path, @errorName(err) });
        return 1;
    };
    defer allocator.free(contents);

    const inline_module = modules.registerInlineFunctions(script.path, script.path, contents) catch |err| switch (err) {
        error.OutOfMemory => {
            try stderr.print(
                "error: failed to parse functions in script '{s}': {s}\n",
                .{ script.path, @errorName(err) },
            );
            return 1;
        },
        else => {
            try stderr.print(
                "error: failed to parse functions in script '{s}': {s}\n",
                .{ script.path, moduleParseErrorMessage(err) },
            );
            return 1;
        },
    };

    if (inline_module) |module_ptr| {
        for (module_ptr.functions.items) |function| {
            context.declareFunctionBinding(function.name, module_ptr.alias, function.name, false) catch |err| switch (err) {
                error.DuplicateBinding => {
                    try stderr.print(
                        "error: script '{s}' declares duplicate function '{s}'\n",
                        .{ script.path, function.name },
                    );
                    return 1;
                },
                else => return err,
            };
        }
    }

    const computed_label = computeScriptLabel(allocator, script.path) catch null;
    defer if (computed_label) |label| allocator.free(label);
    const script_label = computed_label orelse script.path;

    var aggregated = std.ArrayList(u8).empty;
    defer aggregated.deinit(allocator);

    var interpreter_state = InterpreterState.init(allocator, &runner) catch |err| {
        try stderr.print(
            "error: failed to initialize interpreter: {s}\n",
            .{@errorName(err)},
        );
        return 1;
    };
    defer interpreter_state.deinit();

    var continuing = false;
    var command_start_line: usize = 0;
    var exit_code: ?u8 = null;
    var skip_block_depth: isize = 0;

    var iter = std.mem.splitScalar(u8, contents, '\n');
    var line_number: usize = 0;

    while (iter.next()) |raw_line| {
        line_number += 1;
        const line = trimLineEnding(raw_line);

        if (skip_block_depth > 0) {
            skip_block_depth += braceBalanceDelta(line);
            if (skip_block_depth <= 0) skip_block_depth = 0;
            continue;
        }

        if (!continuing) {
            aggregated.clearRetainingCapacity();
            const trimmed_leading = std.mem.trimLeft(u8, line, " \t");
            if (trimmed_leading.len == 0) continue;
            if (isCommentLine(trimmed_leading)) continue;
            command_start_line = line_number;
        } else if (aggregated.items.len > 0) {
            try aggregated.append(allocator, '\n');
        }

        var trimmed = std.mem.trimRight(u8, line, " \t\r");
        var wants_continuation = false;
        if (trimmed.len > 0 and trimmed[trimmed.len - 1] == '\\') {
            wants_continuation = true;
            trimmed = trimmed[0 .. trimmed.len - 1];
        }

        try aggregated.appendSlice(allocator, trimmed);

        const command_slice = std.mem.trim(u8, aggregated.items, " \t\r\n");
        const needs_let_continuation = !wants_continuation and letCommandNeedsMoreLines(command_slice);
        const needs_error_continuation =
            !wants_continuation and errorDeclarationNeedsMoreLines(command_slice);
        const needs_control_flow_continuation =
            !wants_continuation and controlFlowNeedsMoreLines(command_slice);
        continuing = wants_continuation or needs_let_continuation or needs_error_continuation or
            needs_control_flow_continuation;

        if (continuing) continue;

        if (command_slice.len == 0) continue;

        if (isFunctionDeclaration(command_slice)) {
            const depth = computeFunctionSkipDepth(command_slice);
            skip_block_depth = depth;
            continue;
        }

        if (try executeScriptCommand(
            allocator,
            &runner,
            &env_map,
            &context,
            &modules,
            &interpreter_state,
            script_dir,
            config.module_paths,
            command_slice,
            script.path,
            command_start_line,
            script_label,
            stdout,
            stderr,
        )) |code| {
            exit_code = code;
            break;
        }
    }

    if (exit_code) |code| return code;

    if (continuing) {
        try stderr.print(
            "error: line {d}: line continuation without a terminating line\n",
            .{command_start_line},
        );
        return 1;
    }

    return 0;
}

fn moduleParseErrorMessage(err: anyerror) []const u8 {
    return switch (err) {
        ModuleParseError.MissingFunctionName => "expected function name after 'fn'",
        ModuleParseError.MissingParameterList => "expected '(' after function name",
        ModuleParseError.UnterminatedParameterList => "unterminated parameter list",
        ModuleParseError.MissingFunctionBody => "expected '{' to start function body",
        ModuleParseError.UnterminatedFunctionBody => "unterminated function body",
        ModuleParseError.InvalidParameterName => "parameter names must be valid identifiers",
        ModuleParseError.MissingReturnStatement => "function body must include a 'return' statement",
        ModuleParseError.MissingReturnCommand => "return statement must include a command to run",
        ModuleParseError.MissingValueName => "expected value name after 'let'",
        ModuleParseError.MissingValueInitializer => "expected '=' after value name",
        ModuleParseError.InvalidValueExpression => "value initializer must be an arithmetic expression",
        ModuleParseError.MissingManifestBody => "expected '{' to start manifest body",
        ModuleParseError.UnterminatedManifestBody => "unterminated manifest body",
        ModuleParseError.InvalidSyntax => "invalid module syntax",
        else => "unknown module parse error",
    };
}

const AstRecordResult = union(enum) {
    recorded: AstSnippet,
    skipped,
    syntax_error,
};

fn recordAstSnippet(
    allocator: Allocator,
    command: []const u8,
    script_path: []const u8,
    line_number: usize,
    stderr: *std.Io.Writer,
) !AstRecordResult {
    const trimmed = std.mem.trim(u8, command, " \t\r\n");
    if (trimmed.len == 0) return .skipped;

    const is_control_flow = commandStartsWithIf(trimmed);

    const owned = if (is_control_flow)
        try sanitizeControlFlowSnippet(allocator, command)
    else
        try allocator.dupe(u8, command);
    var cleanup_source = true;
    defer if (cleanup_source) allocator.free(owned);

    var parser_ptr = try allocator.create(Parser);
    parser_ptr.* = Parser.init(allocator, owned);
    var cleanup_parser = true;
    defer if (cleanup_parser) {
        parser_ptr.deinit();
        allocator.destroy(parser_ptr);
    };

    const expression = parser_ptr.parseExpression() catch |err| {
        return try handleAstParseIssue(err, is_control_flow, script_path, line_number, stderr);
    };

    parser_ptr.expectEnd() catch |err| {
        return try handleAstParseIssue(err, is_control_flow, script_path, line_number, stderr);
    };

    cleanup_source = false;
    cleanup_parser = false;
    return .{ .recorded = .{
        .source = owned,
        .parser = parser_ptr,
        .root = expression,
        .script_path = script_path,
        .line_number = line_number,
    } };
}

fn handleAstParseIssue(
    err: anyerror,
    is_control_flow: bool,
    script_path: []const u8,
    line_number: usize,
    stderr: *std.Io.Writer,
) !AstRecordResult {
    return switch (err) {
        ParserError.UnexpectedToken,
        ParserError.UnexpectedEOF,
        LexerError.UnexpectedCharacter,
        LexerError.UnterminatedString,
        LexerError.UnterminatedBlockComment,
        LexerError.TokenConsumptionDepthExceeded,
        => blk: {
            if (is_control_flow) {
                try stderr.print(
                    "{s}:{d}: unable to parse statement ({s})\n",
                    .{ script_path, line_number, @errorName(err) },
                );
                break :blk .syntax_error;
            }
            break :blk .skipped;
        },
        else => err,
    };
}

fn renderInterpreterError(
    stderr: *std.Io.Writer,
    snippet: *const AstSnippet,
    err: anyerror,
) !void {
    try stderr.print(
        "{s}:{d}: interpreter error: {s}\n",
        .{ snippet.script_path, snippet.line_number, @errorName(err) },
    );
}

fn executeScriptCommand(
    allocator: Allocator,
    runner: *CommandRunner,
    env_map: *std.process.EnvMap,
    context: *ScriptContext,
    modules: *ModuleRegistry,
    interpreter: *InterpreterState,
    script_dir: []const u8,
    module_paths: [][]const u8,
    command: []const u8,
    script_path: []const u8,
    line_number: usize,
    script_label: []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !?u8 {
    const error_decl = parseErrorDeclaration(command) catch |err| {
        try stderr.print(
            "{s}:{d}: invalid error declaration ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return 1;
    };
    if (error_decl) |decl| {
        context.declareTypeBinding(decl.name, decl.definition, false) catch |err| {
            try renderBindingError(stderr, script_path, line_number, decl.name, err);
            return 1;
        };
        return null;
    }

    const maybe_binding = parseLetBindingWithOptions(allocator, command, .{
        .context = context,
        .modules = modules,
    }) catch |err| switch (err) {
        error.UnknownFunctionBinding => {
            if (functionCallNameFromLet(command)) |fn_name| {
                try stderr.print(
                    "{s}:{d}: unknown function binding '{s}'\n",
                    .{ script_path, line_number, fn_name },
                );
            } else {
                try stderr.print("{s}:{d}: unknown function binding\n", .{ script_path, line_number });
            }
            return 1;
        },
        ModuleInvocationError.UnsupportedCatchClause => {
            try stderr.print(
                "{s}:{d}: module invocations cannot include 'catch' clauses yet\n",
                .{ script_path, line_number },
            );
            return 1;
        },
        else => {
            try stderr.print(
                "{s}:{d}: invalid let binding ({s})\n",
                .{ script_path, line_number, @errorName(err) },
            );
            return 1;
        },
    };

    if (maybe_binding) |binding| {
        switch (binding.value) {
            .literal => |literal_value| {
                defer allocator.free(literal_value);
                context.declareStringBinding(binding.name, literal_value, binding.is_mutable) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, binding.name, err);
                    return 1;
                };
            },
            .type_expression => |type_literal| {
                defer allocator.free(type_literal);
                context.declareTypeBinding(binding.name, type_literal, binding.is_mutable) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, binding.name, err);
                    return 1;
                };
            },
            .array_literal => |elements| {
                defer freeOwnedStringList(allocator, elements);
                context.declareStringArrayBinding(binding.name, elements, binding.is_mutable) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, binding.name, err);
                    return 1;
                };
            },
            .command => |command_text| {
                defer allocator.free(command_text);
                if (try executeCommandBinding(
                    allocator,
                    runner,
                    env_map,
                    context,
                    command_text,
                    binding.name,
                    binding.is_mutable,
                    script_path,
                    line_number,
                    stdout,
                    stderr,
                )) |code| return code;
            },
            .module_function => |alias_ref| {
                defer {
                    allocator.free(alias_ref.module_alias);
                    allocator.free(alias_ref.function_name);
                }
                context.declareFunctionBinding(
                    binding.name,
                    alias_ref.module_alias,
                    alias_ref.function_name,
                    binding.is_mutable,
                ) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, binding.name, err);
                    return 1;
                };
            },
            .module_invocation => |module_call| {
                var invocation_binding = module_call;
                defer invocation_binding.invocation.deinit(allocator);
                defer allocator.free(invocation_binding.display_command);
                if (try executeModuleInvocationBinding(
                    allocator,
                    runner,
                    env_map,
                    context,
                    modules,
                    invocation_binding.invocation,
                    binding.name,
                    binding.is_mutable,
                    invocation_binding.display_command,
                    script_path,
                    line_number,
                    stdout,
                    stderr,
                )) |code| return code;
            },
            .module_import => |import_binding| {
                defer allocator.free(import_binding.spec);
                modules.ensureImport(script_dir, module_paths, binding.name, import_binding.spec) catch |err| {
                    switch (err) {
                        error.ModuleNotFound => try stderr.print(
                            "{s}:{d}: module '{s}' not found\n",
                            .{ script_path, line_number, import_binding.spec },
                        ),
                        ModuleParseError.MissingFunctionName, ModuleParseError.MissingParameterList, ModuleParseError.UnterminatedParameterList, ModuleParseError.MissingFunctionBody, ModuleParseError.UnterminatedFunctionBody, ModuleParseError.InvalidParameterName, ModuleParseError.MissingReturnStatement, ModuleParseError.MissingReturnCommand => try stderr.print(
                            "{s}:{d}: module '{s}' is invalid: {s}\n",
                            .{ script_path, line_number, import_binding.spec, moduleParseErrorMessage(err) },
                        ),
                        else => try stderr.print(
                            "{s}:{d}: failed to load module '{s}': {s}\n",
                            .{ script_path, line_number, import_binding.spec, @errorName(err) },
                        ),
                    }
                    return 1;
                };
            },
            .module_import_member => |member_binding| {
                defer allocator.free(member_binding.spec);
                defer allocator.free(member_binding.member_name);
                modules.ensureImport(script_dir, module_paths, binding.name, member_binding.spec) catch |err| {
                    switch (err) {
                        error.ModuleNotFound => try stderr.print(
                            "{s}:{d}: module '{s}' not found\n",
                            .{ script_path, line_number, member_binding.spec },
                        ),
                        ModuleParseError.MissingFunctionName, ModuleParseError.MissingParameterList, ModuleParseError.UnterminatedParameterList, ModuleParseError.MissingFunctionBody, ModuleParseError.UnterminatedFunctionBody, ModuleParseError.InvalidParameterName, ModuleParseError.MissingReturnStatement, ModuleParseError.MissingReturnCommand => try stderr.print(
                            "{s}:{d}: module '{s}' is invalid: {s}\n",
                            .{ script_path, line_number, member_binding.spec, moduleParseErrorMessage(err) },
                        ),
                        else => try stderr.print(
                            "{s}:{d}: failed to load module '{s}': {s}\n",
                            .{ script_path, line_number, member_binding.spec, @errorName(err) },
                        ),
                    }
                    return 1;
                };

                if (modules.lookupFunction(binding.name, member_binding.member_name)) |_| {
                    context.declareFunctionBinding(
                        binding.name,
                        binding.name,
                        member_binding.member_name,
                        binding.is_mutable,
                    ) catch |err| {
                        try renderBindingError(stderr, script_path, line_number, binding.name, err);
                        return 1;
                    };
                } else if (modules.lookupValue(binding.name, member_binding.member_name)) |module_value| {
                    context.declareStringBinding(
                        binding.name,
                        module_value.literal,
                        binding.is_mutable,
                    ) catch |err| {
                        try renderBindingError(stderr, script_path, line_number, binding.name, err);
                        return 1;
                    };
                } else {
                    try stderr.print(
                        "{s}:{d}: module '{s}' has no member '{s}'\n",
                        .{ script_path, line_number, member_binding.spec, member_binding.member_name },
                    );
                    return 1;
                }
            },
            .module_value => |value_ref| {
                defer {
                    allocator.free(value_ref.module_alias);
                    allocator.free(value_ref.value_name);
                }
                const module_value = modules.lookupValue(value_ref.module_alias, value_ref.value_name) orelse {
                    try stderr.print(
                        "{s}:{d}: unknown module value {s}.{s}\n",
                        .{ script_path, line_number, value_ref.module_alias, value_ref.value_name },
                    );
                    return 1;
                };
                context.declareStringBinding(binding.name, module_value.literal, binding.is_mutable) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, binding.name, err);
                    return 1;
                };
            },
            .object_literal => |literal| {
                var literal_copy = literal;
                defer literal_copy.deinit(allocator);

                var failure_reference: ?[]const u8 = null;
                var object_value = instantiateObjectLiteral(
                    allocator,
                    context,
                    &literal_copy,
                    &failure_reference,
                ) catch |err| switch (err) {
                    error.UnknownBinding, error.TypeMismatch => {
                        const reference = failure_reference orelse binding.name;
                        try renderReferenceError(stderr, script_path, line_number, reference, err);
                        return 1;
                    },
                    else => return err,
                };
                var object_valid = true;
                defer if (object_valid) object_value.deinit(allocator);

                context.declareObjectBinding(binding.name, &object_value, binding.is_mutable) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, binding.name, err);
                    return 1;
                };
                object_valid = false;
            },
            .binding_expression => |expression| {
                var expr = expression;
                defer expr.deinit(allocator);

                const resolved = switch (expr) {
                    .identifier => |name| resolveStringBinding(context, name) catch |err| {
                        try renderReferenceError(stderr, script_path, line_number, name, err);
                        return 1;
                    },
                    .object_field => |field| blk: {
                        const object_binding = resolveObjectBinding(context, field.object_name) catch |err| {
                            try renderReferenceError(stderr, script_path, line_number, field.object_name, err);
                            return 1;
                        };
                        if (object_binding.getField(field.field_name)) |value| break :blk value;

                        const combined = try std.fmt.allocPrint(allocator, "{s}.{s}", .{
                            field.object_name,
                            field.field_name,
                        });
                        defer allocator.free(combined);
                        try renderReferenceError(stderr, script_path, line_number, combined, error.UnknownBinding);
                        return 1;
                    },
                    .array_element => |element| blk: {
                        const array_binding = resolveStringArrayBinding(context, element.array_name) catch |err| {
                            try renderReferenceError(stderr, script_path, line_number, element.array_name, err);
                            return 1;
                        };
                        if (element.index >= array_binding.len) {
                            try stderr.print(
                                "{s}:{d}: index {d} out of bounds for binding '{s}' (len {d})\n",
                                .{ script_path, line_number, element.index, element.array_name, array_binding.len },
                            );
                            return 1;
                        }
                        break :blk array_binding[element.index];
                    },
                };

                context.declareStringBinding(binding.name, resolved, binding.is_mutable) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, binding.name, err);
                    return 1;
                };
            },
        }
        return null;
    }

    const maybe_assignment = parseIdentifierAssignment(allocator, command) catch |err| {
        try stderr.print(
            "{s}:{d}: invalid assignment ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return 1;
    };

    if (maybe_assignment) |assignment_value| {
        var assignment = assignment_value;
        defer assignment.deinit(allocator);
        switch (assignment.value) {
            .literal => |literal_value| {
                context.assignStringBinding(assignment.name, literal_value) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, assignment.name, err);
                    return 1;
                };
            },
            .arithmetic => |addition| {
                const current = resolveStringBinding(context, assignment.name) catch |err| {
                    try renderReferenceError(stderr, script_path, line_number, assignment.name, err);
                    return 1;
                };
                const parsed = std.fmt.parseInt(i64, current, 10) catch {
                    try stderr.print(
                        "{s}:{d}: binding '{s}' is not numeric\n",
                        .{ script_path, line_number, assignment.name },
                    );
                    return 1;
                };
                const next = std.math.add(i64, parsed, addition.delta) catch {
                    try stderr.print(
                        "{s}:{d}: arithmetic overflow updating '{s}'\n",
                        .{ script_path, line_number, assignment.name },
                    );
                    return 1;
                };
                const literal = try std.fmt.allocPrint(allocator, "{d}", .{next});
                defer allocator.free(literal);

                context.assignStringBinding(assignment.name, literal) catch |err| {
                    try renderBindingError(stderr, script_path, line_number, assignment.name, err);
                    return 1;
                };
            },
        }
        return null;
    }

    var invocation_result = parseModuleInvocation(allocator, command, context) catch |err| switch (err) {
        ModuleInvocationError.UnsupportedCatchClause => {
            try stderr.print(
                "{s}:{d}: module invocations cannot include 'catch' clauses yet\n",
                .{ script_path, line_number },
            );
            return 1;
        },
        else => {
            try stderr.print(
                "{s}:{d}: invalid module invocation ({s})\n",
                .{ script_path, line_number, @errorName(err) },
            );
            return 1;
        },
    };

    if (invocation_result) |*invocation| {
        defer invocation.deinit(allocator);
        return try executeModuleInvocation(
            allocator,
            runner,
            env_map,
            context,
            modules,
            invocation.*,
            command,
            script_path,
            line_number,
            stdout,
            stderr,
        );
    } else if (unknownFunctionBindingNameFromExpression(command, context)) |fn_name| {
        try stderr.print(
            "{s}:{d}: unknown function binding '{s}'\n",
            .{ script_path, line_number, fn_name },
        );
        return 1;
    }

    const ensure_candidate = parseEnsureCall(command) catch |err| {
        try stderr.print(
            "{s}:{d}: invalid ensure statement ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return 1;
    };
    if (ensure_candidate) |ensure_stmt| {
        return try handleEnsureCall(
            allocator,
            context,
            ensure_stmt,
            script_path,
            line_number,
            script_label,
            stdout,
            stderr,
        );
    }

    switch (try recordAstSnippet(allocator, command, script_path, line_number, stderr)) {
        .recorded => |snippet| {
            var owned = snippet;
            defer owned.deinit(allocator);
            if (interpreter.executeSnippet(context, &owned)) {
                return null;
            } else |err| {
                try renderInterpreterError(stderr, &owned, err);
                return 1;
            }
        },
        .skipped => {},
        .syntax_error => return 1,
    }

    const bindings = BindingProvider{ .context = context };
    return try runCommandPipeline(
        allocator,
        runner,
        env_map,
        bindings,
        command,
        command,
        script_path,
        line_number,
        stdout,
        stderr,
    );
}

fn executeModuleInvocation(
    allocator: Allocator,
    runner: *CommandRunner,
    env_map: *std.process.EnvMap,
    context: *ScriptContext,
    modules: *ModuleRegistry,
    invocation: ModuleInvocation,
    display_command: []const u8,
    script_path: []const u8,
    line_number: usize,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !?u8 {
    const outcome = try runModuleInvocationInternal(
        allocator,
        runner,
        env_map,
        context,
        modules,
        invocation,
        display_command,
        script_path,
        line_number,
        stdout,
        stderr,
        false,
        true,
    );
    if (outcome.string_value) |literal| {
        defer allocator.free(literal);
        try stdout.print("{s}\n", .{literal});
        return null;
    }
    if (outcome.exit_code) |code| return code;
    return null;
}

const InterpolationError = error{
    ArrayBindingUnsupported,
} || Allocator.Error;

const PipelineOutcome = struct {
    handle: ?ProcessHandle = null,
    exit_code: ?u8 = null,
    string_value: ?[]const u8 = null,
};

fn runCommandPipeline(
    allocator: Allocator,
    runner: *CommandRunner,
    env_map: *std.process.EnvMap,
    bindings: BindingProvider,
    command_to_run: []const u8,
    display_command: []const u8,
    script_path: []const u8,
    line_number: usize,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !?u8 {
    const outcome = try runPipelineInternal(
        allocator,
        runner,
        env_map,
        bindings,
        command_to_run,
        display_command,
        script_path,
        line_number,
        stdout,
        stderr,
        false,
        true,
    );
    if (outcome.exit_code) |code| return code;
    return null;
}

fn runModuleInvocationInternal(
    allocator: Allocator,
    runner: *CommandRunner,
    env_map: *std.process.EnvMap,
    context: *ScriptContext,
    modules: *ModuleRegistry,
    invocation: ModuleInvocation,
    display_command: []const u8,
    script_path: []const u8,
    line_number: usize,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    capture_handle: bool,
    render_failures: bool,
) !PipelineOutcome {
    const lookup = modules.lookupFunction(invocation.alias, invocation.function) orelse {
        try stderr.print(
            "{s}:{d}: unknown module function {s}.{s}\n",
            .{ script_path, line_number, invocation.alias, invocation.function },
        );
        return .{ .exit_code = 1 };
    };

    const function = lookup.function;
    if (function.params.len != invocation.args.len) {
        try stderr.print(
            "{s}:{d}: expected {d} arguments for {s}.{s} but received {d}\n",
            .{ script_path, line_number, function.params.len, invocation.alias, invocation.function, invocation.args.len },
        );
        return .{ .exit_code = 1 };
    }
    var overrides: []BindingProvider.Binding = &[_]BindingProvider.Binding{};
    if (function.params.len > 0) {
        overrides = try allocator.alloc(BindingProvider.Binding, function.params.len);
        for (function.params, 0..) |param_name, idx| {
            overrides[idx] = .{ .name = param_name, .value = invocation.args[idx] };
        }
    }
    defer if (overrides.len > 0) allocator.free(overrides);

    if (try modules.evaluateFunctionLiteral(allocator, lookup.module, function, overrides)) |literal_value| {
        return .{ .string_value = literal_value };
    }

    const provider = BindingProvider{ .context = context, .overrides = overrides };
    return try runPipelineInternal(
        allocator,
        runner,
        env_map,
        provider,
        function.command,
        display_command,
        script_path,
        line_number,
        stdout,
        stderr,
        capture_handle,
        render_failures,
    );
}

fn executeCommandBinding(
    allocator: Allocator,
    runner: *CommandRunner,
    env_map: *std.process.EnvMap,
    context: *ScriptContext,
    command_text: []const u8,
    binding_name: []const u8,
    is_mutable: bool,
    script_path: []const u8,
    line_number: usize,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !?u8 {
    const bindings = BindingProvider{ .context = context };
    const outcome = try runPipelineInternal(
        allocator,
        runner,
        env_map,
        bindings,
        command_text,
        command_text,
        script_path,
        line_number,
        stdout,
        stderr,
        true,
        false,
    );

    if (outcome.exit_code) |code| return code;
    var handle = outcome.handle orelse return null;
    defer handle.deinit();

    var snapshot = handle.destructure(allocator) catch |err| {
        try stderr.print(
            "{s}:{d}: failed to capture pipeline output ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return 1;
    };
    var snapshot_valid = true;
    defer if (snapshot_valid) snapshot.deinit();

    context.declareProcessBinding(binding_name, snapshot, is_mutable) catch |err| {
        try renderBindingError(stderr, script_path, line_number, binding_name, err);
        return 1;
    };
    snapshot_valid = false;

    return null;
}

fn executeModuleInvocationBinding(
    allocator: Allocator,
    runner: *CommandRunner,
    env_map: *std.process.EnvMap,
    context: *ScriptContext,
    modules: *ModuleRegistry,
    invocation: ModuleInvocation,
    binding_name: []const u8,
    is_mutable: bool,
    display_command: []const u8,
    script_path: []const u8,
    line_number: usize,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !?u8 {
    const outcome = try runModuleInvocationInternal(
        allocator,
        runner,
        env_map,
        context,
        modules,
        invocation,
        display_command,
        script_path,
        line_number,
        stdout,
        stderr,
        true,
        false,
    );
    if (outcome.string_value) |literal| {
        defer allocator.free(literal);
        context.declareStringBinding(binding_name, literal, is_mutable) catch |err| {
            try renderBindingError(stderr, script_path, line_number, binding_name, err);
            return 1;
        };
        return null;
    }
    if (outcome.exit_code) |code| return code;

    var handle = outcome.handle orelse {
        try stderr.print(
            "{s}:{d}: module function {s}.{s} produced no command to capture\n",
            .{ script_path, line_number, invocation.alias, invocation.function },
        );
        return 1;
    };
    defer handle.deinit();

    var snapshot = handle.destructure(allocator) catch |err| {
        try stderr.print(
            "{s}:{d}: failed to capture pipeline output ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return 1;
    };
    var snapshot_valid = true;
    defer if (snapshot_valid) snapshot.deinit();

    context.declareProcessBinding(binding_name, snapshot, is_mutable) catch |err| {
        try renderBindingError(stderr, script_path, line_number, binding_name, err);
        return 1;
    };
    snapshot_valid = false;

    return null;
}

fn runPipelineInternal(
    allocator: Allocator,
    runner: *CommandRunner,
    env_map: *std.process.EnvMap,
    bindings: BindingProvider,
    command_to_run: []const u8,
    display_command: []const u8,
    script_path: []const u8,
    line_number: usize,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    capture_handle: bool,
    render_failures: bool,
) !PipelineOutcome {
    const normalized_command = trimCommandExpression(command_to_run);

    var tokens = TokenList.init(allocator);
    defer tokens.deinit();
    tokens.populate(normalized_command) catch |err| {
        try stderr.print(
            "{s}:{d}: invalid pipeline syntax ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return .{ .exit_code = 1 };
    };
    if (tokens.items.len == 0) return .{};
    var interpolation_issue: ?[]const u8 = null;
    interpolateTokensWithBindings(&tokens, bindings, &interpolation_issue) catch |err| switch (err) {
        error.ArrayBindingUnsupported => {
            const binding_name = interpolation_issue orelse "binding";
            try stderr.print(
                "{s}:{d}: cannot interpolate array binding '{s}'; convert it to a string first\n",
                .{ script_path, line_number, binding_name },
            );
            return .{ .exit_code = 1 };
        },
        else => return err,
    };

    var pipeline_builder = Pipeline.init(allocator);
    defer pipeline_builder.deinit();
    pipeline_builder.build(tokens.items) catch |err| {
        try stderr.print(
            "{s}:{d}: unable to build pipeline ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return .{ .exit_code = 1 };
    };
    try rewriteBuiltinCommands(allocator, pipeline_builder.specs);

    for (pipeline_builder.specs) |*spec| {
        spec.env_map = env_map;
    }

    var handle = runner.*.runPipeline(pipeline_builder.specs) catch |err| {
        try renderScriptRunnerError(allocator, stderr, script_path, line_number, display_command, err);
        return .{ .exit_code = 1 };
    };
    defer {
        if (!capture_handle) handle.deinit();
    }

    try forwardHandleOutput(&handle, stdout, stderr);

    if (!handle.status.ok and render_failures) {
        try renderScriptStageFailure(
            allocator,
            stderr,
            script_path,
            line_number,
            pipeline_builder.specs,
            &handle,
            display_command,
        );
        const exit_code = handle.status.exit_code orelse 1;
        if (capture_handle) {
            handle.deinit();
        }
        return .{ .exit_code = exit_code };
    }

    if (capture_handle) {
        return .{ .handle = handle };
    }

    return .{};
}

fn trimCommandExpression(command: []const u8) []const u8 {
    var trimmed = std.mem.trim(u8, command, " \t\r\n");
    while (hasWrappingParentheses(trimmed)) {
        const inner = std.mem.trim(u8, trimmed[1 .. trimmed.len - 1], " \t\r\n");
        if (inner.len == 0) break;
        trimmed = inner;
    }
    return trimmed;
}

fn hasWrappingParentheses(command: []const u8) bool {
    if (command.len < 2) return false;
    if (command[0] != '(' or command[command.len - 1] != ')') return false;

    var depth: usize = 0;
    var in_single = false;
    var in_double = false;
    var escape = false;

    for (command, 0..) |byte, idx| {
        if (escape) {
            escape = false;
            continue;
        }
        if (!in_single and byte == '\\') {
            escape = true;
            continue;
        }
        if (!in_double and byte == '\'') {
            in_single = !in_single;
            continue;
        }
        if (!in_single and byte == '"') {
            in_double = !in_double;
            continue;
        }
        if (in_single or in_double) continue;

        switch (byte) {
            '(' => depth += 1,
            ')' => {
                if (depth == 0) return false;
                depth -= 1;
                if (depth == 0 and idx != command.len - 1) {
                    return false;
                }
            },
            else => {},
        }
    }

    return depth == 0;
}

fn forwardHandleOutput(handle: *const ProcessHandle, stdout: *std.Io.Writer, stderr: *std.Io.Writer) !void {
    const captures = handle.stageCaptures();
    if (captures.len == 0) return;

    const stdout_bytes = handle.stdoutBytes();
    if (stdout_bytes.len > 0) try stdout.writeAll(stdout_bytes);

    const stderr_bytes = handle.stderrBytes();
    if (stderr_bytes.len > 0) try stderr.writeAll(stderr_bytes);

    try stdout.flush();
    try stderr.flush();
}

fn interpolateTokensWithBindings(
    tokens: *TokenList,
    bindings: BindingProvider,
    offending_binding: *?[]const u8,
) InterpolationError!void {
    offending_binding.* = null;
    for (tokens.items, 0..) |token, idx| {
        if (token.len == 1 and token[0] == '|') continue;
        if (std.mem.indexOfScalar(u8, token, '$') == null) continue;
        const replacement = try interpolateToken(tokens.allocator, token, bindings, offending_binding);
        if (replacement) |value| {
            tokens.allocator.free(token);
            tokens.items[idx] = value;
        }
    }
}

fn interpolateToken(
    allocator: Allocator,
    token: []const u8,
    bindings: BindingProvider,
    offending_binding: *?[]const u8,
) InterpolationError!?[]const u8 {
    var idx: usize = 0;
    var last_emit: usize = 0;
    var changed = false;
    var builder = std.ArrayList(u8).empty;
    defer builder.deinit(allocator);

    while (idx < token.len) {
        if (token[idx] != '$' or idx + 1 >= token.len or token[idx + 1] != '{') {
            idx += 1;
            continue;
        }
        const close = std.mem.indexOfScalarPos(u8, token, idx + 2, '}') orelse break;
        const name = token[idx + 2 .. close];
        if (bindings.get(name)) |value| {
            if (!changed) changed = true;
            try builder.appendSlice(allocator, token[last_emit..idx]);
            try builder.appendSlice(allocator, value);
            last_emit = close + 1;
        } else if (bindings.hasArrayBinding(name)) {
            offending_binding.* = name;
            return InterpolationError.ArrayBindingUnsupported;
        }
        idx = close + 1;
    }

    if (!changed) return null;

    if (last_emit < token.len) {
        try builder.appendSlice(allocator, token[last_emit..]);
    }

    return try builder.toOwnedSlice(allocator);
}

fn renderScriptStageFailure(
    allocator: Allocator,
    stderr: *std.Io.Writer,
    script_path: []const u8,
    line_number: usize,
    specs: []const CommandRunner.CommandSpec,
    handle: *const ProcessHandle,
    command: []const u8,
) !void {
    if (specs.len == 0) return;
    const failed_idx = handle.status.failed_stage orelse (specs.len - 1);
    const statuses = handle.stageStatuses();
    if (failed_idx >= statuses.len) return;

    var trace = StackTrace.init(allocator);
    defer trace.deinit();

    const stage_label = try std.fmt.allocPrint(allocator, "stage {d}", .{failed_idx + 1});
    defer allocator.free(stage_label);

    const detail = try describeStage(allocator, specs[failed_idx].argv, statuses[failed_idx]);
    defer allocator.free(detail);

    try trace.push(.{
        .label = stage_label,
        .detail = detail,
        .location = .{ .file = script_path, .line = line_number, .column = 1 },
    });
    try trace.push(.{ .label = "pipeline", .detail = command });
    try trace.render(stderr);
}

fn renderBindingError(
    stderr: *std.Io.Writer,
    script_path: []const u8,
    line_number: usize,
    name: []const u8,
    err: ScriptContext.BindingError,
) !void {
    switch (err) {
        error.DuplicateBinding => try stderr.print(
            "{s}:{d}: duplicate binding '{s}'\n",
            .{ script_path, line_number, name },
        ),
        error.ImmutableBinding => try stderr.print(
            "{s}:{d}: cannot modify immutable binding '{s}'\n",
            .{ script_path, line_number, name },
        ),
        error.UnknownBinding => try stderr.print(
            "{s}:{d}: unknown binding '{s}'\n",
            .{ script_path, line_number, name },
        ),
        error.TypeMismatch => try stderr.print(
            "{s}:{d}: binding '{s}' has incompatible type\n",
            .{ script_path, line_number, name },
        ),
        else => try stderr.print(
            "{s}:{d}: failed to store binding ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        ),
    }
}

fn renderReferenceError(
    stderr: *std.Io.Writer,
    script_path: []const u8,
    line_number: usize,
    reference: []const u8,
    err: ScriptContext.BindingError,
) !void {
    switch (err) {
        error.UnknownBinding => try stderr.print(
            "{s}:{d}: unknown binding '{s}'\n",
            .{ script_path, line_number, reference },
        ),
        error.TypeMismatch => try stderr.print(
            "{s}:{d}: binding '{s}' has incompatible type\n",
            .{ script_path, line_number, reference },
        ),
        else => try stderr.print(
            "{s}:{d}: failed to resolve binding '{s}' ({s})\n",
            .{ script_path, line_number, reference, @errorName(err) },
        ),
    }
}

fn resolveStringBinding(context: *ScriptContext, name: []const u8) ScriptContext.BindingError![]const u8 {
    if (context.getStringBinding(name)) |value| return value;
    return determineStringBindingError(context, name);
}

fn resolveStringArrayBinding(
    context: *ScriptContext,
    name: []const u8,
) ScriptContext.BindingError![]const []const u8 {
    if (context.getStringArrayBinding(name)) |value| return value;
    return determineArrayBindingError(context, name);
}

fn resolveObjectBinding(
    context: *ScriptContext,
    name: []const u8,
) ScriptContext.BindingError!*const ScriptContext.ObjectValue {
    if (context.getObjectBinding(name)) |value| return value;
    return determineObjectBindingError(context, name);
}

fn determineStringBindingError(context: *ScriptContext, name: []const u8) ScriptContext.BindingError {
    if (context.getStringArrayBinding(name) != null) return error.TypeMismatch;
    if (context.getProcessBinding(name) != null) return error.TypeMismatch;
    if (context.getFunctionBinding(name) != null) return error.TypeMismatch;
    if (context.getObjectBinding(name) != null) return error.TypeMismatch;
    if (context.getTypeBinding(name) != null) return error.TypeMismatch;
    return error.UnknownBinding;
}

fn determineArrayBindingError(context: *ScriptContext, name: []const u8) ScriptContext.BindingError {
    if (context.getStringBinding(name) != null) return error.TypeMismatch;
    if (context.getProcessBinding(name) != null) return error.TypeMismatch;
    if (context.getFunctionBinding(name) != null) return error.TypeMismatch;
    if (context.getObjectBinding(name) != null) return error.TypeMismatch;
    if (context.getTypeBinding(name) != null) return error.TypeMismatch;
    return error.UnknownBinding;
}

fn determineObjectBindingError(context: *ScriptContext, name: []const u8) ScriptContext.BindingError {
    if (context.getStringBinding(name) != null) return error.TypeMismatch;
    if (context.getStringArrayBinding(name) != null) return error.TypeMismatch;
    if (context.getProcessBinding(name) != null) return error.TypeMismatch;
    if (context.getFunctionBinding(name) != null) return error.TypeMismatch;
    if (context.getTypeBinding(name) != null) return error.TypeMismatch;
    return error.UnknownBinding;
}

fn renderScriptRunnerError(
    allocator: Allocator,
    stderr: *std.Io.Writer,
    script_path: []const u8,
    line_number: usize,
    command: []const u8,
    err: anyerror,
) !void {
    var trace = StackTrace.init(allocator);
    defer trace.deinit();

    try trace.push(.{ .label = "command runner", .detail = @errorName(err) });
    try trace.push(.{
        .label = "pipeline",
        .detail = command,
        .location = .{ .file = script_path, .line = line_number, .column = 1 },
    });
    try trace.render(stderr);
}

fn applyEnvOverridesToMap(env_map: *std.process.EnvMap, overrides: []const CliConfig.EnvOverride) !void {
    for (overrides) |override| {
        try env_map.put(override.key, override.value);
    }
}

fn exposeScriptMetadata(env_map: *std.process.EnvMap, script: CliConfig.ScriptInvocation) !void {
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

fn ensureExecutableDirOnPath(allocator: Allocator, env_map: *std.process.EnvMap) !void {
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

fn rewriteBuiltinCommands(allocator: Allocator, specs: []CommandRunner.CommandSpec) !void {
    for (specs) |*spec| {
        if (spec.argv.len == 0) continue;
        const command = spec.argv[0];
        if (std.mem.eql(u8, command, "upper") and spec.argv.len == 1) {
            try rewriteUpperStage(allocator, spec);
        }
    }
}

fn rewriteUpperStage(allocator: Allocator, spec: *CommandRunner.CommandSpec) !void {
    const replacement = try allocator.alloc([]const u8, 3);
    replacement[0] = "tr";
    replacement[1] = "[:lower:]";
    replacement[2] = "[:upper:]";
    allocator.free(spec.argv);
    spec.argv = replacement;
}

fn trimLineEnding(line: []const u8) []const u8 {
    if (line.len == 0) return line;
    if (line[line.len - 1] == '\r') return line[0 .. line.len - 1];
    return line;
}

fn isCommentLine(trimmed: []const u8) bool {
    if (trimmed.len == 0) return false;
    if (trimmed[0] == '#') return true;
    if (trimmed.len >= 2 and trimmed[0] == '/' and trimmed[1] == '/') return true;
    return false;
}

fn braceBalanceDelta(line: []const u8) isize {
    var delta: isize = 0;
    var in_string = false;
    var escape = false;
    for (line) |ch| {
        if (in_string) {
            if (escape) {
                escape = false;
                continue;
            }
            if (ch == '\\') {
                escape = true;
                continue;
            }
            if (ch == '"') {
                in_string = false;
            }
            continue;
        }
        if (ch == '"') {
            in_string = true;
            continue;
        }
        switch (ch) {
            '{' => delta += 1,
            '}' => delta -= 1,
            else => {},
        }
    }
    return delta;
}

const QuoteMode = enum {
    none,
    single,
    double,
};

fn letCommandNeedsMoreLines(command: []const u8) bool {
    if (!commandStartsWithLetOrMut(command)) return false;

    const eq_index = findAssignmentEquals(command) orelse return false;
    if (eq_index + 1 >= command.len) return true;

    var value_slice = command[eq_index + 1 ..];
    value_slice = std.mem.trimLeft(u8, value_slice, " \t\r\n");
    if (value_slice.len == 0) return true;

    var braces: isize = 0;
    var parens: isize = 0;
    var mode: QuoteMode = .none;
    var escape = false;
    var idx: usize = 0;
    while (idx < value_slice.len) : (idx += 1) {
        const ch = value_slice[idx];
        switch (mode) {
            .double => {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '"') mode = .none;
                continue;
            },
            .single => {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '\'') mode = .none;
                continue;
            },
            .none => {
                if (ch == '"') {
                    mode = .double;
                    continue;
                }
                if (ch == '\'') {
                    mode = .single;
                    continue;
                }
                if (ch == '#') break;
                if (ch == '/' and idx + 1 < value_slice.len and value_slice[idx + 1] == '/') break;
                switch (ch) {
                    '{' => braces += 1,
                    '}' => braces -= 1,
                    '(' => parens += 1,
                    ')' => parens -= 1,
                    else => {},
                }
            },
        }
    }

    return braces > 0 or parens > 0;
}

fn controlFlowNeedsMoreLines(command: []const u8) bool {
    if (!commandStartsWithIf(command)) return false;

    var braces: isize = 0;
    var mode: QuoteMode = .none;
    var escape = false;
    var idx: usize = 0;

    while (idx < command.len) {
        const ch = command[idx];
        switch (mode) {
            .double => {
                if (escape) {
                    escape = false;
                } else if (ch == '\\') {
                    escape = true;
                } else if (ch == '"') {
                    mode = .none;
                }
                idx += 1;
                continue;
            },
            .single => {
                if (escape) {
                    escape = false;
                } else if (ch == '\\') {
                    escape = true;
                } else if (ch == '\'') {
                    mode = .none;
                }
                idx += 1;
                continue;
            },
            .none => {
                if (ch == '"') {
                    mode = .double;
                    idx += 1;
                    continue;
                }
                if (ch == '\'') {
                    mode = .single;
                    idx += 1;
                    continue;
                }
                if (ch == '#') {
                    idx += 1;
                    while (idx < command.len and command[idx] != '\n') {
                        idx += 1;
                    }
                    continue;
                }
                if (ch == '/' and idx + 1 < command.len and command[idx + 1] == '/') {
                    idx += 2;
                    while (idx < command.len and command[idx] != '\n') {
                        idx += 1;
                    }
                    continue;
                }
                if (ch == '{') {
                    braces += 1;
                } else if (ch == '}' and braces > 0) {
                    braces -= 1;
                }
                idx += 1;
            },
        }
    }

    return braces > 0;
}

fn sanitizeControlFlowSnippet(allocator: Allocator, command: []const u8) ![]u8 {
    var builder = std.ArrayList(u8).empty;
    defer builder.deinit(allocator);

    var idx: usize = 0;
    while (idx < command.len) {
        const ch = command[idx];
        if (ch == '{') {
            try builder.append(allocator, '{');
            try builder.appendSlice(allocator, " value ");
            idx = skipControlFlowBlock(command, idx + 1);
            if (idx < command.len and command[idx] == '}') {
                try builder.append(allocator, '}');
                idx += 1;
            }
            continue;
        }
        try builder.append(allocator, ch);
        idx += 1;
    }

    return builder.toOwnedSlice(allocator);
}

fn skipControlFlowBlock(command: []const u8, start_idx: usize) usize {
    var idx = start_idx;
    var depth: isize = 1;
    var mode: QuoteMode = .none;
    var escape = false;

    while (idx < command.len) {
        const ch = command[idx];
        switch (mode) {
            .double => {
                if (escape) {
                    escape = false;
                } else if (ch == '\\') {
                    escape = true;
                } else if (ch == '"') {
                    mode = .none;
                }
                idx += 1;
                continue;
            },
            .single => {
                if (escape) {
                    escape = false;
                } else if (ch == '\\') {
                    escape = true;
                } else if (ch == '\'') {
                    mode = .none;
                }
                idx += 1;
                continue;
            },
            .none => {
                if (ch == '"') {
                    mode = .double;
                    idx += 1;
                    continue;
                }
                if (ch == '\'') {
                    mode = .single;
                    idx += 1;
                    continue;
                }
                if (ch == '#') {
                    idx += 1;
                    while (idx < command.len and command[idx] != '\n') {
                        idx += 1;
                    }
                    continue;
                }
                if (ch == '/' and idx + 1 < command.len and command[idx + 1] == '/') {
                    idx += 2;
                    while (idx < command.len and command[idx] != '\n') {
                        idx += 1;
                    }
                    continue;
                }
                if (ch == '{') {
                    depth += 1;
                    idx += 1;
                    continue;
                }
                if (ch == '}') {
                    depth -= 1;
                    break;
                }
                idx += 1;
            },
        }
    }

    return idx;
}

fn errorDeclarationNeedsMoreLines(command: []const u8) bool {
    var trimmed = std.mem.trimLeft(u8, command, " \t");
    if (!std.mem.startsWith(u8, trimmed, "error")) return false;
    if (trimmed.len == "error".len) return true;
    if (!std.ascii.isWhitespace(trimmed["error".len])) return false;

    const eq_index = findAssignmentEquals(trimmed) orelse return true;
    if (eq_index + 1 >= trimmed.len) return true;

    var body_slice = trimmed[eq_index + 1 ..];
    body_slice = std.mem.trimLeft(u8, body_slice, " \t\r\n");
    if (body_slice.len == 0) return true;

    var braces: isize = 0;
    var mode: QuoteMode = .none;
    var escape = false;
    var saw_open = false;
    var idx: usize = 0;

    while (idx < body_slice.len) : (idx += 1) {
        const ch = body_slice[idx];
        switch (mode) {
            .double => {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '"') mode = .none;
                continue;
            },
            .single => {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '\'') mode = .none;
                continue;
            },
            .none => {
                if (ch == '"') {
                    mode = .double;
                    continue;
                }
                if (ch == '\'') {
                    mode = .single;
                    continue;
                }
                if (ch == '#') break;
                if (ch == '/' and idx + 1 < body_slice.len and body_slice[idx + 1] == '/') break;
                switch (ch) {
                    '{' => {
                        braces += 1;
                        saw_open = true;
                    },
                    '}' => braces -= 1,
                    else => {},
                }
            },
        }
    }

    if (!saw_open) return true;
    return braces > 0;
}

fn commandStartsWithIf(command: []const u8) bool {
    var idx: usize = 0;
    while (idx < command.len and (command[idx] == ' ' or command[idx] == '\t')) {
        idx += 1;
    }
    if (idx + 1 >= command.len) return false;
    if (!std.mem.startsWith(u8, command[idx..], "if")) return false;
    const suffix_index = idx + 2;
    if (suffix_index >= command.len) return true;
    const suffix = command[suffix_index];
    if (isIdentifierChar(suffix)) return false;
    if (suffix == '(') return true;
    return std.ascii.isWhitespace(suffix);
}

fn commandStartsWithLetOrMut(command: []const u8) bool {
    if (command.len < 3) return false;
    if (std.mem.startsWith(u8, command, "let")) {
        if (command.len == 3) return true;
        const suffix = command[3];
        return std.ascii.isWhitespace(suffix);
    }
    if (std.mem.startsWith(u8, command, "mut")) {
        if (command.len == 3) return true;
        const suffix = command[3];
        return std.ascii.isWhitespace(suffix);
    }
    return false;
}

fn findAssignmentEquals(command: []const u8) ?usize {
    var mode: QuoteMode = .none;
    var escape = false;
    var idx: usize = 0;
    while (idx < command.len) : (idx += 1) {
        const ch = command[idx];
        switch (mode) {
            .double => {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '"') mode = .none;
                continue;
            },
            .single => {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '\'') mode = .none;
                continue;
            },
            .none => {
                if (ch == '"') {
                    mode = .double;
                    continue;
                }
                if (ch == '\'') {
                    mode = .single;
                    continue;
                }
                if (ch == '=') return idx;
            },
        }
    }
    return null;
}

fn isFunctionDeclaration(command: []const u8) bool {
    const trimmed = std.mem.trimLeft(u8, command, " \t");
    if (trimmed.len < 2) return false;
    if (!std.mem.startsWith(u8, trimmed, "fn")) return false;
    if (trimmed.len == 2) return true;
    const suffix = trimmed[2];
    return std.ascii.isWhitespace(suffix);
}

fn computeFunctionSkipDepth(command: []const u8) isize {
    const delta = braceBalanceDelta(command);
    if (delta > 0) return delta;
    if (std.mem.indexOfScalar(u8, command, '{') == null) return 1;
    return 0;
}

fn computeScriptLabel(allocator: Allocator, script_path: []const u8) ![]u8 {
    const base = std.fs.path.basename(script_path);
    var stem = base;
    if (stem.len > 3 and std.mem.endsWith(u8, stem, ".rn")) {
        stem = stem[0 .. stem.len - 3];
    }
    var label = try allocator.alloc(u8, stem.len);
    for (stem, 0..) |ch, idx| {
        label[idx] = if (ch == '_') '-' else ch;
    }
    return label;
}

fn parseEnsureCall(command: []const u8) EnsureParseError!?EnsureCall {
    const trimmed = std.mem.trim(u8, command, " \t");
    if (!std.mem.startsWith(u8, trimmed, "ensure")) return null;
    if (trimmed.len <= 6) return error.InvalidEnsure;

    var idx: usize = 6;
    if (idx < trimmed.len and trimmed[idx] != '(') {
        if (!std.ascii.isWhitespace(trimmed[idx])) return null;
        while (idx < trimmed.len and std.ascii.isWhitespace(trimmed[idx])) : (idx += 1) {}
        if (idx >= trimmed.len or trimmed[idx] != '(') return error.InvalidEnsure;
    }
    idx += 1;

    var depth: usize = 1;
    var comma_index: ?usize = null;
    var cursor: usize = idx;
    var in_string = false;
    var escape = false;

    ensure_scan: while (cursor < trimmed.len) : (cursor += 1) {
        const ch = trimmed[cursor];
        if (in_string) {
            if (escape) {
                escape = false;
                continue;
            }
            if (ch == '\\') {
                escape = true;
                continue;
            }
            if (ch == '"') {
                in_string = false;
            }
            continue;
        }
        switch (ch) {
            '"' => in_string = true,
            '(' => depth += 1,
            ')' => {
                if (depth == 0) return error.InvalidEnsure;
                depth -= 1;
                if (depth == 0) break :ensure_scan;
            },
            ',' => {
                if (depth == 1 and comma_index == null) comma_index = cursor;
            },
            else => {},
        }
    }

    if (depth != 0) return error.InvalidEnsure;
    const close_index = cursor;
    const comma_pos = comma_index orelse return error.InvalidEnsure;
    if (comma_pos <= idx or comma_pos >= close_index) return error.InvalidEnsure;

    const condition_slice = std.mem.trim(u8, trimmed[idx..comma_pos], " \t");
    const label_slice = std.mem.trim(u8, trimmed[comma_pos + 1 .. close_index], " \t");
    if (condition_slice.len == 0 or label_slice.len == 0) return error.InvalidEnsure;

    for (trimmed[close_index + 1 ..]) |ch| {
        if (!std.ascii.isWhitespace(ch)) return error.InvalidEnsure;
    }

    return EnsureCall{
        .condition = condition_slice,
        .label = label_slice,
    };
}

fn handleEnsureCall(
    allocator: Allocator,
    context: *ScriptContext,
    ensure_call: EnsureCall,
    script_path: []const u8,
    line_number: usize,
    script_label: []const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !?u8 {
    var condition_value = evaluateExpression(allocator, context, ensure_call.condition) catch |err| {
        try stderr.print(
            "{s}:{d}: failed to evaluate ensure condition ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return 1;
    };
    defer condition_value.deinit(allocator);

    if (condition_value.value != .boolean) {
        try stderr.print(
            "{s}:{d}: ensure condition must resolve to a boolean\n",
            .{ script_path, line_number },
        );
        return 1;
    }

    var label_value = evaluateExpression(allocator, context, ensure_call.label) catch |err| {
        try stderr.print(
            "{s}:{d}: failed to evaluate ensure label ({s})\n",
            .{ script_path, line_number, @errorName(err) },
        );
        return 1;
    };
    defer label_value.deinit(allocator);

    if (label_value.value != .string) {
        try stderr.print(
            "{s}:{d}: ensure label must be a string literal\n",
            .{ script_path, line_number },
        );
        return 1;
    }

    if (condition_value.value.boolean) return null;

    try stdout.print("FAIL: {s} => {s}\n", .{ script_label, label_value.value.string });
    try stdout.flush();
    return 1;
}

const ExprValue = union(enum) {
    boolean: bool,
    string: []const u8,
};

const EvaluatedValue = struct {
    value: ExprValue,
    owned: bool = false,

    fn deinit(self: *EvaluatedValue, allocator: Allocator) void {
        if (self.owned) {
            switch (self.value) {
                .string => allocator.free(self.value.string),
                else => {},
            }
        }
    }
};

fn evaluateExpression(
    allocator: Allocator,
    context: *ScriptContext,
    expression: []const u8,
) EnsureEvalError!EvaluatedValue {
    const trimmed = std.mem.trim(u8, expression, " \t");
    if (trimmed.len == 0) return error.InvalidExpression;

    if (findTopLevelComparison(trimmed)) |pos| {
        const left_expr = trimmed[0..pos];
        const right_expr = trimmed[pos + 2 ..];
        var left_value = try evaluateExpression(allocator, context, left_expr);
        defer left_value.deinit(allocator);
        var right_value = try evaluateExpression(allocator, context, right_expr);
        defer right_value.deinit(allocator);

        switch (left_value.value) {
            .boolean => {
                if (right_value.value != .boolean) return error.TypeMismatch;
                const equal = left_value.value.boolean == right_value.value.boolean;
                return EvaluatedValue{ .value = .{ .boolean = equal } };
            },
            .string => {
                if (right_value.value != .string) return error.TypeMismatch;
                const equal = std.mem.eql(u8, left_value.value.string, right_value.value.string);
                return EvaluatedValue{ .value = .{ .boolean = equal } };
            },
        }
    }

    return try evaluateOperand(allocator, context, trimmed);
}

fn evaluateOperand(
    allocator: Allocator,
    context: *ScriptContext,
    operand: []const u8,
) EnsureEvalError!EvaluatedValue {
    if (operand.len == 0) return error.InvalidExpression;

    if (operand[0] == '"') {
        const literal = parseStringLiteral(allocator, operand) catch return error.InvalidExpression;
        if (literal.consumed != operand.len) {
            allocator.free(literal.value);
            return error.InvalidExpression;
        }
        return EvaluatedValue{ .value = .{ .string = literal.value }, .owned = true };
    }

    if (std.mem.eql(u8, operand, "true")) {
        return EvaluatedValue{ .value = .{ .boolean = true } };
    }
    if (std.mem.eql(u8, operand, "false")) {
        return EvaluatedValue{ .value = .{ .boolean = false } };
    }

    return try evaluateIdentifierExpression(context, operand);
}

fn evaluateIdentifierExpression(
    context: *ScriptContext,
    expr: []const u8,
) EnsureEvalError!EvaluatedValue {
    const dot = std.mem.indexOfScalar(u8, expr, '.');
    const name_slice = std.mem.trim(u8, if (dot) |pos| expr[0..pos] else expr, " \t");
    if (name_slice.len == 0) return error.InvalidExpression;

    if (dot == null) {
        if (context.getStringBinding(name_slice)) |value| {
            return EvaluatedValue{ .value = .{ .string = value } };
        }
        if (context.getProcessBinding(name_slice)) |_| {
            return error.TypeMismatch;
        }
        return error.UnknownBinding;
    }

    const tail = expr[dot.? + 1 ..];
    if (context.getObjectBinding(name_slice)) |object_value| {
        return try evaluateObjectField(object_value, tail);
    }
    if (context.getProcessBinding(name_slice)) |snapshot| {
        return try evaluateProcessField(snapshot, tail);
    }
    if (context.getStringBinding(name_slice) != null) return error.TypeMismatch;
    if (context.getStringArrayBinding(name_slice) != null) return error.TypeMismatch;
    if (context.getFunctionBinding(name_slice) != null) return error.TypeMismatch;
    return error.UnknownBinding;
}

fn evaluateProcessField(
    snapshot: *const ProcessSnapshot,
    path: []const u8,
) EnsureEvalError!EvaluatedValue {
    var remaining = path;
    var in_status = false;

    while (true) {
        const dot = std.mem.indexOfScalar(u8, remaining, '.');
        const segment = std.mem.trim(u8, if (dot) |pos| remaining[0..pos] else remaining, " \t");
        if (segment.len == 0) return error.InvalidExpression;
        const tail = if (dot) |pos| remaining[pos + 1 ..] else &[_]u8{};

        if (!in_status) {
            if (std.mem.eql(u8, segment, "stdout")) {
                if (tail.len != 0) return error.InvalidExpression;
                return EvaluatedValue{ .value = .{ .string = snapshot.stdoutBytes() } };
            }
            if (std.mem.eql(u8, segment, "stderr")) {
                if (tail.len != 0) return error.InvalidExpression;
                return EvaluatedValue{ .value = .{ .string = snapshot.stderrBytes() } };
            }
            if (std.mem.eql(u8, segment, "status")) {
                if (tail.len == 0) return error.InvalidExpression;
                in_status = true;
                remaining = tail;
                continue;
            }
            return error.InvalidExpression;
        } else {
            if (std.mem.eql(u8, segment, "ok")) {
                if (tail.len != 0) return error.InvalidExpression;
                return EvaluatedValue{ .value = .{ .boolean = snapshot.status.ok } };
            }
            return error.InvalidExpression;
        }
    }
}

fn evaluateObjectField(
    object: *const ScriptContext.ObjectValue,
    path: []const u8,
) EnsureEvalError!EvaluatedValue {
    const segment = std.mem.trim(u8, path, " \t");
    if (segment.len == 0) return error.InvalidExpression;
    if (std.mem.indexOfScalar(u8, segment, '.') != null) return error.InvalidExpression;

    if (object.getField(segment)) |value| {
        return EvaluatedValue{ .value = .{ .string = value } };
    }
    return error.UnknownBinding;
}

fn findTopLevelComparison(expr: []const u8) ?usize {
    var idx: usize = 0;
    var in_string = false;
    var escape = false;
    while (idx + 1 < expr.len) : (idx += 1) {
        const ch = expr[idx];
        if (in_string) {
            if (escape) {
                escape = false;
                continue;
            }
            if (ch == '\\') {
                escape = true;
                continue;
            }
            if (ch == '"') {
                in_string = false;
            }
            continue;
        }

        if (ch == '"') {
            in_string = true;
            continue;
        }

        if (ch == '=' and expr[idx + 1] == '=') {
            return idx;
        }
    }
    return null;
}

fn computeScriptDirectory(allocator: Allocator, script_path: []const u8) ![]const u8 {
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

const LetBinding = struct {
    name: []const u8,
    value: LetValue,
    is_mutable: bool,
};

const IdentifierAssignment = struct {
    name: []const u8,
    value: AssignmentValue,

    const AssignmentValue = union(enum) {
        literal: []u8,
        arithmetic: ArithmeticUpdate,
    };

    const ArithmeticUpdate = struct {
        delta: i64,
    };

    fn deinit(self: *IdentifierAssignment, allocator: Allocator) void {
        switch (self.value) {
            .literal => |literal| allocator.free(literal),
            .arithmetic => {},
        }
        self.* = undefined;
    }
};

const ModuleFunctionAlias = struct {
    module_alias: []u8,
    function_name: []u8,
};

const ModuleInvocationBinding = struct {
    invocation: ModuleInvocation,
    display_command: []u8,
};

const ModuleValueBinding = struct {
    module_alias: []u8,
    value_name: []u8,
};

const ModuleImportBinding = struct {
    spec: []u8,
};

const ModuleImportMemberBinding = struct {
    spec: []u8,
    member_name: []u8,
};

const LetValue = union(enum) {
    literal: []u8,
    command: []u8,
    module_function: ModuleFunctionAlias,
    module_invocation: ModuleInvocationBinding,
    module_value: ModuleValueBinding,
    module_import: ModuleImportBinding,
    module_import_member: ModuleImportMemberBinding,
    array_literal: [][]u8,
    object_literal: ObjectLiteral,
    binding_expression: BindingExpression,
    type_expression: []u8,
};

const BindingExpression = union(enum) {
    identifier: []u8,
    object_field: ObjectFieldReference,
    array_element: ArrayElementReference,

    const ObjectFieldReference = struct {
        object_name: []u8,
        field_name: []u8,
    };

    const ArrayElementReference = struct {
        array_name: []u8,
        index: usize,
    };

    fn deinit(self: *BindingExpression, allocator: Allocator) void {
        switch (self.*) {
            .identifier => |name| allocator.free(name),
            .object_field => |field| {
                allocator.free(field.object_name);
                allocator.free(field.field_name);
            },
            .array_element => |element| allocator.free(element.array_name),
        }
        self.* = undefined;
    }
};

const ObjectLiteral = struct {
    entries: []Entry,

    const Entry = struct {
        key: []u8,
        value: Value,
    };

    const Value = union(enum) {
        literal: []u8,
        identifier: []u8,
    };

    fn deinit(self: *ObjectLiteral, allocator: Allocator) void {
        for (self.entries) |*entry| {
            allocator.free(entry.key);
            switch (entry.value) {
                .literal, .identifier => |text| allocator.free(text),
            }
        }
        allocator.free(self.entries);
        self.* = undefined;
    }
};

const EnsureCall = struct {
    condition: []const u8,
    label: []const u8,
};

const EnsureParseError = error{
    InvalidEnsure,
};

const EnsureEvalError = error{
    InvalidExpression,
    UnknownBinding,
    TypeMismatch,
};

const LetParseError = error{
    MissingIdentifier,
    InvalidIdentifier,
    MissingEquals,
    MissingValue,
    MissingTypeAnnotation,
    TrailingCharacters,
    MissingClosingBracket,
    InvalidArrayLiteral,
    MissingClosingBrace,
    InvalidObjectLiteral,
    InvalidExpression,
    InvalidTypeExpression,
    MissingClosingParen,
    UnknownFunctionBinding,
};

const ParseStringError = error{
    UnterminatedStringLiteral,
    MissingStringLiteral,
    InvalidEscapeSequence,
};

const BindingInfo = struct { len: usize, is_mutable: bool };

const ParseLetBindingOptions = struct {
    context: ?*const ScriptContext = null,
    modules: ?*const ModuleRegistry = null,
};

const ErrorDeclarationParseError = error{
    MissingName,
    InvalidName,
    MissingEquals,
    MissingDefinition,
    MissingBodyKind,
};

const ParsedErrorDeclaration = struct {
    name: []const u8,
    definition: []const u8,
};

const ParseLetBindingError = LetParseError || ModuleInvocationError || ParseStringError || Allocator.Error;

const TypeExpressionParseError = error{
    UnexpectedToken,
    UnexpectedEOF,
    MissingIdentifier,
    MissingFieldType,
    MissingClosingBrace,
    MissingClosingParen,
};

const TypeExpressionParser = struct {
    text: []const u8,
    idx: usize,

    fn parse(text: []const u8) TypeExpressionParseError!void {
        var parser = TypeExpressionParser{ .text = text, .idx = 0 };
        parser.skipWhitespace();
        if (parser.idx >= parser.text.len) return error.UnexpectedEOF;
        try parser.parseTypeExpression();
        parser.skipWhitespace();
        if (parser.idx != parser.text.len) return error.UnexpectedToken;
    }

    fn parseTypeExpression(self: *TypeExpressionParser) TypeExpressionParseError!void {
        try self.parsePrefixType();
        self.skipWhitespace();
        if (self.consume('!')) {
            self.skipWhitespace();
            try self.parsePrefixType();
        }
    }

    fn parsePrefixType(self: *TypeExpressionParser) TypeExpressionParseError!void {
        self.skipWhitespace();
        while (true) {
            if (self.consume('?') or self.consume('^')) {
                self.skipWhitespace();
                continue;
            }
            break;
        }
        try self.parsePrimaryType();
    }

    fn parsePrimaryType(self: *TypeExpressionParser) TypeExpressionParseError!void {
        self.skipWhitespace();
        if (self.matchKeyword("struct")) {
            return self.parseStructType();
        }
        try self.parseNamedType();
    }

    fn parseStructType(self: *TypeExpressionParser) TypeExpressionParseError!void {
        self.skipWhitespace();
        if (!self.consume('{')) return error.UnexpectedToken;
        self.skipWhitespace();
        if (self.consume('}')) return;

        while (true) {
            self.skipWhitespace();
            try self.parseIdentifierToken();
            self.skipWhitespace();
            if (!self.consume(':')) return error.MissingFieldType;
            self.skipWhitespace();
            try self.parseTypeExpression();
            self.skipWhitespace();
            if (self.consume('}')) break;
            if (self.consume(',')) {
                self.skipWhitespace();
                if (self.consume('}')) break;
                continue;
            }
            if (self.idx >= self.text.len) return error.MissingClosingBrace;
            return error.UnexpectedToken;
        }
    }

    fn parseNamedType(self: *TypeExpressionParser) TypeExpressionParseError!void {
        self.skipWhitespace();
        try self.parsePath();
        self.skipWhitespace();
        if (self.consume('(')) {
            self.skipWhitespace();
            if (self.consume(')')) return;

            while (true) {
                try self.parseTypeExpression();
                self.skipWhitespace();
                if (self.consume(')')) break;
                if (!self.consume(',')) return error.MissingClosingParen;
                self.skipWhitespace();
                if (self.consume(')')) break;
            }
        }
    }

    fn parsePath(self: *TypeExpressionParser) TypeExpressionParseError!void {
        try self.parseIdentifierToken();
        while (self.consume('.')) {
            try self.parseIdentifierToken();
        }
    }

    fn parseIdentifierToken(self: *TypeExpressionParser) TypeExpressionParseError!void {
        if (self.idx >= self.text.len) return error.UnexpectedEOF;
        const ch = self.text[self.idx];
        if (!isIdentifierStart(ch)) return error.MissingIdentifier;
        self.idx += 1;
        while (self.idx < self.text.len and isIdentifierChar(self.text[self.idx])) : (self.idx += 1) {}
    }

    fn skipWhitespace(self: *TypeExpressionParser) void {
        skipWhitespaceAll(self.text, &self.idx);
    }

    fn consume(self: *TypeExpressionParser, target: u8) bool {
        if (self.idx >= self.text.len) return false;
        if (self.text[self.idx] == target) {
            self.idx += 1;
            return true;
        }
        return false;
    }

    fn matchKeyword(self: *TypeExpressionParser, keyword: []const u8) bool {
        const remaining = self.text[self.idx..];
        if (remaining.len < keyword.len) return false;
        if (!std.mem.startsWith(u8, remaining, keyword)) return false;
        const end_idx = self.idx + keyword.len;
        if (end_idx < self.text.len) {
            const suffix = self.text[end_idx];
            if (isIdentifierChar(suffix)) return false;
        }
        self.idx = end_idx;
        return true;
    }
};

fn shouldParseAsTypeExpression(text: []const u8) bool {
    var idx: usize = 0;
    skipWhitespaceAll(text, &idx);
    if (idx >= text.len) return false;
    const remaining = text[idx..];
    const first = remaining[0];

    if (first == '?' or first == '^') return true;
    if (remaining.len >= 6 and std.mem.startsWith(u8, remaining, "struct")) {
        if (remaining.len == 6) return true;
        const suffix = remaining[6];
        if (!isIdentifierChar(suffix)) return true;
    }
    if (remaining.len >= 5 and std.mem.startsWith(u8, remaining, "error")) {
        if (remaining.len == 5) return true;
        const suffix = remaining[5];
        if (suffix == '.' or std.ascii.isWhitespace(suffix)) return true;
    }
    if (std.ascii.isUpper(first)) return true;
    var first_token_end: usize = 0;
    while (first_token_end < remaining.len and !std.ascii.isWhitespace(remaining[first_token_end])) {
        first_token_end += 1;
    }
    const first_token = remaining[0..first_token_end];
    if (std.mem.indexOfScalar(u8, first_token, '.')) |dot_idx| {
        if (dot_idx + 1 < first_token.len and std.ascii.isUpper(first_token[dot_idx + 1])) {
            return true;
        }
    }
    return false;
}

fn parseTypeExpressionLiteral(text: []const u8) TypeExpressionParseError!void {
    return TypeExpressionParser.parse(text);
}

fn parseLetBinding(
    allocator: Allocator,
    command: []const u8,
) ParseLetBindingError!?LetBinding {
    return parseLetBindingWithOptions(allocator, command, .{});
}

fn parseLetBindingWithOptions(
    allocator: Allocator,
    command: []const u8,
    options: ParseLetBindingOptions,
) ParseLetBindingError!?LetBinding {
    var trimmed = std.mem.trim(u8, command, " \t\r\n");
    if (trimmed.len < 3) return null;

    const keyword: BindingInfo = blk: {
        if (std.mem.startsWith(u8, trimmed, "let")) {
            if (trimmed.len > 3) {
                const suffix = trimmed[3];
                if (!std.ascii.isWhitespace(suffix)) break :blk null;
            }
            break :blk BindingInfo{ .len = 3, .is_mutable = false };
        }
        if (std.mem.startsWith(u8, trimmed, "mut")) {
            if (trimmed.len > 3) {
                const suffix = trimmed[3];
                if (!std.ascii.isWhitespace(suffix)) break :blk null;
            }
            break :blk BindingInfo{ .len = 3, .is_mutable = true };
        }
        break :blk null;
    } orelse return null;

    const is_mutable = keyword.is_mutable;

    var idx: usize = keyword.len;
    skipInlineWhitespace(trimmed, &idx);
    if (idx >= trimmed.len) return error.MissingIdentifier;

    if (!isIdentifierStart(trimmed[idx])) return error.InvalidIdentifier;
    const name_start = idx;
    idx += 1;
    while (idx < trimmed.len and isIdentifierChar(trimmed[idx])) : (idx += 1) {}
    const name = trimmed[name_start..idx];

    skipInlineWhitespace(trimmed, &idx);

    if (idx < trimmed.len and trimmed[idx] == ':') {
        idx += 1;
        const annotation_start = idx;
        while (idx < trimmed.len and trimmed[idx] != '=') : (idx += 1) {}
        if (idx == trimmed.len) return error.MissingEquals;
        const annotation = std.mem.trim(u8, trimmed[annotation_start..idx], " \t");
        if (annotation.len == 0) return error.MissingTypeAnnotation;
    }

    if (idx >= trimmed.len or trimmed[idx] != '=') return error.MissingEquals;
    idx += 1;

    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len) return error.MissingValue;

    if (trimmed[idx] == '"') {
        const literal = try parseStringLiteral(allocator, trimmed[idx..]);
        errdefer allocator.free(literal.value);
        idx += literal.consumed;

        skipInlineWhitespace(trimmed, &idx);
        if (idx != trimmed.len) return error.TrailingCharacters;

        return LetBinding{
            .name = name,
            .value = .{ .literal = literal.value },
            .is_mutable = is_mutable,
        };
    }

    if (trimmed[idx] == '[') {
        const array_literal = try parseStringArrayLiteral(allocator, trimmed[idx..]);
        errdefer freeOwnedStringList(allocator, array_literal.values);
        idx += array_literal.consumed;

        skipInlineWhitespace(trimmed, &idx);
        if (idx != trimmed.len) return error.TrailingCharacters;

        return LetBinding{
            .name = name,
            .value = .{ .array_literal = array_literal.values },
            .is_mutable = is_mutable,
        };
    }

    var object_start: ?usize = null;
    if (idx < trimmed.len) {
        if (trimmed[idx] == '{') {
            object_start = idx;
        } else if (trimmed[idx] == '.') {
            var after_dot = idx + 1;
            skipWhitespaceAll(trimmed, &after_dot);
            if (after_dot < trimmed.len and trimmed[after_dot] == '{') {
                object_start = after_dot;
            }
        }
    }

    if (object_start) |start_idx| {
        var object_literal = try parseObjectLiteral(allocator, trimmed[start_idx..]);
        errdefer object_literal.literal.deinit(allocator);
        idx = start_idx + object_literal.consumed;

        skipInlineWhitespace(trimmed, &idx);
        if (idx != trimmed.len) return error.TrailingCharacters;

        return LetBinding{
            .name = name,
            .value = .{ .object_literal = object_literal.literal },
            .is_mutable = is_mutable,
        };
    }

    const literal_start = idx;
    while (idx < trimmed.len and trimmed[idx] != ' ' and trimmed[idx] != '\t') {
        idx += 1;
    }
    const literal = trimmed[literal_start..idx];
    if (literal.len == 0) return error.MissingValue;

    skipInlineWhitespace(trimmed, &idx);
    if (idx == trimmed.len) {
        if (try parseModuleFunctionReference(allocator, trimmed[literal_start..])) |reference| {
            if (shouldParseAsModuleFunction(options, reference.module_alias)) {
                return LetBinding{
                    .name = name,
                    .value = .{ .module_function = reference },
                    .is_mutable = is_mutable,
                };
            }

            allocator.free(reference.module_alias);
            allocator.free(reference.function_name);
        }

        if (isSupportedLetLiteral(literal)) {
            const value = try allocator.dupe(u8, literal);
            return LetBinding{
                .name = name,
                .value = .{ .literal = value },
                .is_mutable = is_mutable,
            };
        }
    }

    const command_text = std.mem.trim(u8, trimmed[literal_start..], " \t");
    if (command_text.len == 0) return error.MissingValue;

    if (options.modules != null) {
        if (try parseModuleImportMemberExpression(allocator, command_text)) |member_spec| {
            return LetBinding{
                .name = name,
                .value = .{ .module_import_member = member_spec },
                .is_mutable = is_mutable,
            };
        }
        if (try parseModuleImportExpression(allocator, command_text)) |import_spec| {
            return LetBinding{
                .name = name,
                .value = .{ .module_import = .{ .spec = import_spec } },
                .is_mutable = is_mutable,
            };
        }
    }

    const type_candidate = std.mem.trim(u8, command_text, " \t\r\n");
    if (type_candidate.len > 0 and shouldParseAsTypeExpression(type_candidate)) {
        parseTypeExpressionLiteral(type_candidate) catch |err| switch (err) {
            TypeExpressionParseError.MissingClosingBrace => return error.MissingClosingBrace,
            TypeExpressionParseError.MissingClosingParen => return error.MissingClosingParen,
            else => return error.InvalidTypeExpression,
        };

        const type_literal = try allocator.dupe(u8, type_candidate);
        return LetBinding{
            .name = name,
            .value = .{ .type_expression = type_literal },
            .is_mutable = is_mutable,
        };
    }

    if (options.modules != null) {
        if (try parseModuleInvocation(allocator, command_text, options.context)) |module_call| {
            const command_owned = try allocator.dupe(u8, command_text);
            return LetBinding{
                .name = name,
                .value = .{ .module_invocation = .{
                    .invocation = module_call,
                    .display_command = command_owned,
                } },
                .is_mutable = is_mutable,
            };
        }
        if (try parseModuleValueReference(allocator, options.modules.?, command_text)) |value_ref| {
            return LetBinding{
                .name = name,
                .value = .{ .module_value = value_ref },
                .is_mutable = is_mutable,
            };
        }
    }
    if (unknownFunctionBindingNameFromExpression(command_text, options.context)) |_| {
        return error.UnknownFunctionBinding;
    }
    if (try parseBindingExpression(allocator, command_text)) |expr| {
        return LetBinding{
            .name = name,
            .value = .{ .binding_expression = expr },
            .is_mutable = is_mutable,
        };
    }
    if (looksLikeArithmeticExpression(command_text)) {
        const literal_value = evaluateArithmeticLiteral(allocator, command_text) catch {
            return error.InvalidExpression;
        };
        return LetBinding{
            .name = name,
            .value = .{ .literal = literal_value },
            .is_mutable = is_mutable,
        };
    }
    const command_owned = try allocator.dupe(u8, command_text);
    return LetBinding{
        .name = name,
        .value = .{ .command = command_owned },
        .is_mutable = is_mutable,
    };
}

fn parseErrorDeclaration(command: []const u8) ErrorDeclarationParseError!?ParsedErrorDeclaration {
    var trimmed = std.mem.trim(u8, command, " \t\r\n");
    if (trimmed.len < "error".len) return null;
    if (!std.mem.startsWith(u8, trimmed, "error")) return null;
    if (trimmed.len == "error".len) return error.MissingName;
    if (!std.ascii.isWhitespace(trimmed["error".len])) return null;

    var idx: usize = "error".len;
    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len) return error.MissingName;
    if (!isIdentifierStart(trimmed[idx])) return error.InvalidName;
    const name_start = idx;
    idx += 1;
    while (idx < trimmed.len and isIdentifierChar(trimmed[idx])) : (idx += 1) {}
    const name = trimmed[name_start..idx];

    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len or trimmed[idx] != '=') return error.MissingEquals;
    idx += 1;

    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len) return error.MissingDefinition;
    const definition = std.mem.trim(u8, trimmed[idx..], " \t\r\n");
    if (definition.len == 0) return error.MissingDefinition;

    const starts_with_union = startsWithErrorBody(definition, "union");
    const starts_with_enum = startsWithErrorBody(definition, "enum");
    if (!starts_with_union and !starts_with_enum) return error.MissingBodyKind;

    return ParsedErrorDeclaration{
        .name = name,
        .definition = definition,
    };
}

fn startsWithErrorBody(definition: []const u8, keyword: []const u8) bool {
    if (definition.len < keyword.len) return false;
    if (!std.mem.startsWith(u8, definition, keyword)) return false;
    if (definition.len == keyword.len) return true;
    const suffix = definition[keyword.len];
    if (suffix == '{') return true;
    return std.ascii.isWhitespace(suffix);
}

fn shouldParseAsModuleFunction(
    options: ParseLetBindingOptions,
    alias: []const u8,
) bool {
    if (options.context) |ctx| {
        if (ctx.getObjectBinding(alias)) |_| {
            return false;
        }
    }
    return options.modules != null;
}

fn instantiateObjectLiteral(
    allocator: Allocator,
    context: *ScriptContext,
    literal: *const ObjectLiteral,
    failed_reference: *?[]const u8,
) (ScriptContext.BindingError || Allocator.Error)!ScriptContext.ObjectValue {
    failed_reference.* = null;
    const entries = literal.entries;
    if (entries.len == 0) {
        return ScriptContext.ObjectValue{
            .entries = &[_]ScriptContext.ObjectValue.ObjectEntry{},
        };
    }

    var object_entries = try allocator.alloc(ScriptContext.ObjectValue.ObjectEntry, entries.len);
    var filled: usize = 0;
    errdefer {
        for (object_entries[0..filled]) |entry| {
            allocator.free(entry.key);
            allocator.free(entry.value);
        }
        allocator.free(object_entries);
    }

    for (entries, 0..) |entry, idx| {
        object_entries[idx].key = try allocator.dupe(u8, entry.key);
        switch (entry.value) {
            .literal => |literal_value| {
                object_entries[idx].value = try allocator.dupe(u8, literal_value);
            },
            .identifier => |identifier| {
                const resolved = resolveStringBinding(context, identifier) catch |err| {
                    failed_reference.* = identifier;
                    return err;
                };
                object_entries[idx].value = try allocator.dupe(u8, resolved);
            },
        }
        filled += 1;
    }

    return ScriptContext.ObjectValue{ .entries = object_entries };
}

fn parseModuleFunctionReference(
    allocator: Allocator,
    value: []const u8,
) !?ModuleFunctionAlias {
    const trimmed = std.mem.trim(u8, value, " \t");
    if (trimmed.len == 0) return null;

    var idx: usize = 0;
    if (!isIdentifierStart(trimmed[idx])) return null;
    const alias_start = idx;
    idx += 1;
    while (idx < trimmed.len and isIdentifierChar(trimmed[idx])) : (idx += 1) {}
    const alias_end = idx;

    skipInlineWhitespace(trimmed, &idx);
    if (idx >= trimmed.len or trimmed[idx] != '.') return null;
    idx += 1;

    skipInlineWhitespace(trimmed, &idx);
    if (idx >= trimmed.len or !isIdentifierStart(trimmed[idx])) return null;
    const fn_start = idx;
    idx += 1;
    while (idx < trimmed.len and isIdentifierChar(trimmed[idx])) : (idx += 1) {}
    const fn_end = idx;

    skipInlineWhitespace(trimmed, &idx);
    if (idx != trimmed.len) return null;

    const alias_slice = trimmed[alias_start..alias_end];
    const function_slice = trimmed[fn_start..fn_end];
    const alias_owned = try allocator.dupe(u8, alias_slice);
    errdefer allocator.free(alias_owned);
    const function_owned = try allocator.dupe(u8, function_slice);
    return ModuleFunctionAlias{
        .module_alias = alias_owned,
        .function_name = function_owned,
    };
}

fn parseModuleValueReference(
    allocator: Allocator,
    modules: *const ModuleRegistry,
    value: []const u8,
) !?ModuleValueBinding {
    const trimmed = std.mem.trim(u8, value, " \t");
    if (trimmed.len == 0) return null;

    var idx: usize = 0;
    if (!isIdentifierStart(trimmed[idx])) return null;
    const alias_start = idx;
    idx += 1;
    while (idx < trimmed.len and isIdentifierChar(trimmed[idx])) : (idx += 1) {}
    const alias_end = idx;

    skipInlineWhitespace(trimmed, &idx);
    if (idx >= trimmed.len or trimmed[idx] != '.') return null;
    idx += 1;

    skipInlineWhitespace(trimmed, &idx);
    if (idx >= trimmed.len or !isIdentifierStart(trimmed[idx])) return null;
    const value_start = idx;
    idx += 1;
    while (idx < trimmed.len and isIdentifierChar(trimmed[idx])) : (idx += 1) {}
    const value_end = idx;

    skipInlineWhitespace(trimmed, &idx);
    if (idx != trimmed.len) return null;

    const alias_slice = trimmed[alias_start..alias_end];
    const value_slice = trimmed[value_start..value_end];
    if (modules.lookupValue(alias_slice, value_slice) == null) return null;

    const alias_owned = try allocator.dupe(u8, alias_slice);
    errdefer allocator.free(alias_owned);
    const value_owned = try allocator.dupe(u8, value_slice);

    return ModuleValueBinding{
        .module_alias = alias_owned,
        .value_name = value_owned,
    };
}

const ModuleImportParseResult = struct {
    spec: []u8,
    next_index: usize,
};

fn parseModuleImportPrefix(
    allocator: Allocator,
    trimmed: []const u8,
) (ParseLetBindingError)!?ModuleImportParseResult {
    if (!std.mem.startsWith(u8, trimmed, "import")) return null;
    if (trimmed.len > 6) {
        const suffix = trimmed[6];
        if (!std.ascii.isWhitespace(suffix) and suffix != '(') return null;
    } else {
        return error.MissingValue;
    }

    var idx: usize = 6;
    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len or trimmed[idx] != '(') return error.InvalidExpression;
    idx += 1;

    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len) return error.MissingValue;
    if (trimmed[idx] != '"') return error.MissingStringLiteral;

    const literal = try parseStringLiteral(allocator, trimmed[idx..]);
    idx += literal.consumed;

    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len or trimmed[idx] != ')') {
        allocator.free(literal.value);
        return error.MissingClosingParen;
    }
    idx += 1;

    return ModuleImportParseResult{
        .spec = literal.value,
        .next_index = idx,
    };
}

fn parseModuleImportExpression(
    allocator: Allocator,
    value: []const u8,
) (ParseLetBindingError)!?[]u8 {
    const trimmed = std.mem.trim(u8, value, " \t\r\n");
    const prefix = try parseModuleImportPrefix(allocator, trimmed) orelse return null;
    var idx = prefix.next_index;

    skipWhitespaceAll(trimmed, &idx);
    if (idx != trimmed.len) {
        allocator.free(prefix.spec);
        return error.TrailingCharacters;
    }

    return prefix.spec;
}

fn parseModuleImportMemberExpression(
    allocator: Allocator,
    value: []const u8,
) (ParseLetBindingError)!?ModuleImportMemberBinding {
    const trimmed = std.mem.trim(u8, value, " \t\r\n");
    const prefix = try parseModuleImportPrefix(allocator, trimmed) orelse return null;
    var idx = prefix.next_index;

    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len or trimmed[idx] != '.') {
        allocator.free(prefix.spec);
        return null;
    }
    idx += 1;

    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len or !isIdentifierStart(trimmed[idx])) {
        allocator.free(prefix.spec);
        return error.InvalidExpression;
    }
    const member_start = idx;
    idx += 1;
    while (idx < trimmed.len and isIdentifierChar(trimmed[idx])) : (idx += 1) {}
    const member_slice = trimmed[member_start..idx];

    skipWhitespaceAll(trimmed, &idx);
    if (idx != trimmed.len) {
        allocator.free(prefix.spec);
        return error.TrailingCharacters;
    }

    const member_owned = try allocator.dupe(u8, member_slice);
    return ModuleImportMemberBinding{
        .spec = prefix.spec,
        .member_name = member_owned,
    };
}

const ArithmeticParseError = error{
    InvalidToken,
    Overflow,
    DivisionByZero,
};

fn looksLikeArithmeticExpression(text: []const u8) bool {
    var saw_digit = false;
    for (text) |ch| {
        if (std.ascii.isWhitespace(ch)) continue;
        switch (ch) {
            '(', ')', '+', '-', '*', '/' => continue,
            else => {
                if (std.ascii.isDigit(ch)) {
                    saw_digit = true;
                    continue;
                }
                return false;
            },
        }
    }
    return saw_digit;
}

fn evaluateArithmeticLiteral(
    allocator: Allocator,
    expression: []const u8,
) (ArithmeticParseError || Allocator.Error)![]u8 {
    var parser = ArithmeticParser{ .text = expression };
    const value = try parser.parseExpression();
    parser.skipWhitespace();
    if (parser.idx != parser.text.len) {
        return ArithmeticParseError.InvalidToken;
    }
    return try std.fmt.allocPrint(allocator, "{d}", .{value});
}

const ArithmeticParser = struct {
    text: []const u8,
    idx: usize = 0,

    fn parseExpression(self: *ArithmeticParser) ArithmeticParseError!i64 {
        var value = try self.parseTerm();
        while (true) {
            self.skipWhitespace();
            if (self.consume('+')) {
                const rhs = try self.parseTerm();
                value = std.math.add(i64, value, rhs) catch return ArithmeticParseError.Overflow;
                continue;
            }
            if (self.consume('-')) {
                const rhs = try self.parseTerm();
                value = std.math.sub(i64, value, rhs) catch return ArithmeticParseError.Overflow;
                continue;
            }
            break;
        }
        return value;
    }

    fn parseTerm(self: *ArithmeticParser) ArithmeticParseError!i64 {
        var value = try self.parseFactor();
        while (true) {
            self.skipWhitespace();
            if (self.consume('*')) {
                const rhs = try self.parseFactor();
                value = std.math.mul(i64, value, rhs) catch return ArithmeticParseError.Overflow;
                continue;
            }
            if (self.consume('/')) {
                const rhs = try self.parseFactor();
                value = std.math.divTrunc(i64, value, rhs) catch |err| switch (err) {
                    error.DivisionByZero => return ArithmeticParseError.DivisionByZero,
                    error.Overflow => return ArithmeticParseError.Overflow,
                };
                continue;
            }
            break;
        }
        return value;
    }

    fn parseFactor(self: *ArithmeticParser) ArithmeticParseError!i64 {
        self.skipWhitespace();
        if (self.consume('+')) {
            return try self.parseFactor();
        }
        if (self.consume('-')) {
            const operand = try self.parseFactor();
            return std.math.sub(i64, 0, operand) catch return ArithmeticParseError.Overflow;
        }
        if (self.consume('(')) {
            const value = try self.parseExpression();
            self.skipWhitespace();
            if (!self.consume(')')) return ArithmeticParseError.InvalidToken;
            return value;
        }
        return try self.parseNumber();
    }

    fn parseNumber(self: *ArithmeticParser) ArithmeticParseError!i64 {
        self.skipWhitespace();
        const start = self.idx;
        while (self.idx < self.text.len and std.ascii.isDigit(self.text[self.idx])) : (self.idx += 1) {}
        if (start == self.idx) return ArithmeticParseError.InvalidToken;
        const literal = self.text[start..self.idx];
        return std.fmt.parseInt(i64, literal, 10) catch return ArithmeticParseError.Overflow;
    }

    fn consume(self: *ArithmeticParser, target: u8) bool {
        if (self.idx >= self.text.len) return false;
        if (self.text[self.idx] == target) {
            self.idx += 1;
            return true;
        }
        return false;
    }

    fn skipWhitespace(self: *ArithmeticParser) void {
        while (self.idx < self.text.len and std.ascii.isWhitespace(self.text[self.idx])) : (self.idx += 1) {}
    }
};

fn parseIdentifierAssignment(allocator: Allocator, command: []const u8) (LetParseError || ParseStringError || Allocator.Error)!?IdentifierAssignment {
    var trimmed = std.mem.trim(u8, command, " \t\r\n");
    if (trimmed.len == 0) return null;

    var idx: usize = 0;
    if (!isIdentifierStart(trimmed[idx])) return null;

    const name_start = idx;
    idx += 1;
    while (idx < trimmed.len and isIdentifierChar(trimmed[idx])) : (idx += 1) {}
    const name = trimmed[name_start..idx];

    skipInlineWhitespace(trimmed, &idx);
    const op = parseAssignmentOperator(trimmed, &idx) orelse return null;

    skipInlineWhitespace(trimmed, &idx);
    if (idx >= trimmed.len) return error.MissingValue;

    if (op == .add_assign) {
        const addition = try parseArithmeticUpdate(trimmed[idx..]);
        return IdentifierAssignment{
            .name = name,
            .value = .{ .arithmetic = addition },
        };
    }

    if (try parseSelfAddition(trimmed[idx..], name)) |addition| {
        return IdentifierAssignment{
            .name = name,
            .value = .{ .arithmetic = addition },
        };
    }

    if (trimmed[idx] == '"') {
        const literal = try parseStringLiteral(allocator, trimmed[idx..]);
        idx += literal.consumed;

        skipInlineWhitespace(trimmed, &idx);
        if (idx != trimmed.len) return error.TrailingCharacters;

        return IdentifierAssignment{
            .name = name,
            .value = .{ .literal = literal.value },
        };
    }

    const literal_start = idx;
    while (idx < trimmed.len and trimmed[idx] != ' ' and trimmed[idx] != '\t') {
        idx += 1;
    }
    const literal = trimmed[literal_start..idx];
    if (literal.len == 0) return error.MissingValue;

    skipInlineWhitespace(trimmed, &idx);
    if (idx != trimmed.len) return error.TrailingCharacters;

    if (!isSupportedLetLiteral(literal)) return null;

    const value = try allocator.dupe(u8, literal);
    return IdentifierAssignment{
        .name = name,
        .value = .{ .literal = value },
    };
}

const AssignmentOperator = enum {
    assign,
    add_assign,
};

fn parseAssignmentOperator(text: []const u8, idx: *usize) ?AssignmentOperator {
    if (idx.* >= text.len) return null;
    if (text[idx.*] == '=') {
        idx.* += 1;
        return .assign;
    }
    if (text[idx.*] == '+' and idx.* + 1 < text.len and text[idx.* + 1] == '=') {
        idx.* += 2;
        return .add_assign;
    }
    return null;
}

fn parseArithmeticUpdate(text: []const u8) LetParseError!IdentifierAssignment.ArithmeticUpdate {
    const delta = try parseArithmeticDelta(text);
    return .{ .delta = delta };
}

fn parseSelfAddition(
    text: []const u8,
    name: []const u8,
) LetParseError!?IdentifierAssignment.ArithmeticUpdate {
    if (text.len < name.len) return null;
    if (!std.mem.eql(u8, text[0..name.len], name)) return null;

    var idx: usize = name.len;
    if (idx < text.len and isIdentifierChar(text[idx])) return null;

    skipInlineWhitespace(text, &idx);
    if (idx >= text.len or text[idx] != '+') return null;
    idx += 1;

    skipInlineWhitespace(text, &idx);
    if (idx >= text.len) return error.MissingValue;

    const addition = try parseArithmeticUpdate(text[idx..]);
    return addition;
}

fn parseArithmeticDelta(text: []const u8) LetParseError!i64 {
    const trimmed = std.mem.trim(u8, text, " \t");
    if (trimmed.len == 0) return error.MissingValue;

    var parser = ArithmeticParser{ .text = trimmed };
    const value = parser.parseExpression() catch return error.InvalidExpression;
    parser.skipWhitespace();
    if (parser.idx != parser.text.len) return error.InvalidExpression;

    return value;
}

const ParseModuleInvocationError = ModuleInvocationError || ParseStringError || Allocator.Error;

fn catchClauseStartsHere(text: []const u8) bool {
    if (text.len < "catch".len) return false;
    if (!std.mem.startsWith(u8, text, "catch")) return false;
    if (text.len == "catch".len) return true;
    const next = text["catch".len];
    return std.ascii.isWhitespace(next) or next == '|' or next == '{';
}

fn ReturnType(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .@"fn" => |f| f.return_type orelse void,
        .pointer => |p| ReturnType(p.child),
        else => @compileError(@typeName(T) ++ " is not a function"),
    };
}

fn JoinOptional(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .optional => |o| switch (@typeInfo(o.child)) {
            .optional => |o2| ?o2.child,
            else => T,
        },
        else => @compileError(@typeName(T) ++ " is not an optional type"),
    };
}

fn parseExact(
    parser: anytype,
    command: []const u8,
    idx: *usize,
) JoinOptional(?ReturnType(@TypeOf(parser))) {
    const start = idx.*;
    const result = parser(command, idx);
    if (idx.* + start == command.len) return result;
    idx.* = start;
    return null;
}

fn parseIdentifier(
    command: []const u8,
    idx: *usize,
) ?[]const u8 {
    skipWhitespaceAll(command, idx);
    if (idx.* >= command.len) return null;
    if (!isIdentifierStart(command[idx.*])) return null;
    const alias_start = idx.*;
    idx.* += 1;
    while (idx.* < command.len and isIdentifierChar(command[idx.*])) : (idx.* += 1) {}
    return command[alias_start..idx.*];
}

fn parseModuleInvocation(
    allocator: Allocator,
    command: []const u8,
    context: ?*const ScriptContext,
) ParseModuleInvocationError!?ModuleInvocation {
    var idx: usize = 0;
    const identifier = parseIdentifier(command, &idx) orelse return null;
    const alias_end = idx;
    skipWhitespaceAll(command, &idx);
    const consumed_after_alias = idx - alias_end;
    if (idx >= command.len) return null;

    if (command[idx] == '.' and consumed_after_alias == 0) {
        idx += 1;
        const function_name = parseIdentifier(command, &idx) orelse return error.MissingFunctionName;

        skipWhitespaceAll(command, &idx);
        const nextChar = peekChar(command, idx) orelse return null;
        if (nextChar != '(') return null;

        const args = try parseInvocationArguments(allocator, command, &idx);
        errdefer freeStringList(allocator, args);

        skipWhitespaceAll(command, &idx);
        if (idx != command.len) {
            if (catchClauseStartsHere(command[idx..])) {
                return ModuleInvocationError.UnsupportedCatchClause;
            }
            return null;
        }

        return ModuleInvocation{
            .alias = identifier,
            .function = function_name,
            .args = args,
        };
    }

    if (command[idx] == '(') {
        const ctx = context orelse return null;
        const binding = ctx.getFunctionBinding(identifier) orelse return null;

        const args = try parseInvocationArguments(allocator, command, &idx);
        errdefer freeStringList(allocator, args);

        skipWhitespaceAll(command, &idx);
        if (idx != command.len) {
            if (catchClauseStartsHere(command[idx..])) {
                return ModuleInvocationError.UnsupportedCatchClause;
            }
            return null;
        }

        return ModuleInvocation{
            .alias = binding.module_alias,
            .function = binding.function_name,
            .args = args,
        };
    }

    return null;
}

fn parseInvocationArguments(
    allocator: Allocator,
    command: []const u8,
    idx_ptr: *usize,
) (ModuleInvocationError || ParseStringError || Allocator.Error)![][]const u8 {
    var idx = idx_ptr.*;
    parseChar('(', command, &idx) orelse return error.MissingArgumentList;

    var args = ManagedArrayList([]const u8).init(allocator);
    errdefer {
        freeStringList(allocator, args.items);
        args.deinit();
    }

    skipWhitespaceAll(command, &idx);
    if (idx >= command.len) return error.MissingArgumentList;
    if (command[idx] != ')') {
        while (idx < command.len) {
            skipWhitespaceAll(command, &idx);
            if (idx >= command.len) return error.MissingClosingParen;
            var value: []u8 = undefined;
            if (command[idx] == '"') {
                const literal = try parseStringLiteral(allocator, command[idx..]);
                value = literal.value;
                idx += literal.consumed;
            } else {
                const literal_start = idx;
                while (idx < command.len and command[idx] != ',' and command[idx] != ')') : (idx += 1) {}
                const literal_slice = std.mem.trim(u8, command[literal_start..idx], " \t\r");
                if (literal_slice.len == 0) return error.MissingArgumentList;
                value = try allocator.dupe(u8, literal_slice);
            }

            try args.append(value);
            skipWhitespaceAll(command, &idx);
            if (idx >= command.len) return error.MissingClosingParen;
            if (command[idx] == ')') break;
            if (command[idx] != ',') return error.TrailingCharacters;
            idx += 1;
        }
    }

    skipWhitespaceAll(command, &idx);
    if (idx >= command.len) return error.MissingClosingParen;
    if (command[idx] != ')') return error.MissingClosingParen;
    idx += 1;

    const owned = try args.toOwnedSlice();
    args.deinit();
    idx_ptr.* = idx;
    return owned;
}

fn parseChar(char: u8, command: []const u8, idx: *usize) ?void {
    if (idx.* >= command.len) return null;
    if (command[idx.* + 1] == char) {
        idx.* += 1;
        return;
    }
    return null;
}

fn peekChar(command: []const u8, idx: usize) ?u8 {
    if (idx >= command.len) return null;
    return command[idx + 1];
}

fn parseFunctionCallName(command: []const u8) ?[]const u8 {
    var idx: usize = 0;
    const function_name = parseIdentifier(command, &idx) orelse return null;
    const nextChar = peekChar(command, idx) orelse return null;
    if (nextChar != '(') return null;
    return function_name;
}

fn unknownFunctionBindingNameFromExpression(
    expression: []const u8,
    context: ?*const ScriptContext,
) ?[]const u8 {
    const ctx = context orelse return null;
    const name = parseFunctionCallName(expression) orelse return null;
    if (ctx.getFunctionBinding(name)) |_| return null;
    return name;
}

fn functionCallNameFromLet(command: []const u8) ?[]const u8 {
    const eq_idx = findAssignmentEquals(command) orelse return null;
    var idx = eq_idx + 1;
    skipWhitespaceAll(command, &idx);
    if (idx >= command.len) return null;
    return parseFunctionCallName(command[idx..]);
}

fn parseBindingExpression(allocator: Allocator, input: []const u8) !?BindingExpression {
    var idx: usize = 0;
    const base_name = parseIdentifier(input, &idx) orelse return null;
    skipInlineWhitespace(input, &idx);
    if (idx == input.len) {
        const owned = try allocator.dupe(u8, base_name);
        return BindingExpression{ .identifier = owned };
    }

    if (input[idx] == '.') {
        idx += 1;

        const field_name = parseIdentifier(input, &idx) orelse return null;
        skipInlineWhitespace(input, &idx);
        if (idx != input.len) return null;

        const object_owned = try allocator.dupe(u8, base_name);
        errdefer allocator.free(object_owned);
        const field_owned = try allocator.dupe(u8, field_name);

        return BindingExpression{
            .object_field = .{
                .object_name = object_owned,
                .field_name = field_owned,
            },
        };
    }

    if (input[idx] == '[') {
        idx += 1;
        skipInlineWhitespace(input, &idx);
        if (idx >= input.len) return null;

        const index_start = idx;
        while (idx < input.len and std.ascii.isDigit(input[idx])) : (idx += 1) {}
        if (index_start == idx) return null;

        const index_slice = input[index_start..idx];
        const element_index = std.fmt.parseUnsigned(usize, index_slice, 10) catch {
            return null;
        };

        skipInlineWhitespace(input, &idx);
        if (idx >= input.len or input[idx] != ']') return null;
        idx += 1;

        skipInlineWhitespace(input, &idx);
        if (idx != input.len) return null;

        const array_owned = try allocator.dupe(u8, base_name);
        return BindingExpression{
            .array_element = .{
                .array_name = array_owned,
                .index = element_index,
            },
        };
    }

    return null;
}

fn parseStringArrayLiteral(allocator: Allocator, input: []const u8) (LetParseError || ParseStringError || Allocator.Error)!struct {
    values: [][]u8,
    consumed: usize,
} {
    if (input.len == 0 or input[0] != '[') return error.InvalidArrayLiteral;

    var values = ManagedArrayList([]u8).init(allocator);
    errdefer {
        for (values.items) |item| allocator.free(item);
        values.deinit();
    }

    var idx: usize = 1;
    skipWhitespaceAll(input, &idx);
    if (idx >= input.len) return error.MissingClosingBracket;
    if (input[idx] == ']') {
        idx += 1;
        const owned = try values.toOwnedSlice();
        values.deinit();
        return .{ .values = owned, .consumed = idx };
    }

    while (idx < input.len) {
        skipWhitespaceAll(input, &idx);
        if (idx >= input.len) return error.MissingClosingBracket;
        if (input[idx] != '"') return error.InvalidArrayLiteral;
        const literal = try parseStringLiteral(allocator, input[idx..]);
        try values.append(literal.value);
        idx += literal.consumed;

        skipWhitespaceAll(input, &idx);
        if (idx >= input.len) return error.MissingClosingBracket;
        if (input[idx] == ',') {
            idx += 1;
            continue;
        }
        if (input[idx] == ']') {
            idx += 1;
            break;
        }
        return error.InvalidArrayLiteral;
    }

    if (idx > input.len or input[idx - 1] != ']') {
        return error.MissingClosingBracket;
    }

    const owned = try values.toOwnedSlice();
    values.deinit();
    return .{ .values = owned, .consumed = idx };
}

const ParseObjectError = LetParseError || ParseStringError || Allocator.Error;

fn parseObjectLiteral(allocator: Allocator, input: []const u8) ParseObjectError!struct {
    literal: ObjectLiteral,
    consumed: usize,
} {
    if (input.len == 0 or input[0] != '{') return error.InvalidObjectLiteral;

    var entries = ManagedArrayList(ObjectLiteral.Entry).init(allocator);
    errdefer {
        for (entries.items) |*entry| {
            allocator.free(entry.key);
            switch (entry.value) {
                .literal, .identifier => |text| allocator.free(text),
            }
        }
        entries.deinit();
    }

    var idx: usize = 1;
    skipWhitespaceAll(input, &idx);
    if (idx >= input.len) return error.MissingClosingBrace;
    if (input[idx] == '}') {
        idx += 1;
        const literal_entries = try entries.toOwnedSlice();
        entries.deinit();
        return .{ .literal = .{ .entries = literal_entries }, .consumed = idx };
    }

    while (idx < input.len) {
        skipWhitespaceAll(input, &idx);
        if (idx >= input.len) return error.MissingClosingBrace;
        if (input[idx] == '}') {
            idx += 1;
            break;
        }
        if (input[idx] == '.') {
            idx += 1;
            skipWhitespaceAll(input, &idx);
        }

        if (idx >= input.len or !isIdentifierStart(input[idx])) return error.InvalidObjectLiteral;
        const key_start = idx;
        idx += 1;
        while (idx < input.len and isIdentifierChar(input[idx])) : (idx += 1) {}
        const key_slice = input[key_start..idx];

        skipWhitespaceAll(input, &idx);
        if (idx >= input.len) return error.MissingClosingBrace;
        const separator = input[idx];
        if (separator != ':' and separator != '=') return error.InvalidObjectLiteral;
        idx += 1;

        skipWhitespaceAll(input, &idx);
        if (idx >= input.len) return error.MissingClosingBrace;

        var entry_value: ObjectLiteral.Value = undefined;
        if (input[idx] == '"') {
            const literal = try parseStringLiteral(allocator, input[idx..]);
            entry_value = .{ .literal = literal.value };
            idx += literal.consumed;
        } else {
            const value_start = idx;
            while (idx < input.len and input[idx] != ',' and input[idx] != '}') : (idx += 1) {}
            const value_slice = std.mem.trim(u8, input[value_start..idx], " \t\r\n");
            if (value_slice.len == 0) return error.InvalidObjectLiteral;

            if (isSupportedLetLiteral(value_slice)) {
                entry_value = .{ .literal = try allocator.dupe(u8, value_slice) };
            } else {
                var name_idx: usize = 0;
                _ = parseExact(parseIdentifier, value_slice, &name_idx) orelse return error.InvalidObjectLiteral;
                entry_value = .{ .identifier = try allocator.dupe(u8, value_slice) };
            }
        }

        const key_owned = try allocator.dupe(u8, key_slice);
        try entries.append(.{
            .key = key_owned,
            .value = entry_value,
        });

        skipWhitespaceAll(input, &idx);
        if (idx >= input.len) return error.MissingClosingBrace;
        if (input[idx] == ',') {
            idx += 1;
            continue;
        }
        if (input[idx] == '}') {
            idx += 1;
            break;
        }
        return error.InvalidObjectLiteral;
    }

    if (idx > input.len or input[idx - 1] != '}') return error.MissingClosingBrace;

    const literal_entries = try entries.toOwnedSlice();
    entries.deinit();
    return .{ .literal = .{ .entries = literal_entries }, .consumed = idx };
}

fn parseStringLiteral(allocator: Allocator, input: []const u8) (ParseStringError || Allocator.Error)!struct {
    value: []u8,
    consumed: usize,
} {
    if (input.len == 0 or input[0] != '"') return error.MissingStringLiteral;

    var builder = std.ArrayList(u8).empty;
    defer builder.deinit(allocator);

    var idx: usize = 1;
    var closed = false;

    while (idx < input.len) {
        const ch = input[idx];
        if (ch == '"') {
            closed = true;
            idx += 1;
            break;
        }
        if (ch == '\\') {
            idx += 1;
            if (idx >= input.len) return error.UnterminatedStringLiteral;
            const escaped: u8 = switch (input[idx]) {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '"' => '"',
                '\\' => '\\',
                else => return error.InvalidEscapeSequence,
            };
            try builder.append(allocator, escaped);
            idx += 1;
            continue;
        }
        try builder.append(allocator, ch);
        idx += 1;
    }

    if (!closed) return error.UnterminatedStringLiteral;

    return .{
        .value = try builder.toOwnedSlice(allocator),
        .consumed = idx,
    };
}

fn skipInlineWhitespace(command: []const u8, idx: *usize) void {
    while (idx.* < command.len) {
        const ch = command[idx.*];
        if (ch != ' ' and ch != '\t') break;
        idx.* += 1;
    }
}

fn isIdentifierStart(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isIdentifierChar(ch: u8) bool {
    return isIdentifierStart(ch) or std.ascii.isDigit(ch);
}

fn skipWhitespaceAll(text: []const u8, idx: *usize) void {
    while (idx.* < text.len) {
        const ch = text[idx.*];
        switch (ch) {
            ' ', '\t', '\n', '\r' => idx.* += 1,
            else => return,
        }
    }
}

fn skipWhitespaceAndComments(text: []const u8, idx: *usize) void {
    while (idx.* < text.len) {
        skipWhitespaceAll(text, idx);
        if (idx.* >= text.len) break;

        const remaining = text[idx.*..];
        if (remaining.len >= 2 and remaining[0] == '/' and remaining[1] == '/') {
            idx.* += 2;
            while (idx.* < text.len and text[idx.*] != '\n') : (idx.* += 1) {}
            continue;
        }
        if (remaining[0] == '#') {
            idx.* += 1;
            while (idx.* < text.len and text[idx.*] != '\n') : (idx.* += 1) {}
            continue;
        }
        if (remaining.len >= 2 and remaining[0] == '/' and remaining[1] == '*') {
            idx.* += 2;
            while (idx.* < text.len) : (idx.* += 1) {
                if (idx.* < text.len and text[idx.* - 1] == '*' and text[idx.*] == '/') {
                    idx.* += 1;
                    break;
                }
            }
            continue;
        }

        break;
    }
}

fn joinAndCheck(
    allocator: Allocator,
    base: []const u8,
    relative: []const u8,
) !?[]u8 {
    if (base.len == 0) return null;
    const candidate = try std.fs.path.join(allocator, &.{ base, relative });
    errdefer allocator.free(candidate);
    if (fileExists(candidate)) {
        return candidate;
    }
    allocator.free(candidate);
    return null;
}

fn ensureFileExists(path: []const u8) !void {
    std.fs.cwd().access(path, .{}) catch |err| switch (err) {
        error.FileNotFound => return error.ModuleNotFound,
        else => return err,
    };
}

fn fileExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

const ReturnCommandOptions = struct {
    require_return: bool = true,
    allow_empty_command: bool = false,
};

fn functionReturnTypeIsVoid(source: []const u8) bool {
    const cleaned = cleanReturnTypeAnnotation(source);
    if (cleaned.len == 0) return false;
    return std.ascii.eqlIgnoreCase(cleaned, "void");
}

fn cleanReturnTypeAnnotation(raw: []const u8) []const u8 {
    var trimmed = std.mem.trim(u8, raw, " \t\r\n");
    if (trimmed.len == 0) return trimmed;

    var idx: usize = 0;
    while (idx < trimmed.len) : (idx += 1) {
        const ch = trimmed[idx];
        if (ch == '/' and idx + 1 < trimmed.len) {
            const next = trimmed[idx + 1];
            if (next == '/' or next == '*') {
                trimmed = trimmed[0..idx];
                break;
            }
        }
        if (ch == '#') {
            trimmed = trimmed[0..idx];
            break;
        }
    }

    return std.mem.trim(u8, trimmed, " \t\r\n");
}

fn isCommentStart(text: []const u8) bool {
    if (text.len == 0) return false;
    if (text[0] == '#') return true;
    if (text[0] == '/' and text.len >= 2) {
        const next = text[1];
        return next == '/' or next == '*';
    }
    return false;
}

fn extractReturnCommand(allocator: Allocator, body: []const u8, options: ReturnCommandOptions) ![]const u8 {
    const trimmed = std.mem.trim(u8, body, " \t\r\n");
    if (trimmed.len == 0) {
        if (options.require_return) return ModuleParseError.MissingReturnStatement;
        return try allocator.dupe(u8, &[_]u8{});
    }

    const return_idx = findReturnKeyword(trimmed) orelse {
        if (options.require_return) return ModuleParseError.MissingReturnStatement;
        return try allocator.dupe(u8, &[_]u8{});
    };
    var idx = return_idx + "return".len;
    skipWhitespaceAll(trimmed, &idx);
    if (idx >= trimmed.len) {
        if (options.allow_empty_command) {
            return try allocator.dupe(u8, &[_]u8{});
        }
        return ModuleParseError.MissingReturnCommand;
    }

    var command_region = trimmed[idx..];
    var region_end = command_region.len;
    var scan_idx: usize = 0;
    var mode: QuoteMode = .none;
    var escape = false;
    while (scan_idx < command_region.len) : (scan_idx += 1) {
        const ch = command_region[scan_idx];
        switch (mode) {
            .double => {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '"') mode = .none;
                continue;
            },
            .single => {
                if (escape) {
                    escape = false;
                    continue;
                }
                if (ch == '\\') {
                    escape = true;
                    continue;
                }
                if (ch == '\'') mode = .none;
                continue;
            },
            .none => {},
        }

        if (mode == .none) {
            if (ch == '\n' or ch == '\r') {
                region_end = scan_idx;
                break;
            }
            if (ch == '#') {
                region_end = scan_idx;
                break;
            }
            if (ch == '/' and scan_idx + 1 < command_region.len and command_region[scan_idx + 1] == '/') {
                region_end = scan_idx;
                break;
            }
            if (ch == '}') {
                region_end = scan_idx;
                break;
            }
        }
    }
    command_region = command_region[0..region_end];
    var command_slice = std.mem.trim(u8, command_region, " \t\r\n");
    if (command_slice.len == 0) {
        if (options.allow_empty_command) {
            return try allocator.dupe(u8, &[_]u8{});
        }
        return ModuleParseError.MissingReturnCommand;
    }
    if (command_slice[command_slice.len - 1] == ';') {
        command_slice = std.mem.trim(u8, command_slice[0 .. command_slice.len - 1], " \t\r\n");
    }

    if (options.allow_empty_command and isCommentStart(command_slice)) {
        command_slice = command_slice[0..0];
    }

    if (command_slice.len == 0) {
        if (options.allow_empty_command) {
            return try allocator.dupe(u8, &[_]u8{});
        }
        return ModuleParseError.MissingReturnCommand;
    }
    return try allocator.dupe(u8, command_slice);
}

fn findReturnKeyword(body: []const u8) ?usize {
    var idx: usize = 0;
    while (idx < body.len) {
        const ch = body[idx];
        if (ch == '"' or ch == 0x27) {
            const quote = ch;
            idx += 1;
            while (idx < body.len) {
                if (body[idx] == '\\' and idx + 1 < body.len) {
                    idx += 2;
                    continue;
                }
                if (body[idx] == quote) {
                    idx += 1;
                    break;
                }
                idx += 1;
            }
            continue;
        }

        if (ch == '/' and idx + 1 < body.len) {
            const next = body[idx + 1];
            if (next == '/') {
                idx += 2;
                while (idx < body.len and body[idx] != '\n') : (idx += 1) {}
                continue;
            }
            if (next == '*') {
                idx += 2;
                while (idx < body.len) : (idx += 1) {
                    if (idx < body.len and body[idx - 1] == '*' and body[idx] == '/') {
                        idx += 1;
                        break;
                    }
                }
                continue;
            }
        }

        if (ch == '#') {
            idx += 1;
            while (idx < body.len and body[idx] != '\n') : (idx += 1) {}
            continue;
        }

        if (ch == 'r') {
            const keyword = "return";
            if (idx + keyword.len <= body.len and std.mem.eql(u8, body[idx .. idx + keyword.len], keyword)) {
                const before_ok = idx == 0 or !isIdentifierChar(body[idx - 1]);
                const after_idx = idx + keyword.len;
                const after_ok = after_idx >= body.len or !isIdentifierChar(body[after_idx]);
                if (before_ok and after_ok) return idx;
            }
        }

        idx += 1;
    }
    return null;
}

test "extractReturnCommand tolerates comment prologues" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const source =
        \\// doc comment
        \\# shell note
        \\/* block */
        \\return echo "hello"
    ;

    const command = try extractReturnCommand(gpa.allocator(), source, .{});
    defer gpa.allocator().free(command);
    try std.testing.expectEqualStrings("echo \"hello\"", command);
}

test "extractReturnCommand finds return after other statements" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const source =
        \\let ignored = "value"
        \\env.set("SOMETHING", ignored)
        \\return echo hi
    ;

    const command = try extractReturnCommand(gpa.allocator(), source, .{});
    defer gpa.allocator().free(command);
    try std.testing.expectEqualStrings("echo hi", command);
}

test "extractReturnCommand allows missing return for optional cases" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const source =
        \\let ignored = "value"
        \\env.set("SOMETHING", ignored)
    ;

    const command = try extractReturnCommand(gpa.allocator(), source, .{
        .require_return = false,
        .allow_empty_command = true,
    });
    defer gpa.allocator().free(command);
    try std.testing.expectEqual(@as(usize, 0), command.len);
}

test "extractReturnCommand allows bare return without command" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const source =
        \\return;
    ;

    const command = try extractReturnCommand(gpa.allocator(), source, .{
        .require_return = false,
        .allow_empty_command = true,
    });
    defer gpa.allocator().free(command);
    try std.testing.expectEqual(@as(usize, 0), command.len);
}

test "extractReturnCommand stops at return line" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const source =
        \\  if count > 1 {
        \\    return "plural"
        \\  } else if count == 1 {
        \\    return "singular"
        \\  } else {
        \\    return "empty"
        \\  }
    ;

    const command = try extractReturnCommand(gpa.allocator(), source, .{});
    defer gpa.allocator().free(command);
    try std.testing.expectEqualStrings("\"plural\"", command);
}

fn isAbsolutePath(path: []const u8) bool {
    if (path.len == 0) return false;
    if (path[0] == '/' or path[0] == '\\') return true;
    if (path.len >= 2 and std.ascii.isAlphabetic(path[0]) and path[1] == ':') return true;
    return false;
}

fn isSupportedLetLiteral(literal: []const u8) bool {
    if (literal.len == 0) return false;
    if (std.mem.eql(u8, literal, "true") or std.mem.eql(u8, literal, "false")) return true;

    var idx: usize = 0;
    if (literal[0] == '+' or literal[0] == '-') {
        idx = 1;
        if (idx == literal.len) return false;
    }

    var seen_digit = false;
    var seen_dot = false;
    while (idx < literal.len) : (idx += 1) {
        const ch = literal[idx];
        if (std.ascii.isDigit(ch)) {
            seen_digit = true;
            continue;
        }
        if (ch == '.' and !seen_dot) {
            seen_dot = true;
            continue;
        }
        return false;
    }

    return seen_digit;
}

test "parse let binding with annotation" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let greeting: Str = \"hello\"");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.literal);
    try std.testing.expectEqualStrings("greeting", result.name);
    try std.testing.expectEqualStrings("hello", result.value.literal);
}

test "parse let binding rejects trailing commands" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try std.testing.expectError(
        LetParseError.TrailingCharacters,
        parseLetBinding(gpa.allocator(), "let greeting = \"hi\" echo hi"),
    );
}

test "parse let binding captures numeric literal" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let count: Int = 42");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.literal);
    try std.testing.expectEqualStrings("count", result.name);
    try std.testing.expectEqualStrings("42", result.value.literal);
}

test "parse let binding captures boolean literal" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let enabled = true");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.literal);
    try std.testing.expectEqualStrings("enabled", result.name);
    try std.testing.expectEqualStrings("true", result.value.literal);
}

test "parse let binding captures struct type expression" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let Position = struct { x: Float, y: Float, }");
    try std.testing.expect(binding != null);
    const result = binding.?;
    try std.testing.expect(result.value == .type_expression);
    defer gpa.allocator().free(result.value.type_expression);
    try std.testing.expectEqualStrings("struct { x: Float, y: Float, }", result.value.type_expression);
}

test "parse let binding captures named type expression" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let MaybeInt = ?Int");
    try std.testing.expect(binding != null);
    const result = binding.?;
    try std.testing.expect(result.value == .type_expression);
    defer gpa.allocator().free(result.value.type_expression);
    try std.testing.expectEqualStrings("?Int", result.value.type_expression);
}

test "parse let binding rejects invalid struct type expression" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try std.testing.expectError(
        LetParseError.MissingClosingBrace,
        parseLetBinding(gpa.allocator(), "let Position = struct { x: Float"),
    );
}

test "parse error declaration captures union body" {
    const command =
        \\error FileError = union {
        \\  NotFound: { path: Str },
        \\  Unreadable: { path: Str },
        \\}
    ;
    const parsed = try parseErrorDeclaration(command);
    try std.testing.expect(parsed != null);
    const decl = parsed.?;
    try std.testing.expectEqualStrings("FileError", decl.name);
    try std.testing.expect(std.mem.startsWith(u8, decl.definition, "union"));
    try std.testing.expect(std.mem.indexOf(u8, decl.definition, "Unreadable") != null);
}

test "parse error declaration rejects missing body kind" {
    try std.testing.expectError(
        ErrorDeclarationParseError.MissingBodyKind,
        parseErrorDeclaration("error Something = struct { value: Int }"),
    );
}

test "let continuation detection tracks struct type expressions" {
    try std.testing.expect(letCommandNeedsMoreLines("let Result = struct {"));
    try std.testing.expect(letCommandNeedsMoreLines("let Result = struct {\n  value: ?Float,"));
    try std.testing.expect(!letCommandNeedsMoreLines("let Result = struct {\n  value: ?Float,\n}"));
    try std.testing.expect(!letCommandNeedsMoreLines(
        "let Result = struct { value: ?Float, } // trailing comment {",
    ));
}

test "let continuation detection counts parentheses and empty values" {
    try std.testing.expect(letCommandNeedsMoreLines("let MaybePair = Result("));
    try std.testing.expect(letCommandNeedsMoreLines("let MaybePair = Result(Float,\n  Float,"));
    try std.testing.expect(!letCommandNeedsMoreLines("let MaybePair = Result(Float, Float)"));
    try std.testing.expect(!letCommandNeedsMoreLines("let pattern = \"{value}\""));
    try std.testing.expect(letCommandNeedsMoreLines("let Something ="));
}

test "error declaration continuation detection tracks braces" {
    try std.testing.expect(errorDeclarationNeedsMoreLines("error ParserError = union {"));
    try std.testing.expect(
        errorDeclarationNeedsMoreLines(
            "error ParserError = union {\n  Missing: { path: Str },\n  Invalid: { value: Str },",
        ),
    );
    try std.testing.expect(
        !errorDeclarationNeedsMoreLines(
            "error ParserError = union {\n  Missing: { path: Str },\n  Invalid: { value: Str },\n}",
        ),
    );
    try std.testing.expect(errorDeclarationNeedsMoreLines("error ParserError = union"));
}

test "control flow continuation detection tracks if expressions" {
    try std.testing.expect(controlFlowNeedsMoreLines("if (value) {"));
    try std.testing.expect(controlFlowNeedsMoreLines("if (value) {\n  echo \"hi\"\n} else {"));
    try std.testing.expect(!controlFlowNeedsMoreLines("if (value) {\n  echo \"{ ok }\"\n}"));
    try std.testing.expect(
        !controlFlowNeedsMoreLines("if (value) {\n  echo \"hi\"\n} else {\n  echo \"bye\"\n}"),
    );
    try std.testing.expect(controlFlowNeedsMoreLines("if (value) {\n  // brace in comment }\n"));
}

test "control flow sanitizer replaces block bodies" {
    const allocator = std.testing.allocator;
    const sanitized = try sanitizeControlFlowSnippet(
        allocator,
        "if (value) |port| {\n  ensure(port == 8080, \"msg\")\n} else {\n  echo \"{value}\"\n}\n",
    );
    defer allocator.free(sanitized);
    try std.testing.expectEqualStrings("if (value) |port| { value } else { value }\n", sanitized);
}

test "script context bindings seed interpreter scopes" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();
    try context.declareStringBinding("greeting", "hello", false);
    try context.declareStringBinding("mutable_value", "42", true);

    var scopes = ScopeStack.init(gpa.allocator());
    defer scopes.deinit();

    try reseedInterpreterScopeWithContext(gpa.allocator(), &scopes, &context);

    const greeting = scopes.lookup("greeting");
    try std.testing.expect(greeting != null);
    try std.testing.expectEqualStrings("hello", greeting.?.value.string);

    const mutable_binding = scopes.lookup("mutable_value");
    try std.testing.expect(mutable_binding != null);
    try std.testing.expect(mutable_binding.?.is_mutable);
}

test "parse mut binding marks binding mutable" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "mut retries: Int = 2");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.literal);
    try std.testing.expectEqualStrings("retries", result.name);
    try std.testing.expectEqualStrings("2", result.value.literal);
    try std.testing.expect(result.is_mutable);
}

test "parse let binding ignores non-string assignments" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let value = echo hi");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.command);
    try std.testing.expectEqualStrings("value", result.name);
    try std.testing.expectEqualStrings("echo hi", result.value.command);
}

test "parse let binding captures pipeline commands" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let pipe = echo \"hi\" | upper");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.command);
    try std.testing.expectEqualStrings("pipe", result.name);
    try std.testing.expectEqualStrings("echo \"hi\" | upper", result.value.command);
}

test "trimCommandExpression unwraps parenthesized pipelines" {
    const command = "  (true | true)  ";
    try std.testing.expectEqualStrings("true | true", trimCommandExpression(command));
}

test "trimCommandExpression respects unmatched parentheses" {
    const command = "(echo hi) | upper";
    try std.testing.expectEqualStrings(command, trimCommandExpression(command));
}

test "trimCommandExpression ignores quoted parentheses" {
    const command = "(echo \"(\")";
    try std.testing.expectEqualStrings("echo \"(\"", trimCommandExpression(command));
}

test "parse let binding supports command arguments with leading dot" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let listing = ls ./tests");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.command);
    try std.testing.expectEqualStrings("listing", result.name);
    try std.testing.expectEqualStrings("ls ./tests", result.value.command);
}

test "parse let binding captures array literals" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let services = [\"api\", \"jobs\"]");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer freeOwnedStringList(gpa.allocator(), result.value.array_literal);

    try std.testing.expectEqualStrings("services", result.name);
    try std.testing.expect(result.value == .array_literal);
    try std.testing.expectEqual(@as(usize, 2), result.value.array_literal.len);
    try std.testing.expectEqualStrings("api", result.value.array_literal[0]);
    try std.testing.expectEqualStrings("jobs", result.value.array_literal[1]);
}

test "parse let binding captures object literals" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let thresholds = { api: 200, jobs: typed_port }");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer switch (result.value) {
        .object_literal => |*literal| @constCast(literal).deinit(gpa.allocator()),
        else => {},
    };
    try std.testing.expectEqualStrings("thresholds", result.name);
    try std.testing.expect(result.value == .object_literal);
    try std.testing.expectEqual(@as(usize, 2), result.value.object_literal.entries.len);
    try std.testing.expectEqualStrings("api", result.value.object_literal.entries[0].key);
    try std.testing.expect(result.value.object_literal.entries[0].value == .literal);
    try std.testing.expect(result.value.object_literal.entries[1].value == .identifier);
}

test "parse let binding allows trailing commas in object literals" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(
        gpa.allocator(),
        "let thresholds = { api: 200, jobs: typed_port, }",
    );
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer switch (result.value) {
        .object_literal => |*literal| @constCast(literal).deinit(gpa.allocator()),
        else => {},
    };
    try std.testing.expectEqualStrings("thresholds", result.name);
    try std.testing.expect(result.value == .object_literal);
    try std.testing.expectEqual(@as(usize, 2), result.value.object_literal.entries.len);
    try std.testing.expectEqualStrings("api", result.value.object_literal.entries[0].key);
    try std.testing.expect(result.value.object_literal.entries[0].value == .literal);
    try std.testing.expect(result.value.object_literal.entries[1].value == .identifier);
}

test "parse let binding captures zig-style object literals" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(
        gpa.allocator(),
        "let thresholds = .{ .api = 200, .jobs = typed_port }",
    );
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer switch (result.value) {
        .object_literal => |*literal| @constCast(literal).deinit(gpa.allocator()),
        else => {},
    };

    try std.testing.expectEqualStrings("thresholds", result.name);
    try std.testing.expect(result.value == .object_literal);
    try std.testing.expectEqual(@as(usize, 2), result.value.object_literal.entries.len);
    try std.testing.expectEqualStrings("api", result.value.object_literal.entries[0].key);
    try std.testing.expectEqualStrings("jobs", result.value.object_literal.entries[1].key);
    try std.testing.expect(result.value.object_literal.entries[0].value == .literal);
    try std.testing.expect(result.value.object_literal.entries[1].value == .identifier);
}

test "parse let binding captures multiline zig-style object literals" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const source =
        \\let result: Result = .{
        \\  .value = 1,
        \\  .ok = false,
        \\}
    ;

    const binding = try parseLetBinding(gpa.allocator(), source);
    try std.testing.expect(binding != null);
    var result = binding.?;
    defer switch (result.value) {
        .object_literal => |*literal| @constCast(literal).deinit(gpa.allocator()),
        else => {},
    };

    try std.testing.expectEqualStrings("result", result.name);
    try std.testing.expect(result.value == .object_literal);
    try std.testing.expectEqual(@as(usize, 2), result.value.object_literal.entries.len);
    try std.testing.expectEqualStrings("value", result.value.object_literal.entries[0].key);
    try std.testing.expectEqualStrings("ok", result.value.object_literal.entries[1].key);
    try std.testing.expect(result.value.object_literal.entries[0].value == .literal);
    try std.testing.expect(result.value.object_literal.entries[1].value == .literal);
}

test "parse let binding captures object field expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let job_threshold = thresholds.jobs");
    try std.testing.expect(binding != null);
    var result = binding.?;
    defer switch (result.value) {
        .binding_expression => |*expr| expr.deinit(gpa.allocator()),
        else => {},
    };
    try std.testing.expectEqualStrings("job_threshold", result.name);
    try std.testing.expect(result.value == .binding_expression);
    switch (result.value.binding_expression) {
        .object_field => |field| {
            try std.testing.expectEqualStrings("thresholds", field.object_name);
            try std.testing.expectEqualStrings("jobs", field.field_name);
        },
        else => {
            try std.testing.expect(false);
        },
    }
}

test "parse let binding captures array element expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let first_service = services[0]");
    try std.testing.expect(binding != null);
    var result = binding.?;
    defer switch (result.value) {
        .binding_expression => |*expr| expr.deinit(gpa.allocator()),
        else => {},
    };
    try std.testing.expect(result.value == .binding_expression);
    switch (result.value.binding_expression) {
        .array_element => |element| {
            try std.testing.expectEqualStrings("services", element.array_name);
            try std.testing.expectEqual(@as(usize, 0), element.index);
        },
        else => try std.testing.expect(false),
    }
}

test "parse let binding recognizes module function references" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var modules = ModuleRegistry.init(gpa.allocator());
    defer modules.deinit();

    const binding = try parseLetBindingWithOptions(
        gpa.allocator(),
        "let ensure = testing.ensure",
        .{ .modules = &modules },
    );
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer {
        gpa.allocator().free(result.value.module_function.module_alias);
        gpa.allocator().free(result.value.module_function.function_name);
    }
    try std.testing.expectEqualStrings("ensure", result.name);
    try std.testing.expectEqualStrings("testing", result.value.module_function.module_alias);
    try std.testing.expectEqualStrings("ensure", result.value.module_function.function_name);
}

test "parse let binding recognizes module imports" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var modules = ModuleRegistry.init(gpa.allocator());
    defer modules.deinit();

    const binding = try parseLetBindingWithOptions(
        gpa.allocator(),
        "let helpers = import(\"lib/helpers\")",
        .{ .modules = &modules },
    );
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.module_import.spec);
    try std.testing.expectEqualStrings("helpers", result.name);
    try std.testing.expect(result.value == .module_import);
    try std.testing.expectEqualStrings("lib/helpers", result.value.module_import.spec);
}

test "parse let binding recognizes inline module import members" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var modules = ModuleRegistry.init(gpa.allocator());
    defer modules.deinit();

    const binding = try parseLetBindingWithOptions(
        gpa.allocator(),
        "let ensure = import(\"lib/helpers\").ensure",
        .{ .modules = &modules },
    );
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer {
        gpa.allocator().free(result.value.module_import_member.spec);
        gpa.allocator().free(result.value.module_import_member.member_name);
    }
    try std.testing.expectEqualStrings("ensure", result.name);
    try std.testing.expect(result.value == .module_import_member);
    try std.testing.expectEqualStrings("lib/helpers", result.value.module_import_member.spec);
    try std.testing.expectEqualStrings("ensure", result.value.module_import_member.member_name);
}

test "parse let binding recognizes module invocations" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var modules = ModuleRegistry.init(gpa.allocator());
    defer modules.deinit();

    const binding = try parseLetBindingWithOptions(
        gpa.allocator(),
        "let sum = math.add(1, 2)",
        .{ .modules = &modules },
    );
    try std.testing.expect(binding != null);
    var result = binding.?;
    defer if (result.value == .module_invocation) {
        result.value.module_invocation.invocation.deinit(gpa.allocator());
        gpa.allocator().free(result.value.module_invocation.display_command);
    };
    try std.testing.expectEqualStrings("sum", result.name);
    try std.testing.expect(result.value == .module_invocation);
    try std.testing.expectEqualStrings("math.add(1, 2)", result.value.module_invocation.display_command);
    try std.testing.expectEqualStrings("math", result.value.module_invocation.invocation.alias);
    try std.testing.expectEqualStrings("add", result.value.module_invocation.invocation.function);
    try std.testing.expectEqual(@as(usize, 2), result.value.module_invocation.invocation.args.len);
    try std.testing.expectEqualStrings("1", result.value.module_invocation.invocation.args[0]);
    try std.testing.expectEqualStrings("2", result.value.module_invocation.invocation.args[1]);
}

test "parse let binding rejects module invocation catch clauses" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    try context.declareFunctionBinding("bootstrap", "script", "bootstrap", false);

    try std.testing.expectError(
        ModuleInvocationError.UnsupportedCatchClause,
        parseLetBindingWithOptions(
            gpa.allocator(),
            \\let recovered = bootstrap() catch |err| {
            \\  echo err
            \\}
        ,
            .{ .context = &context },
        ),
    );
}

test "parse let binding prefers object field references when modules are available" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    const entries = try gpa.allocator().alloc(ScriptContext.ObjectValue.ObjectEntry, 1);
    entries[0] = .{
        .key = try gpa.allocator().dupe(u8, "jobs"),
        .value = try gpa.allocator().dupe(u8, "450"),
    };
    var object = ScriptContext.ObjectValue{ .entries = entries };
    var object_cleanup = true;
    defer if (object_cleanup) object.deinit(gpa.allocator());
    try context.declareObjectBinding("thresholds", &object, false);
    object_cleanup = false;

    var modules = ModuleRegistry.init(gpa.allocator());
    defer modules.deinit();

    const binding = try parseLetBindingWithOptions(
        gpa.allocator(),
        "let job_threshold = thresholds.jobs",
        .{ .context = &context, .modules = &modules },
    );
    try std.testing.expect(binding != null);
    var result = binding.?;
    defer switch (result.value) {
        .binding_expression => |*expr| expr.deinit(gpa.allocator()),
        else => {},
    };
    try std.testing.expect(result.value == .binding_expression);
    switch (result.value.binding_expression) {
        .object_field => |field| {
            try std.testing.expectEqualStrings("thresholds", field.object_name);
            try std.testing.expectEqualStrings("jobs", field.field_name);
        },
        else => try std.testing.expect(false),
    }
}

test "parse let binding evaluates arithmetic expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const binding = try parseLetBinding(gpa.allocator(), "let expression_value = (2 + 3) * 2");
    try std.testing.expect(binding != null);
    const result = binding.?;
    defer gpa.allocator().free(result.value.literal);
    try std.testing.expectEqualStrings("expression_value", result.name);
    try std.testing.expectEqualStrings("10", result.value.literal);
}

test "parse let binding rejects invalid arithmetic expressions" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try std.testing.expectError(
        LetParseError.InvalidExpression,
        parseLetBinding(gpa.allocator(), "let broken = (2 + 3"),
    );
}

test "parse let binding rejects unknown function bindings" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    try std.testing.expectError(
        LetParseError.UnknownFunctionBinding,
        parseLetBindingWithOptions(
            gpa.allocator(),
            "let has_port = maybe_lookup(true)",
            .{ .context = &context },
        ),
    );
}

test "parse identifier assignment captures numeric literal" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const assignment = try parseIdentifierAssignment(gpa.allocator(), "count = 3");
    try std.testing.expect(assignment != null);
    var result = assignment.?;
    defer result.deinit(gpa.allocator());
    try std.testing.expectEqualStrings("count", result.name);
    try std.testing.expect(result.value == .literal);
    try std.testing.expectEqualStrings("3", result.value.literal);
}

test "parse identifier assignment respects quoted values" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const assignment = try parseIdentifierAssignment(gpa.allocator(), "greeting = \"hi there\"");
    try std.testing.expect(assignment != null);
    var result = assignment.?;
    defer result.deinit(gpa.allocator());
    try std.testing.expectEqualStrings("greeting", result.name);
    try std.testing.expect(result.value == .literal);
    try std.testing.expectEqualStrings("hi there", result.value.literal);
}

test "parse identifier assignment handles self addition" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const assignment = try parseIdentifierAssignment(gpa.allocator(), "count = count + 2");
    try std.testing.expect(assignment != null);
    var result = assignment.?;
    defer result.deinit(gpa.allocator());
    try std.testing.expectEqualStrings("count", result.name);
    try std.testing.expect(result.value == .arithmetic);
    try std.testing.expectEqual(@as(i64, 2), result.value.arithmetic.delta);
}

test "parse identifier assignment handles compound addition" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const assignment = try parseIdentifierAssignment(gpa.allocator(), "retries += 5");
    try std.testing.expect(assignment != null);
    var result = assignment.?;
    defer result.deinit(gpa.allocator());
    try std.testing.expectEqualStrings("retries", result.name);
    try std.testing.expect(result.value == .arithmetic);
    try std.testing.expectEqual(@as(i64, 5), result.value.arithmetic.delta);
}

test "token interpolation applies context bindings" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();
    try context.declareStringBinding("greeting", "hello", false);

    var tokens = TokenList.init(gpa.allocator());
    defer tokens.deinit();
    try tokens.populate("echo \"${greeting}, world\"");

    const bindings = BindingProvider{ .context = &context };
    var issue: ?[]const u8 = null;
    try interpolateTokensWithBindings(&tokens, bindings, &issue);
    try std.testing.expect(issue == null);

    try std.testing.expectEqual(@as(usize, 2), tokens.items.len);
    try std.testing.expectEqualStrings("echo", tokens.items[0]);
    try std.testing.expectEqualStrings("hello, world", tokens.items[1]);
}

test "token interpolation resolves object fields" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();

    const entries = try gpa.allocator().alloc(ScriptContext.ObjectValue.ObjectEntry, 1);
    var entries_owned = true;
    defer if (entries_owned) gpa.allocator().free(entries);

    entries[0].key = try gpa.allocator().dupe(u8, "value");
    var key_owned = true;
    defer if (key_owned) gpa.allocator().free(entries[0].key);

    entries[0].value = try gpa.allocator().dupe(u8, "1");
    var value_owned = true;
    defer if (value_owned) gpa.allocator().free(entries[0].value);

    var object_value = ScriptContext.ObjectValue{ .entries = entries };
    var object_owned = false;
    defer if (object_owned) object_value.deinit(gpa.allocator());

    object_owned = true;
    entries_owned = false;
    key_owned = false;
    value_owned = false;

    try context.declareObjectBinding("result", &object_value, false);
    object_owned = false;

    var tokens = TokenList.init(gpa.allocator());
    defer tokens.deinit();
    try tokens.populate("echo ${result.value}");

    const bindings = BindingProvider{ .context = &context };
    var issue: ?[]const u8 = null;
    try interpolateTokensWithBindings(&tokens, bindings, &issue);
    try std.testing.expect(issue == null);
    try std.testing.expectEqual(@as(usize, 2), tokens.items.len);
    try std.testing.expectEqualStrings("echo", tokens.items[0]);
    try std.testing.expectEqualStrings("1", tokens.items[1]);
}

test "token interpolation preserves unknown bindings" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();
    try context.declareStringBinding("greeting", "hello", false);

    var tokens = TokenList.init(gpa.allocator());
    defer tokens.deinit();
    try tokens.populate("echo \"${unknown}-${greeting}\"");

    const bindings = BindingProvider{ .context = &context };
    var issue: ?[]const u8 = null;
    try interpolateTokensWithBindings(&tokens, bindings, &issue);
    try std.testing.expect(issue == null);

    try std.testing.expectEqual(@as(usize, 2), tokens.items.len);
    try std.testing.expectEqualStrings("echo", tokens.items[0]);
    try std.testing.expectEqualStrings("${unknown}-hello", tokens.items[1]);
}

test "token interpolation rejects array bindings" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();
    const values = [_][]const u8{ "api", "jobs" };
    try context.declareStringArrayBinding("services", &values, false);

    var tokens = TokenList.init(gpa.allocator());
    defer tokens.deinit();
    try tokens.populate("echo ${services}");

    const bindings = BindingProvider{ .context = &context };
    var issue: ?[]const u8 = null;
    try std.testing.expectError(
        error.ArrayBindingUnsupported,
        interpolateTokensWithBindings(&tokens, bindings, &issue),
    );
    try std.testing.expect(issue != null);
    try std.testing.expectEqualStrings("services", issue.?);
}

test "parse module invocation captures alias and args" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const parsed = try parseModuleInvocation(gpa.allocator(), "lib.greet(\"hi\")", null);
    try std.testing.expect(parsed != null);
    var invocation = parsed.?;
    defer invocation.deinit(gpa.allocator());
    try std.testing.expectEqualStrings("lib", invocation.alias);
    try std.testing.expectEqualStrings("greet", invocation.function);
    try std.testing.expectEqual(@as(usize, 1), invocation.args.len);
    try std.testing.expectEqualStrings("hi", invocation.args[0]);
}

test "parse module invocation resolves function aliases" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var context = ScriptContext.init(gpa.allocator());
    defer context.deinit();
    try context.declareFunctionBinding("ensure", "testing", "ensure", false);

    const parsed = try parseModuleInvocation(gpa.allocator(), "ensure(\"ok\")", &context);
    try std.testing.expect(parsed != null);
    var invocation = parsed.?;
    defer invocation.deinit(gpa.allocator());
    try std.testing.expectEqualStrings("testing", invocation.alias);
    try std.testing.expectEqualStrings("ensure", invocation.function);
    try std.testing.expectEqual(@as(usize, 1), invocation.args.len);
    try std.testing.expectEqualStrings("ok", invocation.args[0]);
}

test "parse module invocation ignores plain commands" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const parsed = try parseModuleInvocation(gpa.allocator(), "echo hi", null);
    try std.testing.expect(parsed == null);
}

test "module registry parses module functions via parser" {
    var modules = ModuleRegistry.init(std.testing.allocator);
    defer modules.deinit();

    var module = ModuleRegistry.Module{
        .alias = try std.testing.allocator.dupe(u8, "inline"),
        .spec = try std.testing.allocator.dupe(u8, "inline"),
        .path = try std.testing.allocator.dupe(u8, "inline.rn"),
        .functions = .empty,
        .values = .empty,
    };
    defer module.deinit(std.testing.allocator);

    const source =
        \\fn greet(name: Str) Void {
        \\  return echo name
        \\}
    ;

    try modules.parseModuleFunctions(&module, source);
    try std.testing.expectEqual(@as(usize, 1), module.functions.items.len);
    const fn_entry = module.functions.items[0];
    try std.testing.expectEqualStrings("greet", fn_entry.name);
    try std.testing.expectEqual(@as(usize, 1), fn_entry.params.len);
    try std.testing.expectEqualStrings("name", fn_entry.params[0]);
    try std.testing.expectEqualStrings("echo name", fn_entry.command);
}

fn parseCommandLine(allocator: Allocator, argv: []const []const u8) !ParseResult {
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
            if (std.mem.startsWith(u8, arg, trace_prefix)) {
                const value = arg[trace_prefix.len..];
                if (value.len == 0) return usageError(allocator, "--trace requires a topic name", .{});
                try trace_topics.append(try dupSlice(allocator, value));
                continue;
            }
            if (argEqual(arg, "--trace")) {
                if (idx >= argv.len) return usageError(allocator, "--trace requires a topic name", .{});
                const value = argv[idx];
                idx += 1;
                if (value.len == 0) return usageError(allocator, "--trace requires a topic name", .{});
                try trace_topics.append(try dupSlice(allocator, value));
                continue;
            }
            if (std.mem.startsWith(u8, arg, module_prefix)) {
                const value = arg[module_prefix.len..];
                if (value.len == 0) return usageError(allocator, "--module-path requires a directory", .{});
                try module_paths.append(try dupSlice(allocator, value));
                continue;
            }
            if (argEqual(arg, "--module-path")) {
                if (idx >= argv.len) return usageError(allocator, "--module-path requires a directory", .{});
                const value = argv[idx];
                idx += 1;
                if (value.len == 0) return usageError(allocator, "--module-path requires a directory", .{});
                try module_paths.append(try dupSlice(allocator, value));
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
            script_path = try dupSlice(allocator, arg);
            parsing_options = false;
            continue;
        }

        try script_args.append(try dupSlice(allocator, arg));
    }

    if (script_path != null and repl_requested) {
        return usageError(allocator, "Cannot combine --repl with a script path.", .{});
    }
    if (script_path == null and !repl_requested) {
        return usageError(allocator, "Provide a Runic script path or pass --repl for interactive mode.", .{});
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
        },
    };
}

fn printUsage(writer: *std.Io.Writer) !void {
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

    const key = try dupSlice(allocator, raw[0..eq_index]);
    errdefer allocator.free(key);

    const value = try dupSlice(allocator, raw[eq_index + 1 ..]);
    return .{ .key = key, .value = value };
}

fn usageError(allocator: Allocator, comptime message_fmt: []const u8, args: anytype) !ParseResult {
    const message = try std.fmt.allocPrint(allocator, message_fmt, args);
    return ParseResult{ .usage_error = message };
}

fn dupSlice(allocator: Allocator, bytes: []const u8) ![]const u8 {
    const copy = try allocator.alloc(u8, bytes.len);
    @memcpy(copy, bytes);
    return copy;
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

fn freeOwnedStringList(allocator: Allocator, values: [][]u8) void {
    if (values.len == 0) return;
    for (values) |value| allocator.free(value);
    allocator.free(values);
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

test "parse script invocation with tracing" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const args = [_][]const u8{
        "runic",
        "--trace",
        "lexer",
        "--module-path",
        "lib",
        "script.rn",
        "--",
        "--flag",
    };

    const argv = args[0..args.len];
    const result = try parseCommandLine(gpa.allocator(), @constCast(argv));
    defer switch (result) {
        .ready => |config| {
            var cfg = config;
            cfg.deinit(gpa.allocator());
        },
        .usage_error => |message| gpa.allocator().free(message),
        else => {},
    };

    const config = switch (result) {
        .ready => |cfg| cfg,
        .usage_error => return error.UnexpectedUsageFailure,
        .show_help => return error.UnexpectedHelp,
    };
    try std.testing.expect(std.meta.activeTag(config.mode) == .script);
    const script = config.mode.script;
    try std.testing.expectEqualStrings("script.rn", script.path);
    try std.testing.expectEqual(@as(usize, 1), script.args.len);
    try std.testing.expectEqualStrings("--flag", script.args[0]);
    try std.testing.expectEqual(@as(usize, 1), config.trace_topics.len);
    try std.testing.expectEqualStrings("lexer", config.trace_topics[0]);
    try std.testing.expectEqual(@as(usize, 1), config.module_paths.len);
    try std.testing.expectEqualStrings("lib", config.module_paths[0]);
}

test "parse repl invocation with env override" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const args = [_][]const u8{
        "runic",
        "--repl",
        "--env",
        "PORT=8080",
    };

    const argv = args[0..args.len];
    const result = try parseCommandLine(gpa.allocator(), @constCast(argv));
    defer switch (result) {
        .ready => |config| {
            var cfg = config;
            cfg.deinit(gpa.allocator());
        },
        .usage_error => |message| gpa.allocator().free(message),
        else => {},
    };

    const config = switch (result) {
        .ready => |cfg| cfg,
        .usage_error => return error.UnexpectedUsageFailure,
        .show_help => return error.UnexpectedHelp,
    };
    try std.testing.expect(std.meta.activeTag(config.mode) == .repl);
    try std.testing.expectEqual(@as(usize, 1), config.env_overrides.len);
    try std.testing.expectEqualStrings("PORT", config.env_overrides[0].key);
    try std.testing.expectEqualStrings("8080", config.env_overrides[0].value);
}

test "comment detection recognizes // and #" {
    try std.testing.expect(isCommentLine("// todo"));
    try std.testing.expect(isCommentLine("# note"));
    try std.testing.expect(!isCommentLine("echo hi"));
}

test "script metadata populates env map with args" {
    var env_map = std.process.EnvMap.init(std.testing.allocator);
    defer env_map.deinit();

    const args = [_][]const u8{ "one", "two" };
    const invocation = CliConfig.ScriptInvocation{
        .path = "demo.rn",
        .args = args[0..],
    };
    try exposeScriptMetadata(&env_map, invocation);

    try std.testing.expectEqualStrings("demo.rn", env_map.get("RUNIC_SCRIPT_PATH").?);
    try std.testing.expectEqualStrings("2", env_map.get("RUNIC_ARGC").?);
    try std.testing.expectEqualStrings("one", env_map.get("RUNIC_ARG_0").?);
    try std.testing.expectEqualStrings("two", env_map.get("RUNIC_ARG_1").?);
}
