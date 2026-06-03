const std = @import("std");
const ast = @import("../frontend/ast.zig");
const rainbow = @import("../rainbow.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;
const token = @import("../frontend/token.zig");

const Scope = @import("scope.zig").Scope;

const logging_name = "TYPE_CHECKER";
const prefix_color = rainbow.beginColor(.blue);
const span_color = rainbow.beginBgColor(.green) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

pub const TypeChecker = struct {
    arena: std.heap.ArenaAllocator,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    logging_enabled: bool,
    document_store: *DocumentStore,
    modules: std.StringArrayHashMapUnmanaged(*Scope),
    env: ?*const std.process.EnvMap = null,
    /// Stack of the enclosing functions' declared stdout types. Pushed when a
    /// function body is type-checked and consulted by `runYield` so that every
    /// `yield &1` is validated in the scope where it actually appears (e.g.
    /// inside a `for (&0) |v| { yield v }` loop, where `v` is only bound in the
    /// loop's child scope). A null entry means the enclosing function declared
    /// no stdout type, so its yields are unconstrained.
    stdout_type_stack: std.ArrayListUnmanaged(?*const ast.TypeExpr) = .empty,

    pub const Error = Scope.Error ||
        std.fs.File.OpenError ||
        std.fs.File.ReadError ||
        std.fs.File.StatError ||
        std.Io.Writer.Error ||
        DocumentStore.Error ||
        error{
            BindingPatternNotSupported,
            DocumentNotParsed,
            ErrorNotInErrorSet,
            FileTooBig,
            ForSourcesAndBindingsNeedToBeTheSameLength,
            IdentifierNotFound,
            MemberAccessOnOptional,
            MemberObjectTypeUndefined,
            MemberNotFound,
            ModuleNotFound,
            TypeMismatch,
            UnresolvedTypeLiteral,
            UnsupportedExpression,
            UnsupportedMemberAccess,
            UnsupportedStatement,
            UnsupportedTypeExpression,
            UnsupportedTypeResolve,
        };

    pub const Diagnostic = struct {
        err: Error,
        _span: ast.Span,
        message: []const u8,
        _severity: Severity,

        pub const Severity = enum {
            @"error",
            warning,
            information,
            hint,
        };

        pub fn span(self: Diagnostic) ast.Span {
            return self._span;
        }

        pub fn severity(self: Diagnostic) []const u8 {
            return @tagName(self._severity);
        }

        pub fn path(self: Diagnostic) []const u8 {
            return self.span().start.file;
        }
    };

    pub const Result = union(enum) {
        err: struct {
            _diagnostics: []Diagnostic,

            pub fn diagnostics(self: @This()) []const Diagnostic {
                return self._diagnostics;
            }
        },
        success,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        document_store: *DocumentStore,
        env: ?*const std.process.EnvMap,
    ) TypeChecker {
        const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_" ++ logging_name) catch "";
        defer allocator.free(logging_enabled_s);
        const logging_enabled = std.mem.eql(u8, logging_enabled_s, "1");

        return .{
            .arena = .init(allocator),
            .logging_enabled = logging_enabled,
            .document_store = document_store,
            .modules = .empty,
            .env = env,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        for (self.modules.keys()) |key| {
            self.arena.child_allocator.free(key);
        }
        self.arena.deinit();
    }

    fn reportSpanError(
        self: *TypeChecker,
        span: ast.Span,
        err: Error,
        severity: Diagnostic.Severity,
        comptime fmt: []const u8,
        args: anytype,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}\n", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, span);
        try self.logWithoutPrefix("error message: " ++ fmt ++ "\n", args);

        try self.diagnostics.append(self.arena.allocator(), .{
            .err = err,
            ._span = span,
            .message = try std.fmt.allocPrint(
                self.arena.allocator(),
                fmt,
                args,
            ),
            ._severity = severity,
        });
    }

    fn FallbackFormatter(comptime Optional: type, comptime Fallback: type) type {
        return struct {
            optional: Optional,
            fallback: Fallback,

            pub fn init(optional: Optional, fallback: Fallback) @This() {
                return .{
                    .optional = optional,
                    .fallback = fallback,
                };
            }

            pub fn format(self: @This(), writer: *std.Io.Writer) !void {
                if (self.optional) |o| return writer.print("{f}", .{o});
                try writer.print("{f}", .{self.fallback});
            }
        };
    }

    fn reportAssignmentError(
        self: *TypeChecker,
        expected: anytype,
        actual: anytype,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, options.span);

        const binding_formatter = FallbackFormatter(
            @TypeOf(options.binding_alias),
            @TypeOf(expected),
        ).init(options.binding_alias, expected);

        const assignment_formatter = FallbackFormatter(
            @TypeOf(options.assignment_alias),
            @TypeOf(actual),
        ).init(options.assignment_alias, actual);

        try self.reportSpanError(
            options.span,
            Error.TypeMismatch,
            .@"error",
            "expected type {f}, actual: {f}",
            .{ binding_formatter, assignment_formatter },
        );
    }

    pub fn log(self: *@This(), comptime fmt: []const u8, args: anytype) Error!void {
        if (!self.logging_enabled) return;

        var stderr = std.fs.File.stderr().writer(&.{});
        const writer = &stderr.interface;

        try writer.print("[{s}{*}{s}]\n", .{ prefix_color, self, end_color });
        try writer.print(fmt ++ "\n", args);
        try writer.flush();
    }

    pub fn logWithoutPrefix(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        if (!self.logging_enabled) return;

        var stderr = std.fs.File.stderr().writer(&.{});
        const writer = &stderr.interface;

        try writer.print(fmt, args);
    }

    pub fn logTypeCheckTrace(self: *TypeChecker, label: []const u8, span: ast.Span) !void {
        try self.log("{s}:{}:{}: {s}", .{ span.start.file, span.start.line, span.start.column, label });
    }

    fn logTypeCheckStatement(self: *TypeChecker, statement: *const ast.Statement) !void {
        if (!self.logging_enabled) return;

        const span = statement.span();
        const source = try self.document_store.getSource(span.start.file);

        try self.logTypeCheckSpan(span, source);
    }

    fn logTypeCheckExpression(self: *TypeChecker, expr: *const ast.Expression) !void {
        if (!self.logging_enabled) return;

        const span = expr.span();
        const source = try self.document_store.getSource(span.start.file);

        try self.logTypeCheckSpan(span, source);
    }

    fn logTypeCheckTypeExpression(self: *TypeChecker, expr: *const ast.TypeExpr) !void {
        if (!self.logging_enabled) return;

        const span = expr.span();
        const source = try self.document_store.getSource(span.start.file);

        try self.logTypeCheckSpan(span, source);
    }

    fn logTypeCheckSpan(self: *TypeChecker, span: ast.Span, source: []const u8) !void {
        try self.logWithoutPrefix("{s}:\n", .{span.start.file});

        var lineIt = std.mem.splitScalar(u8, source, '\n');
        var i: usize = 0;
        while (lineIt.next()) |line| : (i += 1) {
            if (i >= span.start.line -| 3 and i <= span.end.line +| 3) {
                if (span.start.line == i + 1 and span.end.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 ..],
                        end_color,
                    });
                } else if (span.end.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line[0 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line - 1 <= i and i <= span.end.line - 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line,
                        end_color,
                    });
                } else {
                    try self.logWithoutPrefix("{:>4}:{s}\n", .{ i + 1, line });
                }
            }
        }
    }

    pub fn allocTypeExpression(self: *TypeChecker, type_expr: ast.TypeExpr) Error!*const ast.TypeExpr {
        const ptr = try self.arena.allocator().create(ast.TypeExpr);
        ptr.* = type_expr;
        return ptr;
    }

    pub fn typeCheck(self: *TypeChecker, path: []const u8) Error!Result {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name ++ ": {s}", .{path});

        _ = try self.scopesFromAst(path);

        return self.compileResult();
    }

    fn scopesFromAst(self: *TypeChecker, path: []const u8) Error!*Scope {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name ++ ": {s}", .{path});

        if (self.modules.contains(path)) return self.modules.get(path).?;

        const script = try self.document_store.getAst(path) orelse return error.DocumentNotParsed;

        const scope = try self.arena.allocator().create(Scope);
        scope.* = .init(script.span);

        const global_scope = try addGlobalScope(self.arena.allocator(), scope);

        if (script.signature) |signature| {
            switch (signature.params) {
                ._non_variadic => |params| for (params) |param| {
                    switch (param.pattern.*) {
                        .discard => {},
                        .identifier => |identifier| {
                            try global_scope.declare(
                                self.arena.allocator(),
                                identifier,
                                param.type_annotation,
                                true,
                                false,
                            );
                        },
                        .tuple, .record => return error.UnsupportedStatement,
                    }
                },
                ._variadic => return error.UnsupportedStatement,
            }
        }

        const path_owned = try self.arena.child_allocator.dupe(u8, path);
        try self.modules.put(
            self.arena.allocator(),
            path_owned,
            global_scope,
        );

        var root_block = ast.Block{
            .statements = script.statements,
            .span = script.span,
        };
        try self.runBlock(global_scope, &root_block);

        return scope;
    }

    pub fn invalidateDocument(self: *TypeChecker, path: []const u8) void {
        if (self.modules.fetchSwapRemove(path)) |entry| {
            self.arena.child_allocator.free(entry.key);
        }
        var i: usize = 0;
        while (i < self.diagnostics.items.len) {
            const d = self.diagnostics.items[i];

            if (std.mem.eql(u8, d.path(), path)) {
                _ = self.diagnostics.swapRemove(i);
                continue;
            }

            i += 1;
        }
    }

    fn getModuleScope(self: *TypeChecker, path: []const u8) Error!*Scope {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name ++ ": {s}", .{path});

        return self.modules.get(path) orelse {
            return error.ModuleNotFound;
        };
    }

    fn requestModuleScope(self: *TypeChecker, module: ast.TypeExpr.ModuleType) Error!?*Scope {
        return self.getModuleScope(module.path) catch |err| switch (err) {
            Error.ModuleNotFound => self.scopesFromAst(module.path) catch |err_| switch (err_) {
                DocumentStore.Error.DocumentNotFound => {
                    std.log.err("document not found: {s}", .{module.path});
                    try self.reportSpanError(
                        module.span,
                        Error.ModuleNotFound,
                        .@"error",
                        "module {s} not found",
                        .{module.path},
                    );
                    return null;
                },
                else => err_,
            },
            else => err,
        };
    }

    pub fn resolveModuleScopeForMemberCompletion(
        self: *TypeChecker,
        module: ast.TypeExpr.ModuleType,
    ) Error!?*Scope {
        return self.requestModuleScope(module);
    }

    fn allocStringType(self: *TypeChecker) Error!*const ast.TypeExpr {
        const byte_type = try self.allocTypeExpression(.global(.byte));
        return self.allocTypeExpression(.{ .array = .{
            .element = byte_type,
            .span = .global,
        } });
    }

    fn buildModuleValueType(
        self: *TypeChecker,
        module: ast.TypeExpr.ModuleType,
    ) Error!?*const ast.TypeExpr {
        const module_scope = try self.requestModuleScope(module) orelse return null;

        var public_count: usize = 0;
        var it_count = module_scope.bindings.iterator();
        while (it_count.next()) |entry| {
            if (entry.value_ptr.is_pub) public_count += 1;
        }

        const fields = try self.arena.allocator().alloc(ast.TypeExpr.StructField, 3 + public_count);
        fields[0] = .{
            .name = ast.Identifier.global("stdout"),
            .type_expr = try self.allocStringType(),
            .span = .global,
        };
        fields[1] = .{
            .name = ast.Identifier.global("stderr"),
            .type_expr = try self.allocStringType(),
            .span = .global,
        };
        fields[2] = .{
            .name = ast.Identifier.global("exit_code"),
            .type_expr = try self.allocTypeExpression(.global(.integer)),
            .span = .global,
        };

        var i: usize = 3;
        var it = module_scope.bindings.iterator();
        while (it.next()) |entry| {
            const binding = entry.value_ptr.*;
            if (!binding.is_pub) continue;
            fields[i] = .{
                .name = binding.identifier,
                .type_expr = binding.type_expr orelse try self.allocTypeExpression(.global(.void)),
                .span = binding.identifier.span,
            };
            i += 1;
        }

        return self.allocTypeExpression(.{ .struct_type = .{
            .fields = fields,
            .decls = &.{},
            .span = module.span,
        } });
    }

    fn runBlock(self: *TypeChecker, scope: *Scope, block: *ast.Block) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, block.span);

        for (block.statements) |statement| {
            try self.runStatement(scope, statement);
        }
    }

    fn runBlockInNewScope(self: *TypeChecker, scope: *Scope, block: *ast.Block) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, block.span);

        const block_scope = try scope.addChild(self.arena.allocator(), block.span);
        try self.runBlock(block_scope, block);
    }

    fn runStatement(self: *TypeChecker, scope: *Scope, statement: *ast.Statement) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, statement.span());

        try self.log("<{s}>", .{@tagName(statement.*)});
        try self.logTypeCheckStatement(statement);

        return switch (statement.*) {
            .type_binding_decl => |*type_binding_decl| self.runTypeBindingDecl(scope, type_binding_decl),
            .binding_decl => |*binding_decl| self.runBindingDecl(scope, binding_decl),
            .return_stmt => |*return_stmt| self.runReturn(scope, return_stmt),
            .exit_stmt => |*exit_stmt| self.runExit(scope, exit_stmt),
            .yield_stmt => |*yield_stmt| self.runYield(scope, yield_stmt),
            .expression => |*expr_stmt| self.runExpressionStatement(scope, expr_stmt),
            else => error.UnsupportedStatement,
        };
    }

    fn runReturn(self: *TypeChecker, scope: *Scope, return_stmt: *ast.ReturnStmt) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, return_stmt.span);

        if (return_stmt.value) |value| try self.runExpression(scope, value);
    }

    fn runYield(self: *TypeChecker, scope: *Scope, yield_stmt: *ast.YieldStmt) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, yield_stmt.span);

        try self.runExpression(scope, yield_stmt.value);

        // Only `yield`s to stdout (&1) are constrained by the enclosing
        // function's declared stdout type; stderr (&2) carries untyped
        // diagnostic output. Validating here (rather than in a separate body
        // walk) means the yielded expression is resolved in the exact scope it
        // appears in, including loop-capture bindings like `for (&0) |v|`.
        if (yield_stmt.fd != 1) return;
        if (self.stdout_type_stack.items.len == 0) return;
        const declared_stdout = self.stdout_type_stack.items[self.stdout_type_stack.items.len - 1] orelse return;
        const yielded = try self.resolveExprType(scope, yield_stmt.value) orelse return;
        const resolved = try self.resolvePipeType(scope, yielded) orelse return;
        if (self.pipeTypesEqual(resolved, declared_stdout)) return;
        try self.reportSpanError(
            yield_stmt.span,
            Error.TypeMismatch,
            .@"error",
            "yield type mismatch: function yields {f}, but declared stdout type is {f}",
            .{ resolved, declared_stdout },
        );
    }

    fn runExit(self: *TypeChecker, scope: *Scope, exit_stmt: *ast.ExitStmt) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, exit_stmt.span);

        if (exit_stmt.value) |value| try self.runExpression(scope, value);
    }

    fn runTypeBindingDecl(
        self: *TypeChecker,
        scope: *Scope,
        type_binding_decl: *ast.TypeBindingDecl,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, type_binding_decl.span);

        try self.runTypeExpression(scope, type_binding_decl.type_expr);

        const resolved_type_expr = try self.resolveTypeExpr(scope, type_binding_decl.type_expr);

        scope.declare(
            self.arena.allocator(),
            type_binding_decl.identifier,
            resolved_type_expr,
            type_binding_decl.is_pub,
            false,
        ) catch |err| try switch (err) {
            error.IdentifierAlreadyDeclared => {
                try self.reportSpanError(
                    type_binding_decl.identifier.span,
                    error.IdentifierAlreadyDeclared,
                    .@"error",
                    "identifier {s} already declared",
                    .{type_binding_decl.identifier.name},
                );
            },
            else => err,
        };
    }

    fn runBindingDecl(
        self: *TypeChecker,
        scope: *Scope,
        binding_decl: *ast.BindingDecl,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, binding_decl.span);

        const binding_annotation_type_expr = brk: {
            if (binding_decl.annotation) |annotation| {
                break :brk try self.resolveTypeExpr(scope, annotation);
            } else {
                break :brk null;
            }
        };

        try self.runExpression(scope, binding_decl.initializer);

        const initializer_type = try self.resolveExprType(scope, binding_decl.initializer);

        const type_expr = binding_annotation_type_expr orelse initializer_type;

        if (binding_annotation_type_expr) |annotation_type| {
            if (initializer_type) |init_type| {
                try self.validateTypeAssignment(
                    annotation_type,
                    init_type,
                    .{ .span = binding_decl.initializer.span() },
                );
            }
        }

        if (type_expr) |t| try self.runTypeExpression(scope, t);

        try self.runBindingPattern(
            scope,
            binding_decl.pattern,
            type_expr,
            binding_decl.is_pub,
            binding_decl.is_mutable,
        );
    }

    fn resolveTypeExpr(
        self: *TypeChecker,
        scope: *Scope,
        type_expr: *const ast.TypeExpr,
    ) Error!*const ast.TypeExpr {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, type_expr.span());
        try self.log("<{s}>", .{@tagName(type_expr.*)});
        try self.logTypeCheckTypeExpression(type_expr);

        return switch (type_expr.*) {
            .identifier => |*identifier| self.resolveTypeIdentifierToAlias(scope, identifier),
            .optional => |optional| try self.allocTypeExpression(.{
                .optional = .{
                    .child = try self.resolveTypeExpr(scope, optional.child),
                    .span = optional.span,
                },
            }),
            .promise => |promise| try self.allocTypeExpression(.{
                .promise = .{
                    .child = try self.resolveTypeExpr(scope, promise.child),
                    .span = promise.span,
                },
            }),
            .error_union => |error_union| try self.allocTypeExpression(.{
                .error_union = .{
                    .err_set = try self.resolveTypeExpr(scope, error_union.err_set),
                    .payload = try self.resolveTypeExpr(scope, error_union.payload),
                    .span = error_union.span,
                },
            }),
            .array => |array| try self.allocTypeExpression(.{
                .array = .{
                    .element = try self.resolveTypeExpr(scope, array.element),
                    .span = array.span,
                },
            }),
            else => type_expr,
        };
    }

    fn resolveTypeIdentifierToAlias(
        self: *TypeChecker,
        scope: *Scope,
        identifier: *const ast.TypeExpr.NamedType,
    ) Error!*const ast.TypeExpr {
        const name = identifier.path.segments[0].name;

        const binding = scope.lookup(name) orelse {
            try self.reportSpanError(
                identifier.span,
                Error.IdentifierNotFound,
                .@"error",
                "type {s} not declared",
                .{name},
            );

            return try self.allocTypeExpression(.{ .failed = .{ .span = identifier.span } });
        };

        const type_expr = binding.type_expr orelse try self.allocTypeExpression(
            .{ .failed = .{ .span = identifier.span } },
        );

        return try self.allocTypeExpression(.{
            .alias = .{
                .name = name,
                .span = identifier.span,
                .type_expr = type_expr,
            },
        });
    }

    fn runBindingPattern(
        self: *TypeChecker,
        scope: *Scope,
        pattern: *ast.BindingPattern,
        type_expr: ?*const ast.TypeExpr,
        is_pub: bool,
        is_mutable: bool,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, pattern.span());

        switch (pattern.*) {
            .identifier => |identifier| {
                scope.declare(
                    self.arena.allocator(),
                    identifier,
                    type_expr,
                    is_pub,
                    is_mutable,
                ) catch |err| try switch (err) {
                    error.IdentifierAlreadyDeclared => {
                        try self.reportSpanError(
                            pattern.span(),
                            error.IdentifierAlreadyDeclared,
                            .@"error",
                            "identifier {s} already declared",
                            .{identifier.name},
                        );
                    },
                    else => err,
                };
            },
            .discard => {},
            .tuple, .record => return error.BindingPatternNotSupported,
        }
    }

    fn runFnDecl(self: *TypeChecker, scope: *Scope, fn_decl: *ast.FunctionDecl) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, fn_decl.span);

        if (fn_decl.name) |identifier| {
            try scope.declare(
                self.arena.allocator(),
                identifier,
                try self.resolveExprType(scope, fn_decl),
                fn_decl.is_pub,
                false,
            );
        }

        const fn_scope = try scope.addChild(self.arena.allocator(), fn_decl.span);

        if (fn_decl.stdin_type) |stdin_type| {
            const resolved = try self.resolveTypeExpr(fn_scope, stdin_type);
            try fn_scope.declare(
                self.arena.allocator(),
                ast.Identifier.global(ast.FdExpr.stdin_binding_name),
                resolved,
                false,
                false,
            );
        }

        switch (fn_decl.params) {
            ._non_variadic => |params| for (params) |param| {
                const param_type = try self.resolveExprType(
                    fn_scope,
                    param,
                );
                try self.runBindingPattern(
                    fn_scope,
                    param.pattern,
                    param_type,
                    false,
                    false,
                );
            },
            ._variadic => |param| {
                const param_type = try self.resolveExprType(
                    fn_scope,
                    param,
                );
                try self.runBindingPattern(
                    fn_scope,
                    param.pattern,
                    param_type,
                    false,
                    false,
                );
            },
        }

        // Make the declared stdout type visible to every `yield` in the body
        // (including yields nested in loops/blocks/matches) via the stack. Each
        // `runYield` validates against the top entry in the scope it runs in.
        try self.stdout_type_stack.append(
            self.arena.allocator(),
            if (fn_decl.return_type) |return_type|
                try self.resolveTypeExpr(fn_scope, return_type)
            else
                null,
        );
        defer _ = self.stdout_type_stack.pop();

        // Run the body in a scope we keep a handle to. For a block body, run its
        // statements directly in `body_scope` instead of letting `runExpression`
        // create and discard an internal child scope. That way the stdin/stdout
        // type resolution below can see bindings declared in the body (e.g.
        // `const n = &0` referenced from a later `yield n * 2`).
        const body_scope = if (fn_decl.body.* == .block) blk: {
            const bs = try fn_scope.addChild(self.arena.allocator(), fn_decl.body.block.span);
            try self.runBlock(bs, &fn_decl.body.block);
            break :blk bs;
        } else blk: {
            try self.runExpression(fn_scope, fn_decl.body);
            break :blk fn_scope;
        };

        if (fn_decl.stdin_type) |stdin_type| {
            try self.validateFunctionBodyStdin(
                body_scope,
                fn_decl.body,
                try self.resolveTypeExpr(fn_scope, stdin_type),
            );
        }

        // Output to stdout is now explicit via `yield`; each `yield &1` was
        // already validated against the declared stdout type in `runYield`
        // (using the `stdout_type_stack` entry pushed above), so the body value
        // / `return` value is left unchecked here.
    }

    fn validateFunctionBodyStdin(
        self: *TypeChecker,
        scope: *Scope,
        expr: *ast.Expression,
        enclosing_stdin: *const ast.TypeExpr,
    ) Error!void {
        switch (expr.*) {
            .call => |call| try self.validateCallStdin(scope, call, enclosing_stdin),
            .pipeline => |pipeline| {
                if (pipeline.stages.len > 0) {
                    try self.validateFunctionBodyStdin(scope, pipeline.stages[0], enclosing_stdin);
                }
            },
            .block => |block| {
                for (block.statements) |statement| {
                    try self.validateStatementStdin(scope, statement, enclosing_stdin);
                }
            },
            .if_expr => |if_expr| {
                try self.validateFunctionBodyStdin(scope, if_expr.then_expr, enclosing_stdin);
                if (if_expr.else_branch) |else_branch| switch (else_branch) {
                    .expr => |else_expr| try self.validateFunctionBodyStdin(scope, else_expr, enclosing_stdin),
                    .if_expr => |else_if| try self.validateIfExprStdin(scope, else_if, enclosing_stdin),
                    .condition => {},
                };
            },
            .for_expr => |for_expr| try self.validateFunctionBodyStdin(scope, for_expr.body, enclosing_stdin),
            .match_expr => |match_expr| for (match_expr.cases) |case| {
                try self.validateBlockStdin(scope, case.body, enclosing_stdin);
            },
            .try_expr => |try_expr| try self.validateFunctionBodyStdin(scope, try_expr.subject, enclosing_stdin),
            .catch_expr => |catch_expr| {
                try self.validateFunctionBodyStdin(scope, catch_expr.subject, enclosing_stdin);
                try self.validateBlockStdin(scope, catch_expr.handler.body, enclosing_stdin);
            },
            .unary => |unary| try self.validateFunctionBodyStdin(scope, unary.operand, enclosing_stdin),
            .binary => |binary| {
                try self.validateFunctionBodyStdin(scope, binary.left, enclosing_stdin);
                try self.validateFunctionBodyStdin(scope, binary.right, enclosing_stdin);
            },
            .member => |member| try self.validateFunctionBodyStdin(scope, member.object, enclosing_stdin),
            .index => |index| {
                try self.validateFunctionBodyStdin(scope, index.target, enclosing_stdin);
                try self.validateFunctionBodyStdin(scope, index.index, enclosing_stdin);
            },
            .assignment => |assignment| try self.validateFunctionBodyStdin(scope, assignment.expr, enclosing_stdin),
            .subshell => |subshell| try self.validateFunctionBodyStdin(scope, subshell.child, enclosing_stdin),
            .array => |array| for (array.elements) |element| {
                try self.validateFunctionBodyStdin(scope, element, enclosing_stdin);
            },
            .map => |map| for (map.entries) |entry| {
                try self.validateFunctionBodyStdin(scope, entry.key, enclosing_stdin);
                try self.validateFunctionBodyStdin(scope, entry.value, enclosing_stdin);
            },
            .range => |range| {
                try self.validateFunctionBodyStdin(scope, range.start, enclosing_stdin);
                if (range.end) |end| try self.validateFunctionBodyStdin(scope, end, enclosing_stdin);
            },
            .fn_decl, .identifier, .env_var, .path, .literal, .pipeline_deprecated, .import_expr, .executable, .builtin, .fd => {},
        }
    }

    fn validateIfExprStdin(
        self: *TypeChecker,
        scope: *Scope,
        if_expr: *ast.IfExpr,
        enclosing_stdin: *const ast.TypeExpr,
    ) Error!void {
        try self.validateFunctionBodyStdin(scope, if_expr.then_expr, enclosing_stdin);
        if (if_expr.else_branch) |else_branch| switch (else_branch) {
            .expr => |else_expr| try self.validateFunctionBodyStdin(scope, else_expr, enclosing_stdin),
            .if_expr => |else_if| try self.validateIfExprStdin(scope, else_if, enclosing_stdin),
            .condition => {},
        };
    }

    fn validateBlockStdin(
        self: *TypeChecker,
        scope: *Scope,
        block: ast.Block,
        enclosing_stdin: *const ast.TypeExpr,
    ) Error!void {
        for (block.statements) |statement| {
            try self.validateStatementStdin(scope, statement, enclosing_stdin);
        }
    }

    fn validateStatementStdin(
        self: *TypeChecker,
        scope: *Scope,
        statement: *ast.Statement,
        enclosing_stdin: *const ast.TypeExpr,
    ) Error!void {
        switch (statement.*) {
            .expression => |expr_stmt| try self.validateFunctionBodyStdin(scope, expr_stmt.expression, enclosing_stdin),
            .binding_decl => |binding_decl| try self.validateFunctionBodyStdin(scope, binding_decl.initializer, enclosing_stdin),
            .return_stmt => |return_stmt| if (return_stmt.value) |value| try self.validateFunctionBodyStdin(scope, value, enclosing_stdin),
            .exit_stmt => |exit_stmt| if (exit_stmt.value) |value| try self.validateFunctionBodyStdin(scope, value, enclosing_stdin),
            .yield_stmt => |yield_stmt| try self.validateFunctionBodyStdin(scope, yield_stmt.value, enclosing_stdin),
            .while_stmt => |while_stmt| try self.validateBlockStdin(scope, while_stmt.body, enclosing_stdin),
            .type_binding_decl, .error_decl, .bash_block => {},
        }
    }

    fn validateCallStdin(
        self: *TypeChecker,
        scope: *Scope,
        call: ast.CallExpr,
        enclosing_stdin: *const ast.TypeExpr,
    ) Error!void {
        const callee_type = try self.resolvePipeType(scope, try self.resolveExprType(scope, call.callee)) orelse return;
        if (callee_type.* != .function) return;
        if (self.isExecutableFunctionType(callee_type.function)) return;
        const callee_stdin = try self.resolvePipeType(scope, callee_type.function.stdin_type) orelse return;
        if (self.pipeTypesEqual(enclosing_stdin, callee_stdin)) return;

        try self.reportSpanError(
            call.span,
            Error.TypeMismatch,
            .@"error",
            "function stdin type mismatch: enclosing stdin is {f}, callee stdin expects {f}",
            .{ enclosing_stdin, callee_stdin },
        );
    }

    fn isExecutableFunctionType(_: *TypeChecker, function_type: ast.TypeExpr.FunctionType) bool {
        const return_type = function_type.return_type orelse return false;
        return return_type.* == .execution;
    }

    fn runCall(self: *TypeChecker, scope: *Scope, call: *ast.CallExpr) Error!void {
        try self.runExpression(scope, call.callee);
        for (call.arguments) |arg| try self.runExpression(scope, arg);
        for (call.redirects) |*redirect| {
            switch (redirect.target) {
                .path => |p| try self.runExpression(scope, p.value),
                .fd => {},
            }
        }
    }

    fn runExpressionStatement(
        self: *TypeChecker,
        scope: *Scope,
        expr_stmt: *ast.ExpressionStmt,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, expr_stmt.span);

        try self.runExpression(scope, expr_stmt.expression);
    }

    fn runExpression(
        self: *TypeChecker,
        scope: *Scope,
        expr: *ast.Expression,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, expr.span());
        try self.log("<{s}>", .{@tagName(expr.*)});
        try self.logTypeCheckExpression(expr);

        try switch (expr.*) {
            .identifier => |*identifier| _ = try self.runIdentifier(scope, identifier),
            .env_var => {},
            .literal => |*literal| self.runLiteral(scope, literal),
            .array => |*array| self.runArray(scope, array),
            .range => |*range| self.runRange(scope, range),
            .pipeline => |*pipeline| self.runPipeline(scope, pipeline),
            .member => |*member| self.runMember(scope, member),
            .unary => |*unary| self.runUnary(scope, unary),
            .binary => |*binary| self.runBinary(scope, binary),
            .block => |*block| self.runBlockInNewScope(scope, block),
            .if_expr => |*if_expr| self.runIfExpr(scope, if_expr),
            .for_expr => |*for_expr| self.runForExpr(scope, for_expr),
            .match_expr => |*match_expr| self.runMatchExpr(scope, match_expr),
            .import_expr => |*import_expr| self.runImportExpr(scope, import_expr),
            .fn_decl => |*fn_decl| self.runFnDecl(scope, fn_decl),
            .call => |*call| self.runCall(scope, call),
            .subshell => |*subshell| self.runExpression(scope, subshell.child),
            .fd => |*fd_expr| self.runFd(scope, fd_expr),
            else => return error.UnsupportedExpression,
        };
    }

    fn runFd(self: *TypeChecker, scope: *Scope, fd_expr: *ast.FdExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, fd_expr.span);

        _ = scope;
        switch (fd_expr.fd) {
            // `&0` reads stdin. Inside a function it is typed by the declared
            // stdin (via the `&0` scope binding); as a block/expression pipeline
            // stage its type is inferred from the upstream by the compiler, so
            // it is accepted here regardless.
            0 => {},
            1, 2 => try self.reportSpanError(
                fd_expr.span,
                Error.UnsupportedExpression,
                .@"error",
                "&{d} is a write-only stream; use `yield` (or `yield &2 ...`) to write to it",
                .{fd_expr.fd},
            ),
            else => try self.reportSpanError(
                fd_expr.span,
                Error.UnsupportedExpression,
                .@"error",
                "unknown file descriptor &{d}; only &0, &1, and &2 are supported",
                .{fd_expr.fd},
            ),
        }
    }

    fn runTypeExpression(
        self: *TypeChecker,
        scope: *Scope,
        expr: *const ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, expr.span());
        try self.log("<{s}>", .{@tagName(expr.*)});
        try self.logTypeCheckTypeExpression(expr);

        try switch (expr.*) {
            .identifier => |*identifier| self.runTypeIdentifier(scope, identifier),
            .alias => |*alias| self.runTypeAlias(alias),
            .failed => {},
            .void, .integer, .float, .boolean, .byte, .null, .execution, .thread => {},
            .optional, .promise => |prefix| try self.runTypeExpression(scope, prefix.child),
            .error_union => |error_union| {
                try self.runTypeExpression(scope, error_union.err_set);
                try self.runTypeExpression(scope, error_union.payload);
            },
            .error_set, .err => {},
            .array => |*array| self.runTypeArray(scope, array),
            .struct_type, .module, .tuple, .function, .fn_ref_type => {},
        };
    }

    fn runTypeIdentifier(
        self: *TypeChecker,
        scope: *Scope,
        named_type: *const ast.TypeExpr.NamedType,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, named_type.span);

        _ = scope;

        const identifier = named_type.path.segments[0];
        _ = identifier;

        return;
    }

    fn runTypeAlias(
        self: *TypeChecker,
        alias: *const ast.TypeExpr.AliasedType,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, alias.span);
        // this has to be valid because when we create an alias we check it
    }

    fn runTypeArray(
        self: *TypeChecker,
        scope: *Scope,
        array: *const ast.TypeExpr.ArrayType,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, array.span);

        try self.runTypeExpression(scope, array.element);
    }

    pub fn runForExpr(self: *TypeChecker, scope: *Scope, for_expr: *ast.ForExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, for_expr.span);

        if (for_expr.sources.len != for_expr.capture.bindings.len) {
            return error.ForSourcesAndBindingsNeedToBeTheSameLength;
        }

        const for_scope = try scope.addChild(self.arena.allocator(), for_expr.span);

        for (for_expr.sources, for_expr.capture.bindings) |source, pattern| {
            try self.runExpression(scope, source);
            const type_expr = try self.resolveExprType(scope, source);
            try self.runBindingPattern(for_scope, pattern, type_expr, false, false);
        }

        try self.runExpression(for_scope, for_expr.body);
    }

    pub fn runMatchExpr(self: *TypeChecker, scope: *Scope, match_expr: *ast.MatchExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, match_expr.span);

        try self.runExpression(scope, match_expr.subject);

        for (match_expr.cases) |case| {
            if (case.capture != null) {
                try self.reportSpanError(
                    case.span,
                    Error.UnsupportedExpression,
                    .@"error",
                    "match captures are not yet supported",
                    .{},
                );
                continue;
            }

            try switch (case.pattern) {
                .wildcard => {},
                .literal => |*literal| self.runLiteral(scope, @constCast(literal)),
                .binding => |binding| {
                    const matcher_expr = try self.matchPatternToExpression(case.pattern);
                    const call_expr = try self.allocMatcherCallExpression(matcher_expr, match_expr.subject, case.span);
                    try self.runExpression(scope, call_expr);
                    _ = try self.resolveExprType(scope, call_expr);
                    _ = binding;
                },
                .path => {
                    const matcher_expr = try self.matchPatternToExpression(case.pattern);
                    const call_expr = try self.allocMatcherCallExpression(matcher_expr, match_expr.subject, case.span);
                    try self.runExpression(scope, call_expr);
                    _ = try self.resolveExprType(scope, call_expr);
                },
                else => {
                    try self.reportSpanError(
                        case.pattern.span(),
                        Error.UnsupportedExpression,
                        .@"error",
                        "match currently supports only literal and _ patterns",
                        .{},
                    );
                    continue;
                },
            };

            try self.runBlockInNewScope(scope, @constCast(&case.body));
        }
    }

    fn matchPatternToExpression(
        self: *TypeChecker,
        pattern: ast.MatchPattern,
    ) Error!*ast.Expression {
        return switch (pattern) {
            .binding => |binding| self.allocExpression(.{ .identifier = binding }),
            .path => |path| self.allocPathExpression(path),
            else => error.UnsupportedExpression,
        };
    }

    fn allocPathExpression(
        self: *TypeChecker,
        path: ast.Path,
    ) Error!*ast.Expression {
        var expr = try self.allocExpression(.{ .identifier = path.segments[0] });
        for (path.segments[1..]) |segment| {
            expr = try self.allocExpression(.{ .member = .{
                .object = expr,
                .member = segment,
                .span = expr.span().endAt(segment.span),
            } });
        }
        return expr;
    }

    fn allocMatcherCallExpression(
        self: *TypeChecker,
        callee: *ast.Expression,
        subject: *ast.Expression,
        span: ast.Span,
    ) Error!*ast.Expression {
        const args = try self.arena.allocator().alloc(*ast.Expression, 1);
        args[0] = subject;
        return self.allocExpression(.{ .call = .{
            .callee = callee,
            .arguments = args,
            .redirects = &.{},
            .span = span,
        } });
    }

    fn allocExpression(
        self: *TypeChecker,
        expr: ast.Expression,
    ) Error!*ast.Expression {
        const ptr = try self.arena.allocator().create(ast.Expression);
        ptr.* = expr;
        return ptr;
    }

    pub fn runIfExpr(self: *TypeChecker, scope: *Scope, if_expr: *ast.IfExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, if_expr.span);

        try self.runExpression(scope, if_expr.condition);
        const condition_type = try self.resolveExprType(scope, if_expr.condition);
        const then_scope = try scope.addChild(self.arena.allocator(), if_expr.span);
        try self.runIfCapture(then_scope, if_expr, condition_type);
        try self.runExpression(then_scope, if_expr.then_expr);
        if (if_expr.else_branch) |*else_branch| {
            try self.runElseBranch(scope, else_branch);
        }
    }

    fn runIfCapture(
        self: *TypeChecker,
        then_scope: *Scope,
        if_expr: *ast.IfExpr,
        condition_type: ?*const ast.TypeExpr,
    ) Error!void {
        const capture = if_expr.capture orelse return;

        if (capture.bindings.len != 1) {
            try self.reportSpanError(
                capture.span,
                Error.BindingPatternNotSupported,
                .@"error",
                "if capture clauses currently require exactly one binding",
                .{},
            );
            return;
        }

        const resolved_condition_type = condition_type orelse blk: {
            if (if_expr.condition.* == .identifier) {
                const identifier = if_expr.condition.identifier;
                if (then_scope.parent.?.lookup(identifier.name)) |binding| {
                    break :blk binding.type_expr;
                }
            }
            break :blk null;
        };

        const cond_type = resolved_condition_type orelse {
            try self.reportSpanError(
                if_expr.condition.span(),
                Error.TypeMismatch,
                .@"error",
                "if capture requires an optional condition",
                .{},
            );
            return;
        };

        switch (cond_type.*) {
            .optional => |optional| try self.runBindingPattern(
                then_scope,
                capture.bindings[0],
                optional.child,
                false,
                false,
            ),
            else => try self.runBindingPattern(
                then_scope,
                capture.bindings[0],
                cond_type,
                false,
                false,
            ),
        }
    }

    pub fn runElseBranch(
        self: *TypeChecker,
        scope: *Scope,
        else_branch: *ast.IfExpr.ElseBranch,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, else_branch.span());

        // TODO: handle captures

        switch (else_branch.*) {
            .if_expr => |if_expr| try self.runIfExpr(scope, if_expr),
            .expr => |expr| try self.runExpression(scope, expr),
            .condition => {},
        }
    }

    pub fn runUnary(self: *TypeChecker, scope: *Scope, unary: *ast.UnaryExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, unary.span);

        // TODO: add checking operator type compatability

        try self.runExpression(scope, unary.operand);
        _ = try self.resolveExprType(scope, unary.operand);
    }

    pub fn runBinary(self: *TypeChecker, scope: *Scope, binary: *ast.BinaryExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, binary.span);

        try self.runExpression(scope, binary.left);
        try self.runExpression(scope, binary.right);
        const maybe_left_type = try self.resolveExprType(scope, binary.left);
        const maybe_right_type = try self.resolveExprType(scope, binary.right);

        // TODO: implement all kinds of semantic checks here
        // TODO: implement type expr "locations" that can be populated. These locations should be able to be resolved through expressions. This is for populating inferred type expressions.

        const left_type = maybe_left_type orelse return;
        const right_type = maybe_right_type orelse return;

        if (binary.op == .@"orelse") {
            switch (left_type.*) {
                .optional => |optional| try self.validateTypeAssignment(
                    optional.child,
                    right_type,
                    .{ .span = right_type.span() },
                ),
                .null => {},
                else => {
                    try self.reportSpanError(
                        binary.left.span(),
                        Error.TypeMismatch,
                        .@"error",
                        "left side of orelse must be an optional or null",
                        .{},
                    );
                },
            }
            return;
        }

        if (binary.op.isAssignment()) {
            if (binary.left.* == .env_var and isEnvAssignableType(right_type)) {
                return;
            }
            try self.validateTypeAssignment(left_type, right_type, .{ .span = right_type.span() });
        }
    }

    pub fn runMember(self: *TypeChecker, scope: *Scope, member: *ast.MemberExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, member.span);

        try self.runExpression(scope, member.object);
        const object_type = try self.resolveExprType(scope, member.object) orelse {
            return error.MemberObjectTypeUndefined;
        };

        if (std.mem.eql(u8, member.member.name, "?")) {
            return switch (object_type.*) {
                .optional => {},
                else => {
                    try self.reportSpanError(
                        member.span,
                        Error.UnsupportedMemberAccess,
                        .@"error",
                        "optional unwrap requires an optional value",
                        .{},
                    );
                },
            };
        }

        if (std.mem.eql(u8, member.member.name, "wait")) {
            return switch (object_type.*) {
                .execution, .thread => {},
                .struct_type => |struct_type| if (isExecutionLikeStruct(struct_type)) {} else error.MemberNotFound,
                else => error.UnsupportedMemberAccess,
            };
        }

        switch (object_type.*) {
            .failed => {},
            .identifier => return error.UnresolvedTypeLiteral,
            .optional => return error.MemberAccessOnOptional,
            .array => |array| try self.runArrayMemberAccess(array, &member.member),
            .thread => try self.runThreadMemberAccess(&member.member),
            .struct_type => |struct_type| try self.runStructMemberAccess(struct_type, &member.member),
            .null, .promise, .error_union, .error_set, .err, .tuple, .function, .fn_ref_type, .integer, .float, .boolean, .byte, .alias, .void => return error.UnsupportedMemberAccess,
            .module => |module| try self.runModuleMemberAccess(module, &member.member),
            .execution => |execution| try self.runExecutionMemberAccess(execution, &member.member),
            // .lazy => {
            //     // TODO: Figure out what to do here, do we setup a check after the lazy type has been resolved, or do we always assume we have the types resolved here? <|:)---<
            //     return error.UnsupportedMemberAccess;
            // },
        }
    }

    pub fn runArrayMemberAccess(
        self: *TypeChecker,
        _: ast.TypeExpr.ArrayType,
        identifier: *ast.Identifier,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, identifier.span);

        if (std.mem.eql(u8, identifier.name, "len")) {
            return;
        }

        return error.MemberNotFound;
    }

    pub fn runStructMemberAccess(
        self: *TypeChecker,
        struct_type: ast.TypeExpr.StructType,
        identifier: *ast.Identifier,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, identifier.span);

        if (struct_type.memberType(identifier.name) != null) return;

        return error.MemberNotFound;
    }

    pub fn runModuleMemberAccess(
        self: *TypeChecker,
        module: ast.TypeExpr.ModuleType,
        identifier: *ast.Identifier,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, identifier.span);

        const module_scope = try self.requestModuleScope(module) orelse return;

        // Only pub declarations are accessible on a module; fall back to
        // execution-result fields (exit_code, stdout, stderr, wait) for the rest.
        if (module_scope.lookup(identifier.name)) |binding| {
            if (binding.is_pub) return;
        }

        try self.runExecutionMemberAccess(undefined, identifier);
    }

    pub fn runExecutionMemberAccess(
        self: *TypeChecker,
        _: ast.TypeExpr.PrimitiveType,
        identifier: *ast.Identifier,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, identifier.span);

        const valid_members: []const []const u8 = &.{ "exit_code", "stdout", "stderr", "wait" };

        for (valid_members) |m| if (std.mem.eql(u8, m, identifier.name)) {
            return;
        };

        return error.MemberNotFound;
    }

    pub fn runThreadMemberAccess(
        self: *TypeChecker,
        identifier: *ast.Identifier,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, identifier.span);

        if (std.mem.eql(u8, identifier.name, "wait")) return;

        return error.MemberNotFound;
    }

    fn isExecutionLikeStruct(struct_type: ast.TypeExpr.StructType) bool {
        return struct_type.memberType("stdout") != null and struct_type.memberType("stderr") != null;
    }

    fn isEnvAssignableType(type_expr: *const ast.TypeExpr) bool {
        return switch (type_expr.*) {
            .execution => true,
            .struct_type => |struct_type| isExecutionLikeStruct(struct_type),
            else => false,
        };
    }

    pub fn runPipeline(self: *TypeChecker, scope: *Scope, pipeline: *ast.Pipeline) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, pipeline.span);

        for (pipeline.stages) |stage| {
            try self.runExpression(scope, stage);
        }

        if (pipeline.stages.len < 2) return;

        var upstream_stdout = try self.resolvePipelineStageStdoutType(scope, pipeline.stages[0]);
        for (pipeline.stages[1..]) |stage| {
            const downstream_stdin = try self.resolvePipelineStageStdinType(scope, stage);
            if (upstream_stdout) |stdout_type| {
                if (downstream_stdin) |stdin_type| {
                    try self.validatePipeBoundary(stdout_type, stdin_type, stage.span());
                }
            }
            upstream_stdout = try self.resolvePipelineStageStdoutType(scope, stage);
        }
    }

    fn resolvePipelineStageStdoutType(
        self: *TypeChecker,
        scope: *Scope,
        stage: *ast.Expression,
    ) Error!?*const ast.TypeExpr {
        if (stage.* == .call) {
            const call = stage.call;
            const callee_type = try self.resolvePipeType(scope, try self.resolveExprType(scope, call.callee));
            if (callee_type) |resolved_callee_type| {
                if (resolved_callee_type.* == .function) {
                    const return_type = try self.resolvePipeType(scope, resolved_callee_type.function.return_type);
                    if (return_type) |resolved_return_type| {
                        if (resolved_return_type.* == .execution) return try self.allocStringType();
                        return resolved_return_type;
                    }
                }
            }
        }

        const stage_type = try self.resolvePipeType(scope, try self.resolveExprType(scope, stage));
        const resolved_stage_type = stage_type orelse return null;

        return switch (resolved_stage_type.*) {
            .function => |function| try self.resolvePipeType(scope, function.return_type),
            else => resolved_stage_type,
        };
    }

    fn resolvePipelineStageStdinType(
        self: *TypeChecker,
        scope: *Scope,
        stage: *ast.Expression,
    ) Error!?*const ast.TypeExpr {
        const target = switch (stage.*) {
            .call => |call| call.callee,
            else => stage,
        };

        const target_type = try self.resolvePipeType(scope, try self.resolveExprType(scope, target));
        const resolved_target_type = target_type orelse return null;

        return switch (resolved_target_type.*) {
            .function => |function| try self.resolvePipeType(scope, function.stdin_type),
            else => null,
        };
    }

    fn resolvePipeType(
        self: *TypeChecker,
        scope: *Scope,
        maybe_type: ?*const ast.TypeExpr,
    ) Error!?*const ast.TypeExpr {
        const type_expr = maybe_type orelse return null;
        return try self.resolveTypeExpr(scope, type_expr);
    }

    /// Describes how adjacent pipeline stages are connected at a boundary.
    pub const PipeBoundaryKind = enum {
        /// Both sides agree on the same non-void, non-execution type.
        /// Value can be passed directly once runtime typed transport is in place.
        exact_typed,
        /// Upstream provides T, downstream expects ?T.  The value is forwarded
        /// unchanged; the downstream treats it as the non-null case.
        coerced_optional,
        /// Upstream provides T, downstream expects E!T.  The value is forwarded
        /// unchanged; the downstream treats it as the success case.
        coerced_error_union,
        /// At least one side carries an ExecutionResult-returning function (an
        /// external executable). The current implementation always uses byte pipes
        /// for this case.
        byte_stream,
        /// One side is Void; no value is transported.
        void_boundary,
        /// Types differ and a diagnostic has been emitted.
        incompatible,
    };

    /// Classify a pipe boundary without emitting any diagnostic. Useful for the
    /// compiler to decide the execution path.
    pub fn classifyPipeBoundary(
        self: *TypeChecker,
        upstream_stdout: *const ast.TypeExpr,
        downstream_stdin: *const ast.TypeExpr,
    ) PipeBoundaryKind {
        const up = self.unaliasType(upstream_stdout);
        const down = self.unaliasType(downstream_stdin);

        if (up.* == .void or down.* == .void) return .void_boundary;

        if (up.* == .execution or down.* == .execution) return .byte_stream;

        if (self.pipeTypesEqual(upstream_stdout, downstream_stdin)) return .exact_typed;

        // T → ?T
        if (down.* == .optional) {
            const inner = self.unaliasType(down.optional.child);
            if (self.pipeTypesEqual(up, inner)) return .coerced_optional;
        }

        // T → E!T
        if (down.* == .error_union) {
            const payload = self.unaliasType(down.error_union.payload);
            if (self.pipeTypesEqual(up, payload)) return .coerced_error_union;
        }

        return .incompatible;
    }

    fn validatePipeBoundary(
        self: *TypeChecker,
        upstream_stdout: *const ast.TypeExpr,
        downstream_stdin: *const ast.TypeExpr,
        span: ast.Span,
    ) Error!void {
        // Exact match (including Void→Void) is always valid.
        if (self.pipeTypesEqual(upstream_stdout, downstream_stdin)) return;

        const up = self.unaliasType(upstream_stdout);
        const down = self.unaliasType(downstream_stdin);

        // A Void boundary that is not an exact Void→Void match is a mismatch:
        // Void→non-Void and non-Void→Void are both rejected.
        if (up.* == .void or down.* == .void) {
            try self.reportSpanError(
                span,
                Error.TypeMismatch,
                .@"error",
                "pipeline type mismatch: upstream stdout is {f}, downstream stdin expects {f}",
                .{ upstream_stdout, downstream_stdin },
            );
            return;
        }

        // Accepted coercions: T→?T and T→E!T. The value flows through unchanged;
        // the downstream treats it as the non-null / success case.
        if (down.* == .optional) {
            const inner = self.unaliasType(down.optional.child);
            if (self.pipeTypesEqual(up, inner)) return;
        }
        if (down.* == .error_union) {
            const payload = self.unaliasType(down.error_union.payload);
            if (self.pipeTypesEqual(up, payload)) return;
        }

        try self.reportSpanError(
            span,
            Error.TypeMismatch,
            .@"error",
            "pipeline type mismatch: upstream stdout is {f}, downstream stdin expects {f}",
            .{ upstream_stdout, downstream_stdin },
        );
    }

    fn pipeTypesEqual(
        self: *TypeChecker,
        left: *const ast.TypeExpr,
        right: *const ast.TypeExpr,
    ) bool {
        const resolved_left = self.unaliasType(left);
        const resolved_right = self.unaliasType(right);

        if (std.meta.activeTag(resolved_left.*) != std.meta.activeTag(resolved_right.*)) return false;

        return switch (resolved_left.*) {
            .array => |left_array| self.pipeTypesEqual(left_array.element, resolved_right.array.element),
            .optional => |left_optional| self.pipeTypesEqual(left_optional.child, resolved_right.optional.child),
            .promise => |left_promise| self.pipeTypesEqual(left_promise.child, resolved_right.promise.child),
            .error_union => |left_error_union| self.pipeTypesEqual(left_error_union.err_set, resolved_right.error_union.err_set) and
                self.pipeTypesEqual(left_error_union.payload, resolved_right.error_union.payload),
            .void, .integer, .float, .boolean, .byte, .null, .execution, .thread => true,
            else => std.meta.eql(resolved_left.*, resolved_right.*),
        };
    }

    fn unaliasType(self: *TypeChecker, type_expr: *const ast.TypeExpr) *const ast.TypeExpr {
        return switch (type_expr.*) {
            .alias => |*alias| self.unaliasType(self.resolveAliasType(alias)),
            else => type_expr,
        };
    }

    pub fn runPipelineStage(
        self: *TypeChecker,
        scope: *Scope,
        stage: *const ast.PipelineStage,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, stage.span);

        switch (stage.payload) {
            .expression => |expr| try self.runExpression(scope, expr),
            .command => |*command| try self.runCommand(scope, command),
        }
    }

    pub fn runCommand(
        self: *TypeChecker,
        scope: *Scope,
        command: *const ast.CommandInvocation,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, command.span);

        try self.runCommandPart(scope, &command.name);
        for (command.args) |*expr| try self.runCommandPart(scope, expr);
    }

    pub fn runCommandPart(
        self: *TypeChecker,
        scope: *Scope,
        part: *const ast.CommandPart,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, part.span());

        switch (part.*) {
            .expr => |expr| try self.runExpression(scope, expr),
            .word => {},
            .string => |*string_literal| try self.runStringLiteral(scope, string_literal),
        }
    }

    pub fn runRange(self: *TypeChecker, scope: *Scope, range: *ast.RangeLiteral) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, range.span);

        try self.runExpression(scope, range.start);
        if (range.end) |end| try self.runExpression(scope, end);
    }

    pub fn runArray(self: *TypeChecker, scope: *Scope, array: *ast.ArrayLiteral) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, array.span);

        for (array.elements) |expr| try self.runExpression(scope, expr);
    }

    pub fn runLiteral(self: *TypeChecker, scope: *Scope, literal: *ast.Literal) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, literal.span());

        switch (literal.*) {
            .string => |*string_literal| try self.runStringLiteral(scope, string_literal),
            else => {},
        }
    }

    pub fn runStringLiteral(
        self: *TypeChecker,
        scope: *Scope,
        string_literal: *const ast.StringLiteral,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, string_literal.span);

        for (string_literal.segments) |*segment| {
            try self.runStringLiteralSegment(scope, segment);
        }
    }

    pub fn runStringLiteralSegment(
        self: *TypeChecker,
        scope: *Scope,
        segment: *const ast.StringLiteral.Segment,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, segment.span());

        switch (segment.*) {
            .interpolation => |expr| try self.runExpression(scope, expr),
            .text => {},
        }
    }

    const RunIdentifierResult = enum { found, not_found };

    pub fn runIdentifier(
        self: *TypeChecker,
        scope: *Scope,
        identifier: *ast.Identifier,
    ) Error!RunIdentifierResult {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, identifier.span);

        if (scope.lookup(identifier.name) == null) {
            // try self.reportSpanError(
            //     identifier.span,
            //     error.IdentifierNotFound,
            //     .@"error",
            //     "identifier \"{s}\" not declared",
            //     .{identifier.name},
            // );

            return .not_found;
        }

        return .found;
    }

    pub fn runImportExpr(self: *TypeChecker, scope: *Scope, import: *ast.ImportExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, import.span);

        const raw_module_type = try import.resolveType(self.arena.allocator(), scope) orelse {
            try self.reportSpanError(
                import.span,
                Error.ModuleNotFound,
                .@"error",
                "module {s} not found",
                .{import.module_name},
            );
            return;
        };

        _ = try self.requestModuleScope(raw_module_type.module);

        // Modules must not declare parameters — use a function in a module instead.
        const module_script = try self.document_store.getAst(raw_module_type.module.path) orelse return;
        if (module_script.signature) |sig| {
            switch (sig.params) {
                ._non_variadic => |params| {
                    if (params.len > 0) {
                        try self.reportSpanError(
                            import.span,
                            Error.UnsupportedExpression,
                            .@"error",
                            "module \"{s}\" cannot be imported because it declares parameters; expose its functionality as pub functions instead",
                            .{import.module_name},
                        );
                    }
                },
                ._variadic => {
                    try self.reportSpanError(
                        import.span,
                        Error.UnsupportedExpression,
                        .@"error",
                        "module \"{s}\" cannot be imported because it declares parameters; expose its functionality as pub functions instead",
                        .{import.module_name},
                    );
                },
            }
        }
    }

    fn isUnion(comptime T: type) bool {
        return switch (@typeInfo(T)) {
            .@"union" => true,
            .pointer => |p| isUnion(p.child),
            else => false,
        };
    }

    fn isPointer(comptime T: type) bool {
        return switch (@typeInfo(T)) {
            .pointer => true,
            else => false,
        };
    }

    fn resolveExprType(
        self: *TypeChecker,
        scope: *Scope,
        expr: anytype,
    ) Error!?*const ast.TypeExpr {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        const T = @TypeOf(expr);
        const span = if (std.meta.hasMethod(T, "span")) expr.span() else expr.span;
        var alloc_writer = std.Io.Writer.Allocating.init(self.arena.allocator());
        defer alloc_writer.deinit();
        try alloc_writer.writer.writeAll(@src().fn_name ++ "\n");
        try switch (@typeInfo(T)) {
            .pointer => |p| switch (@typeInfo(p.child)) {
                .@"union" => alloc_writer.writer.print(@typeName(T) ++ ".{t}", .{expr.*}),
                else => alloc_writer.writer.writeAll(@typeName(T)),
            },
            .@"union" => alloc_writer.writer.print(@typeName(T) ++ ".{t}", .{expr}),
            else => alloc_writer.writer.writeAll(@typeName(T)),
        };
        try self.logTypeCheckTrace(alloc_writer.written(), span);

        const result = try expr.resolveType(
            self.arena.allocator(),
            scope,
        );

        if (T == *ast.ImportExpr) {
            if (result) |resolved| switch (resolved.*) {
                .module => |module| return self.buildModuleValueType(module),
                else => {},
            };
        }
        if (T == ast.ImportExpr) {
            if (result) |resolved| switch (resolved.*) {
                .module => |module| return self.buildModuleValueType(module),
                else => {},
            };
        }

        try self.logWithoutPrefix("result: {?f}\n", .{result});

        return result;
    }

    fn materializeBindingType(
        self: *TypeChecker,
        scope: *Scope,
        name: []const u8,
        maybe_type_expr: ?*const ast.TypeExpr,
        span: ast.Span,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});
        try self.logWithoutPrefix("identifier: {s}\n", .{name});
        try self.logWithoutPrefix("assignment type = ", .{});

        const type_expr = maybe_type_expr orelse {
            try self.logWithoutPrefix("???\n", .{});
            return;
        };

        try self.logWithoutPrefix("{f}\n", .{type_expr});

        const binding_ref = scope.lookup(name) orelse return error.IdentifierNotFound;

        try self.logWithoutPrefix("binding type = {?f}\n", .{binding_ref.type_expr});

        if (binding_ref.type_expr) |binding_type| {
            try self.validateTypeAssignment(
                binding_type,
                type_expr,
                .{ .span = span },
            );
        } else {
            binding_ref.type_expr = type_expr;
        }
    }

    pub const ValidateTypeAssignmentOptions = struct {
        span: ast.Span,
        binding_alias: ?*const ast.TypeExpr.AliasedType = null,
        assignment_alias: ?*const ast.TypeExpr.AliasedType = null,
    };

    fn validateTypeAssignment(
        self: *TypeChecker,
        binding_type: *const ast.TypeExpr,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        try switch (binding_type.*) {
            .failed => {},
            .thread => unreachable,
            .void => |*void_| self.validateTypeAssignmentVoid(
                void_,
                assignment_type,
                options,
            ),
            .identifier => return error.UnresolvedTypeLiteral,
            .alias => |*alias| self.validateTypeAssignmentAlias(
                alias,
                assignment_type,
                options,
            ),
            .optional => |optional| self.validateTypeAssignmentOptional(
                optional,
                assignment_type,
                options,
            ),
            .promise => |promise| self.validateTypeAssignmentPromise(
                promise,
                assignment_type,
                options,
            ),
            .error_union => |error_union| self.validateTypeAssignmentErrorUnion(
                error_union,
                assignment_type,
                options,
            ),
            .error_set => |error_set| self.validateTypeAssignmentErrorSet(
                error_set,
                assignment_type,
                options,
            ),
            .err => |err| self.validateTypeAssignmentErrorType(
                err,
                assignment_type,
                options,
            ),
            .array => |array| self.validateTypeAssignmentArray(
                array,
                assignment_type,
                options,
            ),
            .struct_type => |struct_type| self.validateTypeAssignmentStruct(
                struct_type,
                assignment_type,
                options,
            ),
            .module => |module| self.validateTypeAssignmentModule(
                module,
                assignment_type,
                options,
            ),
            .tuple => |tuple| self.validateTypeAssignmentTuple(
                tuple,
                assignment_type,
                options,
            ),
            .function => |function| self.validateTypeAssignmentFunction(
                function,
                assignment_type,
                options,
            ),
            .integer => |*integer| self.validateTypeAssignmentInteger(
                integer,
                assignment_type,
                options,
            ),
            .float => |*float| self.validateTypeAssignmentFloat(
                float,
                assignment_type,
                options,
            ),
            .boolean => |*boolean| self.validateTypeAssignmentBoolean(
                boolean,
                assignment_type,
                options,
            ),
            .null => |*null_| self.validateTypeAssignmentNull(
                null_,
                assignment_type,
                options,
            ),
            .byte => |*byte| self.validateTypeAssignmentByte(
                byte,
                assignment_type,
                options,
            ),
            .execution => |*execution| self.validateTypeAssignmentExecution(
                execution,
                assignment_type,
                options,
            ),
            .fn_ref_type => {},
            // .lazy => |lazy| self.validateTypeAssignmentLazy(
            //     lazy,
            //     assignment_type,
            // ),
        };
    }

    pub fn validateTypeAssignmentVoid(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.PrimitiveType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        if (assignment_type.* == .void) return;

        try self.reportAssignmentError(
            @as(*const ast.TypeExpr, @fieldParentPtr("void", assignee)),
            assignment_type,
            options,
        );
    }

    pub fn validateTypeAssignmentAlias(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.AliasedType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        var options_ = options;
        if (options_.binding_alias == null) options_.binding_alias = assignee;

        const actual_type = self.resolveAliasType(assignee);
        try self.validateTypeAssignment(actual_type, assignment_type, options_);
    }

    pub fn resolveAliasType(
        _: *TypeChecker,
        alias_type: *const ast.TypeExpr.AliasedType,
    ) *const ast.TypeExpr {
        return alias_type.type_expr;
        // const scope = self.getScopeFromLoc(alias_type.span.start);
        // return scope.lookup(alias_type.name);
    }

    pub fn getScopeFromLoc(
        self: *TypeChecker,
        location: token.Location,
    ) ?*Scope {
        const module = self.modules.get(location.file) orelse return null;
        // TODO: implement finding the correct scope

        return getScopeFromLocAux(location, module) orelse module;
    }

    fn getScopeFromLocAux(location: token.Location, scope: *Scope) ?*Scope {
        if (scope.span.containsLoc(location)) {
            for (scope.children.items) |child| {
                if (getScopeFromLocAux(location, child)) |found| return found;
            }

            return scope;
        } else return null;
    }

    pub fn validateTypeAssignmentOptional(
        self: *TypeChecker,
        assignee: ast.TypeExpr.PrefixType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .optional => |optional| try self.validateTypeAssignment(
                assignee.child,
                optional.child,
                options,
            ),
            .null => return,
            else => try self.validateTypeAssignment(assignee.child, assignment_type, options),
        }
    }

    pub fn validateTypeAssignmentNull(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.PrimitiveType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        if (assignment_type.* == .null) return;

        try self.reportAssignmentError(
            @as(*const ast.TypeExpr, @fieldParentPtr("null", assignee)),
            assignment_type,
            options,
        );
    }

    pub fn validateTypeAssignmentPromise(
        self: *TypeChecker,
        assignee: ast.TypeExpr.PrefixType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .promise => |promise| try self.validateTypeAssignment(
                assignee.child,
                promise.child,
                options,
            ),
            else => try self.validateTypeAssignment(assignee.child, assignment_type, options),
        }
    }

    pub fn validateTypeAssignmentErrorUnion(
        self: *TypeChecker,
        assignee: ast.TypeExpr.ErrorUnion,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .error_union => |error_union| {
                try self.validateTypeAssignmentErrorSet(
                    assignee.err_set.error_set,
                    error_union.err_set,
                    options,
                );
                try self.validateTypeAssignment(
                    assignee.payload,
                    error_union.payload,
                    options,
                );
            },
            .err => try self.validateErrorInSet(assignee.err_set.error_set, assignment_type, options),
            else => try self.validateTypeAssignment(assignee.payload, assignment_type, options),
        }
    }

    pub fn validateTypeAssignmentErrorSet(
        self: *TypeChecker,
        error_set: ast.TypeExpr.ErrorSet,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        for (assignment_type.error_set.error_types) |error_set_type| {
            try self.validateErrorInSet(error_set, error_set_type, options);
        }
    }

    pub fn validateErrorInSet(
        self: *TypeChecker,
        error_set: ast.TypeExpr.ErrorSet,
        err: *const ast.TypeExpr,
        _: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, err.span());

        for (error_set.error_types) |error_set_type| {
            if (error_set_type == err) return;
        }

        try self.reportSpanError(
            err.span(),
            Error.ErrorNotInErrorSet,
            .@"error",
            "error not in expected error set",
            .{},
        );
    }

    pub fn validateTypeAssignmentErrorType(
        self: *TypeChecker,
        error_type: ast.TypeExpr.ErrorType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        // TODO: see if we need to have a resolve type on assignment_type here
        if (error_type.error_payload == assignment_type) {
            return;
        }

        try self.reportAssignmentError(
            error_type,
            assignment_type,
            options,
        );
    }

    pub fn validateTypeAssignmentArray(
        self: *TypeChecker,
        assignee: ast.TypeExpr.ArrayType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .array => |array| {
                try self.validateTypeAssignment(assignee.element, array.element, options);
            },
            else => try self.reportAssignmentError(
                assignee,
                assignment_type,
                options,
            ),
        }
    }

    pub fn validateTypeAssignmentStruct(
        self: *TypeChecker,
        _: ast.TypeExpr.StructType,
        assignment_type: *const ast.TypeExpr,
        _: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        // TODO: implement
    }

    pub fn validateTypeAssignmentModule(
        self: *TypeChecker,
        assignee: ast.TypeExpr.ModuleType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .module => |module| if (std.mem.eql(u8, assignee.path, module.path)) return,
            else => {},
        }

        try self.reportAssignmentError(
            assignee,
            assignment_type,
            options,
        );
    }

    pub fn validateTypeAssignmentTuple(
        self: *TypeChecker,
        _: ast.TypeExpr.TupleType,
        assignment_type: *const ast.TypeExpr,
        _: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());
    }

    pub fn validateTypeAssignmentFunction(
        self: *TypeChecker,
        _: ast.TypeExpr.FunctionType,
        assignment_type: *const ast.TypeExpr,
        _: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        // TODO: should this even be implement or should we have dynamic function pointers? (no?)
    }

    pub fn validateTypeAssignmentInteger(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.PrimitiveType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .integer => {},
            else => try self.reportAssignmentError(
                @as(*const ast.TypeExpr, @fieldParentPtr("integer", assignee)),
                assignment_type,
                options,
            ),
        }
    }

    pub fn validateTypeAssignmentFloat(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.PrimitiveType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .integer, .float => {},
            else => try self.reportAssignmentError(
                @as(*const ast.TypeExpr, @fieldParentPtr("float", assignee)),
                assignment_type,
                options,
            ),
        }
    }

    pub fn validateTypeAssignmentBoolean(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.PrimitiveType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .boolean => {},
            else => try self.reportAssignmentError(
                @as(*const ast.TypeExpr, @fieldParentPtr("boolean", assignee)),
                assignment_type,
                options,
            ),
        }
    }

    pub fn validateTypeAssignmentByte(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.PrimitiveType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .byte => {},
            else => try self.reportAssignmentError(
                @as(*const ast.TypeExpr, @fieldParentPtr("byte", assignee)),
                assignment_type,
                options,
            ),
        }
    }

    pub fn validateTypeAssignmentExecution(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.PrimitiveType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .execution => {},
            else => try self.reportAssignmentError(
                @as(*const ast.TypeExpr, @fieldParentPtr("execution", assignee)),
                assignment_type,
                options,
            ),
        }
    }

    pub fn validateTypeAssignmentLazy(
        self: *TypeChecker,
        _: ast.TypeExpr.LazyType,
        assignment_type: *const ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());
    }

    fn compileResult(self: *TypeChecker) Error!Result {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});

        if (self.diagnostics.items.len > 0) {
            return .{ .err = .{ ._diagnostics = self.diagnostics.items } };
        }

        return .success;
    }
};

const Definition = struct {
    identifier: ast.Identifier,
    type_expr: *const ast.TypeExpr,

    pub fn init(comptime name: []const u8, comptime type_expr: ast.TypeExpr) @This() {
        return .{
            .identifier = .{ .name = name, .span = .global },
            .type_expr = &type_expr,
        };
    }
};

const GlobalTypes = struct {
    pub const Void = ast.TypeExpr{ .void = .{ .span = .global } };
    pub const Int = ast.TypeExpr{ .integer = .{ .span = .global } };
    pub const Float = ast.TypeExpr{ .float = .{ .span = .global } };
    pub const Boole = ast.TypeExpr{ .boolean = .{ .span = .global } };
    pub const Byte = ast.TypeExpr{ .byte = .{ .span = .global } };
    pub fn Array(comptime element: ast.TypeExpr) ast.TypeExpr {
        return .{ .array = .{ .element = &element, .span = .global } };
    }
};

/// Stable storage for the `parseInt` builtin's function type:
/// `fn String parseInt() Int`.
const parse_int_byte_type = ast.TypeExpr{ .byte = .{ .span = .global } };
const parse_int_string_type = ast.TypeExpr{ .array = .{ .element = &parse_int_byte_type, .span = .global } };
const parse_int_return_type = ast.TypeExpr{ .integer = .{ .span = .global } };
const parse_int_fn_type = ast.TypeExpr{ .function = .{
    .params = .nonVariadic(&.{}),
    .stdin_type = &parse_int_string_type,
    .return_type = &parse_int_return_type,
    .span = .global,
} };

const global_scope_definitions = [_]Definition{
    .init("Void", GlobalTypes.Void),
    .init("Int", GlobalTypes.Int),
    .init("Float", GlobalTypes.Float),
    .init("Bool", GlobalTypes.Boole),
    .init("Boole", GlobalTypes.Boole),
    .init("Boolean", GlobalTypes.Boole),
    .init("Byte", GlobalTypes.Byte),
    .init("String", GlobalTypes.Array(GlobalTypes.Byte)),
};

/// Builtin value bindings (not types) available in every module's global scope.
const global_value_definitions = [_]Definition{
    .{ .identifier = .{ .name = "parseInt", .span = .global }, .type_expr = &parse_int_fn_type },
};

fn addGlobalScope(allocator: std.mem.Allocator, scope: *Scope) !*Scope {
    const global_scope = try scope.addChild(allocator, scope.span);

    for (global_scope_definitions) |definition| {
        try global_scope.declare(allocator, definition.identifier, definition.type_expr, true, false);
    }

    for (global_value_definitions) |definition| {
        try global_scope.declare(allocator, definition.identifier, definition.type_expr, true, false);
    }

    return global_scope;
}
