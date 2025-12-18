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
    ) TypeChecker {
        const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_" ++ logging_name) catch "";
        defer allocator.free(logging_enabled_s);
        const logging_enabled = std.mem.eql(u8, logging_enabled_s, "1");

        return .{
            .arena = .init(allocator),
            .logging_enabled = logging_enabled,
            .document_store = document_store,
            .modules = .empty,
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
        const duped_path = try self.arena.child_allocator.dupe(u8, path);

        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name ++ ": {s}", .{duped_path});

        _ = try self.scopesFromAst(duped_path);

        return self.compileResult();
    }

    fn scopesFromAst(self: *TypeChecker, path: []const u8) Error!*Scope {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name ++ ": {s}", .{path});

        if (self.modules.contains(path)) return self.modules.get(path).?;

        var script = try self.document_store.getAst(path) orelse return error.DocumentNotParsed;

        const scope = try self.arena.allocator().create(Scope);
        scope.* = .init(script.span);

        const global_scope = try addGlobalScope(self.arena.allocator(), scope);

        try self.modules.put(
            self.arena.allocator(),
            path,
            global_scope,
        );

        try self.runBlock(global_scope, &script);

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
            .expression => |*expr_stmt| self.runExpressionStatement(scope, expr_stmt),
            else => error.UnsupportedStatement,
        };
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

        const type_expr = binding_annotation_type_expr orelse try self.resolveExprType(
            scope,
            binding_decl.initializer,
        );

        if (type_expr) |t| try self.runTypeExpression(scope, t);

        try self.runBindingPattern(
            scope,
            binding_decl.pattern,
            type_expr,
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
            else => return error.UnsupportedTypeResolve,
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
                false,
            );
        }

        const fn_scope = try scope.addChild(self.arena.allocator(), fn_decl.span);

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
                );
            },
        }

        try self.runExpression(fn_scope, fn_decl.body);
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
            .literal => |*literal| self.runLiteral(scope, literal),
            .array => |*array| self.runArray(scope, array),
            .range => |*range| self.runRange(scope, range),
            .pipeline => |*pipeline| self.runPipeline(scope, pipeline),
            .member => |*member| self.runMember(scope, member),
            .binary => |*binary| self.runBinary(scope, binary),
            .block => |*block| self.runBlockInNewScope(scope, block),
            .if_expr => |*if_expr| self.runIfExpr(scope, if_expr),
            .for_expr => |*for_expr| self.runForExpr(scope, for_expr),
            .import_expr => |*import_expr| self.runImportExpr(scope, import_expr),
            .assignment => |*assignment| self.runAssignment(scope, assignment),
            .fn_decl => |*fn_decl| self.runFnDecl(scope, fn_decl),
            else => return error.UnsupportedExpression,
        };
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
            .integer, .float, .boolean, .byte => {},
            .array => |*array| self.runTypeArray(scope, array),
            else => return error.UnsupportedTypeExpression,
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
            try self.runBindingPattern(for_scope, pattern, type_expr, false);
        }

        try self.runBlock(for_scope, &for_expr.body);
    }

    pub fn runIfExpr(self: *TypeChecker, scope: *Scope, if_expr: *ast.IfExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, if_expr.span);

        try self.runExpression(scope, if_expr.condition);
        const then_scope = try scope.addChild(self.arena.allocator(), if_expr.span);
        try self.runExpression(then_scope, if_expr.then_expr);
        if (if_expr.else_branch) |*else_branch| {
            try self.runElseBranch(then_scope, else_branch);
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
        }
    }

    pub fn runBinary(self: *TypeChecker, scope: *Scope, binary: *ast.BinaryExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, binary.span);

        try self.runExpression(scope, binary.left);
        try self.runExpression(scope, binary.right);
        const left_type = try self.resolveExprType(scope, binary.left);
        const right_type = try self.resolveExprType(scope, binary.right);
        _ = left_type;
        _ = right_type;

        // TODO: implement all kinds of semantic checks here
    }

    pub fn runMember(self: *TypeChecker, scope: *Scope, member: *ast.MemberExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, member.span);

        try self.runExpression(scope, member.object);
        const object_type = try self.resolveExprType(scope, member.object) orelse {
            return error.MemberObjectTypeUndefined;
        };

        switch (object_type.*) {
            .failed => {},
            .identifier => return error.UnresolvedTypeLiteral,
            .optional => return error.MemberAccessOnOptional,
            .promise, .error_union, .error_set, .err, .array, .struct_type, .tuple, .function, .integer, .float, .boolean, .byte, .alias, .void => return error.UnsupportedMemberAccess,
            .module => |module| try self.runModuleMemberAccess(module, &member.member),
            // .lazy => {
            //     // TODO: Figure out what to do here, do we setup a check after the lazy type has been resolved, or do we always assume we have the types resolved here? <|:)---<
            //     return error.UnsupportedMemberAccess;
            // },
        }
    }

    pub fn runModuleMemberAccess(
        self: *TypeChecker,
        module: ast.TypeExpr.ModuleType,
        identifier: *ast.Identifier,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, identifier.span);

        const module_scope = try self.requestModuleScope(module) orelse return;
        _ = try self.runIdentifier(module_scope, identifier);
    }

    pub fn runPipeline(self: *TypeChecker, scope: *Scope, pipeline: *ast.Pipeline) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, pipeline.span);

        for (pipeline.stages) |stage| {
            try self.runExpression(scope, stage);
        }
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
            try self.reportSpanError(
                identifier.span,
                error.IdentifierNotFound,
                .@"error",
                "identifier \"{s}\" not declared",
                .{identifier.name},
            );

            return .not_found;
        }

        return .found;
    }

    pub fn runImportExpr(self: *TypeChecker, scope: *Scope, import: *ast.ImportExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, import.span);

        const module_type = try self.resolveExprType(scope, import) orelse {
            try self.reportSpanError(
                import.span,
                Error.ModuleNotFound,
                .@"error",
                "module {s} not found",
                .{import.module_name},
            );
            return;
        };

        _ = try self.requestModuleScope(module_type.module);
    }

    pub fn runAssignment(self: *TypeChecker, scope: *Scope, assignment: *ast.Assignment) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment.span);

        try self.runExpression(scope, assignment.expr);

        const found_identifier = try self.runIdentifier(scope, &assignment.identifier);
        if (found_identifier == .not_found) return;

        try self.materializeBindingType(
            scope,
            assignment.identifier.name,
            try self.resolveExprType(scope, assignment.expr),
            assignment.expr.span(),
        );
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
            .byte => |*byte| self.validateTypeAssignmentByte(
                byte,
                assignment_type,
                options,
            ),
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
            else => try self.validateTypeAssignment(assignee.child, assignment_type, options),
        }
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

fn addGlobalScope(allocator: std.mem.Allocator, scope: *Scope) !*Scope {
    const global_scope = try scope.addChild(allocator, scope.span);

    for (global_scope_definitions) |definition| {
        try global_scope.declare(allocator, definition.identifier, definition.type_expr, false);
    }

    return global_scope;
}
