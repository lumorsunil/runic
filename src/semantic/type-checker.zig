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
    io: std.Io,
    arena: std.heap.ArenaAllocator,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    logging_enabled: bool,
    document_store: *DocumentStore,
    modules: std.StringArrayHashMapUnmanaged(*Scope),
    env: ?*std.process.Environ.Map = null,
    /// Stack of the enclosing functions' declared stdout types. Pushed when a
    /// function body is type-checked and consulted by `runYield` so that every
    /// `yield &1` is validated in the scope where it actually appears (e.g.
    /// inside a `for (&0) |v| { yield v }` loop, where `v` is only bound in the
    /// loop's child scope). A null entry means the enclosing function declared
    /// no stdout type, so its yields are unconstrained.
    stdout_type_stack: std.ArrayListUnmanaged(?*const ast.TypeExpr) = .empty,

    /// Concrete variants inferred for each leading-`!T` (inferred) error set,
    /// keyed by the placeholder `error_set` node (shared by pointer between the
    /// AST and every resolved view of it). Populated while a function body is
    /// walked; read by `matchErrorSet` so exhaustiveness and callers see the
    /// real set. Lives in the type-checker arena and is rebuilt each pass, so
    /// no AST mutation occurs (keeps the cached LSP AST safe across re-checks).
    inferred_error_sets: std.AutoHashMapUnmanaged(*const ast.TypeExpr, []const ast.TypeExpr.ErrorSet.Variant) = .empty,
    /// Stack of in-progress inferred-error collectors, one per enclosing
    /// function. A null entry means the enclosing function's return is not an
    /// inferred error union, so there is nothing to collect.
    inferred_collector_stack: std.ArrayListUnmanaged(?*InferredErrorCollector) = .empty,

    /// Accumulates the error variants a single inferred-error function body can
    /// produce (yielded error values + `try` propagation), deduped by name.
    pub const InferredErrorCollector = struct {
        /// The inferred `error_set` placeholder node this collector populates.
        key: *const ast.TypeExpr,
        variants: std.ArrayListUnmanaged(ast.TypeExpr.ErrorSet.Variant) = .empty,
    };

    pub const Error = Scope.Error ||
        std.Io.File.OpenError ||
        std.Io.File.Reader.Error ||
        std.Io.File.StatError ||
        std.Io.Writer.Error ||
        DocumentStore.Error ||
        error{
            BindingPatternNotSupported,
            DocumentNotParsed,
            DuplicateErrorVariant,
            ErrorNotInErrorSet,
            NonExhaustiveMatch,
            FileTooBig,
            ForSourcesAndBindingsNeedToBeTheSameLength,
            IdentifierNotFound,
            MemberAccessOnOptional,
            MemberObjectTypeUndefined,
            MemberNotFound,
            ModuleNotFound,
            TypeMismatch,
            UnhandledError,
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
        io: std.Io,
        allocator: std.mem.Allocator,
        document_store: *DocumentStore,
        env: *std.process.Environ.Map,
    ) TypeChecker {
        const logging_enabled_s = env.get("RUNIC_LOG_" ++ logging_name) orelse "";
        const logging_enabled = std.mem.eql(u8, logging_enabled_s, "1");

        return .{
            .io = io,
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

        var stderr = std.Io.File.stderr().writer(self.io, &.{});
        const writer = &stderr.interface;

        try writer.print("[{s}{*}{s}]\n", .{ prefix_color, self, end_color });
        try writer.print(fmt ++ "\n", args);
        try writer.flush();
    }

    pub fn logWithoutPrefix(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        if (!self.logging_enabled) return;

        var stderr = std.Io.File.stderr().writer(self.io, &.{});
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
            .exit_stmt => |*exit_stmt| self.runExit(scope, exit_stmt),
            .yield_stmt => |*yield_stmt| self.runYield(scope, yield_stmt),
            .expression => |*expr_stmt| self.runExpressionStatement(scope, expr_stmt),
            else => error.UnsupportedStatement,
        };
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

        // Coerce into an error-union stdout type: a bare ok payload value (`T`)
        // or an error value both satisfy `E!T`.
        const declared_unaliased = self.unaliasType(declared_stdout);
        if (declared_unaliased.* == .error_union and
            self.yieldCoercesToErrorUnion(resolved, declared_unaliased.error_union))
        {
            // For an inferred set, record any error variants this yield produces.
            try self.collectInferredFromType(resolved);
            return;
        }

        // Coerce into an optional stdout type: a bare `T` value or `null`
        // both satisfy `?T`.
        if (declared_unaliased.* == .optional and
            self.yieldCoercesToOptional(resolved, declared_unaliased.optional)) return;

        // Coerce into a sum stdout type: a bare member value (or a sub-sum)
        // satisfies `A || B`.
        if (declared_unaliased.* == .sum and
            self.yieldCoercesToSum(resolved, declared_unaliased.sum)) return;

        try self.reportSpanError(
            yield_stmt.span,
            Error.TypeMismatch,
            .@"error",
            "yield type mismatch: function yields {f}, but declared stdout type is {f}",
            .{ resolved, declared_stdout },
        );
    }

    /// True if `yielded` (a resolved type) satisfies an `E!T` stdout type either
    /// as a bare ok payload value (`T`) or as an error value (an error set whose
    /// variants are all members of `E`).
    fn yieldCoercesToErrorUnion(
        self: *TypeChecker,
        yielded: *const ast.TypeExpr,
        error_union: ast.TypeExpr.ErrorUnion,
    ) bool {
        // Ok value: matches the payload type.
        if (self.pipeTypesEqual(yielded, error_union.payload)) return true;

        // Error value: an error set whose variants are all in the union's set.
        const yielded_unaliased = self.unaliasType(yielded);
        if (yielded_unaliased.* != .error_set) return false;
        const union_set = self.unaliasType(error_union.err_set);
        if (union_set.* != .error_set) return false;
        // An inferred error set (leading `!T`, empty placeholder) accepts any
        // error — its concrete members are inferred from what the body produces.
        if (isInferredErrorSet(union_set.error_set)) return true;
        for (yielded_unaliased.error_set.variants) |variant| {
            if (union_set.error_set.variant(variant.name.name) == null) return false;
        }
        return true;
    }

    /// True if `yielded` satisfies a sum stdout type: its type is one of the
    /// sum's members, or it is a sub-sum whose members are all present.
    fn yieldCoercesToSum(
        self: *TypeChecker,
        yielded: *const ast.TypeExpr,
        sum: ast.TypeExpr.SumType,
    ) bool {
        const y = self.unaliasType(yielded);
        if (y.* == .sum) {
            for (y.sum.members) |m| {
                if (!self.sumHasMember(sum, m)) return false;
            }
            return true;
        }
        return self.sumHasMember(sum, y);
    }

    /// True if `yielded` satisfies a `?T` stdout type: a bare `T` value or `null`.
    fn yieldCoercesToOptional(
        self: *TypeChecker,
        yielded: *const ast.TypeExpr,
        optional: ast.TypeExpr.PrefixType,
    ) bool {
        if (self.unaliasType(yielded).* == .null) return true;
        return self.pipeTypesEqual(yielded, optional.child);
    }

    /// An empty error set marks an inferred set (produced by leading-`!T` return
    /// types); its members are derived from the function body rather than written.
    fn isInferredErrorSet(error_set: ast.TypeExpr.ErrorSet) bool {
        return error_set.variants.len == 0;
    }

    /// The active inferred-error collector for the innermost enclosing function,
    /// or null when that function has no inferred error union return.
    fn activeInferredCollector(self: *TypeChecker) ?*InferredErrorCollector {
        if (self.inferred_collector_stack.items.len == 0) return null;
        return self.inferred_collector_stack.items[self.inferred_collector_stack.items.len - 1];
    }

    /// Records one error variant on the active collector, deduped by name.
    fn collectInferredVariant(
        self: *TypeChecker,
        variant: ast.TypeExpr.ErrorSet.Variant,
    ) Error!void {
        const collector = self.activeInferredCollector() orelse return;
        for (collector.variants.items) |existing| {
            if (std.mem.eql(u8, existing.name.name, variant.name.name)) return;
        }
        try collector.variants.append(self.arena.allocator(), variant);
    }

    /// Records every variant reachable from a resolved error-like type (an error
    /// set, or the error set of an error union) onto the active collector. A
    /// non-error type (e.g. an ok payload value, or a command `execution`) is a
    /// no-op, so this is safe to call on any yielded / propagated type.
    ///
    /// The error set is resolved through `resolveInferredErrorSet`, so when an
    /// inferred (`!T`) function yields or propagates a call to *another* inferred
    /// function, it inherits that function's collected variants rather than its
    /// empty placeholder — cross-function propagation. This works in source
    /// order (the callee finalizes its set before the caller's body is walked);
    /// a forward reference or mutual recursion would still see an unfinalized
    /// (empty) set, which needs a fixpoint pass and remains deferred.
    fn collectInferredFromType(self: *TypeChecker, t: *const ast.TypeExpr) Error!void {
        if (self.activeInferredCollector() == null) return;
        const unaliased = self.unaliasType(t);
        const set_node: *const ast.TypeExpr = switch (unaliased.*) {
            .error_set => unaliased,
            .error_union => |error_union| error_union.err_set,
            else => return,
        };
        const resolved_set = self.resolveInferredErrorSet(set_node) orelse return;
        for (resolved_set.variants) |variant| {
            try self.collectInferredVariant(variant);
        }
    }

    /// Resolves an error-set node to its concrete variants, substituting the
    /// inferred set collected from a function body when the node is an inferred
    /// (`!T`) placeholder. Returns null when the node is not an error set.
    fn resolveInferredErrorSet(self: *TypeChecker, set_node: *const ast.TypeExpr) ?ast.TypeExpr.ErrorSet {
        const set = self.unaliasType(set_node);
        if (set.* != .error_set) return null;
        if (isInferredErrorSet(set.error_set)) {
            if (self.inferred_error_sets.get(set_node)) |variants| {
                return .{ .variants = variants, .span = set.error_set.span };
            }
        }
        return set.error_set;
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

        // Resolve the initializer's type so a raw type (e.g. a function call's
        // return with unresolved member identifiers) compares against the
        // annotation.
        const initializer_type = if (try self.resolveExprType(scope, binding_decl.initializer)) |raw|
            try self.resolveTypeExpr(scope, raw)
        else
            null;

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
            .type_merge => |merge| try self.resolveTypeMerge(scope, merge),
            // Resolve each member so a sum that arrives with raw member
            // identifiers (e.g. a function call's return type) compares correctly.
            .sum => |sum| blk: {
                const members = try self.arena.allocator().alloc(*const ast.TypeExpr, sum.members.len);
                for (sum.members, members) |src, *dst| dst.* = try self.resolveTypeExpr(scope, src);
                break :blk try self.allocTypeExpression(.{ .sum = .{ .members = members, .span = sum.span } });
            },
            else => type_expr,
        };
    }

    /// Resolves a type-level `A || B` merge. When both operands are error sets
    /// the result is a single merged `error_set` (backlog #18 — see
    /// `mergeErrorSets`). Otherwise it is a structural `sum` type whose members
    /// are normalized: nested sums are flattened and duplicates removed (see
    /// `future/sum-types-plan.md`). Nested merges (`A || B || C`) compose via
    /// recursion.
    fn resolveTypeMerge(
        self: *TypeChecker,
        scope: *Scope,
        merge: ast.TypeExpr.TypeMerge,
    ) Error!*const ast.TypeExpr {
        const lhs = self.unaliasType(try self.resolveTypeExpr(scope, merge.lhs));
        const rhs = self.unaliasType(try self.resolveTypeExpr(scope, merge.rhs));

        if (lhs.* == .error_set and rhs.* == .error_set) {
            return try self.mergeErrorSets(scope, lhs.error_set, rhs.error_set, merge.span);
        }

        return try self.buildSumType(lhs, rhs, merge.span);
    }

    /// Unions two error sets by name into one `error_set` (#18). A duplicate
    /// variant name must carry a compatible payload type, else a diagnostic.
    fn mergeErrorSets(
        self: *TypeChecker,
        scope: *Scope,
        lhs: ast.TypeExpr.ErrorSet,
        rhs: ast.TypeExpr.ErrorSet,
        span: ast.Span,
    ) Error!*const ast.TypeExpr {
        var variants: std.ArrayListUnmanaged(ast.TypeExpr.ErrorSet.Variant) = .empty;
        try variants.appendSlice(self.arena.allocator(), lhs.variants);

        for (rhs.variants) |rv| {
            if (lhs.variant(rv.name.name)) |lv| {
                if (!try self.mergeVariantPayloadsCompatible(scope, lv.payload, rv.payload)) {
                    try self.reportSpanError(
                        span,
                        Error.TypeMismatch,
                        .@"error",
                        "conflicting payload for variant '{s}' in merged error set",
                        .{rv.name.name},
                    );
                }
                continue;
            }
            try variants.append(self.arena.allocator(), rv);
        }

        return try self.allocTypeExpression(.{
            .error_set = .{
                .variants = try variants.toOwnedSlice(self.arena.allocator()),
                .span = span,
            },
        });
    }

    /// Builds a normalized structural `sum` type from two (already unaliased)
    /// member types: flattens any operand that is itself a sum, and drops
    /// members structurally equal to one already present.
    fn buildSumType(
        self: *TypeChecker,
        lhs: *const ast.TypeExpr,
        rhs: *const ast.TypeExpr,
        span: ast.Span,
    ) Error!*const ast.TypeExpr {
        var members: std.ArrayListUnmanaged(*const ast.TypeExpr) = .empty;
        try self.appendSumMembers(&members, lhs);
        try self.appendSumMembers(&members, rhs);

        return try self.allocTypeExpression(.{
            .sum = .{
                .members = try members.toOwnedSlice(self.arena.allocator()),
                .span = span,
            },
        });
    }

    /// Appends `t`'s members to `members`, flattening a nested sum and skipping
    /// any member already present (structural equality via `pipeTypesEqual`).
    fn appendSumMembers(
        self: *TypeChecker,
        members: *std.ArrayListUnmanaged(*const ast.TypeExpr),
        t: *const ast.TypeExpr,
    ) Error!void {
        const unaliased = self.unaliasType(t);
        if (unaliased.* == .sum) {
            for (unaliased.sum.members) |m| try self.appendSumMembers(members, m);
            return;
        }
        for (members.items) |existing| {
            if (self.pipeTypesEqual(existing, unaliased)) return;
        }
        try members.append(self.arena.allocator(), unaliased);
    }

    /// Two same-named variants from merged error sets are compatible when both
    /// are payload-less, or both carry the same payload type. Payload type-exprs
    /// are resolved first (they are written as bare identifiers like `String`).
    fn mergeVariantPayloadsCompatible(
        self: *TypeChecker,
        scope: *Scope,
        a: ?*const ast.TypeExpr,
        b: ?*const ast.TypeExpr,
    ) Error!bool {
        if (a == null and b == null) return true;
        if (a == null or b == null) return false;
        const ra = try self.resolveTypeExpr(scope, a.?);
        const rb = try self.resolveTypeExpr(scope, b.?);
        return self.pipeTypesEqual(ra, rb);
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
        const resolved_return: ?*const ast.TypeExpr = if (fn_decl.return_type) |return_type|
            try self.resolveTypeExpr(fn_scope, return_type)
        else
            null;
        try self.stdout_type_stack.append(self.arena.allocator(), resolved_return);
        defer _ = self.stdout_type_stack.pop();

        // If the return type is a leading-`!T` (inferred) error union, set up a
        // collector so the body's yielded/propagated errors become the concrete
        // set. Keyed by the placeholder node (shared by pointer with callers'
        // resolved views), finalized into `inferred_error_sets` after the walk.
        const inferred_collector: ?*InferredErrorCollector = blk: {
            const rr = resolved_return orelse break :blk null;
            const unaliased = self.unaliasType(rr);
            if (unaliased.* != .error_union) break :blk null;
            const set_node = unaliased.error_union.err_set;
            const set = self.unaliasType(set_node);
            if (set.* != .error_set or !isInferredErrorSet(set.error_set)) break :blk null;
            const collector = try self.arena.allocator().create(InferredErrorCollector);
            collector.* = .{ .key = set_node };
            break :blk collector;
        };
        try self.inferred_collector_stack.append(self.arena.allocator(), inferred_collector);
        defer {
            _ = self.inferred_collector_stack.pop();
            if (inferred_collector) |collector| {
                self.inferred_error_sets.put(
                    self.arena.allocator(),
                    collector.key,
                    collector.variants.items,
                ) catch {};
            }
        }

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
                try self.validateFunctionBodyStdin(scope, catch_expr.handler, enclosing_stdin);
            },
            .is_expr => |is_expr| try self.validateFunctionBodyStdin(scope, is_expr.subject, enclosing_stdin),
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
            .struct_literal => |struct_literal| for (struct_literal.fields) |field| {
                try self.validateFunctionBodyStdin(scope, field.value, enclosing_stdin);
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
            .exit_stmt => |exit_stmt| if (exit_stmt.value) |value| try self.validateFunctionBodyStdin(scope, value, enclosing_stdin),
            .yield_stmt => |yield_stmt| try self.validateFunctionBodyStdin(scope, yield_stmt.value, enclosing_stdin),
            .while_stmt => |while_stmt| try self.validateBlockStdin(scope, while_stmt.body, enclosing_stdin),
            .type_binding_decl, .bash_block => {},
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

        // Enforce error handling: a bare statement whose result is an error
        // (a value, a call, or a pipeline whose final stage yields an error
        // union) leaves it unhandled. At the top level there is nothing to
        // propagate to, so it must be `catch`/`try`'d (or `||`'d to discard).
        // `catch`/`try` already consume the error. Commands keep the exit-code
        // model — their `ExecutableError` is exempt (else every bare command
        // would need a catch).
        switch (expr_stmt.expression.*) {
            .catch_expr, .try_expr => {},
            else => {
                if (try self.statementHasUnhandledError(scope, expr_stmt.expression)) {
                    try self.reportSpanError(
                        expr_stmt.expression.span(),
                        Error.UnhandledError,
                        .@"error",
                        "error is not handled; use catch (or || to discard) or propagate it",
                        .{},
                    );
                }
            },
        }
    }

    /// Whether a bare expression statement's result is an unhandled
    /// (non-`ExecutableError`) error — the error escapes as the statement's
    /// value (a bare error value, a call to an error-returning function, or a
    /// pipeline whose final stage yields an error union). Commands
    /// (`.execution` / `ExecutableError`) are exempt.
    fn statementHasUnhandledError(self: *TypeChecker, scope: *Scope, expr: *ast.Expression) Error!bool {
        const raw = (try self.resolveExprType(scope, expr)) orelse return false;
        // Resolve so a function call's raw `identifier` err_set (e.g.
        // `ExecutableError`) becomes an alias `isExecutableErrorSet` can see.
        const expr_type = try self.resolveTypeExpr(scope, raw);
        return self.isUnhandledErrorType(expr_type);
    }

    /// True for an error union or error value whose set is not the builtin
    /// `ExecutableError` (commands keep the implicit exit-code model).
    fn isUnhandledErrorType(self: *TypeChecker, t: *const ast.TypeExpr) bool {
        const unaliased = self.unaliasType(t);
        return switch (unaliased.*) {
            .error_union => |error_union| !self.isExecutableErrorSet(error_union.err_set),
            .error_set => !self.isExecutableErrorSet(unaliased),
            .err => true,
            else => false,
        };
    }

    /// Whether an error set is exempt from mandatory handling — i.e. it carries
    /// *only* command failure modes, so a value of it keeps the implicit
    /// exit-code model. This is a **subset** test (every variant is one of the
    /// builtin `ExecutableError`'s), not a signature-presence test: a merged set
    /// that also carries a user error (e.g. `ExecutableError || MyError`) has
    /// non-command variants and is therefore *not* exempt — handling it is
    /// required. An empty/inferred placeholder is not exempt (no concrete
    /// command-only variants to vouch for).
    fn isExecutableErrorSet(self: *TypeChecker, err_set: *const ast.TypeExpr) bool {
        const set = self.unaliasType(err_set);
        if (set.* != .error_set) return false;
        if (set.error_set.variants.len == 0) return false;
        for (set.error_set.variants) |v| {
            if (ast.TypeExpr.executableErrorSet.variant(v.name.name) == null) return false;
        }
        return true;
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
            .struct_literal => |*struct_literal| self.runStructLiteral(scope, struct_literal),
            .range => |*range| self.runRange(scope, range),
            .pipeline => |*pipeline| self.runPipeline(scope, pipeline),
            .member => |*member| self.runMember(scope, member),
            .unary => |*unary| self.runUnary(scope, unary),
            .binary => |*binary| self.runBinary(scope, binary),
            .block => |*block| self.runBlockInNewScope(scope, block),
            .if_expr => |*if_expr| self.runIfExpr(scope, if_expr),
            .for_expr => |*for_expr| self.runForExpr(scope, for_expr),
            .match_expr => |*match_expr| self.runMatchExpr(scope, match_expr),
            .catch_expr => |*catch_expr| self.runCatch(scope, catch_expr),
            .try_expr => |*try_expr| self.runTry(scope, try_expr),
            .is_expr => |*is_expr| self.runIs(scope, is_expr),
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
            .error_set => |error_set| self.runErrorSet(scope, error_set),
            .type_merge => |merge| {
                try self.runTypeExpression(scope, merge.lhs);
                try self.runTypeExpression(scope, merge.rhs);
            },
            .sum => |sum| for (sum.members) |member| try self.runTypeExpression(scope, member),
            .err => {},
            .array => |*array| self.runTypeArray(scope, array),
            .struct_type, .module, .tuple, .function, .fn_ref_type => {},
        };
    }

    /// Validates an error set declaration: resolves each variant's payload type
    /// and reports duplicate variant names.
    fn runErrorSet(
        self: *TypeChecker,
        scope: *Scope,
        error_set: ast.TypeExpr.ErrorSet,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, error_set.span);

        for (error_set.variants, 0..) |variant, i| {
            if (variant.payload) |payload| {
                try self.runTypeExpression(scope, payload);
            }

            for (error_set.variants[0..i]) |prev| {
                if (std.mem.eql(u8, prev.name.name, variant.name.name)) {
                    try self.reportSpanError(
                        variant.name.span,
                        Error.DuplicateErrorVariant,
                        .@"error",
                        "duplicate error variant {s}",
                        .{variant.name.name},
                    );
                    break;
                }
            }
        }
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

    /// If `subject` is error-like, returns the error set being matched (the
    /// union's set, or a bare error set); otherwise null.
    fn matchErrorSet(self: *TypeChecker, scope: *Scope, subject: *ast.Expression) Error!?ast.TypeExpr.ErrorSet {
        const raw = try self.resolveSubjectType(scope, subject) orelse return null;
        const subject_type = self.unaliasType(raw);
        return switch (subject_type.*) {
            .error_set => self.resolveInferredErrorSet(subject_type),
            .error_union => |error_union| self.resolveInferredErrorSet(error_union.err_set),
            else => null,
        };
    }

    /// If the match subject resolves to a sum type, returns it (for type-pattern
    /// dispatch); otherwise null.
    fn matchSumType(self: *TypeChecker, scope: *Scope, subject: *ast.Expression) Error!?ast.TypeExpr.SumType {
        const raw = try self.resolveSubjectType(scope, subject) orelse return null;
        const subject_type = self.unaliasType(raw);
        return switch (subject_type.*) {
            .sum => |sum| sum,
            else => null,
        };
    }

    /// Type-matches a sum value: each case pattern is a member-type name
    /// (`Int`, `String`, …) or `_`. Inside a case body the subject binding is
    /// narrowed to that member (a scoped shadow), and exhaustiveness over the
    /// members is enforced unless a `_` case is present.
    fn runSumMatch(
        self: *TypeChecker,
        scope: *Scope,
        match_expr: *ast.MatchExpr,
        sum: ast.TypeExpr.SumType,
    ) Error!void {
        const subject_name = referencedBindingName(match_expr.subject);
        var has_wildcard = false;
        // Track which members are covered for exhaustiveness.
        var covered = try self.arena.allocator().alloc(bool, sum.members.len);
        @memset(covered, false);

        for (match_expr.cases) |case| {
            const body_scope = try scope.addChild(self.arena.allocator(), case.span);

            const member: ?*const ast.TypeExpr = switch (case.pattern) {
                .wildcard => blk: {
                    has_wildcard = true;
                    break :blk null;
                },
                .binding => |binding| blk: {
                    const idx = self.sumMemberIndexByName(sum, binding.name) orelse {
                        try self.reportSpanError(
                            binding.span,
                            Error.TypeMismatch,
                            .@"error",
                            "'{s}' is not a member of the sum type being matched",
                            .{binding.name},
                        );
                        break :blk null;
                    };
                    covered[idx] = true;
                    break :blk sum.members[idx];
                },
                else => blk: {
                    try self.reportSpanError(
                        case.pattern.span(),
                        Error.UnsupportedExpression,
                        .@"error",
                        "sum match patterns must be a member type (e.g. Int) or _",
                        .{},
                    );
                    break :blk null;
                },
            };

            // Narrow the subject binding to the matched member inside the body,
            // and bind an optional `|n|` capture to the narrowed value (useful
            // when the subject isn't a plain binding, e.g. `match f() { … }`).
            if (member) |m| {
                if (subject_name) |name| {
                    try self.installNarrowFacts(body_scope, &.{.{ .name = name, .type_expr = m }});
                }
                if (case.capture) |capture| {
                    if (capture.bindings.len == 1) {
                        try self.runBindingPattern(body_scope, capture.bindings[0], m, false, false);
                    } else {
                        try self.reportSpanError(
                            capture.span,
                            Error.BindingPatternNotSupported,
                            .@"error",
                            "match captures require exactly one binding",
                            .{},
                        );
                    }
                }
            }

            try self.runBlock(body_scope, @constCast(&case.body));
        }

        if (!has_wildcard) {
            for (sum.members, covered) |m, c| {
                if (!c) try self.reportSpanError(
                    match_expr.span,
                    Error.NonExhaustiveMatch,
                    .@"error",
                    "match is not exhaustive: missing member '{f}' (add it or a `_` case)",
                    .{m},
                );
            }
        }
    }

    /// The index of the sum member matching a primitive type name (`Int`,
    /// `Float`, `Bool`, `String`), or null.
    fn sumMemberIndexByName(self: *TypeChecker, sum: ast.TypeExpr.SumType, name: []const u8) ?usize {
        for (sum.members, 0..) |member, i| {
            const m = self.unaliasType(member);
            const matches = switch (m.*) {
                .integer => std.mem.eql(u8, name, "Int"),
                .float => std.mem.eql(u8, name, "Float"),
                .boolean => std.mem.eql(u8, name, "Bool"),
                .array => |array| array.element.* == .byte and std.mem.eql(u8, name, "String"),
                else => false,
            };
            if (matches) return i;
        }
        return null;
    }

    fn runErrorMatch(
        self: *TypeChecker,
        scope: *Scope,
        match_expr: *ast.MatchExpr,
        error_set: ast.TypeExpr.ErrorSet,
    ) Error!void {
        var has_wildcard = false;
        for (match_expr.cases) |case| {
            if (case.pattern == .wildcard) has_wildcard = true;
            const body_scope = try scope.addChild(self.arena.allocator(), case.span);

            const variant: ?ast.TypeExpr.ErrorSet.Variant = switch (case.pattern) {
                .wildcard => null,
                .path => |path| blk: {
                    const variant_name = path.segments[path.segments.len - 1].name;
                    const v = error_set.variant(variant_name) orelse {
                        try self.reportSpanError(
                            path.span,
                            Error.ErrorNotInErrorSet,
                            .@"error",
                            "error set has no variant '{s}'",
                            .{variant_name},
                        );
                        break :blk null;
                    };
                    break :blk v;
                },
                else => blk: {
                    try self.reportSpanError(
                        case.pattern.span(),
                        Error.UnsupportedExpression,
                        .@"error",
                        "error match patterns must be Set.Variant or _",
                        .{},
                    );
                    break :blk null;
                },
            };

            if (case.capture) |capture| {
                if (capture.bindings.len != 1) {
                    try self.reportSpanError(
                        capture.span,
                        Error.BindingPatternNotSupported,
                        .@"error",
                        "match captures require exactly one binding",
                        .{},
                    );
                } else if (variant) |v| {
                    if (v.payload) |payload| {
                        try self.runBindingPattern(
                            body_scope,
                            capture.bindings[0],
                            try self.resolveTypeExpr(body_scope, payload),
                            false,
                            false,
                        );
                    } else {
                        try self.reportSpanError(
                            capture.span,
                            Error.TypeMismatch,
                            .@"error",
                            "error variant '{s}' has no payload to capture",
                            .{v.name.name},
                        );
                    }
                }
            }

            try self.runBlock(body_scope, @constCast(&case.body));
        }

        // Exhaustiveness: without a `_` case, every variant must be covered.
        // (An inferred/open set has no concrete variants, so nothing to check.)
        if (!has_wildcard) {
            for (error_set.variants) |variant| {
                var covered = false;
                for (match_expr.cases) |case| {
                    if (case.pattern == .path) {
                        const segments = case.pattern.path.segments;
                        if (std.mem.eql(u8, segments[segments.len - 1].name, variant.name.name)) {
                            covered = true;
                            break;
                        }
                    }
                }
                if (!covered) {
                    try self.reportSpanError(
                        match_expr.span,
                        Error.NonExhaustiveMatch,
                        .@"error",
                        "match is not exhaustive: missing variant '{s}' (add it or a `_` case)",
                        .{variant.name.name},
                    );
                }
            }
        }
    }

    pub fn runMatchExpr(self: *TypeChecker, scope: *Scope, match_expr: *ast.MatchExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, match_expr.span);

        try self.runExpression(scope, match_expr.subject);

        // Matching on an error value: cases are `Set.Variant` patterns and may
        // capture the variant's payload.
        if (try self.matchErrorSet(scope, match_expr.subject)) |error_set| {
            try self.runErrorMatch(scope, match_expr, error_set);
            return;
        }

        // Matching on a sum value: cases are member-type patterns
        // (`Int => …, String => …`), each narrowing the subject in its body.
        if (try self.matchSumType(scope, match_expr.subject)) |sum| {
            try self.runSumMatch(scope, match_expr, sum);
            return;
        }

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

    /// A flow-narrowing fact: within a branch, `name` has the refined type
    /// `type_expr` (a scoped shadow of its declared type). See
    /// `future/sum-types-plan.md`.
    const NarrowFact = struct { name: []const u8, type_expr: *const ast.TypeExpr };

    pub fn runIfExpr(self: *TypeChecker, scope: *Scope, if_expr: *ast.IfExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, if_expr.span);

        try self.runExpression(scope, if_expr.condition);
        const condition_type = try self.resolveConditionType(scope, if_expr.condition);

        // Derive flow-narrowing facts the condition proves about sum-typed
        // bindings, and install them as scoped shadows so the branch bodies see
        // the refined types (then: the tested type; else: the rest).
        var then_facts: std.ArrayListUnmanaged(NarrowFact) = .empty;
        var else_facts: std.ArrayListUnmanaged(NarrowFact) = .empty;
        try self.collectNarrowingFacts(scope, if_expr.condition, &then_facts, &else_facts);

        const then_scope = try scope.addChild(self.arena.allocator(), if_expr.span);
        try self.installNarrowFacts(then_scope, then_facts.items);
        try self.runIfCapture(then_scope, if_expr, condition_type);
        try self.runExpression(then_scope, if_expr.then_expr);

        if (if_expr.else_branch) |*else_branch| {
            const else_scope = try scope.addChild(self.arena.allocator(), if_expr.span);
            try self.installNarrowFacts(else_scope, else_facts.items);
            try self.runElseBranch(else_scope, else_branch);
        }
    }

    /// Installs narrowing facts into a branch scope as shadowing bindings, so
    /// lookups inside the branch resolve to the refined type.
    fn installNarrowFacts(self: *TypeChecker, branch_scope: *Scope, facts: []const NarrowFact) Error!void {
        for (facts) |fact| {
            branch_scope.declare(
                self.arena.allocator(),
                ast.Identifier.global(fact.name),
                fact.type_expr,
                false,
                false,
            ) catch |err| switch (err) {
                // Already shadowed (e.g. a capture) — leave it.
                error.IdentifierAlreadyDeclared => {},
                else => return err,
            };
        }
    }

    /// Extracts the narrowing facts a condition proves: `then_facts` hold inside
    /// the then-branch, `else_facts` inside the else-branch. Handles `x is T`
    /// and `&&` conjunctions; other conditions contribute nothing (yet). Only
    /// immutable bindings are narrowed for now (a `var`'s flow type can change on
    /// reassignment — a later phase).
    fn collectNarrowingFacts(
        self: *TypeChecker,
        scope: *Scope,
        condition: *const ast.Expression,
        then_facts: *std.ArrayListUnmanaged(NarrowFact),
        else_facts: *std.ArrayListUnmanaged(NarrowFact),
    ) Error!void {
        switch (condition.*) {
            .is_expr => |is_expr| {
                const name = referencedBindingName(is_expr.subject) orelse return;
                const binding = scope.lookup(name) orelse return;
                const declared = binding.type_expr orelse return;
                const tested = self.unaliasType(try self.resolveTypeExpr(scope, is_expr.type_expr));

                try then_facts.append(self.arena.allocator(), .{ .name = name, .type_expr = tested });
                if (try self.sumWithout(declared, tested)) |narrowed| {
                    try else_facts.append(self.arena.allocator(), .{ .name = name, .type_expr = narrowed });
                }
            },
            .binary => |binary| switch (binary.op) {
                // `a && b` proves both in the then-branch; the else-branch can't
                // be narrowed (only that *some* operand is false).
                .logical_and => {
                    var discard: std.ArrayListUnmanaged(NarrowFact) = .empty;
                    try self.collectNarrowingFacts(scope, binary.left, then_facts, &discard);
                    try self.collectNarrowingFacts(scope, binary.right, then_facts, &discard);
                },
                // `x == v` narrows the then-branch to the members of `x`'s sum
                // that `v`'s type can be (the intersection); `!=` narrows the
                // else-branch symmetrically. The other branch can't narrow.
                .equal => try self.collectEqualityNarrowing(scope, binary, then_facts),
                .not_equal => try self.collectEqualityNarrowing(scope, binary, else_facts),
                // Relational ops narrow the then-branch to the members that
                // *support* the operator (numeric members).
                .less, .less_equal, .greater, .greater_equal => try self.collectRelationalNarrowing(scope, binary, then_facts),
                else => {},
            },
            else => {},
        }
    }

    /// Narrowing for `x == v` / (else of) `x != v`: narrow `x` to the
    /// intersection of its sum members with `v`'s type. Either operand may be
    /// the binding.
    fn collectEqualityNarrowing(
        self: *TypeChecker,
        scope: *Scope,
        binary: ast.BinaryExpr,
        target: *std.ArrayListUnmanaged(NarrowFact),
    ) Error!void {
        const sides = bindingAndValueSides(binary.left, binary.right) orelse return;
        const binding = scope.lookup(sides.name) orelse return;
        const declared = binding.type_expr orelse return;
        const value_raw = (try self.resolveExprType(scope, sides.value)) orelse return;
        const value_type = self.unaliasType(try self.resolveTypeExpr(scope, value_raw));
        if (try self.sumIntersect(declared, value_type)) |narrowed| {
            try target.append(self.arena.allocator(), .{ .name = sides.name, .type_expr = narrowed });
        }
    }

    /// Narrowing for a relational comparison (`<`, `>`, `<=`, `>=`): narrow `x`
    /// to its numeric members (the ones that support the operator).
    fn collectRelationalNarrowing(
        self: *TypeChecker,
        scope: *Scope,
        binary: ast.BinaryExpr,
        target: *std.ArrayListUnmanaged(NarrowFact),
    ) Error!void {
        const sides = bindingAndValueSides(binary.left, binary.right) orelse return;
        const binding = scope.lookup(sides.name) orelse return;
        const d = self.unaliasType(binding.type_expr orelse return);
        if (d.* != .sum) return;

        var numeric: std.ArrayListUnmanaged(*const ast.TypeExpr) = .empty;
        for (d.sum.members) |m| {
            switch (m.*) {
                .integer, .float => try numeric.append(self.arena.allocator(), m),
                else => {},
            }
        }
        if (numeric.items.len == 0 or numeric.items.len == d.sum.members.len) return;
        const narrowed = if (numeric.items.len == 1)
            numeric.items[0]
        else
            try self.allocTypeExpression(.{ .sum = .{ .members = try numeric.toOwnedSlice(self.arena.allocator()), .span = d.sum.span } });
        try target.append(self.arena.allocator(), .{ .name = sides.name, .type_expr = narrowed });
    }

    const BindingValueSides = struct { name: []const u8, value: *ast.Expression };

    /// For a binary comparison, identifies which operand is a narrowable binding
    /// reference and which is the compared value (in either order).
    fn bindingAndValueSides(left: *ast.Expression, right: *ast.Expression) ?BindingValueSides {
        if (referencedBindingName(left)) |name| return .{ .name = name, .value = right };
        if (referencedBindingName(right)) |name| return .{ .name = name, .value = left };
        return null;
    }

    /// The intersection of a sum's members with `other` (a single type, or the
    /// members of another sum): the members of `declared` compatible with
    /// `other`, collapsed to a single type when one remains. Null when `declared`
    /// isn't a sum or nothing intersects.
    fn sumIntersect(
        self: *TypeChecker,
        declared: *const ast.TypeExpr,
        other: *const ast.TypeExpr,
    ) Error!?*const ast.TypeExpr {
        const d = self.unaliasType(declared);
        if (d.* != .sum) return null;

        var kept: std.ArrayListUnmanaged(*const ast.TypeExpr) = .empty;
        for (d.sum.members) |m| {
            const matches = if (other.* == .sum) blk: {
                for (other.sum.members) |om| {
                    if (self.pipeTypesEqual(m, om)) break :blk true;
                }
                break :blk false;
            } else self.pipeTypesEqual(m, other);
            if (matches) try kept.append(self.arena.allocator(), m);
        }
        if (kept.items.len == 0) return null;
        if (kept.items.len == 1) return kept.items[0];
        return try self.allocTypeExpression(.{
            .sum = .{ .members = try kept.toOwnedSlice(self.arena.allocator()), .span = d.sum.span },
        });
    }

    /// The type a sum has after removing `member` (the else-branch of `x is T`):
    /// the remaining members, collapsed to a single type when one remains. Null
    /// when `declared` isn't a sum, or nothing remains.
    fn sumWithout(
        self: *TypeChecker,
        declared: *const ast.TypeExpr,
        member: *const ast.TypeExpr,
    ) Error!?*const ast.TypeExpr {
        const d = self.unaliasType(declared);
        if (d.* != .sum) return null;

        var remaining: std.ArrayListUnmanaged(*const ast.TypeExpr) = .empty;
        for (d.sum.members) |m| {
            if (!self.pipeTypesEqual(m, member)) try remaining.append(self.arena.allocator(), m);
        }
        if (remaining.items.len == 0) return null;
        if (remaining.items.len == 1) return remaining.items[0];
        return try self.allocTypeExpression(.{
            .sum = .{ .members = try remaining.toOwnedSlice(self.arena.allocator()), .span = d.sum.span },
        });
    }

    /// The binding name referenced by an expression (a bare identifier, or the
    /// zero-arg call a bare identifier parses into), or null.
    fn referencedBindingName(expr: *const ast.Expression) ?[]const u8 {
        return switch (expr.*) {
            .identifier => |identifier| identifier.name,
            .call => |call| if (call.arguments.len == 0 and call.callee.* == .identifier)
                call.callee.identifier.name
            else
                null,
            else => null,
        };
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
            // `if (errorUnion) |value|` binds the ok payload.
            .error_union => |error_union| try self.runBindingPattern(
                then_scope,
                capture.bindings[0],
                error_union.payload,
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

    pub fn runCatch(self: *TypeChecker, scope: *Scope, catch_expr: *ast.CatchExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, catch_expr.span);

        try self.runExpression(scope, catch_expr.subject);
        const subject_raw = try self.resolveSubjectType(scope, catch_expr.subject);
        const subject_type = if (subject_raw) |t| self.unaliasType(t) else null;

        // The handler runs in a child scope so the optional `|err|` capture is
        // visible only inside it.
        const handler_scope = try scope.addChild(self.arena.allocator(), catch_expr.span);
        if (catch_expr.capture) |capture| {
            if (capture.bindings.len != 1) {
                try self.reportSpanError(
                    capture.span,
                    Error.BindingPatternNotSupported,
                    .@"error",
                    "catch capture clauses currently require exactly one binding",
                    .{},
                );
            } else {
                try self.runBindingPattern(
                    handler_scope,
                    capture.bindings[0],
                    self.catchErrorSetType(subject_type),
                    false,
                    false,
                );
            }
        }
        try self.runExpression(handler_scope, catch_expr.handler);

        // The left-hand side must be error-like (per the spec, `catch` on a
        // non-error value is a type error). A command (`execution`) is accepted:
        // it is treated as `ExecutableError!String`.
        if (subject_type) |st| switch (st.*) {
            .error_union, .error_set, .err, .failed, .execution => {},
            else => try self.reportSpanError(
                catch_expr.subject.span(),
                Error.TypeMismatch,
                .@"error",
                "catch requires an error union or error value on the left-hand side, found {f}",
                .{st},
            ),
        };
    }

    /// `x is T` — type-check the subject and the tested type. Always evaluates
    /// to `Bool`. (Narrowing facts derived from it are handled where conditions
    /// are analyzed; see future/sum-types-plan.md.)
    pub fn runIs(self: *TypeChecker, scope: *Scope, is_expr: *ast.IsExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, is_expr.span);

        try self.runExpression(scope, is_expr.subject);
        try self.runTypeExpression(scope, is_expr.type_expr);
    }

    pub fn runTry(self: *TypeChecker, scope: *Scope, try_expr: *ast.TryExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, try_expr.span);

        try self.runExpression(scope, try_expr.subject);
        const subject_raw = try self.resolveSubjectType(scope, try_expr.subject);
        const subject_type = if (subject_raw) |t| self.unaliasType(t) else null;

        // The subject must be error-like; the error case is propagated out of
        // the enclosing function. (Validating that the enclosing function's
        // error set accepts the propagated error is deferred to Phase 6.) A
        // command (`execution`) is accepted as `ExecutableError!String`.
        if (subject_type) |st| {
            // `try` re-raises the subject's errors out of the enclosing
            // function, so they belong to its inferred set (if any).
            try self.collectInferredFromType(st);
            switch (st.*) {
                .error_union, .error_set => try self.validateTryPropagation(try_expr, st),
                // `try cmd` propagates the command's `ExecutableError`; record it
                // on the enclosing inferred set (#3c). Safe now that the
                // exemption test is subset-based: a set that mixes these with a
                // user error is correctly non-exempt.
                .execution => for (ast.TypeExpr.executableErrorVariants) |v| try self.collectInferredVariant(v),
                .err, .failed => {},
                else => try self.reportSpanError(
                    try_expr.subject.span(),
                    Error.TypeMismatch,
                    .@"error",
                    "try requires an error union or error value, found {f}",
                    .{st},
                ),
            }
        }
    }

    /// Verifies the enclosing function covers the error set `try` propagates:
    /// every variant the subject can raise must be in the function's declared
    /// error set, else propagating it would escape the declared type. A function
    /// whose return type is not an error union (or which declares no return type)
    /// covers nothing, so propagating into it is an error — as is a top-level
    /// `try`, which has no enclosing function to propagate to. `ExecutableError`
    /// is exempt (commands keep the implicit exit-code model), and an inferred
    /// (`!T`) declared set collects what the body raises rather than constraining
    /// it.
    fn validateTryPropagation(
        self: *TypeChecker,
        try_expr: *ast.TryExpr,
        subject_type: *const ast.TypeExpr,
    ) Error!void {
        const propagated_node: *const ast.TypeExpr = switch (subject_type.*) {
            .error_union => |error_union| error_union.err_set,
            .error_set => subject_type,
            else => return,
        };
        const propagated = self.resolveInferredErrorSet(propagated_node) orelse return;

        // Commands keep the implicit exit-code model: an `ExecutableError` need
        // not be declared in the enclosing function's return type to propagate.
        if (propagated.variant("NonZeroExit") != null and propagated.variant("SpawnFailed") != null) return;

        // Locate the enclosing function's declared error set, if any.
        const declared_stdout: ?*const ast.TypeExpr = if (self.stdout_type_stack.items.len == 0)
            null
        else
            self.stdout_type_stack.items[self.stdout_type_stack.items.len - 1];

        if (declared_stdout) |stdout| {
            const declared = self.unaliasType(stdout);
            if (declared.* == .error_union) {
                const declared_set = self.unaliasType(declared.error_union.err_set);
                // An unresolved set can't be enforced yet; an inferred (`!T`) set
                // collects what the body raises rather than constraining it.
                if (declared_set.* != .error_set) return;
                if (isInferredErrorSet(declared_set.error_set)) return;

                for (propagated.variants) |variant| {
                    if (declared_set.error_set.variant(variant.name.name) == null) {
                        try self.reportSpanError(
                            try_expr.subject.span(),
                            Error.ErrorNotInErrorSet,
                            .@"error",
                            "try propagates error '{s}', which is not in the enclosing function's error set {f}",
                            .{ variant.name.name, declared_set },
                        );
                    }
                }
                return;
            }
        }

        // No enclosing error set to propagate into: either the enclosing
        // function's return type is not an error union, or there is no enclosing
        // function at all (a top-level `try`). Every propagated variant escapes
        // undeclared, so it must be handled here instead.
        const at_top_level = self.stdout_type_stack.items.len == 0;
        for (propagated.variants) |variant| {
            if (at_top_level) {
                try self.reportSpanError(
                    try_expr.subject.span(),
                    Error.ErrorNotInErrorSet,
                    .@"error",
                    "try propagates error '{s}', but there is no enclosing function to propagate to; handle it with catch",
                    .{variant.name.name},
                );
            } else {
                try self.reportSpanError(
                    try_expr.subject.span(),
                    Error.ErrorNotInErrorSet,
                    .@"error",
                    "try propagates error '{s}', but the enclosing function's return type declares no error set to cover it; handle it with catch or declare it in the return type",
                    .{variant.name.name},
                );
            }
        }
    }

    /// Resolves the type of a `catch`/`try`/`match` subject, seeing through the
    /// zero-arg call wrapper a bare identifier parses into — but only when the
    /// identifier is error-like, so a zero-arg command (`ls catch x`) still
    /// resolves to its `execution` type.
    fn resolveSubjectType(self: *TypeChecker, scope: *Scope, subject: *ast.Expression) Error!?*const ast.TypeExpr {
        // A pipeline aborts to an error if any stage can produce one, so as a
        // `catch`/`try`/`match` subject it is an error union (`E!<last stage>`)
        // even when the final stage's own type is plain `T` (the error
        // short-circuits past it at runtime). This is only the *subject* view;
        // the pipeline's ordinary result type is unchanged, so a bare pipeline
        // statement still prints rather than being forced to handle.
        if (subject.* == .pipeline) {
            if (try self.pipelineErrorResultType(scope, &subject.pipeline)) |t| return t;
        }
        if (subject.* == .call and subject.call.arguments.len == 0 and subject.call.callee.* == .identifier) {
            if (try self.resolveExprType(scope, subject.call.callee)) |callee_type| {
                switch (self.unaliasType(callee_type).*) {
                    .error_union, .error_set, .err => return callee_type,
                    else => {},
                }
            }
        }
        // Resolve the result: a function call's return type comes back raw
        // (e.g. `E!String`'s err_set is an unresolved `identifier`), which
        // `match`/`catch` capture handling must see through to the error set.
        const resolved = try self.resolveExprType(scope, subject) orelse return null;
        return try self.resolveTypeExpr(scope, resolved);
    }

    /// If any stage of `pipeline` can produce a (non-`ExecutableError`) error,
    /// returns the pipeline's error-union result type `E!<final stage's ok type>`
    /// — the type a surrounding `catch`/`try`/`match` operates on, since such an
    /// error aborts the pipeline and becomes its value. Returns null when no
    /// stage can error (or the final stage's type can't be resolved).
    fn pipelineErrorResultType(self: *TypeChecker, scope: *Scope, pipeline: *ast.Pipeline) Error!?*const ast.TypeExpr {
        if (pipeline.stages.len == 0) return null;
        var err_set: ?*const ast.TypeExpr = null;
        for (pipeline.stages) |stage| {
            const stage_type = (try self.resolvePipelineStageStdoutType(scope, stage)) orelse continue;
            const unaliased = self.unaliasType(stage_type);
            if (unaliased.* == .error_union and !self.isExecutableErrorSet(unaliased.error_union.err_set)) {
                err_set = unaliased.error_union.err_set;
                break;
            }
        }
        const set = err_set orelse return null;

        // Payload = the final stage's ok type (unwrap its own error union).
        const last = pipeline.stages[pipeline.stages.len - 1];
        const last_type = (try self.resolvePipelineStageStdoutType(scope, last)) orelse return null;
        const payload = switch (self.unaliasType(last_type).*) {
            .error_union => |error_union| error_union.payload,
            else => last_type,
        };
        return try self.allocTypeExpression(.{ .error_union = .{
            .err_set = set,
            .payload = payload,
            .span = pipeline.span,
        } });
    }

    /// Resolves an `if` condition's *value* type. A bare identifier parses as a
    /// zero-arg call, so see through it: a variable yields its own type, a
    /// function yields its return type (so `if (fn) |v|` binds the ok payload of
    /// an error-union/optional-returning function, like a direct binding would).
    fn resolveConditionType(self: *TypeChecker, scope: *Scope, condition: *ast.Expression) Error!?*const ast.TypeExpr {
        if (condition.* == .call and condition.call.arguments.len == 0 and condition.call.callee.* == .identifier) {
            if (try self.resolveExprType(scope, condition.call.callee)) |callee_type| {
                return switch (self.unaliasType(callee_type).*) {
                    .function => |function| function.return_type,
                    else => callee_type,
                };
            }
        }
        return self.resolveExprType(scope, condition);
    }

    /// The error set type bound to a `catch |err|` capture: the union's error
    /// set, or the subject itself when it is already an error value.
    fn catchErrorSetType(self: *TypeChecker, subject_type: ?*const ast.TypeExpr) ?*const ast.TypeExpr {
        const st = subject_type orelse return null;
        return switch (st.*) {
            .error_union => |error_union| self.unaliasType(error_union.err_set),
            else => st,
        };
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

        // Member-specific operation enforcement: a *bare* (un-narrowed) sum is
        // never numeric, so it can't be used in arithmetic — narrow it first
        // (with `is`/`==`/match). A *narrowed* operand resolves to its member
        // type here (via the scoped shadow binding), so this only rejects the
        // un-narrowed case. Comparison/relational ops are intentionally allowed:
        // they are how you narrow (`if (x > 5)`, `if (x == 0)`).
        switch (binary.op) {
            .add, .subtract, .multiply, .divide, .remainder => {
                try self.rejectBareSum(binary.left, left_type, "use in arithmetic");
                try self.rejectBareSum(binary.right, right_type, "use in arithmetic");
            },
            // Comparing a sum to a value that can never share a member is almost
            // always a mistake (the result is constant). Reject it when the
            // intersection is empty. (`==`/`!=`/relational double as narrowing.)
            .equal, .not_equal, .less, .less_equal, .greater, .greater_equal => {
                try self.rejectEmptyComparison(binary.left, left_type, binary.right, right_type);
            },
            else => {},
        }

        // Member access parses as a `.member` binary op (e.g. `MyError.Nope`).
        // Validate error-set variant access here so an unknown variant is a
        // type-checker diagnostic (stderr) rather than a later compiler error
        // (stdout). Other member kinds are typed via `resolveExprType`.
        if (binary.op == .member) {
            if (binary.right.* == .identifier) {
                switch (self.unaliasType(left_type).*) {
                    .error_set => |error_set| try self.runErrorSetMemberAccess(error_set, &binary.right.identifier),
                    else => {},
                }
            }
            return;
        }

        if (binary.op == .@"orelse") {
            switch (self.unaliasType(left_type).*) {
                // Resolve the child: a function call's return type comes back raw
                // (e.g. `?String`'s child is an unresolved `identifier`), which
                // `validateTypeAssignment` rejects as an UnresolvedTypeLiteral.
                .optional => |optional| try self.validateTypeAssignment(
                    try self.resolveTypeExpr(scope, optional.child),
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

            // For a plain `x = v` to an identifier binding, validate against the
            // binding's *declared* type (not its current narrowed flow type — a
            // `var x: Int || String` narrowed to `String` can still be reassigned
            // an Int), then refine the flow type so reads after the assignment
            // see the new (narrowed) type.
            if (binary.op == .assign) {
                if (referencedBindingName(binary.left)) |name| {
                    if (scope.lookup(name)) |binding| {
                        const declared = binding.declared_type orelse left_type;
                        try self.validateTypeAssignment(declared, right_type, .{ .span = right_type.span() });
                        if (self.unaliasType(declared).* == .sum) {
                            binding.type_expr = self.flowTypeForSum(declared, right_type);
                        }
                        return;
                    }
                }
            }

            try self.validateTypeAssignment(left_type, right_type, .{ .span = right_type.span() });
        }
    }

    /// The flow type a sum-declared binding takes after being assigned a value of
    /// `value_type`: the intersection (a single member when the value is one
    /// member), else the declared sum itself (no refinement).
    fn flowTypeForSum(
        self: *TypeChecker,
        declared: *const ast.TypeExpr,
        value_type: *const ast.TypeExpr,
    ) *const ast.TypeExpr {
        const v = self.unaliasType(value_type);
        return (self.sumIntersect(declared, v) catch null) orelse declared;
    }

    /// Reports an error when one side of a comparison is a sum and the other's
    /// type shares no member with it — the comparison can never be true (`==`) or
    /// never false (`!=`), so it's almost certainly a mistake. Only fires when
    /// exactly one side is a sum (sum-vs-sum / member-vs-member are left alone).
    fn rejectEmptyComparison(
        self: *TypeChecker,
        left: *const ast.Expression,
        left_type: *const ast.TypeExpr,
        right: *const ast.Expression,
        right_type: *const ast.TypeExpr,
    ) Error!void {
        const l = self.unaliasType(left_type);
        const r = self.unaliasType(right_type);
        const sum_side: *const ast.TypeExpr, const other: *const ast.TypeExpr, const other_expr =
            if (l.* == .sum and r.* != .sum)
                .{ l, r, right }
            else if (r.* == .sum and l.* != .sum)
                .{ r, l, left }
            else
                return;

        if ((try self.sumIntersect(sum_side, other)) != null) return;
        try self.reportSpanError(
            other_expr.span(),
            Error.TypeMismatch,
            .@"error",
            "comparing {f} with {f}: '{f}' is not one of the sum's members, so the comparison is always {s}",
            .{ sum_side, other, other, @as([]const u8, "false") },
        );
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
            .error_set => |error_set| try self.runErrorSetMemberAccess(error_set, &member.member),
            .null, .promise, .error_union, .err, .tuple, .function, .fn_ref_type, .integer, .float, .boolean, .byte, .alias, .void, .type_merge, .sum => return error.UnsupportedMemberAccess,
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

    pub fn runErrorSetMemberAccess(
        self: *TypeChecker,
        error_set: ast.TypeExpr.ErrorSet,
        identifier: *ast.Identifier,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, identifier.span);

        if (error_set.variant(identifier.name) != null) return;

        try self.reportSpanError(
            identifier.span,
            Error.ErrorNotInErrorSet,
            .@"error",
            "error set has no variant '{s}'",
            .{identifier.name},
        );
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
        /// Upstream provides E!T, downstream expects T.  The ok payload crosses
        /// unchanged; an error short-circuits past the downstream to the nearest
        /// handler (D7).
        short_circuit_error,
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

        // E!T → T (the ok payload crosses; an error short-circuits).
        if (up.* == .error_union) {
            const payload = self.unaliasType(up.error_union.payload);
            if (self.pipeTypesEqual(payload, down)) return .short_circuit_error;
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
        // E!T → T: the ok payload crosses; an error short-circuits downstream.
        if (up.* == .error_union) {
            const payload = self.unaliasType(up.error_union.payload);
            if (self.pipeTypesEqual(payload, down)) return;
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
            // Sums are unordered sets: equal iff same size and every member of
            // one matches a member of the other (members are deduped, so a
            // same-size one-way subset suffices). So `Int || String` ==
            // `String || Int`, and two separately-built identical sums compare equal.
            .sum => |left_sum| blk: {
                const right_sum = resolved_right.sum;
                if (left_sum.members.len != right_sum.members.len) break :blk false;
                for (left_sum.members) |lm| {
                    var found = false;
                    for (right_sum.members) |rm| {
                        if (self.pipeTypesEqual(lm, rm)) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) break :blk false;
                }
                break :blk true;
            },
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

    pub fn runStructLiteral(self: *TypeChecker, scope: *Scope, struct_literal: *ast.StructLiteral) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, struct_literal.span);

        for (struct_literal.fields) |field| try self.runExpression(scope, field.value);

        const binding = scope.lookup(struct_literal.name.name) orelse {
            try self.reportSpanError(
                struct_literal.name.span,
                Error.IdentifierNotFound,
                .@"error",
                "type '{s}' is not declared",
                .{struct_literal.name.name},
            );
            return;
        };

        const type_expr = self.unaliasType(binding.type_expr orelse return);
        switch (type_expr.*) {
            .error_set => |error_set| try self.runErrorValueLiteral(scope, error_set, struct_literal),
            else => try self.reportSpanError(
                struct_literal.name.span,
                Error.UnsupportedExpression,
                .@"error",
                "'{s}' is not an error set; struct literal construction is only supported for error values",
                .{struct_literal.name.name},
            ),
        }
    }

    fn runErrorValueLiteral(
        self: *TypeChecker,
        scope: *Scope,
        error_set: ast.TypeExpr.ErrorSet,
        struct_literal: *ast.StructLiteral,
    ) Error!void {
        if (struct_literal.fields.len != 1) {
            try self.reportSpanError(
                struct_literal.span,
                Error.UnsupportedExpression,
                .@"error",
                "error value construction requires exactly one variant field",
                .{},
            );
            return;
        }

        const field = struct_literal.fields[0];
        const variant = error_set.variant(field.name.name) orelse {
            try self.reportSpanError(
                field.name.span,
                Error.ErrorNotInErrorSet,
                .@"error",
                "error set has no variant '{s}'",
                .{field.name.name},
            );
            return;
        };

        const payload = variant.payload orelse {
            try self.reportSpanError(
                field.name.span,
                Error.TypeMismatch,
                .@"error",
                "error variant '{s}' has no payload; use {s}.{s} instead",
                .{ field.name.name, struct_literal.name.name, field.name.name },
            );
            return;
        };

        const resolved_payload = try self.resolveTypeExpr(scope, payload);
        const value_type = try self.resolveExprType(scope, field.value) orelse return;
        try self.validateTypeAssignment(resolved_payload, value_type, .{ .span = field.value.span() });
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
            .interpolation => |expr| {
                try self.runExpression(scope, expr);
                // Interpolating a bare (un-narrowed) sum has no single string
                // form — narrow it first (with `is`/`==`/match).
                if (try self.resolveExprType(scope, expr)) |t| {
                    try self.rejectBareSum(expr, t, "interpolate");
                }
            },
            .text => {},
        }
    }

    /// Reports an error if `expr`'s type is a bare sum — a sum must be narrowed
    /// (via `is`/`==`/match) before a member-specific operation like `action`.
    fn rejectBareSum(
        self: *TypeChecker,
        expr: *const ast.Expression,
        expr_type: *const ast.TypeExpr,
        comptime action: []const u8,
    ) Error!void {
        if (self.unaliasType(expr_type).* != .sum) return;
        try self.reportSpanError(
            expr.span(),
            Error.TypeMismatch,
            .@"error",
            "cannot " ++ action ++ " a sum type ({f}) directly; narrow it first with `is`, `==`, or match",
            .{self.unaliasType(expr_type)},
        );
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

        const raw_module_type = try import.resolveType(self.io, self.arena.allocator(), scope) orelse {
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

        const result = try expr.resolveType(self.io, self.arena.allocator(), scope);

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
            // A `||` merge is resolved to a concrete `error_set` or `sum` before
            // it is stored as a binding type, so a raw merge should not reach here.
            .type_merge => {},
            .sum => |*sum| self.validateTypeAssignmentSum(
                sum,
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
            .null => return,
            else => try self.validateTypeAssignment(assignee.child, assignment_type, options),
        }
    }

    /// Widening into a sum: a value is assignable to `A || B (|| …)` when its
    /// type is (structurally) one of the members, or itself a sub-sum whose
    /// members are all present. Narrowing (sum → member) is not allowed here —
    /// that requires an explicit type-`match` (Phase 3, future/sum-types-plan.md).
    pub fn validateTypeAssignmentSum(
        self: *TypeChecker,
        assignee: *const ast.TypeExpr.SumType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        const actual = self.unaliasType(assignment_type);
        // `assignee` points into the real `TypeExpr` union (captured by pointer
        // in the dispatch), so recovering it for diagnostics is sound.
        const assignee_type: *const ast.TypeExpr = @fieldParentPtr("sum", assignee);

        // A sub-sum widens iff every one of its members is in the assignee.
        if (actual.* == .sum) {
            for (actual.sum.members) |m| {
                if (!self.sumHasMember(assignee.*, m)) {
                    return try self.reportAssignmentError(assignee_type, assignment_type, options);
                }
            }
            return;
        }

        if (self.sumHasMember(assignee.*, actual)) return;

        try self.reportAssignmentError(assignee_type, assignment_type, options);
    }

    /// Whether `t` is structurally equal to one of the sum's members.
    fn sumHasMember(self: *TypeChecker, sum: ast.TypeExpr.SumType, t: *const ast.TypeExpr) bool {
        for (sum.members) |member| {
            if (self.pipeTypesEqual(member, t)) return true;
        }
        return false;
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

        // The union's error set may be referenced by name (alias/identifier);
        // unalias it before inspecting variants (avoids a wrong-field access).
        const union_err_set: ?ast.TypeExpr.ErrorSet = blk: {
            const unaliased = self.unaliasType(assignee.err_set);
            break :blk if (unaliased.* == .error_set) unaliased.error_set else null;
        };

        switch (assignment_type.*) {
            .error_union => |error_union| {
                if (union_err_set) |us| {
                    try self.validateTypeAssignmentErrorSet(us, error_union.err_set, options);
                }
                try self.validateTypeAssignment(
                    assignee.payload,
                    error_union.payload,
                    options,
                );
            },
            // A bare error value / error set coerces into the union when its
            // variants are all members of the union's error set.
            .error_set => {
                if (union_err_set) |us| {
                    try self.validateTypeAssignmentErrorSet(us, assignment_type, options);
                }
            },
            .err => |err| {
                if (union_err_set) |us| {
                    try self.validateErrorInSet(us, err.name.name, assignment_type.span(), options);
                }
            },
            // A command coerces into an error union: its ok value is its
            // captured String output; failure yields an ExecutableError.
            .execution => {
                const string_type = try self.allocStringType();
                try self.validateTypeAssignment(assignee.payload, string_type, options);
            },
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

        // Every variant of the assigned set must be present in the expected set
        // (the assigned set is a subset of the expected set). The assigned set
        // may be referenced by name, so unalias before reading its variants.
        const assigned = self.unaliasType(assignment_type);
        if (assigned.* != .error_set) {
            try self.reportAssignmentError(error_set, assignment_type, options);
            return;
        }
        for (assigned.error_set.variants) |variant| {
            try self.validateErrorInSet(error_set, variant.name.name, variant.span, options);
        }
    }

    pub fn validateErrorInSet(
        self: *TypeChecker,
        error_set: ast.TypeExpr.ErrorSet,
        variant_name: []const u8,
        span: ast.Span,
        _: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, span);

        if (error_set.variant(variant_name) != null) return;

        try self.reportSpanError(
            span,
            Error.ErrorNotInErrorSet,
            .@"error",
            "error '{s}' not in expected error set",
            .{variant_name},
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

        // TODO(error-handling Phase 3): validate a value being assigned to a
        // single error-variant type against that variant's payload.
        if (error_type.payload) |payload| {
            if (payload == assignment_type) return;
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
/// `fn String parseInt() ParseError!Int` — a bad parse is a catchable error.
const parse_int_byte_type = ast.TypeExpr{ .byte = .{ .span = .global } };
const parse_int_string_type = ast.TypeExpr{ .array = .{ .element = &parse_int_byte_type, .span = .global } };
const parse_int_payload_type = ast.TypeExpr{ .integer = .{ .span = .global } };
const parse_int_return_type = ast.TypeExpr{ .error_union = .{
    .err_set = &ast.TypeExpr.parseErrorType,
    .payload = &parse_int_payload_type,
    .span = .global,
} };
const parse_int_fn_type = ast.TypeExpr{ .function = .{
    .params = .nonVariadic(&.{}),
    .stdin_type = &parse_int_string_type,
    .return_type = &parse_int_return_type,
    .span = .global,
} };

/// Stable storage for the `parseFloat` builtin's function type:
/// `fn String parseFloat() ParseError!Float`.
const parse_float_byte_type = ast.TypeExpr{ .byte = .{ .span = .global } };
const parse_float_string_type = ast.TypeExpr{ .array = .{ .element = &parse_float_byte_type, .span = .global } };
const parse_float_payload_type = ast.TypeExpr{ .float = .{ .span = .global } };
const parse_float_return_type = ast.TypeExpr{ .error_union = .{
    .err_set = &ast.TypeExpr.parseErrorType,
    .payload = &parse_float_payload_type,
    .span = .global,
} };
const parse_float_fn_type = ast.TypeExpr{ .function = .{
    .params = .nonVariadic(&.{}),
    .stdin_type = &parse_float_string_type,
    .return_type = &parse_float_return_type,
    .span = .global,
} };

/// Stable storage for the `lines` builtin's function type:
/// `fn String lines() String` — frames a byte stream into per-line values.
const lines_byte_type = ast.TypeExpr{ .byte = .{ .span = .global } };
const lines_string_type = ast.TypeExpr{ .array = .{ .element = &lines_byte_type, .span = .global } };
const lines_fn_type = ast.TypeExpr{ .function = .{
    .params = .nonVariadic(&.{}),
    .stdin_type = &lines_string_type,
    .return_type = &lines_string_type,
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
    .init("ExecutableError", ast.TypeExpr.executableErrorType),
    .init("ParseError", ast.TypeExpr.parseErrorType),
};

/// Builtin value bindings (not types) available in every module's global scope.
const global_value_definitions = [_]Definition{
    .{ .identifier = .{ .name = "parseInt", .span = .global }, .type_expr = &parse_int_fn_type },
    .{ .identifier = .{ .name = "parseFloat", .span = .global }, .type_expr = &parse_float_fn_type },
    .{ .identifier = .{ .name = "lines", .span = .global }, .type_expr = &lines_fn_type },
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
