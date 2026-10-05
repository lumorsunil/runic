const std = @import("std");
const ast = @import("../frontend/ast.zig");
const builtins = @import("../builtins.zig");
const rainbow = @import("../rainbow.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;
const Parser = @import("../frontend/parser.zig").Parser;
const token = @import("../frontend/token.zig");
const CType = @import("../ffi/ctype.zig").CType;

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
    /// User-defined generic type constructors (`const Box(T) = struct { … }`),
    /// keyed by name. A `Box(Int)` application substitutes the args into `body`.
    generic_type_ctors: std.StringHashMapUnmanaged(GenericTypeCtor) = .empty,
    /// Comptime type functions — an ordinary function whose comptime type params
    /// and `yield`ed type make it a generic-type constructor
    /// (`fn Box(comptime T: type) type { yield struct { value: T } }`). Keyed by
    /// name; a `Box Int` call substitutes the args into the yielded type. The
    /// Zig-native replacement for `generic_type_ctors`.
    comptime_type_fns: std.StringHashMapUnmanaged(GenericTypeCtor) = .empty,
    /// Function overload sets: an original name declared more than once maps to
    /// its candidates, each renamed to a unique mangled name in the AST (so the
    /// compiler sees distinct functions). A call to the original name is resolved
    /// to one candidate by argument types and the expected return type, then its
    /// callee is rewritten to that mangled name. Keyed by original name.
    overload_sets: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(OverloadEntry)) = .empty,
    /// The expected type of the expression currently being checked, when known
    /// from context (a binding annotation, a `yield`'s return type, a call
    /// argument's parameter type). Consulted to disambiguate a return-type-only
    /// overload (`pure` → `Maybe(Int)` vs `[]Int`). A stack for nesting.
    expected_type_stack: std.ArrayListUnmanaged(?*const ast.TypeExpr) = .empty,
    /// The stdout (return) type expected of the next anonymous fn body about to be
    /// checked — the parameter type when a trailing block / anonymous `fn(…) T` is
    /// passed to a `fn(…) T` parameter (`check "m" { … }`, `retry n { … }`). A bare
    /// trailing block declares no return type of its own, so `runFnDecl` adopts
    /// this as the body's stdout type, making a `fn() Bool` body's stray `echo` the
    /// same error a named function gets (and avoiding the capture-pipe deadlock it
    /// would otherwise cause at runtime). Set by `runCall` around the argument and
    /// consumed (cleared) by `runFnDecl` at entry, so nested fns don't inherit it.
    pending_fn_body_stdout: ?*const ast.TypeExpr = null,
    /// Overloaded call nodes already resolved (rewritten or reported), so a call
    /// visited from several sites (walk, type resolution) is handled exactly once.
    overload_resolved: std.AutoHashMapUnmanaged(*const ast.CallExpr, void) = .empty,
    /// Names currently bound by a comptime `for (@fields(T)) |f|` loop. Within the
    /// loop body, `f.type` is a comptime type (a valid `match`/introspection
    /// subject) and `f.name` a string. Saved/restored around each such loop.
    comptime_field_vars: std.StringHashMapUnmanaged(void) = .empty,
    env: ?*std.process.Environ.Map = null,
    /// Lazily-created parser used to re-parse `@insert` operand strings into real
    /// struct fields when materializing a recipe struct (mirrors the IR compiler's
    /// `insert_parser`). Kept for the checker's lifetime so its arena outlives the
    /// fields it produces.
    insert_parser: ?Parser = null,
    /// Strict mode (`--strict`): also require handling of command failures
    /// (`ExecutableError`), which are otherwise exempt (bash-like exit-code
    /// model). A `set -e`-style opt-in — off by default.
    strict: bool = false,
    /// Stack of the enclosing functions' declared stdout types. Pushed when a
    /// function body is type-checked and consulted by `runYield` so that every
    /// `yield &1` is validated in the scope where it actually appears (e.g.
    /// inside a `for (&0) |v| { yield v }` loop, where `v` is only bound in the
    /// loop's child scope). A null entry means the enclosing function declared
    /// no stdout type, so its yields are unconstrained.
    stdout_type_stack: std.ArrayListUnmanaged(?*const ast.TypeExpr) = .empty,
    /// Parallel to `stdout_type_stack`, but holding each function's *raw*
    /// (unresolved) declared return type. `runYield` uses it to name an anonymous
    /// struct literal (`yield .{ … }`) from the return type — the raw form keeps
    /// a constructor name (`Maybe(B)`) that resolution erases into a nameless
    /// struct. Pushed/popped together with `stdout_type_stack`.
    stdout_return_raw_stack: std.ArrayListUnmanaged(?*const ast.TypeExpr) = .empty,

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
        strict: bool,
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
            .strict = strict,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        for (self.modules.keys()) |key| {
            self.arena.child_allocator.free(key);
        }
        self.arena.deinit();
    }

    /// Reclaims all analysis memory so a long-lived checker (the LSP reuses one
    /// per workspace) does not grow without bound across re-checks. Frees the
    /// child-allocated module keys, then resets the arena — which owns every
    /// scope, `TypeExpr`, diagnostic, and the maps' backing — and clears the
    /// now-dangling unmanaged maps/lists. After this the checker is empty, as if
    /// freshly initialized; callers must re-`typeCheck` any documents they still
    /// need scopes/diagnostics for.
    pub fn reset(self: *TypeChecker) void {
        for (self.modules.keys()) |key| {
            self.arena.child_allocator.free(key);
        }
        _ = self.arena.reset(.free_all);
        self.modules = .empty;
        self.diagnostics = .empty;
        self.generic_type_ctors = .empty;
        self.comptime_type_fns = .empty;
        // These collections are arena-backed, so `reset(.free_all)` just freed
        // their storage: clear the now-dangling headers too, otherwise the next
        // pass appends into freed memory (a segfault the moment a function body
        // is type-checked). This is why re-checking after an edit crashed.
        self.stdout_type_stack = .empty;
        self.stdout_return_raw_stack = .empty;
        self.overload_sets = .empty;
        self.expected_type_stack = .empty;
        self.pending_fn_body_stdout = null;
        self.overload_resolved = .empty;
        self.inferred_error_sets = .empty;
        self.inferred_collector_stack = .empty;
        self.comptime_field_vars = .empty;
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
        try self.runTopLevelBlock(global_scope, &root_block);

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
            if (entry.value_ptr.is_pub and !entry.value_ptr.is_global) public_count += 1;
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
            if (!binding.is_pub or binding.is_global) continue;
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

    /// The function declaration a statement is, if it is a bare `fn …` expression
    /// statement; null otherwise.
    fn fnDeclStatement(statement: *ast.Statement) ?*ast.FunctionDecl {
        if (statement.* != .expression) return null;
        if (statement.expression.expression.* != .fn_decl) return null;
        return &statement.expression.expression.fn_decl;
    }

    /// Checks the top-level script block with forward references resolved: two
    /// interleaved passes over the statements. Pass 1 walks in source order —
    /// a non-function statement is checked normally, and a named function
    /// declaration only has its *signature* declared (its body deferred). Because
    /// signatures are declared in source order alongside the consts/types/imports
    /// they reference, a signature still sees its dependencies — while every
    /// function body (pass 2) sees every top-level function's signature, so it can
    /// call functions declared later (mutual recursion, forward references). Pass 2
    /// walks in source order too, checking only the deferred function bodies.
    /// Scoped to the top level: nested-block functions keep declare-before-use, so
    /// the type checker never accepts a forward reference the IR compiler (which
    /// hoists only top-level functions) would fail to compile.
    fn runTopLevelBlock(self: *TypeChecker, scope: *Scope, block: *ast.Block) Error!void {
        // Pass 0: collect overload sets and mangle their declarations, so pass 1
        // declares each candidate under a distinct name and calls can resolve.
        try self.collectOverloads(block);

        // Pass 1 (source order): bindings/type declarations are checked normally,
        // and a function only has its *signature* declared. Declarations come
        // before the signatures that reference them, so a signature still resolves
        // its dependency types.
        for (block.statements) |statement| {
            if (fnDeclStatement(statement)) |fn_decl| {
                try self.declareFunctionSignature(scope, fn_decl);
            } else if (statement.* == .binding_decl or statement.* == .type_binding_decl) {
                try self.runStatement(scope, statement);
            }
        }
        // Pass 2 (source order): function bodies and the imperative statements
        // (calls, `match`, loops, `yield`). Deferring these past pass 1 means a
        // body — or a consumer like `match f` — sees every function's signature
        // (forward references, mutual recursion) *and* its walked body (so an
        // inferred `!T` error set is finalized before a `match`/`catch` reads it).
        for (block.statements) |statement| {
            if (fnDeclStatement(statement) != null) {
                try self.runStatement(scope, statement);
            } else if (statement.* != .binding_decl and statement.* != .type_binding_decl) {
                try self.runStatement(scope, statement);
            }
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
            // `break`/`continue` carry no value and introduce no bindings; the IR
            // compiler validates that they appear inside a loop.
            .break_stmt, .continue_stmt => {},
            .while_stmt => |*while_stmt| self.runWhile(scope, while_stmt),
            .expression => |*expr_stmt| self.runExpressionStatement(scope, expr_stmt),
            else => error.UnsupportedStatement,
        };
    }

    fn runWhile(self: *TypeChecker, scope: *Scope, while_stmt: *ast.WhileStmt) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, while_stmt.span);

        // The condition is resolved in the enclosing scope; the body runs in a
        // fresh child scope so its bindings are loop-local.
        try self.runExpression(scope, while_stmt.condition);
        const condition_type = try self.resolveConditionType(scope, while_stmt.condition);

        const body_scope = try scope.addChild(self.arena.allocator(), while_stmt.body.span);

        // `while (opt) |v| { … }` loops while the optional is present, binding the
        // unwrapped value in the body scope.
        if (while_stmt.capture) |capture| {
            if (capture.bindings.len != 1) {
                try self.reportSpanError(
                    capture.span,
                    Error.BindingPatternNotSupported,
                    .@"error",
                    "while capture clauses currently require exactly one binding",
                    .{},
                );
                return;
            }

            const cond_type = condition_type orelse {
                try self.reportSpanError(
                    while_stmt.condition.span(),
                    Error.TypeMismatch,
                    .@"error",
                    "a `while (…) |v|` capture requires an optional condition",
                    .{},
                );
                return;
            };

            switch (cond_type.*) {
                .optional => |optional| try self.runBindingPattern(
                    body_scope,
                    capture.bindings[0],
                    optional.child,
                    false,
                    false,
                ),
                else => {
                    try self.reportSpanError(
                        while_stmt.condition.span(),
                        Error.TypeMismatch,
                        .@"error",
                        "a `while (…) |v|` capture requires an optional condition",
                        .{},
                    );
                    return;
                },
            }
        }

        try self.runBlock(body_scope, &while_stmt.body);
    }

    fn runYield(self: *TypeChecker, scope: *Scope, yield_stmt: *ast.YieldStmt) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, yield_stmt.span);

        // Inferred struct literal returned by value: `yield .{ .x = 3 }` takes its
        // type from the enclosing function's declared stdout (return) type. Only a
        // yield to stdout (&1) carries that type.
        if (yield_stmt.fd == 1 and exprMayNeedStructInference(yield_stmt.value) and
            self.stdout_return_raw_stack.items.len > 0)
        {
            self.stampInferredLiteral(yield_stmt.value, self.stdout_return_raw_stack.items[self.stdout_return_raw_stack.items.len - 1]);
        }

        // The enclosing function's declared return type is the yielded value's
        // expected type — used to pick a return-type-only overload (`yield pure x`).
        // Push the *raw* return type (resolved lazily in `overloadReturnMatches`,
        // like a binding annotation): a higher-kinded `M(B)` resolves to a bare
        // type variable, which loses the structure needed to disambiguate an
        // overload by its return constructor.
        const yield_expected: ?*const ast.TypeExpr = if (yield_stmt.fd == 1 and self.stdout_return_raw_stack.items.len > 0)
            self.stdout_return_raw_stack.items[self.stdout_return_raw_stack.items.len - 1]
        else
            null;
        try self.expected_type_stack.append(self.arena.allocator(), yield_expected);
        defer _ = self.expected_type_stack.pop();

        try self.runExpression(scope, yield_stmt.value);

        // Only `yield`s to stdout (&1) are constrained by the enclosing
        // function's declared stdout type; stderr (&2) carries untyped
        // diagnostic output. Validating here (rather than in a separate body
        // walk) means the yielded expression is resolved in the exact scope it
        // appears in, including loop-capture bindings like `for (&0) |v|`.
        if (yield_stmt.fd != 1) return;
        if (self.stdout_type_stack.items.len == 0) return;
        const declared_stdout_raw = self.stdout_type_stack.items[self.stdout_type_stack.items.len - 1] orelse return;
        // Resolve the declared type so a generic application (`Box(T)`) is
        // compared as its substituted struct, not the raw application node.
        const declared_stdout = try self.resolveTypeExpr(scope, declared_stdout_raw);
        const yielded = try self.resolveExprType(scope, yield_stmt.value) orelse return;
        const resolved = try self.resolvePipeType(scope, yielded) orelse return;
        if (self.pipeTypesEqual(resolved, declared_stdout)) return;
        // A bare value (or `null`) widens into an optional, including nested in a
        // struct field: `yield .{ .x = n }` satisfies a `Maybe(T)` return.
        if (self.yieldCoercesToType(resolved, declared_stdout)) return;

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

    /// Whether a yielded value type *coerces* to a declared return type — a
    /// directional widening the symmetric `pipeTypesEqual` can't express. On top
    /// of exact equality it allows a bare value or `null` to satisfy an optional
    /// (`T`/`null` → `?T`) and applies that structurally through struct fields and
    /// array elements, so `{ x: Int }` (a `yield .{ .x = n }`) satisfies a
    /// `{ x: ?Int }` return. The reverse — an optional where a bare value is
    /// required — does not hold, which is why this stays out of `pipeTypesEqual`.
    fn yieldCoercesToType(
        self: *TypeChecker,
        yielded: *const ast.TypeExpr,
        declared: *const ast.TypeExpr,
    ) bool {
        if (self.pipeTypesEqual(yielded, declared)) return true;
        const d = self.unaliasType(declared);
        const y = self.unaliasType(yielded);
        switch (d.*) {
            .optional => {
                if (y.* == .null) return true;
                return self.yieldCoercesToType(yielded, d.optional.child);
            },
            .struct_type => {
                if (y.* != .struct_type) return false;
                const yf = y.struct_type.fields;
                const df = d.struct_type.fields;
                if (yf.len != df.len) return false;
                for (yf, df) |ly, ld| {
                    if (!std.mem.eql(u8, ly.name.name, ld.name.name)) return false;
                    if (!self.yieldCoercesToType(ly.type_expr, ld.type_expr)) return false;
                }
                return true;
            },
            .array => return y.* == .array and self.yieldCoercesToType(y.array.element, d.array.element),
            else => return false,
        }
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

    pub const GenericTypeCtor = struct {
        params: []const ast.Identifier,
        body: *const ast.TypeExpr,
    };

    /// One candidate of a function overload set: its mangled (unique) name and
    /// the declaration it was renamed from.
    pub const OverloadEntry = struct {
        mangled: []const u8,
        decl: *ast.FunctionDecl,
    };

    /// Returns a copy of `type_expr` with each identifier that names one of
    /// `params` replaced by the corresponding entry in `args` — the core of a
    /// generic type application (`Box(Int)` substitutes `Int` for `T` in the
    /// constructor's body). Recurses through the composite type shapes.
    fn substituteTypeParams(
        self: *TypeChecker,
        type_expr: *const ast.TypeExpr,
        params: []const ast.Identifier,
        args: []const *const ast.TypeExpr,
    ) Error!*const ast.TypeExpr {
        switch (type_expr.*) {
            .identifier => |named| {
                if (named.path.segments.len == 1) {
                    const name = named.path.segments[0].name;
                    for (params, 0..) |param, i| {
                        if (i < args.len and std.mem.eql(u8, param.name, name)) return args[i];
                    }
                }
                return type_expr;
            },
            .array => |a| return self.allocTypeExpression(.{ .array = .{
                .element = try self.substituteTypeParams(a.element, params, args),
                .span = a.span,
            } }),
            .optional => |o| return self.allocTypeExpression(.{ .optional = .{
                .child = try self.substituteTypeParams(o.child, params, args),
                .span = o.span,
            } }),
            .promise => |p| return self.allocTypeExpression(.{ .promise = .{
                .child = try self.substituteTypeParams(p.child, params, args),
                .span = p.span,
            } }),
            .type_application => |app| {
                const new_args = try self.arena.allocator().alloc(*const ast.TypeExpr, app.args.len);
                for (app.args, new_args) |arg, *dst| dst.* = try self.substituteTypeParams(arg, params, args);
                return self.allocTypeExpression(.{ .type_application = .{ .name = app.name, .args = new_args, .span = app.span } });
            },
            .struct_type => |st| {
                const new_fields = try self.arena.allocator().alloc(ast.TypeExpr.StructField, st.fields.len);
                for (st.fields, new_fields) |field, *dst| {
                    dst.* = field;
                    dst.type_expr = try self.substituteTypeParams(field.type_expr, params, args);
                }
                var new_st = st;
                new_st.fields = new_fields;
                // Substitute a static decl's declared type too (`nothing: ?T` →
                // `?Int`), so member access on the instantiated type sees it
                // concretely.
                if (st.decls.len > 0) {
                    const new_decls = try self.arena.allocator().alloc(ast.TypeExpr.StructDecl, st.decls.len);
                    for (st.decls, new_decls) |decl, *dst| {
                        dst.* = decl;
                        if (decl.type_expr) |t| dst.type_expr = try self.substituteTypeParams(t, params, args);
                    }
                    new_st.decls = new_decls;
                }
                return self.allocTypeExpression(.{ .struct_type = new_st });
            },
            else => return type_expr,
        }
    }

    /// Resolves a generic constructor's body (`const Box(T) = struct { value: T }`)
    /// with its type parameters bound in scope as permissive type variables, so a
    /// bare parameter reference in the body (`value: T`) resolves as that
    /// parameter rather than an undeclared type. Used where the body is resolved
    /// before the type arguments are known — an inferred `Box{ … }` construction.
    /// (A `Box(Int)` *application* substitutes the args first; see
    /// `resolveTypeApplication`.)
    fn resolveGenericCtorBody(
        self: *TypeChecker,
        scope: *Scope,
        ctor: GenericTypeCtor,
    ) Error!*const ast.TypeExpr {
        const ctor_scope = try self.arena.allocator().create(Scope);
        ctor_scope.* = .initWithParent(scope, ctor.body.span());
        for (ctor.params) |param| {
            const marker = try self.allocTypeExpression(.{ .type_var = .{ .name = param.name, .span = param.span } });
            try ctor_scope.declare(self.arena.allocator(), param, marker, false, false);
        }
        const resolved = try self.resolveTypeExpr(ctor_scope, ctor.body);
        // `resolveTypeExpr` leaves a struct's plainly-named field types alone (to
        // avoid expanding ordinary nested structs), so a bare parameter field
        // (`value: T`) would stay unresolved and hit the undeclared-type error.
        // Resolve each field explicitly in the param scope so `T` becomes its
        // type variable — and a field naming a genuinely undeclared type still
        // errors.
        const unaliased = self.unaliasType(resolved);
        if (unaliased.* == .struct_type) {
            const st = unaliased.struct_type;
            const new_fields = try self.arena.allocator().alloc(ast.TypeExpr.StructField, st.fields.len);
            for (st.fields, new_fields) |field, *dst| {
                dst.* = field;
                dst.type_expr = try self.resolveTypeExpr(ctor_scope, field.type_expr);
            }
            var new_st = st;
            new_st.fields = new_fields;
            return try self.allocTypeExpression(.{ .struct_type = new_st });
        }
        return resolved;
    }

    /// Resolves a `Name(args…)` application against a registered generic
    /// constructor: substitutes the args into its body, then resolves the result.
    /// Whether `name` is bound in scope to a type variable (a `|T|`/`|M|` capture
    /// introduced by the enclosing signature), rather than a concrete type.
    fn nameIsTypeVar(self: *TypeChecker, scope: *Scope, name: []const u8) bool {
        const binding = scope.lookup(name) orelse return false;
        const t = binding.type_expr orelse return false;
        return self.unaliasType(t).* == .type_var;
    }

    fn resolveTypeApplication(
        self: *TypeChecker,
        scope: *Scope,
        app: ast.TypeExpr.TypeApplication,
    ) Error!*const ast.TypeExpr {
        // A higher-kinded application `M(A)` whose constructor is a captured type
        // variable is permissive in the generic body — a type variable that
        // unifies with anything. The concrete constructor is bound only by the IR
        // compiler's monomorphization (per `M`), which resolves `M(B)` for real.
        // `M(A)` where `M` is a captured constructor — either written `|M|(A)`
        // (ctor_is_capture) or a later bare `M(B)` whose `M` is a type variable
        // introduced by an earlier `|M|(…)`. Permissive in the generic body; the
        // IR compiler binds `M` concretely at monomorphization.
        if (app.ctor_is_capture or self.nameIsTypeVar(scope, app.name.name)) {
            return self.allocTypeExpression(.{ .type_var = .{ .name = app.name.name, .span = app.span } });
        }
        const ctor = self.generic_type_ctors.get(app.name.name) orelse {
            try self.reportSpanError(
                app.span,
                Error.IdentifierNotFound,
                .@"error",
                "generic type {s} not declared",
                .{app.name.name},
            );
            return self.allocTypeExpression(.{ .failed = .{ .span = app.span } });
        };
        // Resolve the arguments first (`Int` → the primitive, a bound capture →
        // its concrete type) so the substituted field types are concrete.
        const resolved_args = try self.arena.allocator().alloc(*const ast.TypeExpr, app.args.len);
        for (app.args, resolved_args) |arg, *dst| dst.* = try self.resolveTypeExpr(scope, arg);
        const substituted = try self.substituteTypeParams(ctor.body, ctor.params, resolved_args);
        // A comptime type constructor whose body uses `@insert` / `for … @insert`
        // carries an unexpanded recipe. The type checker can't fold the operand
        // strings to full types (no comptime string folder), but it can materialize
        // the field *names* — so member access and (annotation-driven) construction
        // catch typos statically; the field types stay permissive and the IR
        // compiler resolves the real layout.
        // Name the resulting struct after the written application (`Maybe(Int)`,
        // `Pair(Int)(String)`) so hover shows `Maybe(Int){ … }` rather than
        // `<struct>{ … }` — the type-function analog of a `const S = struct { … }`
        // binding getting the name `S`. Display-only; each instantiation gets its
        // own name, like Zig's `Maybe(i32)` vs `Maybe(bool)`.
        if (substituted.* == .struct_type and substituted.struct_type.body_items.len > 0) {
            if (try self.materializeRecipeFields(scope, substituted.struct_type, ctor.params, resolved_args)) |fields| {
                var st = substituted.struct_type;
                st.fields = fields;
                st.body_items = &.{};
                if (st.name == null) st.name = try std.fmt.allocPrint(self.arena.allocator(), "{f}", .{app});
                return self.allocTypeExpression(.{ .struct_type = st });
            }
            // Couldn't statically resolve the recipe (e.g. a dynamic field *name*):
            // keep it permissive and let the IR compiler materialize the layout.
            return substituted;
        }
        const resolved = try self.resolveTypeExpr(scope, substituted);
        if (resolved.* == .struct_type and resolved.struct_type.name == null) {
            var st = resolved.struct_type;
            st.name = try std.fmt.allocPrint(self.arena.allocator(), "{f}", .{app});
            return self.allocTypeExpression(.{ .struct_type = st });
        }
        return resolved;
    }

    /// Materializes a struct recipe (`@insert` / `for (@fields(T)) |f| @insert …`)
    /// into concrete fields for static checking — the same layout the IR compiler
    /// produces, so member access and (annotation-driven) construction catch typos
    /// and get real field types. An explicit (`.field`) item keeps its substituted
    /// type; a generated field's name and type come from folding the operand string
    /// and re-parsing it. Returns null when the recipe can't be statically resolved
    /// (a dynamic field *name*, an unresolvable `@fields(…)` source, or a malformed
    /// operand) — the caller then keeps it permissive for the compiler to handle.
    fn materializeRecipeFields(
        self: *TypeChecker,
        scope: *Scope,
        st: ast.TypeExpr.StructType,
        params: []const ast.Identifier,
        args: []const *const ast.TypeExpr,
    ) Error!?[]const ast.TypeExpr.StructField {
        const gpa = self.arena.allocator();
        var fields = std.ArrayList(ast.TypeExpr.StructField).empty;
        defer fields.deinit(gpa);
        for (st.body_items) |item| switch (item) {
            .field => |f| {
                const subbed = try self.substituteTypeParams(f.type_expr, params, args);
                try fields.append(gpa, .{ .name = f.name, .type_expr = subbed, .span = f.span });
            },
            .insert => |operand| {
                if (!try self.expandInsert(&fields, operand, null, params, args)) return null;
            },
            .for_insert => |fi| {
                const arg = comptimeFieldsCallArg(fi.source) orelse return null;
                // The `@fields(…)` argument is usually the constructor's own type
                // parameter (`@fields(T)`), which maps to the bound argument here —
                // not something resolvable in the use-site scope.
                const arg_type = self.insertSourceType(scope, arg, params, args) orelse return null;
                const src = self.unaliasType(arg_type);
                if (src.* != .struct_type) return null;
                for (src.struct_type.fields) |ff| {
                    if (!try self.expandInsert(&fields, fi.operand, ff, params, args)) return null;
                }
            },
        };
        return try gpa.dupe(ast.TypeExpr.StructField, fields.items);
    }

    /// Resolves the argument of a recipe's `@fields(<arg>)` source: a bare
    /// identifier matching a constructor type parameter maps to its bound argument
    /// (`@fields(T)` → the concrete arg); anything else is resolved in `scope`.
    fn insertSourceType(
        self: *TypeChecker,
        scope: *Scope,
        arg: *const ast.Expression,
        params: []const ast.Identifier,
        args: []const *const ast.TypeExpr,
    ) ?*const ast.TypeExpr {
        const name: ?[]const u8 = switch (arg.*) {
            .identifier => |id| id.name,
            .call => |c| if (c.arguments.len == 0 and c.callee.* == .identifier) c.callee.identifier.name else null,
            else => null,
        };
        if (name) |n| for (params, 0..) |param, i| {
            if (i < args.len and std.mem.eql(u8, param.name, n)) return args[i];
        };
        return self.argToType(scope, arg) catch null orelse null;
    }

    /// Expands one `@insert` operand into concrete fields (appended to `fields`).
    /// The operand string is folded into a re-parseable `name: Type[, …]` list:
    /// `${f.name}` becomes the bound field's literal name, and every other
    /// interpolation (`${T}`, `${f.type}`, …) a fresh placeholder type identifier
    /// whose real type is recorded and substituted back after re-parsing — so the
    /// generated fields carry their true types (`?Int` supports `orelse`, etc.).
    /// Returns false when the recipe can't be resolved statically (no re-parser, a
    /// malformed operand, or an interpolation landing in field-*name* position), so
    /// the caller can keep the whole struct permissive.
    fn expandInsert(
        self: *TypeChecker,
        fields: *std.ArrayList(ast.TypeExpr.StructField),
        operand: *const ast.Expression,
        field_var: ?ast.TypeExpr.StructField,
        params: []const ast.Identifier,
        args: []const *const ast.TypeExpr,
    ) Error!bool {
        if (operand.* != .literal or operand.literal != .string) return false;
        const gpa = self.arena.allocator();

        var buf = std.ArrayList(u8).empty;
        defer buf.deinit(gpa);
        var ph_params = std.ArrayList(ast.Identifier).empty;
        defer ph_params.deinit(gpa);
        var ph_args = std.ArrayList(*const ast.TypeExpr).empty;
        defer ph_args.deinit(gpa);

        for (operand.literal.string.segments) |segment| switch (segment) {
            .text => |t| try buf.appendSlice(gpa, t.payload),
            .interpolation => |ie| {
                // `${f.name}` is the generated field's name — emit it as literal text.
                if (field_var) |fv| {
                    if (memberAccessParts(ie)) |ma| {
                        if (std.mem.eql(u8, ma.member, "name")) {
                            try buf.appendSlice(gpa, fv.name.name);
                            continue;
                        }
                    }
                }
                // Anything else fills a type position: record its resolved type and
                // emit a placeholder identifier to swap back in after re-parsing.
                const ty = self.interpType(ie, field_var, params, args) orelse
                    try self.allocTypeExpression(.{ .failed = .{ .span = operand.span() } });
                const placeholder = try std.fmt.allocPrint(gpa, "__ins_{d}", .{ph_args.items.len});
                try ph_params.append(gpa, .{ .name = placeholder, .span = operand.span() });
                try ph_args.append(gpa, ty);
                try buf.appendSlice(gpa, placeholder);
            },
        };

        const raw = self.reparseInsertFields(buf.items) orelse return false;
        for (raw) |rf| {
            // A placeholder in *name* position means the field name itself was
            // dynamic — we don't know the real shape, so bail to permissive.
            if (std.mem.startsWith(u8, rf.name.name, "__ins_")) return false;
            const resolved = try self.substituteTypeParams(rf.type_expr, ph_params.items, ph_args.items);
            try fields.append(gpa, .{ .name = rf.name, .type_expr = resolved, .span = rf.span });
        }
        return true;
    }

    /// The type an `@insert` interpolation stands for: `${f.type}` → the bound
    /// field's type; a bare `${T}` naming a constructor type parameter → its bound
    /// argument. Null when it names neither (a dynamic type the checker can't fold).
    fn interpType(
        self: *TypeChecker,
        ie: *ast.Expression,
        field_var: ?ast.TypeExpr.StructField,
        params: []const ast.Identifier,
        args: []const *const ast.TypeExpr,
    ) ?*const ast.TypeExpr {
        _ = self;
        if (memberAccessParts(ie)) |ma| {
            if (field_var) |fv| if (std.mem.eql(u8, ma.member, "type")) return fv.type_expr;
            return null;
        }
        const name: ?[]const u8 = switch (ie.*) {
            .identifier => |id| id.name,
            .call => |c| if (c.arguments.len == 0 and c.callee.* == .identifier)
                c.callee.identifier.name
            else
                null,
            else => null,
        };
        if (name) |n| for (params, 0..) |param, i| {
            if (i < args.len and std.mem.eql(u8, param.name, n)) return args[i];
        };
        return null;
    }

    /// Re-parses a folded `@insert` field-list string into struct fields, using a
    /// lazily-created checker-lifetime parser. Null when there's no environment to
    /// build one or the string doesn't parse as a field list.
    fn reparseInsertFields(self: *TypeChecker, text: []const u8) ?[]const ast.TypeExpr.StructField {
        const env = self.env orelse return null;
        if (self.insert_parser == null) {
            self.insert_parser = Parser.init(self.io, self.arena.allocator(), env, self.document_store);
        }
        return self.insert_parser.?.reparseFieldString(text) catch null;
    }

    fn runTypeBindingDecl(
        self: *TypeChecker,
        scope: *Scope,
        type_binding_decl: *ast.TypeBindingDecl,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, type_binding_decl.span);

        // A generic type constructor (`const Box(T) = …`) is registered by name;
        // each `Box(Int)` application substitutes the args into its body.
        if (type_binding_decl.params.len > 0) {
            try self.generic_type_ctors.put(self.arena.allocator(), type_binding_decl.identifier.name, .{
                .params = type_binding_decl.params,
                .body = type_binding_decl.type_expr,
            });
            return;
        }

        try self.runTypeExpression(scope, type_binding_decl.type_expr);

        var resolved_type_expr = try self.resolveTypeExpr(scope, type_binding_decl.type_expr);

        // Name an otherwise-anonymous struct after the first binding it is bound
        // to (`const S = struct { … }` → `S`), the way Zig does, so hover shows
        // `S{ … }` rather than `<struct>{ … }`. A later alias (`const T = S`)
        // resolves through `S` and does not overwrite the name (first binding
        // wins); a struct that already carries a name keeps it.
        if (resolved_type_expr.* == .struct_type and resolved_type_expr.struct_type.name == null) {
            var named = resolved_type_expr.struct_type;
            named.name = type_binding_decl.identifier.name;
            resolved_type_expr = try self.allocTypeExpression(.{ .struct_type = named });
        }

        scope.declareType(
            self.arena.allocator(),
            type_binding_decl.identifier,
            resolved_type_expr,
            type_binding_decl.is_pub,
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

    /// Whether a type expression contains a `|T|` capture anywhere.
    fn typeExprHasCapture(type_expr: *const ast.TypeExpr) bool {
        return switch (type_expr.*) {
            .type_capture => true,
            .array => |a| typeExprHasCapture(a.element),
            .optional => |o| typeExprHasCapture(o.child),
            .promise => |p| typeExprHasCapture(p.child),
            .type_application => |app| blk: {
                // A higher-kinded application `|M|(…)` captures the constructor `M`.
                if (app.ctor_is_capture) break :blk true;
                for (app.args) |arg| if (typeExprHasCapture(arg)) break :blk true;
                break :blk false;
            },
            // A function-type parameter (`fn (|A|) |B|`) captures through its
            // parameter and return types.
            .function => |f| blk: {
                if (f.return_type) |rt| if (typeExprHasCapture(rt)) break :blk true;
                if (f.stdin_type) |st| if (typeExprHasCapture(st)) break :blk true;
                switch (f.params) {
                    ._non_variadic => |ps| for (ps) |p| {
                        if (p) |pt| if (typeExprHasCapture(pt)) break :blk true;
                    },
                    ._variadic => |p| if (p) |pt| {
                        if (typeExprHasCapture(pt)) break :blk true;
                    },
                }
                break :blk false;
            },
            else => false,
        };
    }

    /// Whether a type contains a generic application (`Entry(K, V)`) anywhere,
    /// including nested under a built-in constructor.
    fn typeExprHasApplication(type_expr: *const ast.TypeExpr) bool {
        return switch (type_expr.*) {
            .type_application => true,
            .array => |a| typeExprHasApplication(a.element),
            .optional => |o| typeExprHasApplication(o.child),
            .promise => |p| typeExprHasApplication(p.child),
            else => false,
        };
    }

    /// Unifies a `|T|`-carrying `pattern` against a concrete `subject` type,
    /// declaring each capture's matched type in `scope` so later `: T` uses
    /// resolve to it. Recurses through the built-in generic constructors
    /// (`[]|T|` binds the element type, `?|T|` the child).
    fn bindTypeCaptures(
        self: *TypeChecker,
        scope: *Scope,
        pattern: *const ast.TypeExpr,
        subject: *const ast.TypeExpr,
    ) Error!void {
        switch (pattern.*) {
            .type_capture => |capture| scope.declareType(
                self.arena.allocator(),
                ast.Identifier.global(capture.name),
                subject,
                false,
            ) catch |err| switch (err) {
                error.IdentifierAlreadyDeclared => {},
                else => return err,
            },
            .array => |a| if (subject.* == .array) try self.bindTypeCaptures(scope, a.element, subject.array.element),
            .optional => |o| if (subject.* == .optional) try self.bindTypeCaptures(scope, o.child, subject.optional.child),
            .promise => |p| if (subject.* == .promise) try self.bindTypeCaptures(scope, p.child, subject.promise.child),
            // `Box(|T|)` — substitute into the constructor body (keeping the
            // capture), then match structurally against the subject.
            .type_application => |app| {
                if (self.generic_type_ctors.get(app.name.name)) |ctor| {
                    const substituted = try self.substituteTypeParams(ctor.body, ctor.params, app.args);
                    try self.bindTypeCaptures(scope, substituted, subject);
                }
            },
            // Match a struct pattern field-by-field against the subject struct.
            .struct_type => |st| if (self.unaliasType(subject).* == .struct_type) {
                const subject_st = self.unaliasType(subject).struct_type;
                for (st.fields) |field| {
                    for (subject_st.fields) |sfield| {
                        if (std.mem.eql(u8, field.name.name, sfield.name.name)) {
                            try self.bindTypeCaptures(scope, field.type_expr, sfield.type_expr);
                            break;
                        }
                    }
                }
            },
            else => {},
        }
    }

    /// The type name an inferred struct literal (`.{ … }`) should adopt from a
    /// known context type (a binding annotation, or a callee's parameter type).
    /// A raw single-segment named type (`Vector`) yields that name; a resolved
    /// alias yields its name only when it unaliases to a struct type. Anything
    /// else (a path, generic application, wrapper, or non-struct) yields nothing,
    /// leaving the literal anonymous (and thus a later "cannot infer" error).
    fn structTypeNameToStamp(self: *TypeChecker, context: ?*const ast.TypeExpr) ?ast.Identifier {
        const t = context orelse return null;
        switch (t.*) {
            .identifier => |id| {
                if (id.path.segments.len != 1) return null;
                return id.path.segments[0];
            },
            .alias => |alias| {
                if (self.unaliasType(t).* != .struct_type) return null;
                return .{ .name = alias.name, .span = alias.span };
            },
            // A generic application (`Maybe(B)`): stamp the constructor name, so
            // `yield .{ … }` against a `Maybe(B)` return infers `Maybe{ … }`.
            .type_application => |app| return app.name,
            else => return null,
        }
    }

    /// Whether an expression is an anonymous struct literal `.{ .field = … }`
    /// (empty name, no qualifying object) awaiting a type from context.
    fn isAnonStructLiteral(expr: *const ast.Expression) bool {
        return expr.* == .struct_literal and
            expr.struct_literal.name.name.len == 0 and
            expr.struct_literal.object == null;
    }

    /// Whether an expression contains an anonymous struct literal that a known
    /// context type could give a name — directly, or as an element of an array
    /// literal (`.{ .{ .x = 1 }, … }`). Used to skip context resolution when
    /// there is nothing to infer.
    fn exprMayNeedStructInference(expr: *const ast.Expression) bool {
        if (isAnonStructLiteral(expr)) return true;
        if (expr.* == .array) {
            for (expr.array.elements) |el| {
                if (exprMayNeedStructInference(el)) return true;
            }
        }
        return false;
    }

    /// Give an anonymous struct literal (or the anonymous struct elements of an
    /// array literal) the name of its expected type, taken from `context`. An
    /// array literal against an array context type stamps each element from the
    /// element type, so nested `[]Vector = .{ .{ … }, … }` works. A no-op unless
    /// the expression and context line up.
    fn stampInferredLiteral(
        self: *TypeChecker,
        expr: *ast.Expression,
        context: ?*const ast.TypeExpr,
    ) void {
        const ctx = context orelse return;
        if (isAnonStructLiteral(expr)) {
            if (self.structTypeNameToStamp(ctx)) |named| expr.struct_literal.name = named;
            return;
        }
        if (expr.* == .array and self.unaliasType(ctx).* == .array) {
            const element_type = self.unaliasType(ctx).array.element;
            for (expr.array.elements) |el| self.stampInferredLiteral(el, element_type);
        }
    }

    /// Stamp each anonymous struct-literal argument of a call with the struct type
    /// name of its matching parameter, so `f a .{ .x = 1 }` infers the literal's
    /// type from the callee's signature (mirroring the binding-annotation case).
    fn stampInferredStructArgs(self: *TypeChecker, scope: *Scope, call: *ast.CallExpr) Error!void {
        var any = false;
        for (call.arguments) |arg| {
            if (exprMayNeedStructInference(arg)) {
                any = true;
                break;
            }
        }
        if (!any) return;

        const callee = (try self.calleeFunctionForInference(scope, call.callee)) orelse return;

        for (call.arguments, 0..) |arg, i| {
            if (!exprMayNeedStructInference(arg)) continue;
            // A UFCS receiver fills parameter 0, so the written arguments start at
            // `offset` (0 for a plain call, 1 for `recv.method args`).
            const param_index = i + callee.offset;
            const param_type: ?*const ast.TypeExpr = switch (callee.params) {
                ._non_variadic => |list| if (param_index < list.len) list[param_index] else null,
                ._variadic => |element| element,
            };
            self.stampInferredLiteral(arg, param_type);
        }
    }

    /// Resolve the callee of a call to the function signature its written
    /// arguments should be typed against, plus the offset at which those
    /// arguments begin among the parameters. A plain call (`f a`) or a module
    /// member (`m.f a`) maps arguments 1:1 (offset 0); a UFCS method call
    /// (`recv.method a`) prepends the receiver as parameter 0 (offset 1).
    fn calleeFunctionForInference(
        self: *TypeChecker,
        scope: *Scope,
        callee: *ast.Expression,
    ) Error!?struct { params: ast.TypeExpr.FunctionType.Parameters, offset: usize } {
        // A member access `object.method`: either a module member (no receiver) or
        // a UFCS method (receiver fills parameter 0).
        const member: ?struct { object: *ast.Expression, name: []const u8 } = switch (callee.*) {
            .member => |*m| .{ .object = m.object, .name = m.member.name },
            .binary => |*b| if (b.op == .member and b.right.* == .identifier)
                .{ .object = b.left, .name = b.right.identifier.name }
            else
                null,
            else => null,
        };

        if (member) |ma| {
            const object_is_module = if (try self.resolveExprType(scope, ma.object)) |obj_raw|
                self.unaliasType(obj_raw).* == .module
            else
                false;
            if (!object_is_module) {
                // UFCS: the bare method name resolves to a free function whose
                // first parameter is the receiver.
                if (scope.lookup(ma.name)) |binding| {
                    if (binding.type_expr) |bt| {
                        const t = self.unaliasType(bt);
                        if (t.* == .function) return .{ .params = t.function.params, .offset = 1 };
                    }
                }
                return null;
            }
        }

        const raw = (try self.resolveExprType(scope, callee)) orelse return null;
        const t = self.unaliasType(raw);
        if (t.* != .function) return null;
        return .{ .params = t.function.params, .offset = 0 };
    }

    /// Builds a tuple type from an array literal's elements, typing each position
    /// independently (a permissive type variable when a position's type doesn't
    /// resolve). Used to validate a literal against a *tuple* annotation, where a
    /// homogeneous literal must still be checked position-by-position.
    fn arrayLiteralTupleType(self: *TypeChecker, scope: *Scope, array: ast.ArrayLiteral) Error!*const ast.TypeExpr {
        const elements = try self.arena.allocator().alloc(*const ast.TypeExpr, array.elements.len);
        for (array.elements, elements) |el, *dst| {
            const raw = (try self.resolveExprType(scope, el)) orelse
                try self.allocTypeExpression(.{ .type_var = .{ .name = "_", .span = el.span() } });
            dst.* = try self.resolveTypeExpr(scope, raw);
        }
        return try self.allocTypeExpression(.{ .tuple = .{ .elements = elements, .span = array.span } });
    }

    fn runBindingDecl(
        self: *TypeChecker,
        scope: *Scope,
        binding_decl: *ast.BindingDecl,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, binding_decl.span);

        // A comptime type-function call (`const IntBox = Box Int`) evaluates to a
        // concrete type at compile time; bind the name as that type, so a later
        // `IntBox{ … }` construction resolves it like any other type binding.
        if (try self.evalComptimeTypeCall(scope, binding_decl.initializer)) |type_result| {
            try self.runTypeExpression(scope, type_result);
            try self.runBindingPattern(scope, binding_decl.pattern, type_result, binding_decl.is_pub, binding_decl.is_mutable);
            return;
        }

        // Inferred struct literal: `const v: Vector = .{ .x = 3 }` parses as an
        // anonymous struct literal (empty name); take its type from the binding's
        // annotation by stamping the annotation's type name onto the literal, so
        // every downstream stage treats it exactly like `Vector{ .x = 3 }`.
        if (binding_decl.annotation != null and exprMayNeedStructInference(binding_decl.initializer)) {
            self.stampInferredLiteral(binding_decl.initializer, binding_decl.annotation);
        }

        // The annotation is the initializer's expected type — used to pick a
        // return-type-only overload (`const b: []Int = pure 42`). Pushed raw and
        // resolved lazily only when an overload actually needs it, so a normal
        // binding never re-resolves (and never re-reports) its annotation here.
        try self.expected_type_stack.append(self.arena.allocator(), binding_decl.annotation);
        defer _ = self.expected_type_stack.pop();

        try self.runExpression(scope, binding_decl.initializer);

        // Resolve the initializer's type so a raw type (e.g. a function call's
        // return with unresolved member identifiers) compares against the
        // annotation.
        const initializer_type = if (try self.resolveExprType(scope, binding_decl.initializer)) |raw|
            try self.resolveTypeExpr(scope, raw)
        else
            null;

        // A `|T|`-carrying annotation is a capture pattern: bind its names to the
        // initializer's concrete type, and take that concrete type as the
        // binding's type (rather than the permissive type-variable resolution).
        const binding_annotation_type_expr = brk: {
            const annotation = binding_decl.annotation orelse break :brk null;
            // Bind whatever captures structurally match the initializer, then
            // resolve the annotation: bound captures become their concrete type,
            // unmatched ones (e.g. `?|T| = null`) stay permissive type variables.
            if (typeExprHasCapture(annotation)) {
                if (initializer_type) |it| {
                    try self.bindTypeCaptures(scope, annotation, it);
                } else {
                    // No concrete initializer type to match against (e.g. an
                    // untyped array literal, or `?|T| = null`): declare each
                    // captured name as a permissive type variable so a later bare
                    // reference to it still resolves.
                    var captures: std.StringHashMapUnmanaged(void) = .empty;
                    defer captures.deinit(self.arena.allocator());
                    try self.collectTypeVars(scope, annotation, &captures);
                    var it = captures.keyIterator();
                    while (it.next()) |name| {
                        if (scope.lookup(name.*) != null) continue;
                        const marker = try self.allocTypeExpression(.{ .type_var = .{ .name = name.*, .span = annotation.span() } });
                        try scope.declareType(self.arena.allocator(), ast.Identifier.global(name.*), marker, false);
                    }
                }
            }
            break :brk try self.resolveTypeExpr(scope, annotation);
        };

        // Against a *tuple* annotation, type a `.{ … }` literal position-by-
        // position (even a homogeneous one, which otherwise resolves permissively)
        // so a per-position mismatch — `const t: (Int, String) = .{ 1, 2 }` — is
        // caught.
        const effective_initializer_type = brk: {
            const annotation_type = binding_annotation_type_expr orelse break :brk initializer_type;
            if (self.unaliasType(annotation_type).* == .tuple and binding_decl.initializer.* == .array) {
                break :brk try self.arrayLiteralTupleType(scope, binding_decl.initializer.array);
            }
            break :brk initializer_type;
        };

        const type_expr = binding_annotation_type_expr orelse effective_initializer_type;

        if (binding_annotation_type_expr) |annotation_type| {
            if (effective_initializer_type) |init_type| {
                try self.validateTypeAssignment(
                    annotation_type,
                    init_type,
                    .{ .span = binding_decl.initializer.span() },
                );
            }
        }

        if (type_expr) |t| try self.runTypeExpression(scope, t);

        // A tuple pattern destructuring an array *literal* (`const a, b =
        // .{ e0, e1 }`) types each element binding from its own expression, so a
        // heterogeneous literal keeps per-element types (which the homogenized
        // array element type would lose).
        if (binding_decl.pattern.* == .tuple and binding_decl.initializer.* == .array) {
            const tuple = binding_decl.pattern.tuple;
            const elems = binding_decl.initializer.array.elements;
            if (tuple.elements.len == elems.len) {
                for (tuple.elements, elems) |el_pat, el_expr| {
                    const el_type = if (try self.resolveExprType(scope, el_expr)) |raw| try self.resolveTypeExpr(scope, raw) else null;
                    try self.runBindingPattern(scope, el_pat, el_type, binding_decl.is_pub, binding_decl.is_mutable);
                }
                return;
            }
        }

        try self.runBindingPattern(
            scope,
            binding_decl.pattern,
            type_expr,
            binding_decl.is_pub,
            binding_decl.is_mutable,
        );
    }

    pub fn resolveTypeExpr(
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
            // Resolve each position of a tuple type so named element types
            // (`(Vector, Int)`) normalize the same way an array element does.
            .tuple => |tuple| blk: {
                const elements = try self.arena.allocator().alloc(*const ast.TypeExpr, tuple.elements.len);
                for (tuple.elements, elements) |el, *dst| dst.* = try self.resolveTypeExpr(scope, el);
                break :blk try self.allocTypeExpression(.{ .tuple = .{ .elements = elements, .span = tuple.span } });
            },
            .type_merge => |merge| try self.resolveTypeMerge(scope, merge),
            // Resolve a function type's parts so any type variables in a generic
            // signature are baked into the stored type (as `.type_var`), rather
            // than left as raw identifiers that a call site would fail to resolve.
            .function => |function| blk: {
                const params: ast.TypeExpr.FunctionType.Parameters = switch (function.params) {
                    ._non_variadic => |ps| pblk: {
                        const out = try self.arena.allocator().alloc(?*const ast.TypeExpr, ps.len);
                        for (ps, out) |p, *dst| dst.* = if (p) |pt| try self.resolveTypeExpr(scope, pt) else null;
                        break :pblk .{ ._non_variadic = out };
                    },
                    ._variadic => |p| .{ ._variadic = if (p) |pt| try self.resolveTypeExpr(scope, pt) else null },
                };
                break :blk try self.allocTypeExpression(.{ .function = .{
                    .params = params,
                    .stdin_type = if (function.stdin_type) |st| try self.resolveTypeExpr(scope, st) else null,
                    .return_type = if (function.return_type) |rt| try self.resolveTypeExpr(scope, rt) else null,
                    .span = function.span,
                } });
            },
            // Resolve each member so a sum that arrives with raw member
            // identifiers (e.g. a function call's return type) compares correctly.
            .sum => |sum| blk: {
                const members = try self.arena.allocator().alloc(*const ast.TypeExpr, sum.members.len);
                for (sum.members, members) |src, *dst| dst.* = try self.resolveTypeExpr(scope, src);
                break :blk try self.allocTypeExpression(.{ .sum = .{ .members = members, .span = sum.span } });
            },
            // A `|T|` capture in a generic context (e.g. a function signature) is
            // a permissive type variable, resolved like an implicit uppercase
            // generic. Binding positions bind it to a concrete type separately
            // (see `bindTypeCaptures`), which shadows this.
            .type_capture => |capture| blk: {
                if (scope.lookup(capture.name)) |binding| {
                    if (binding.type_expr) |bound| break :blk try self.resolveTypeExpr(scope, bound);
                }
                break :blk try self.allocTypeExpression(.{ .type_var = .{ .name = capture.name, .span = capture.span } });
            },
            // `Box(Int)` — substitute the args into the constructor's body.
            .type_application => |app| try self.resolveTypeApplication(scope, app),
            // Resolve struct field types that carry a capture or a generic
            // application (`entries: []Entry(K, V)`), so they compare as their
            // substituted structs. Plain named-type fields are left alone (to
            // avoid expanding — and possibly recursing into — ordinary structs).
            .struct_type => |st| blk: {
                var any = false;
                for (st.fields) |field| {
                    if (typeExprHasCapture(field.type_expr) or typeExprHasApplication(field.type_expr)) any = true;
                }
                if (!any) break :blk type_expr;
                const new_fields = try self.arena.allocator().alloc(ast.TypeExpr.StructField, st.fields.len);
                for (st.fields, new_fields) |field, *dst| {
                    dst.* = field;
                    if (typeExprHasCapture(field.type_expr) or typeExprHasApplication(field.type_expr)) {
                        dst.type_expr = try self.resolveTypeExpr(scope, field.type_expr);
                    }
                }
                var new_st = st;
                new_st.fields = new_fields;
                break :blk try self.allocTypeExpression(.{ .struct_type = new_st });
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

    /// Walks a signature type expression and records each uppercase identifier
    /// that isn't a declared type in `outer` — a candidate implicit type variable.
    fn collectTypeVars(
        self: *TypeChecker,
        outer: *Scope,
        type_expr: *const ast.TypeExpr,
        out: *std.StringHashMapUnmanaged(void),
    ) Error!void {
        switch (type_expr.*) {
            // A generic type parameter is introduced only by an explicit `|T|`
            // capture. A bare uppercase identifier is NOT collected — it must
            // resolve to an already-introduced type variable (from a `|T|`
            // capture elsewhere in the signature) or a declared type; otherwise
            // it is an undeclared-type error (a typo), not a silent generic.
            .type_capture => |capture| try out.put(self.arena.allocator(), capture.name, {}),
            .type_application => |app| {
                // A higher-kinded application `|M|(A)` introduces the constructor
                // capture `M` as a type variable too.
                if (app.ctor_is_capture) try out.put(self.arena.allocator(), app.name.name, {});
                for (app.args) |arg| try self.collectTypeVars(outer, arg, out);
            },
            .array => |a| try self.collectTypeVars(outer, a.element, out),
            .optional => |o| try self.collectTypeVars(outer, o.child, out),
            .promise => |p| try self.collectTypeVars(outer, p.child, out),
            .sum => |s| for (s.members) |m| try self.collectTypeVars(outer, m, out),
            .type_merge => |m| {
                try self.collectTypeVars(outer, m.lhs, out);
                try self.collectTypeVars(outer, m.rhs, out);
            },
            .function => |f| {
                if (f.stdin_type) |st| try self.collectTypeVars(outer, st, out);
                if (f.return_type) |rt| try self.collectTypeVars(outer, rt, out);
                switch (f.params) {
                    ._non_variadic => |ps| for (ps) |p| {
                        if (p) |pt| try self.collectTypeVars(outer, pt, out);
                    },
                    ._variadic => |p| if (p) |pt| try self.collectTypeVars(outer, pt, out),
                }
            },
            else => {},
        }
    }

    fn resolveTypeIdentifierToAlias(
        self: *TypeChecker,
        scope: *Scope,
        identifier: *const ast.TypeExpr.NamedType,
    ) Error!*const ast.TypeExpr {
        // A `std.ffi` C type written qualified (`c.Char`, `c.Double`): resolve
        // it to an alias whose NAME is the C type (recovered later for
        // marshalling) and whose underlying type is the Runic type it checks
        // as. This makes `c.X` usable anywhere a type is expected — struct
        // fields, parameters, returns — not just an extern signature.
        if (identifier.path.segments.len == 2 and
            CType.fromName(identifier.path.segments[1].name) != null and
            scope.lookup(identifier.path.segments[0].name) != null)
        {
            const cname = identifier.path.segments[1].name;
            if (try self.cTypeToRunic(cname)) |runic| {
                return try self.allocTypeExpression(.{ .alias = .{
                    .name = cname,
                    .span = identifier.span,
                    .type_expr = runic,
                } });
            }
        }

        // A module-qualified type (`m.Vector3`): resolve the member through the
        // module's scope so it becomes the actual type (a struct, …), not the
        // module `m`. Types can't be `pub`, so visibility is not required.
        if (identifier.path.segments.len == 2) {
            if (scope.lookup(identifier.path.segments[0].name)) |mod_binding| {
                if (mod_binding.type_expr) |mod_type_expr| {
                    const mod_type = self.unaliasType(mod_type_expr);
                    if (mod_type.* == .module) {
                        if (try self.requestModuleScope(mod_type.module)) |module_scope| {
                            if (module_scope.lookup(identifier.path.segments[1].name)) |member| {
                                if (member.type_expr) |member_type| {
                                    return try self.allocTypeExpression(.{
                                        .alias = .{
                                            .name = identifier.path.segments[1].name,
                                            .span = identifier.span,
                                            // Resolve the member's struct fields in the
                                            // *module's* scope, where its own imports
                                            // (`c = import "std/ffi"`) are bound — so a
                                            // later field access or construction in the
                                            // importer's scope doesn't re-resolve a
                                            // `c.Float` field type where `c` is unknown.
                                            .type_expr = try self.resolveModuleMemberFields(module_scope, member_type),
                                        },
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        const name = identifier.path.segments[0].name;

        const binding = scope.lookup(name) orelse {
            // An unresolved *uppercase* type name is an implicit generic type
            // variable (e.g. `T`/`U` in a generic signature, or that signature
            // re-resolved at a call site outside the function scope). Since the
            // runtime is dynamic, it is permissive — resolve it to a type_var
            // rather than reporting "not declared". (A lowercase unknown name is
            // still an error.)
            if (identifier.path.segments[0].isTypeIdentifier()) {
                try self.reportSpanError(
                    identifier.span,
                    Error.IdentifierNotFound,
                    .@"error",
                    "type '{s}' is not declared (to introduce a generic type parameter, write it as |{s}|)",
                    .{ name, name },
                );
            } else {
                try self.reportSpanError(
                    identifier.span,
                    Error.IdentifierNotFound,
                    .@"error",
                    "type {s} not declared",
                    .{name},
                );
            }

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

    /// When a module-qualified type (`m.Rectangle`) resolves to a struct, resolve
    /// its field types in the *module's* scope. A field type such as `c.Float`
    /// refers to the module's own `const c = import "std/ffi"`, which is not in
    /// the importer's scope — so without this, a later field access
    /// (`rect.width`) or construction (`m.Rectangle{ … }`) would try to resolve
    /// `c.Float` where `c` is undeclared ("type c not declared"). Resolves one
    /// level (each field to an alias); a nested struct field stays a bare alias,
    /// which is resolved the same way when it is itself referenced qualified.
    /// Non-struct members are returned unchanged.
    fn resolveModuleMemberFields(
        self: *TypeChecker,
        module_scope: *Scope,
        member_type: *const ast.TypeExpr,
    ) Error!*const ast.TypeExpr {
        const unaliased = self.unaliasType(member_type);
        if (unaliased.* != .struct_type) return member_type;
        const st = unaliased.struct_type;
        const new_fields = try self.arena.allocator().alloc(ast.TypeExpr.StructField, st.fields.len);
        for (st.fields, new_fields) |field, *dst| {
            dst.* = field;
            dst.type_expr = try self.resolveTypeExpr(module_scope, field.type_expr);
        }
        var new_st = st;
        new_st.fields = new_fields;
        return try self.allocTypeExpression(.{ .struct_type = new_st });
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
            .tuple => |tuple| {
                // Positional destructuring: the initializer is an array/tuple.
                // A tuple source gives each binding its own *per-position* type
                // (so a heterogeneous collection keeps element types through a
                // variable); an array source gives every binding the single
                // element type.
                const resolved = if (type_expr) |t| self.unaliasType(t) else null;
                for (tuple.elements, 0..) |el, i| {
                    const el_type: ?*const ast.TypeExpr = if (resolved) |r| switch (r.*) {
                        .tuple => |src| if (i < src.elements.len) src.elements[i] else null,
                        .array => |a| a.element,
                        else => null,
                    } else null;
                    try self.runBindingPattern(scope, el, el_type, is_pub, is_mutable);
                }
            },
            .record => |record| {
                // Field destructuring: the initializer is a struct; each field
                // binds that struct member (to the label, or to the explicit
                // rebinding after `:`).
                const resolved = if (type_expr) |t| self.unaliasType(t) else null;
                for (record.fields) |field| {
                    const field_type: ?*const ast.TypeExpr = if (resolved) |r| switch (r.*) {
                        .struct_type => |st| st.memberType(field.label.name),
                        else => null,
                    } else null;
                    // A named field that the struct doesn't have is an error.
                    if (field_type == null and resolved != null and resolved.?.* == .struct_type) {
                        try self.reportSpanError(field.span, Error.MemberNotFound, .@"error", "struct has no field '{s}'", .{field.label.name});
                    }
                    if (field.binding) |target| {
                        try self.runBindingPattern(scope, target, field_type, is_pub, is_mutable);
                    } else {
                        scope.declare(self.arena.allocator(), field.label, field_type, is_pub, is_mutable) catch |err| try switch (err) {
                            error.IdentifierAlreadyDeclared => self.reportSpanError(field.span, error.IdentifierAlreadyDeclared, .@"error", "identifier {s} already declared", .{field.label.name}),
                            else => err,
                        };
                    }
                }
            },
        }
    }

    /// Declares a function signature's implicit generic type variables in
    /// `fn_scope`. An uppercase type name in the signature that isn't a declared
    /// type is a type variable, scoped to this function (e.g. `T`/`U` in
    /// `fn map(xs: []T, f: fn(T) U) []U`). Declaring them first lets the signature
    /// resolve (no "type T not declared") and — resolved *in the function scope* —
    /// bakes them into the stored function type as `.type_var`, so a call site
    /// never re-resolves a raw `T`/`U`.
    /// The bound name of a `comptime T: type` parameter — one that carries a
    /// type value, usable as a type variable inside the body (`x: T`, `[]T`) and
    /// compared as a value (`T == Int`). Null for any other parameter (including
    /// a comptime *value* param such as `comptime n: Int`).
    fn comptimeTypeParamName(param: *const ast.Parameter) ?[]const u8 {
        if (!param.is_comptime) return null;
        const annotation = param.type_annotation orelse return null;
        if (annotation.* != .type_type) return null;
        return switch (param.pattern.*) {
            .identifier => |id| id.name,
            else => null,
        };
    }

    /// The type a `yield`ed type-value expression denotes — the body of a
    /// type-returning function. Handles a bare `yield <type>` and a block whose
    /// last `yield` produces a type. Null when the body does not simply yield a
    /// type (a more complex comptime body is not reduced to a constructor).
    fn yieldedType(body: *const ast.Expression) ?*const ast.TypeExpr {
        switch (body.*) {
            .type_value => |tv| return tv.type_expr,
            .block => |block| {
                var found: ?*const ast.TypeExpr = null;
                for (block.statements) |stmt| {
                    if (stmt.* == .yield_stmt and stmt.yield_stmt.value.* == .type_value) {
                        found = stmt.yield_stmt.value.type_value.type_expr;
                    }
                }
                return found;
            },
            else => return null,
        }
    }

    /// If `fn_decl` is a comptime type function — returns `type`, has one or more
    /// `comptime T: type` params, and `yield`s a type — the generic-type
    /// constructor it is equivalent to (params + yielded body). Null otherwise.
    fn comptimeTypeCtor(self: *TypeChecker, fn_decl: *const ast.FunctionDecl) Error!?GenericTypeCtor {
        const return_type = fn_decl.return_type orelse return null;
        if (return_type.* != .type_type) return null;
        const body_type = yieldedType(fn_decl.body) orelse return null;

        var names: std.ArrayListUnmanaged(ast.Identifier) = .empty;
        switch (fn_decl.params) {
            ._non_variadic => |params| for (params) |param| {
                if (comptimeTypeParamName(param)) |name| {
                    try names.append(self.arena.allocator(), .{ .name = name, .span = param.span });
                }
            },
            ._variadic => |param| if (comptimeTypeParamName(param)) |name| {
                try names.append(self.arena.allocator(), .{ .name = name, .span = param.span });
            },
        }
        if (names.items.len == 0) return null;
        // Fold the body's `pub const` declarations into the yielded struct's decls
        // so the produced type carries them as static members (`Maybe(Int).nothing`,
        // and the implicit `.nothing`). Mirrors the IR compiler's
        // `withTypeFnPubDecls`.
        const body = try self.foldTypeFnPubDecls(body_type, fn_decl.body);
        return .{ .params = try names.toOwnedSlice(self.arena.allocator()), .body = body };
    }

    /// Returns `body` with every `pub const` in a type function's body block
    /// appended to its struct `decls`. A non-struct or bare body is unchanged.
    fn foldTypeFnPubDecls(self: *TypeChecker, body: *const ast.TypeExpr, fn_body: *const ast.Expression) Error!*const ast.TypeExpr {
        if (body.* != .struct_type) return body;
        if (fn_body.* != .block) return body;
        var decls: std.ArrayListUnmanaged(ast.TypeExpr.StructDecl) = .empty;
        try decls.appendSlice(self.arena.allocator(), body.struct_type.decls);
        for (fn_body.block.statements) |stmt| {
            switch (stmt.*) {
                .binding_decl => |*bd| {
                    if (!bd.is_pub) continue;
                    if (bd.pattern.* != .identifier) continue;
                    try decls.append(self.arena.allocator(), .{
                        .name = bd.pattern.identifier,
                        .type_expr = bd.annotation,
                        .decl_source = .{ .binding_decl = bd },
                        .span = bd.span,
                    });
                },
                else => {},
            }
        }
        if (decls.items.len == body.struct_type.decls.len) return body;
        var st = body.struct_type;
        st.decls = try decls.toOwnedSlice(self.arena.allocator());
        return self.allocTypeExpression(.{ .struct_type = st });
    }

    /// The type a comptime call argument denotes (`Int` → the type Int, a nested
    /// `Box Int` → its result type). Null when the argument is not a type.
    fn argToType(self: *TypeChecker, scope: *Scope, arg: *const ast.Expression) Error!?*const ast.TypeExpr {
        switch (arg.*) {
            .type_value => |tv| return tv.type_expr,
            .identifier => |id| {
                const segments = try self.arena.allocator().alloc(ast.Identifier, 1);
                segments[0] = id;
                const named = try self.allocTypeExpression(.{ .identifier = .{
                    .path = .{ .segments = segments, .span = id.span },
                    .span = id.span,
                } });
                return try self.resolveTypeExpr(scope, named);
            },
            .call => |call| {
                // A bare type name (`Int`) parses as a zero-argument call; unwrap
                // it to its callee. A call with arguments is a nested comptime type
                // call (`Box (Box Int)`).
                if (call.arguments.len == 0) return try self.argToType(scope, call.callee);
                return try self.evalComptimeTypeCall(scope, arg);
            },
            else => return null,
        }
    }

    /// Evaluates a call to a comptime type function (`Box Int`) to the concrete
    /// type it produces, substituting the type arguments into the yielded body.
    /// Null when `expr` is not such a call.
    fn evalComptimeTypeCall(self: *TypeChecker, scope: *Scope, expr: *const ast.Expression) Error!?*const ast.TypeExpr {
        if (expr.* != .call) return null;
        const call = expr.call;
        if (call.callee.* != .identifier) return null;
        const ctor = self.comptime_type_fns.get(call.callee.identifier.name) orelse {
            return null;
        };
        if (call.arguments.len != ctor.params.len) {
            return null;
        }
        const args = try self.arena.allocator().alloc(*const ast.TypeExpr, call.arguments.len);
        for (call.arguments, args) |arg, *dst| {
            dst.* = (try self.argToType(scope, arg)) orelse return null;
        }
        return try self.substituteTypeParams(ctor.body, ctor.params, args);
    }

    fn declareSignatureTypeVars(
        self: *TypeChecker,
        scope: *Scope,
        fn_scope: *Scope,
        fn_decl: *ast.FunctionDecl,
    ) Error!void {
        var type_vars: std.StringHashMapUnmanaged(void) = .empty;
        defer type_vars.deinit(self.arena.allocator());
        if (fn_decl.stdin_type) |st| try self.collectTypeVars(scope, st, &type_vars);
        switch (fn_decl.params) {
            ._non_variadic => |params| for (params) |param| {
                // A `comptime T: type` param introduces `T` as a type variable, so
                // a later param annotation (`x: T`) and the return type resolve it.
                if (comptimeTypeParamName(param)) |name| try type_vars.put(self.arena.allocator(), name, {});
                if (param.type_annotation) |ta| try self.collectTypeVars(scope, ta, &type_vars);
            },
            ._variadic => |param| {
                if (comptimeTypeParamName(param)) |name| try type_vars.put(self.arena.allocator(), name, {});
                if (param.type_annotation) |ta| try self.collectTypeVars(scope, ta, &type_vars);
            },
        }
        if (fn_decl.return_type) |rt| try self.collectTypeVars(scope, rt, &type_vars);
        var it = type_vars.keyIterator();
        while (it.next()) |name| {
            const marker = try self.allocTypeExpression(.{ .type_var = .{ .name = name.*, .span = fn_decl.span } });
            try fn_scope.declare(self.arena.allocator(), .{ .name = name.*, .span = fn_decl.span }, marker, false, false);
        }
    }

    /// Declares a named function's resolved `.function` signature type in `scope`,
    /// so a forward reference or mutual recursion resolves to the function rather
    /// than an unknown external command. Called by `runBlock` as a hoist pre-pass
    /// before any statement body is checked. Idempotent: a no-op when the name is
    /// already declared in this scope (a re-run, or a later `runFnDecl`). Builds a
    /// throwaway function scope only to bake in any signature type variables; the
    /// body is checked later by `runFnDecl` in its own scope.
    /// Pass 0 of top-level checking: any function name declared more than once is
    /// an overload set. Each such declaration is renamed to a unique mangled name
    /// (`name#0`, `name#1`, …) — mutating the AST, which the IR compiler then sees
    /// as distinct functions — and recorded so a call to the original name can be
    /// resolved to one candidate (`resolveOverloadedCall`). The `#` separator can
    /// never appear in a source identifier, so a mangled name cannot collide.
    fn collectOverloads(self: *TypeChecker, block: *ast.Block) Error!void {
        const a = self.arena.allocator();
        var counts: std.StringHashMapUnmanaged(usize) = .empty;
        defer counts.deinit(a);
        for (block.statements) |statement| {
            const fn_decl = fnDeclStatement(statement) orelse continue;
            const name = (fn_decl.name orelse continue).name;
            const gop = try counts.getOrPut(a, name);
            gop.value_ptr.* = (if (gop.found_existing) gop.value_ptr.* else 0) + 1;
        }

        var indices: std.StringHashMapUnmanaged(usize) = .empty;
        defer indices.deinit(a);
        for (block.statements) |statement| {
            const fn_decl = fnDeclStatement(statement) orelse continue;
            const original = (fn_decl.name orelse continue).name;
            if ((counts.get(original) orelse 0) < 2) continue;

            const idx_gop = try indices.getOrPut(a, original);
            const idx = if (idx_gop.found_existing) idx_gop.value_ptr.* else 0;
            idx_gop.value_ptr.* = idx + 1;
            const mangled = try std.fmt.allocPrint(a, "{s}#{d}", .{ original, idx });

            const set_gop = try self.overload_sets.getOrPut(a, original);
            if (!set_gop.found_existing) set_gop.value_ptr.* = .empty;
            try set_gop.value_ptr.append(a, .{ .mangled = mangled, .decl = fn_decl });

            fn_decl.name.?.name = mangled;
        }
    }

    fn currentExpectedType(self: *TypeChecker) ?*const ast.TypeExpr {
        if (self.expected_type_stack.items.len == 0) return null;
        return self.expected_type_stack.items[self.expected_type_stack.items.len - 1];
    }

    /// The concrete struct that the current expected type resolves to when it is an
    /// application of `ctor_name` (`const p: Partial(Point) = …`) that materialized
    /// into real fields — the layout to validate a `Ctor{ … }` literal against, so
    /// construction catches field typos. Null when there's no such expected type,
    /// it names a different constructor, or the recipe stayed permissive (a dynamic
    /// field name), in which case the caller keeps the permissive ctor-body check.
    fn expectedMaterializedStruct(
        self: *TypeChecker,
        scope: *Scope,
        ctor_name: []const u8,
    ) Error!?ast.TypeExpr.StructType {
        const expected = self.currentExpectedType() orelse return null;
        // Trust the expected type as the layout to validate `Ctor{…}` against when
        // it is an application of the *same* constructor (a `Partial(…)` annotation)
        // — never an unrelated `Point(…)`. A parameter type comes in already
        // resolved to its struct layout (the constructor name is gone); that is the
        // declared type this argument must match, so it is trusted directly.
        if (expected.* == .type_application and
            !std.mem.eql(u8, expected.type_application.name.name, ctor_name)) return null;
        const resolved = self.unaliasType(try self.resolveTypeExpr(scope, expected));
        if (resolved.* != .struct_type) return null;
        if (resolved.struct_type.body_items.len > 0) return null; // stayed permissive
        return resolved.struct_type;
    }

    /// Permissive type match for overload selection: a generic parameter/return
    /// (type variable or capture) accepts anything; otherwise the types must be
    /// equal or coerce (a value widening into an optional, either direction — the
    /// candidate's declared shape is what matters, not the yield direction).
    fn overloadTypeMatches(self: *TypeChecker, a_type: *const ast.TypeExpr, b_type: *const ast.TypeExpr) bool {
        const a = self.unaliasType(a_type);
        const b = self.unaliasType(b_type);
        if (a.* == .type_var or a.* == .type_capture) return true;
        if (b.* == .type_var or b.* == .type_capture) return true;
        return self.pipeTypesEqual(a, b) or self.yieldCoercesToType(a, b) or self.yieldCoercesToType(b, a);
    }

    /// Whether an overload candidate can accept the given call arguments: same
    /// arity, and each argument's type matches the parameter's (a captured or
    /// untyped parameter accepts anything; an argument whose type can't be
    /// resolved here is treated permissively).
    fn overloadAcceptsArgs(self: *TypeChecker, scope: *Scope, decl: *ast.FunctionDecl, args: []const *ast.Expression) bool {
        const params = switch (decl.params) {
            ._non_variadic => |ps| ps,
            ._variadic => return true,
        };
        if (params.len != args.len) return false;
        // Resolve each parameter annotation in the candidate's *own* signature
        // scope, so a bare type-variable reference (`ma: []A`, where `A` is
        // introduced by another parameter's `|A|`) resolves to a permissive type
        // variable instead of being reported "not declared" — probing a candidate
        // must not emit errors. Arguments stay in the caller's `scope`.
        const fn_scope = self.arena.allocator().create(Scope) catch return true;
        fn_scope.* = .initWithParent(scope, decl.span);
        self.declareSignatureTypeVars(scope, fn_scope, decl) catch {};
        for (params, args) |param, arg| {
            const ann = param.type_annotation orelse continue;
            // A higher-kinded parameter `|M|(…)` binds `M` to the argument's type
            // *constructor* — which is always a named type (a struct / generic
            // constructor application), never the built-in array or a primitive.
            // So it accepts a constructor-like argument and rejects the rest; this
            // is what lets a `map(…, |M|(A))` monad overload and a `map(…, []A)`
            // list overload dispatch on the same call.
            if (ann.* == .type_application and ann.type_application.ctor_is_capture) {
                // An array literal (`.{ 1, 2, 3 }`) is array-like, never a named
                // constructor — reject it outright. A homogeneous one has no
                // resolved type here (it flows permissively), so the type check
                // below can't catch it.
                if (arg.* == .array) return false;
                const arg_raw = (self.resolveExprType(scope, arg) catch continue) orelse continue;
                const arg_type = self.resolveTypeExpr(scope, arg_raw) catch arg_raw;
                if (!self.typeIsConstructorLike(arg_type)) return false;
                continue;
            }
            if (typeExprHasCapture(ann)) continue;
            const param_type = self.resolveTypeExpr(fn_scope, ann) catch continue;
            if (self.unaliasType(param_type).* == .type_var) continue;
            const arg_raw = (self.resolveExprType(scope, arg) catch continue) orelse continue;
            const arg_type = self.resolveTypeExpr(scope, arg_raw) catch arg_raw;
            if (!self.overloadTypeMatches(arg_type, param_type)) return false;
        }
        return true;
    }

    /// Whether a type has a named constructor `M(…)` can bind to — a struct or a
    /// generic-constructor application. An array, optional, primitive, or function
    /// has no such constructor, so a higher-kinded `|M|(…)` parameter rejects it.
    fn typeIsConstructorLike(self: *TypeChecker, t: *const ast.TypeExpr) bool {
        return switch (self.unaliasType(t).*) {
            .struct_type, .type_application => true,
            else => false,
        };
    }

    /// Whether an overload candidate's declared return type matches `expected`.
    /// The return type is resolved in a signature scope so its own type variables
    /// (`Maybe(A)`) resolve.
    fn overloadReturnMatches(self: *TypeChecker, scope: *Scope, decl: *ast.FunctionDecl, expected_raw: *const ast.TypeExpr) bool {
        const fn_scope = self.arena.allocator().create(Scope) catch return false;
        fn_scope.* = .initWithParent(scope, decl.span);
        self.declareSignatureTypeVars(scope, fn_scope, decl) catch {};
        // A higher-kinded expected type `M(B)` — the captured constructor `M`
        // applied — resolves to a bare type variable, against which *every*
        // candidate matches permissively. But a captured constructor can only ever
        // be a named type constructor, so only a candidate that itself returns a
        // named constructor application (`Maybe(A)`, not `[]A`/`Int`) can produce
        // an `M(B)`. Match structurally on that, so the sole constructor-returning
        // overload is selected (`yield pure x` inside a generic `map`); the caller
        // stays ambiguous only if several candidates qualify. `M` is a type
        // variable in the call-site `scope`; the candidate's return constructor is
        // checked in its own signature scope.
        if (self.isHigherKindedApp(scope, expected_raw)) {
            const rt0 = decl.return_type orelse return false;
            return self.returnIsConstructorApp(fn_scope, rt0);
        }
        // Resolve the expected type here (lazily) rather than at the push site, so
        // an ordinary binding never re-resolves its annotation.
        const expected = self.resolveTypeExpr(fn_scope, expected_raw) catch return false;
        const rt = decl.return_type orelse return self.unaliasType(expected).* == .void;
        const resolved_rt = self.resolveTypeExpr(fn_scope, rt) catch return false;
        return self.overloadTypeMatches(resolved_rt, expected);
    }

    /// Whether `t` is a higher-kinded application `M(…)` whose constructor is a
    /// captured/generic type variable — its concrete constructor is unknown in
    /// this generic body (only monomorphization binds it).
    fn isHigherKindedApp(self: *TypeChecker, scope: *Scope, t: *const ast.TypeExpr) bool {
        return t.* == .type_application and
            (t.type_application.ctor_is_capture or self.nameIsTypeVar(scope, t.type_application.name.name));
    }

    /// Whether a declared return type is a *named* constructor application
    /// (`Maybe(A)`), rather than a capture, array, optional, or primitive. Only
    /// such an overload can satisfy a higher-kinded `M(B)`.
    fn returnIsConstructorApp(self: *TypeChecker, scope: *Scope, t: *const ast.TypeExpr) bool {
        return t.* == .type_application and !t.type_application.ctor_is_capture and
            !self.nameIsTypeVar(scope, t.type_application.name.name);
    }

    /// Resolves an overloaded call in place. If the callee names an overload set,
    /// picks the candidate matching the argument types — and, when several match,
    /// the expected return type from context — then rewrites the callee to that
    /// candidate's mangled name. Idempotent (each call node is resolved once).
    /// Reports an error on no match or an unresolved ambiguity.
    fn resolveOverloadedCall(self: *TypeChecker, scope: *Scope, call: *ast.CallExpr) Error!void {
        if (call.callee.* != .identifier) return;
        const set = self.overload_sets.getPtr(call.callee.identifier.name) orelse return;
        if (self.overload_resolved.contains(call)) return;
        try self.overload_resolved.put(self.arena.allocator(), call, {});
        const name = call.callee.identifier.name;

        var matches: std.ArrayListUnmanaged(OverloadEntry) = .empty;
        defer matches.deinit(self.arena.allocator());
        for (set.items) |cand| {
            if (self.overloadAcceptsArgs(scope, cand.decl, call.arguments)) {
                try matches.append(self.arena.allocator(), cand);
            }
        }

        if (matches.items.len == 0) {
            try self.reportSpanError(call.span, Error.UnsupportedExpression, .@"error", "no overload of '{s}' matches the given arguments", .{name});
            return;
        }

        var chosen: ?OverloadEntry = if (matches.items.len == 1) matches.items[0] else null;
        if (chosen == null) {
            if (self.currentExpectedType()) |expected| {
                var count: usize = 0;
                for (matches.items) |cand| {
                    if (self.overloadReturnMatches(scope, cand.decl, expected)) {
                        chosen = cand;
                        count += 1;
                    }
                }
                if (count != 1) chosen = null;
            }
        }

        if (chosen) |c| {
            call.callee.identifier.name = c.mangled;
        } else {
            try self.reportSpanError(call.span, Error.UnsupportedExpression, .@"error", "ambiguous call to overloaded '{s}'; annotate the expected type to select an overload", .{name});
        }
    }

    fn declareFunctionSignature(self: *TypeChecker, scope: *Scope, fn_decl: *ast.FunctionDecl) Error!void {
        const identifier = fn_decl.name orelse return;
        if (scope.bindings.contains(identifier.name)) return;
        // A *detached* scope (parented to `scope` for lookups, but not added to
        // its children) — the body is checked later by `runFnDecl` in its own
        // real child scope, so registering one here would leave an empty duplicate
        // in the scope tree and mislead LSP scope-location lookups.
        const fn_scope = try self.arena.allocator().create(Scope);
        fn_scope.* = .initWithParent(scope, fn_decl.span);
        try self.declareSignatureTypeVars(scope, fn_scope, fn_decl);
        // A type-returning comptime function acts as a generic-type constructor.
        // Register it under `comptime_type_fns` so a `Box Int` value call resolves,
        // and under `generic_type_ctors` so a `Box(Int)` application in a type
        // position (e.g. a struct field `entries: []Entry(K, V)`) also resolves.
        if (try self.comptimeTypeCtor(fn_decl)) |ctor| {
            try self.comptime_type_fns.put(self.arena.allocator(), identifier.name, ctor);
            try self.generic_type_ctors.put(self.arena.allocator(), identifier.name, ctor);
        }

        const raw_fn_type = try self.resolveExprType(fn_scope, fn_decl);
        const resolved_fn_type = if (raw_fn_type) |t| try self.resolveTypeExpr(fn_scope, t) else null;
        try scope.declare(self.arena.allocator(), identifier, resolved_fn_type, fn_decl.is_pub, false);
    }

    /// A function parameter must carry a type annotation (or a default value it
    /// can be inferred from). Report a clean, located diagnostic when it does
    /// not — `Parameter.resolveType` returns null for such a param rather than
    /// aborting the whole checker run.
    fn checkParamAnnotated(self: *TypeChecker, param: *const ast.Parameter) Error!void {
        if (param.type_annotation != null or param.default_value != null) return;
        if (param.pattern.* == .identifier) {
            try self.reportSpanError(
                param.span,
                Error.TypeNotFound,
                .@"error",
                "parameter '{s}' requires a type annotation (e.g. `{s}: Int`)",
                .{ param.pattern.identifier.name, param.pattern.identifier.name },
            );
        } else {
            try self.reportSpanError(
                param.span,
                Error.TypeNotFound,
                .@"error",
                "function parameter requires a type annotation (e.g. `name: Int`)",
                .{},
            );
        }
    }

    fn runFnDecl(self: *TypeChecker, scope: *Scope, fn_decl: *ast.FunctionDecl) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, fn_decl.span);

        const fn_scope = try scope.addChild(self.arena.allocator(), fn_decl.span);
        try self.declareSignatureTypeVars(scope, fn_scope, fn_decl);

        if (fn_decl.name) |identifier| {
            // The signature is normally already declared by `runBlock`'s hoist
            // pre-pass (so forward references and mutual recursion resolve); only
            // declare it here when it wasn't (e.g. a `fn` reached outside a block
            // pre-pass). Resolve the type in the function scope so type variables
            // are baked in, then expose the binding in the outer scope.
            if (!scope.bindings.contains(identifier.name)) {
                const raw_fn_type = try self.resolveExprType(fn_scope, fn_decl);
                const resolved_fn_type = if (raw_fn_type) |t| try self.resolveTypeExpr(fn_scope, t) else null;
                try scope.declare(
                    self.arena.allocator(),
                    identifier,
                    resolved_fn_type,
                    fn_decl.is_pub,
                    false,
                );
            }
        }

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
                // A `comptime T: type` param is already declared as a type variable
                // by `declareSignatureTypeVars`; declaring it again as a value here
                // would shadow that, so `x: T` would no longer resolve as a type.
                if (comptimeTypeParamName(param) != null) continue;
                try self.checkParamAnnotated(param);
                // Resolve the param's declared type (like the stdin/fn types
                // above), so a primitive annotation such as `Int` — parsed as a
                // bare identifier — becomes the resolved primitive rather than an
                // unresolved `.identifier`. Otherwise every use of the param
                // (a struct-literal field, an assignment, …) compares a resolved
                // type against a raw `Int` and spuriously fails.
                const raw = try self.resolveExprType(fn_scope, param);
                const param_type = if (raw) |t| try self.resolveTypeExpr(fn_scope, t) else null;
                try self.runBindingPattern(
                    fn_scope,
                    param.pattern,
                    param_type,
                    false,
                    param.is_mutable,
                );
            },
            ._variadic => |param| {
                if (comptimeTypeParamName(param) == null) {
                    try self.checkParamAnnotated(param);
                    const raw = try self.resolveExprType(fn_scope, param);
                    const param_type = if (raw) |t| try self.resolveTypeExpr(fn_scope, t) else null;
                    try self.runBindingPattern(
                        fn_scope,
                        param.pattern,
                        param_type,
                        false,
                        param.is_mutable,
                    );
                }
            },
        }

        // Make the declared stdout type visible to every `yield` in the body
        // (including yields nested in loops/blocks/matches) via the stack. Each
        // `runYield` validates against the top entry in the scope it runs in.
        // A bare trailing block / anonymous fn declares no return type; adopt the
        // stdout type its parameter context expects (`pending_fn_body_stdout`, set
        // by `runCall`), so a `fn() Bool` body's stray `echo` is the same error a
        // named `fn … Bool` gets. Consume it immediately so nested fns don't inherit.
        const pending_stdout = self.pending_fn_body_stdout;
        self.pending_fn_body_stdout = null;
        const resolved_return: ?*const ast.TypeExpr = if (fn_decl.return_type) |return_type|
            try self.resolveTypeExpr(fn_scope, return_type)
        else if (pending_stdout) |p|
            try self.resolveTypeExpr(fn_scope, p)
        else
            null;
        try self.stdout_type_stack.append(self.arena.allocator(), resolved_return);
        defer _ = self.stdout_type_stack.pop();
        try self.stdout_return_raw_stack.append(self.arena.allocator(), fn_decl.return_type);
        defer _ = self.stdout_return_raw_stack.pop();

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
            .comptime_expr => |comptime_expr| try self.validateFunctionBodyStdin(scope, comptime_expr.operand, enclosing_stdin),
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
            .fn_decl, .identifier, .env_var, .path, .literal, .pipeline_deprecated, .import_expr, .cimport_expr, .executable, .builtin, .fd, .type_value => {},
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
            .type_binding_decl, .bash_block, .break_stmt, .continue_stmt => {},
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
        // A callee that reads no stdin (declared `Void`, like an absent stdin
        // type) imposes no constraint on the enclosing function's stdin — it does
        // not consume the inherited stream. Only a callee that actually reads
        // stdin must match the enclosing stdin type.
        if (self.unaliasType(callee_stdin).* == .void) return;
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
        // Resolve an overloaded callee to a concrete candidate first: the original
        // (overloaded) name is not bound as a value, so running the callee before
        // rewriting it would fail to resolve.
        try self.resolveOverloadedCall(scope, call);
        try self.runExpression(scope, call.callee);

        // Inferred struct-literal arguments: `f entity .{ .x = 3 }` takes each
        // anonymous literal's type from the matching parameter of the callee, so
        // the struct name need not be repeated at the call site. Stamp the name
        // before checking the argument, so it validates like the named form.
        try self.stampInferredStructArgs(scope, call);

        // A recipe-ctor struct-literal argument (`f Partial{ … }`) is validated
        // against the parameter's materialized type: push the parameter type as the
        // expected type so `runStructLiteral` checks the fields (an unknown/missing
        // field, a wrong value type) against the layout the parameter's annotation
        // materializes — the same check a `const p: Partial(…) = Partial{ … }`
        // binding gets. Only recipe-ctor literals opt in, to avoid disturbing
        // return-type-overloaded arguments.
        const arg_callee = try self.calleeFunctionForInference(scope, call.callee);
        for (call.arguments, 0..) |arg, i| {
            var pushed = false;
            if (arg.* == .struct_literal and
                self.generic_type_ctors.contains(arg.struct_literal.name.name))
            {
                if (arg_callee) |cf| {
                    const param_index = i + cf.offset;
                    const param_type: ?*const ast.TypeExpr = switch (cf.params) {
                        ._non_variadic => |list| if (param_index < list.len) list[param_index] else null,
                        ._variadic => |element| element,
                    };
                    if (param_type) |pt| {
                        try self.expected_type_stack.append(self.arena.allocator(), pt);
                        pushed = true;
                    }
                }
            }
            // A trailing block / anonymous fn argument adopts its `fn(…) T`
            // parameter's return type `T` as the body's stdout type (so a `fn() Bool`
            // body's stray `echo` is caught, like a named function's). `runFnDecl`
            // reads and clears `pending_fn_body_stdout` at entry.
            if (arg.* == .fn_decl and arg.fn_decl.return_type == null) {
                if (arg_callee) |cf| {
                    const param_index = i + cf.offset;
                    const param_type: ?*const ast.TypeExpr = switch (cf.params) {
                        ._non_variadic => |list| if (param_index < list.len) list[param_index] else null,
                        ._variadic => |element| element,
                    };
                    if (param_type) |pt| {
                        const unaliased = self.unaliasType(pt);
                        if (unaliased.* == .function) {
                            self.pending_fn_body_stdout = unaliased.function.return_type;
                        }
                    }
                }
            }
            try self.runExpression(scope, arg);
            self.pending_fn_body_stdout = null;
            if (pushed) _ = self.expected_type_stack.pop();
        }

        // A bare command (an identifier callee with no scope binding — `echo`,
        // `ls`, …) serializes each argument to a string, so a whole struct has
        // no valid form. User functions (which have a binding) and module calls
        // (member callees) may legitimately take struct arguments. `@`-builtins
        // (`@field(x)(name)`, `@fields(T)`, …) are not commands and take types /
        // struct values by design, so they are exempt.
        const is_command = call.callee.* == .identifier and
            scope.lookup(call.callee.identifier.name) == null and
            !(call.callee.identifier.name.len > 0 and call.callee.identifier.name[0] == '@');
        if (is_command) {
            for (call.arguments) |arg| {
                // A struct-typed argument, or a generic struct literal (`Box{ … }`)
                // whose type doesn't resolve standalone — both would fail while
                // being serialized to a command string. A type identifier (which
                // serializes to its name) is fine.
                const is_struct = blk: {
                    if (self.isTypeIdentifierExpr(scope, arg)) break :blk false;
                    if (arg.* == .struct_literal and self.generic_type_ctors.contains(arg.struct_literal.name.name)) {
                        break :blk true;
                    }
                    if (try self.resolveExprType(scope, arg)) |t| {
                        const unaliased = self.unaliasType(t);
                        break :blk unaliased.* == .struct_type and !isExecutionLikeStruct(unaliased.struct_type);
                    }
                    break :blk false;
                };
                if (is_struct) {
                    try self.reportSpanError(
                        arg.span(),
                        Error.TypeMismatch,
                        .@"error",
                        "cannot pass a whole struct as a command argument; pass a field instead (e.g. value.field)",
                        .{},
                    );
                }
            }
        }
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

        try self.checkBareStatementStdout(scope, expr_stmt.expression);

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

    /// A bare statement that produces stdout output writes it to the enclosing
    /// function's stdout (`&1`). Its output type must match the function's stdout
    /// type; when it does not, the output corrupts the yielded value (and, on the
    /// capture path, deadlocks). Two producers are checked:
    ///
    ///   - a command (`echo "x"`, `ls …`) — an identifier callee with no scope
    ///     binding — writes text, i.e. `String`;
    ///   - a bare call to a user function (`helper`, `g x`) forwards that
    ///     function's own stdout (its declared return type).
    ///
    /// A byte-channel stdout (`Void` passthrough, `String`, `Byte`) accepts any
    /// output, so it never mismatches. A command's result that is *bound*
    /// (`const x = echo …`) is a binding, not an expression statement, so it never
    /// reaches here; one that redirects stdout elsewhere (`>&2`, `> file`) is
    /// skipped, as is a call to a `Void` function (it produces no stdout).
    fn checkBareStatementStdout(self: *TypeChecker, scope: *Scope, expr: *ast.Expression) Error!void {
        if (self.stdout_type_stack.items.len == 0) return; // top level: no stdout type
        const declared = self.stdout_type_stack.items[self.stdout_type_stack.items.len - 1] orelse return;
        // A byte-channel stdout accepts any output — no mismatch is possible.
        // A `Void` stdout means the function produces *nothing* on `&1`: unlike a
        // byte channel (`String`/`Byte`/a command `execution`), it does not accept a
        // bare command's or `echo`'s output. (Run the command for effect by binding
        // its result — `const _ = mkdir …` — redirecting it, or `@log`; a function
        // that genuinely produces text declares a `String` stdout.) This guarantee —
        // a `Void` function is silent — is what makes a bare `Void` call safe inside
        // a value-returning body (no stray bytes corrupt the captured value).
        if (self.stdoutAcceptsCommandBytes(declared) and self.unaliasType(declared).* != .void) return;
        if (expr.* != .call) return;
        const call = expr.call;
        if (call.callee.* != .identifier) return; // UFCS / pipelines: not handled here
        const name = call.callee.identifier.name;
        // Builtins that don't write to `&1`: `@…` builtins (`@log` goes to the real
        // stdout by design), and `cd`/`setenv`, which change process state and
        // produce no output — so they are allowed in a `Void` function.
        if (name.len > 0 and name[0] == '@') return;
        if (std.mem.eql(u8, name, "cd") or std.mem.eql(u8, name, "setenv")) return;
        if (call.redirects.len != 0) return; // stdout may be redirected away — skip

        // The statement's stdout output type: `String` for a command (no binding),
        // else the called function's declared return type.
        const produced: *const ast.TypeExpr = if (scope.lookup(name)) |binding| blk: {
            const t = binding.type_expr orelse return;
            if (self.unaliasType(t).* != .function) return; // a value, not a call
            const rt = self.unaliasType(t).function.return_type orelse return;
            // A `Void` function produces no stdout output — nothing to mismatch.
            if (self.unaliasType(rt).* == .void) return;
            break :blk rt;
        } else try self.allocStringType();

        // Byte output (a command's `String`, or a `String`/`Byte`-returning call)
        // into a stdout that is *not* a byte channel (we skipped those above) is a
        // mismatch outright — never run it through the permissive unifier, which
        // would wrongly match `String` (`[]Byte`) against a generic `[]T` because
        // the element `T` is a type variable. For any other produced type, unify
        // permissively so a type variable/capture never false-positives in a
        // generic body; mismatch only on a concrete clash.
        if (!self.stdoutAcceptsCommandBytes(produced) and self.overloadTypeMatches(produced, declared)) return;

        var pw = std.Io.Writer.Allocating.init(self.arena.allocator());
        defer pw.deinit();
        produced.format(&pw.writer) catch {};
        var dw = std.Io.Writer.Allocating.init(self.arena.allocator());
        defer dw.deinit();
        declared.format(&dw.writer) catch {};
        try self.reportSpanError(
            expr.span(),
            Error.TypeMismatch,
            .@"error",
            "'{s}' writes '{s}' to stdout, but this function's stdout type is '{s}'; bind the result (const x = …), redirect it (>&2 or > \"file\"), or use @log for debug output",
            .{ name, pw.written(), dw.written() },
        );
    }

    /// Whether a function stdout type is a raw byte channel that a command's text
    /// output is compatible with: `Void` (inherited/passthrough), `String`
    /// (`[]Byte`), `Byte`, or a command `execution`. Any other type (`Int`,
    /// `[]T`, `Maybe(T)`, an optional, a struct, …) carries a typed value.
    fn stdoutAcceptsCommandBytes(self: *TypeChecker, t: *const ast.TypeExpr) bool {
        return switch (self.unaliasType(t).*) {
            .void, .execution, .byte => true,
            .array => |a| a.element.* == .byte,
            .identifier => |named| named.path.segments.len == 1 and
                std.mem.eql(u8, named.path.segments[named.path.segments.len - 1].name, "String"),
            else => false,
        };
    }

    /// Whether a function stdout type is a raw byte channel that a command's text
    /// output is compatible with: `Void` (inherited/passthrough), `String`
    /// (`[]Byte`), `Byte`, or a command `execution`. Any other type (`Int`,
    /// `[]T`, `Maybe(T)`, an optional, a struct, …) carries a typed value.
    /// Whether a bare expression statement's result is an unhandled
    /// (non-`ExecutableError`) error — the error escapes as the statement's
    /// value (a bare error value, a call to an error-returning function, or a
    /// pipeline whose final stage yields an error union). Commands
    /// (`.execution` / `ExecutableError`) are exempt.
    fn statementHasUnhandledError(self: *TypeChecker, scope: *Scope, expr: *ast.Expression) Error!bool {
        // A function *declaration* statement never produces an unhandled error,
        // and resolving its type here would re-resolve the signature (e.g. a
        // generic return `|T|`) in this outer scope, where the function's type
        // variables are not declared — a spurious "type not declared".
        if (expr.* == .fn_decl) return false;
        const raw = (try self.resolveExprType(scope, expr)) orelse return false;
        // Resolve so a function call's raw `identifier` err_set (e.g.
        // `ExecutableError`) becomes an alias `isExecutableErrorSet` can see.
        const expr_type = try self.resolveTypeExpr(scope, raw);
        return self.isUnhandledErrorType(expr_type);
    }

    /// True for an error union or error value whose set is not the builtin
    /// `ExecutableError` (commands keep the implicit exit-code model). In strict
    /// mode (`--strict`) command failures are *not* exempt: a bare command
    /// (`.execution`) and an `ExecutableError` union/set also count as unhandled.
    fn isUnhandledErrorType(self: *TypeChecker, t: *const ast.TypeExpr) bool {
        const unaliased = self.unaliasType(t);
        return switch (unaliased.*) {
            .error_union => |error_union| self.strict or !self.isExecutableErrorSet(error_union.err_set),
            .error_set => self.strict or !self.isExecutableErrorSet(unaliased),
            .err => true,
            // A bare command's result — exempt by default, enforced under strict.
            .execution => self.strict,
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
            .cimport_expr => |*cimport_expr| self.runCImportExpr(scope, cimport_expr),
            .fn_decl => |*fn_decl| self.runFnDecl(scope, fn_decl),
            .call => |*call| self.runCall(scope, call),
            .comptime_expr => |*comptime_expr| self.runExpression(scope, comptime_expr.operand),
            .subshell => |*subshell| self.runExpression(scope, subshell.child),
            .fd => |*fd_expr| self.runFd(scope, fd_expr),
            // A type written in value position: validate the type it denotes so
            // its parts (a struct field's type, a `comptime T` reference) resolve.
            .type_value => |*type_value| self.runTypeExpression(scope, type_value.type_expr),
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
            .type_var => {},
            .void, .integer, .float, .boolean, .byte, .null, .execution, .thread, .type_type => {},
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
            // A `|T|` capture introduces a type variable; nothing to validate.
            .type_capture => {},
            // A `Box(args…)` application is validated where it resolves.
            .type_application => {},
            .struct_type => |st| self.runStructType(scope, st),
            // Validate each position of a tuple type so an unknown element type is
            // reported (`(Vector, Nope)`).
            .tuple => |tuple| for (tuple.elements) |element| try self.runTypeExpression(scope, element),
            .module, .function, .fn_ref_type => {},
        };
    }

    /// Validates a struct type: resolves each field's type and reports duplicate
    /// field names (mirrors runErrorSet for error sets). Struct types were
    /// previously not validated at all, so `struct { x: Int, x: Int }` was
    /// silently accepted.
    fn runStructType(
        self: *TypeChecker,
        scope: *Scope,
        struct_type: ast.TypeExpr.StructType,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};

        for (struct_type.fields, 0..) |field, i| {
            try self.runTypeExpression(scope, field.type_expr);

            for (struct_type.fields[0..i]) |prev| {
                if (std.mem.eql(u8, prev.name.name, field.name.name)) {
                    try self.reportSpanError(
                        field.name.span,
                        Error.TypeMismatch,
                        .@"error",
                        "duplicate field '{s}' in struct type",
                        .{field.name.name},
                    );
                    break;
                }
            }
        }
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

        // `for (@fields(T)) |f| { … }` — comptime iteration over a struct type's
        // fields. `f` is bound permissively; within the body `f.type` is a
        // comptime type and `f.name` a string. The IR compiler unrolls it.
        if (for_expr.sources.len == 1 and comptimeFieldsCallArg(for_expr.sources[0]) != null) {
            const arg = comptimeFieldsCallArg(for_expr.sources[0]).?;
            try self.runExpression(scope, arg);
            const binding = for_expr.capture.bindings[0];
            try self.runBindingPattern(for_scope, binding, null, false, false);
            const var_name: ?[]const u8 = switch (binding.*) {
                .identifier => |id| id.name,
                else => null,
            };
            const had_prev = var_name != null and self.comptime_field_vars.contains(var_name.?);
            if (var_name) |vn| try self.comptime_field_vars.put(self.arena.allocator(), vn, {});
            defer if (var_name) |vn| {
                if (!had_prev) _ = self.comptime_field_vars.remove(vn);
            };
            try self.runExpression(for_scope, for_expr.body);
            return;
        }

        for (for_expr.sources, for_expr.capture.bindings) |source, pattern| {
            try self.runExpression(scope, source);
            const source_type = try self.resolveExprType(scope, source);
            // The capture binds to one *element*, so unwrap an array source to
            // its element type (`[]String` → `String`). A non-array source (a
            // range `0..n`, `&0` stdin) already resolves to the element type.
            const element_type: ?*const ast.TypeExpr = blk: {
                const st = source_type orelse break :blk null;
                const unaliased = self.unaliasType(st);
                // A tuple iterates like an array of its unified element type
                // (permissive when its positions differ).
                if (unaliased.* == .tuple) {
                    const el = self.tupleElementType(unaliased.tuple) orelse break :blk null;
                    break :blk try self.resolveTypeExpr(scope, el);
                }
                if (unaliased.* != .array) break :blk source_type;
                // Resolve the element so a named primitive element (`[]Int` whose
                // element parsed as the identifier `Int`) normalizes to its tag.
                break :blk try self.resolveTypeExpr(scope, unaliased.array.element);
            };
            try self.runBindingPattern(for_scope, pattern, element_type, false, false);
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

        // Matching on a comptime type (`match (T) { Int => …, Box(|E|) => … }`):
        // the subject is a type, each case is a type pattern, and any `|capture|`
        // in a pattern binds as a type variable in that arm's body.
        if (try self.isComptimeTypeSubject(scope, match_expr.subject)) {
            try self.runComptimeTypeMatch(scope, match_expr);
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

    /// Whether a match subject is a comptime type value — a `comptime T: type`
    /// param (its type resolves to a `.type_var`/`.type_type` marker) or a
    /// literal type value (`match (Box(Int))`).
    fn isComptimeTypeSubject(self: *TypeChecker, scope: *Scope, subject: *ast.Expression) Error!bool {
        if (subject.* == .type_value) return true;
        // `f.type` of a comptime `@fields` loop variable is a comptime type.
        if (memberAccessParts(subject)) |ma| {
            if (std.mem.eql(u8, ma.member, "type")) {
                if (comptimeFieldVarName(ma.object)) |name| {
                    if (self.comptime_field_vars.contains(name)) return true;
                }
            }
        }
        const t = (try self.resolveExprType(scope, subject)) orelse return false;
        const u = self.unaliasType(t);
        return u.* == .type_var or u.* == .type_type;
    }

    const MemberParts = struct { object: *ast.Expression, member: []const u8 };

    /// Normalizes a member access, which the general expression parser encodes as
    /// a `.binary` with the `.member` op while the interpolation parser uses a
    /// `.member` node. Returns the object and member name for either form.
    fn memberAccessParts(expr: *ast.Expression) ?MemberParts {
        switch (expr.*) {
            .member => |m| return .{ .object = m.object, .member = m.member.name },
            .binary => |b| {
                if (b.op != .member) return null;
                const name = switch (b.right.*) {
                    .identifier => |id| id.name,
                    .call => |c| if (c.arguments.len == 0 and c.callee.* == .identifier)
                        c.callee.identifier.name
                    else
                        return null,
                    else => return null,
                };
                return .{ .object = b.left, .member = name };
            },
            else => return null,
        }
    }

    /// The identifier name of a comptime field-loop variable used as the object
    /// of an `f.name`/`f.type` access (`f`, or its zero-arg-call spelling).
    fn comptimeFieldVarName(object: *const ast.Expression) ?[]const u8 {
        return switch (object.*) {
            .identifier => |id| id.name,
            .call => |c| if (c.arguments.len == 0 and c.callee.* == .identifier)
                c.callee.identifier.name
            else
                null,
            else => null,
        };
    }

    /// The type argument of a `@fields(T)` call, or null when `expr` is not one.
    fn comptimeFieldsCallArg(expr: *const ast.Expression) ?*ast.Expression {
        if (expr.* != .call) return null;
        const call = expr.call;
        if (call.callee.* != .identifier) return null;
        if (!std.mem.eql(u8, call.callee.identifier.name, "@fields")) return null;
        if (call.arguments.len == 0) return null;
        return call.arguments[0];
    }

    /// Type-checks `match (T) { Int => …, Box(|E|) => …, _ => … }`. Each arm is a
    /// type pattern; a pattern's `|capture|` binds as a type variable visible in
    /// that arm's body. The actual pruning/binding happens at comptime in the IR
    /// compiler — here we only validate the arms and scope the captures.
    fn runComptimeTypeMatch(self: *TypeChecker, scope: *Scope, match_expr: *ast.MatchExpr) Error!void {
        for (match_expr.cases) |case| {
            const case_scope = try scope.addChild(self.arena.allocator(), case.span);
            switch (case.pattern) {
                // `_` and a bare type name (`Int`, `Box`) bind nothing.
                .wildcard, .binding => {},
                .type_pattern => |type_expr| {
                    // Declare each `|capture|` as a type variable so the arm body
                    // can reference it (`${E}`, `E == Int`).
                    var captures: std.StringHashMapUnmanaged(void) = .empty;
                    defer captures.deinit(self.arena.allocator());
                    try self.collectTypeVars(case_scope, type_expr, &captures);
                    var it = captures.keyIterator();
                    while (it.next()) |name| {
                        if (case_scope.lookup(name.*) != null) continue;
                        const marker = try self.allocTypeExpression(.{ .type_var = .{ .name = name.*, .span = type_expr.span() } });
                        try case_scope.declareType(self.arena.allocator(), ast.Identifier.global(name.*), marker, false);
                    }
                },
                else => try self.reportSpanError(
                    case.pattern.span(),
                    Error.UnsupportedExpression,
                    .@"error",
                    "matching on a type expects a type pattern (`Int`, `Box(|E|)`) or `_`",
                    .{},
                ),
            }
            if (case.capture) |capture| {
                try self.reportSpanError(
                    capture.span,
                    Error.UnsupportedExpression,
                    .@"error",
                    "a type match arm binds through its pattern (`Box(|E|)`), not a `=> |x|` clause",
                    .{},
                );
            }
            try self.runBlockInNewScope(case_scope, @constCast(&case.body));
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

    /// The root binding name of a member-access chain (`a.b.c` → `a`), or the
    /// bare identifier itself. Null for anything not rooted in an identifier.
    fn rootBindingName(expr: *const ast.Expression) ?[]const u8 {
        return switch (expr.*) {
            .identifier => |identifier| identifier.name,
            .binary => |binary| if (binary.op == .member) rootBindingName(binary.left) else null,
            .member => |member| rootBindingName(member.object),
            else => null,
        };
    }

    /// The struct operand of a `@field(value)(name)` assignment target, or null
    /// when `expr` is not such a call. Used to enforce mutability of the value
    /// being written through and to find its root binding.
    fn fieldBuiltinTarget(expr: *const ast.Expression) ?*const ast.Expression {
        if (expr.* != .call) return null;
        const call = expr.call;
        if (call.callee.* != .identifier) return null;
        if (!std.mem.eql(u8, call.callee.identifier.name, "@field")) return null;
        if (call.arguments.len == 0) return null;
        return call.arguments[0];
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

        // `@field(p)(name) = v` — assignment through a comptime-named field. Handled
        // before the type-resolution early-return below, because a `@field(…)` call
        // has no resolvable value type here (so `left_type` would be null and the
        // assignment checks skipped). Enforce that the value written through is
        // mutable; the field's type often can't be resolved statically (a
        // comptime-loop name doesn't fold in the checker), so value validation is
        // deferred to the per-monomorphization member write in the IR compiler.
        if (binary.op.isAssignment()) {
            if (fieldBuiltinTarget(binary.left)) |object| {
                // The object often parses as a bare command-style call (`p` → `p()`)
                // in argument position; unwrap that to reach the root binding.
                const obj = if (object.* == .call and object.call.arguments.len == 0)
                    object.call.callee
                else
                    object;
                if (rootBindingName(obj)) |root| {
                    if (scope.lookup(root)) |binding| if (!binding.is_mutable) {
                        try self.reportSpanError(
                            binary.left.span(),
                            Error.TypeMismatch,
                            .@"error",
                            "cannot assign to a field of immutable '{s}'; declare it with var",
                            .{root},
                        );
                    };
                }
                return;
            }
        }

        const maybe_left_type = try self.resolveExprType(scope, binary.left);
        const maybe_right_type = try self.resolveExprType(scope, binary.right);

        // TODO: implement all kinds of semantic checks here
        // TODO: implement type expr "locations" that can be populated. These locations should be able to be resolved through expressions. This is for populating inferred type expressions.

        const left_type = maybe_left_type orelse return;
        const right_type = maybe_right_type orelse return;

        //     break :brk .{ left_type, right_type };
        // };

        // Member-specific operation enforcement: a *bare* (un-narrowed) sum is
        // never numeric, so it can't be used in arithmetic — narrow it first
        // (with `is`/`==`/match). A *narrowed* operand resolves to its member
        // type here (via the scoped shadow binding), so this only rejects the
        // un-narrowed case. Comparison/relational ops are intentionally allowed:
        // they are how you narrow (`if (x > 5)`, `if (x == 0)`).
        // Arithmetic (and `>>` shift-right) can't be applied to a bare sum;
        // narrow it first. `.shift_or_append` is `>>` on values (a command-left
        // `>>` is an append-redirect handled in the IR, never a sum).
        if (binary.op.isArithmetic() or binary.op.category() == .shift_or_append) {
            try self.rejectBareSum(binary.left, left_type, "use in arithmetic");
            try self.rejectBareSum(binary.right, right_type, "use in arithmetic");
        } else if (binary.op.isComparison()) {
            // Comparing a sum to a value that can never share a member is almost
            // always a mistake (the result is constant); reject an empty
            // intersection. (`==`/`!=`/relational double as narrowing.)
            try self.rejectEmptyComparison(binary.left, left_type, binary.right, right_type);
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
                // A type that failed to resolve already produced its own error;
                // don't cascade an orelse mismatch on top of it.
                .failed => {},
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

            // An operand can surface as an unresolved type identifier: a struct
            // field's declared type comes back through member access and
            // arithmetic as the bare name `Int` rather than the resolved
            // primitive (so `a.x += 1`, i.e. `a.x = a.x + 1`, would otherwise
            // compare `Int` against `Int` and fail, or hit an UnresolvedTypeLiteral
            // for a compound assignment). Resolve such a right side before
            // validating. This covers `=` and every compound assignment.
            const resolved_right = if (right_type.* == .identifier)
                try self.resolveTypeExpr(scope, right_type)
            else
                right_type;

            // For `x = v` / `x += v` to an identifier binding, validate against
            // the binding's *declared* type (not its current narrowed flow type
            // — a `var x: Int || String` narrowed to `String` can still be
            // reassigned an Int), then, for a plain `=` to a sum, refine the flow
            // type so reads after the assignment see the new (narrowed) type.
            if (referencedBindingName(binary.left)) |name| {
                if (scope.lookup(name)) |binding| {
                    // Reassigning (or compound-assigning) an immutable binding is
                    // rejected — a `const` (and a plain, non-`var` parameter) may
                    // not change. Without this the assignment slipped through to
                    // the IR compiler, which panicked on a const's value source.
                    if (!binding.is_mutable) {
                        try self.reportSpanError(
                            binary.left.span(),
                            Error.TypeMismatch,
                            .@"error",
                            "cannot assign to immutable '{s}'; declare it with var",
                            .{name},
                        );
                        return;
                    }
                    const declared = binding.declared_type orelse left_type;
                    try self.validateTypeAssignment(declared, resolved_right, .{ .span = right_type.span() });
                    if (binary.op == .assign and self.unaliasType(declared).* == .sum) {
                        binding.type_expr = self.flowTypeForSum(declared, resolved_right);
                    }
                    return;
                }
            }

            // Field assignment `p.x = v` / `p.x += v`: the target is a member
            // access. Enforce that the root binding is mutable and validate `v`
            // against the (resolved) field type.
            if (binary.left.* == .binary and binary.left.binary.op == .member) {
                if (rootBindingName(binary.left)) |root| {
                    if (scope.lookup(root)) |binding| if (!binding.is_mutable) {
                        try self.reportSpanError(
                            binary.left.span(),
                            Error.TypeMismatch,
                            .@"error",
                            "cannot assign to a field of immutable '{s}'; declare it with var",
                            .{root},
                        );
                    };
                }
                const field_type = try self.resolveTypeExpr(scope, left_type);
                try self.validateTypeAssignment(field_type, resolved_right, .{ .span = right_type.span() });
                return;
            }

            try self.validateTypeAssignment(left_type, resolved_right, .{ .span = right_type.span() });
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

    /// Whether a member access's object is the empty-identifier sentinel the
    /// parser emits for an implicit `.name` access (`const v: T = .name`), to be
    /// resolved against the result-location type.
    fn isImplicitMemberObject(object: *const ast.Expression) bool {
        return object.* == .identifier and object.identifier.name.len == 0;
    }

    pub fn runMember(self: *TypeChecker, scope: *Scope, member: *ast.MemberExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, member.span);

        // Implicit member access `.name` (object is the empty-identifier sentinel):
        // resolved against the result-location type by the IR compiler. Accept it
        // permissively here, the same way the explicit `Type.name` form is at this
        // stage — don't run the empty identifier as an expression.
        if (isImplicitMemberObject(member.object)) return;

        try self.runExpression(scope, member.object);
        const raw_object_type = try self.resolveExprType(scope, member.object) orelse {
            return error.MemberObjectTypeUndefined;
        };
        // Unwrap aliases so a struct-typed binding/param (`p: Point`) resolves to
        // its struct type for member access.
        const object_type = self.unaliasType(raw_object_type);

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
            .type_var => {},
            .identifier => return error.UnresolvedTypeLiteral,
            .optional => return error.MemberAccessOnOptional,
            .array => |array| try self.runArrayMemberAccess(array, &member.member),
            // A tuple supports the same members as an array (`len`, …); it shares
            // the array's positional/slice representation.
            .tuple => try self.runArrayMemberAccess(undefined, &member.member),
            .thread => try self.runThreadMemberAccess(&member.member),
            .struct_type => |struct_type| try self.runStructMemberAccess(struct_type, &member.member),
            .error_set => |error_set| try self.runErrorSetMemberAccess(error_set, &member.member),
            .null, .promise, .error_union, .err, .function, .fn_ref_type, .integer, .float, .boolean, .byte, .alias, .void, .type_merge, .sum, .type_capture, .type_application, .type_type => return error.UnsupportedMemberAccess,
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
        // An unexpanded `@insert` recipe struct has no resolved fields yet — accept
        // any member permissively; the IR compiler resolves the real layout.
        if (struct_type.body_items.len > 0) return;

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

        // Only pub declarations are accessible on a module — except a type
        // member (`m.Vector3`), which can't be declared `pub` (the parser
        // rejects `pub const X = struct {…}`), so it is always reachable.
        // Otherwise fall back to execution-result fields (exit_code, stdout, …).
        if (module_scope.lookup(identifier.name)) |binding| {
            if (binding.is_pub or binding.is_type) return;
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
            .function => |function| blk: {
                const stdin = try self.resolvePipeType(scope, function.stdin_type);
                // Pipeline↔param coercion: a function with a Void (or absent)
                // stdin and exactly one parameter accepts the upstream value in
                // that parameter, so the parameter type is its effective input.
                const void_stdin = stdin == null or self.unaliasType(stdin.?).* == .void;
                if (void_stdin) {
                    if (singleParamType(function)) |pt| {
                        break :blk try self.resolvePipeType(scope, pt);
                    }
                }
                break :blk stdin;
            },
            else => null,
        };
    }

    /// The single parameter's declared type for a one-parameter function, else
    /// null (zero, many, variadic, or untyped-parameter functions).
    fn singleParamType(function: ast.TypeExpr.FunctionType) ?*const ast.TypeExpr {
        return switch (function.params) {
            ._non_variadic => |params| if (params.len == 1) params[0] else null,
            ._variadic => null,
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

        // A type variable (generic) unifies with anything — the same permissive
        // treatment used elsewhere. This lets a generic function return a generic
        // struct: `struct { value: T }` (yielded) matches `struct { value: T }`
        // (declared) even though the two `T`s are distinct nodes.
        if (resolved_left.* == .type_var or resolved_right.* == .type_var) return true;

        // `null` inhabits any optional (`?T`), including nested in a struct field
        // (`{ x: null }` satisfying `{ x: ?T }`) — the same coercion the top-level
        // optional-yield check allows, applied structurally.
        if (resolved_left.* == .null and resolved_right.* == .optional) return true;
        if (resolved_left.* == .optional and resolved_right.* == .null) return true;

        if (std.meta.activeTag(resolved_left.*) != std.meta.activeTag(resolved_right.*)) return false;

        return switch (resolved_left.*) {
            .array => |left_array| self.pipeTypesEqual(left_array.element, resolved_right.array.element),
            // Structs compare structurally (same fields, in order, with equal
            // types) — not by node identity, so two separately-built identical
            // struct types (e.g. a generic constructor applied twice) are equal.
            .struct_type => |left_st| blk: {
                const right_st = resolved_right.struct_type;
                if (left_st.fields.len != right_st.fields.len) break :blk false;
                for (left_st.fields, right_st.fields) |lf, rf| {
                    if (!std.mem.eql(u8, lf.name.name, rf.name.name)) break :blk false;
                    if (!self.pipeTypesEqual(lf.type_expr, rf.type_expr)) break :blk false;
                }
                break :blk true;
            },
            // A generic application (`Entry(K, V)`) may remain unresolved inside a
            // struct field; compare by constructor name and pairwise arguments.
            .type_application => |left_app| blk: {
                const right_app = resolved_right.type_application;
                if (!std.mem.eql(u8, left_app.name.name, right_app.name.name)) break :blk false;
                if (left_app.args.len != right_app.args.len) break :blk false;
                for (left_app.args, right_app.args) |la, ra| {
                    if (!self.pipeTypesEqual(la, ra)) break :blk false;
                }
                break :blk true;
            },
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
            // Tuples compare structurally: same arity, pairwise-equal positions
            // (like arrays and structs, not by node identity).
            .tuple => |left_tuple| blk: {
                const right_tuple = resolved_right.tuple;
                if (left_tuple.elements.len != right_tuple.elements.len) break :blk false;
                for (left_tuple.elements, right_tuple.elements) |le, re| {
                    if (!self.pipeTypesEqual(le, re)) break :blk false;
                }
                break :blk true;
            },
            .void, .integer, .float, .boolean, .byte, .null, .execution, .thread, .type_type => true,
            else => std.meta.eql(resolved_left.*, resolved_right.*),
        };
    }

    fn unaliasType(self: *TypeChecker, type_expr: *const ast.TypeExpr) *const ast.TypeExpr {
        return switch (type_expr.*) {
            .alias => |*alias| self.unaliasType(self.resolveAliasType(alias)),
            else => type_expr,
        };
    }

    /// The single element type of a tuple when every position shares it (under
    /// `pipeTypesEqual`), else null. A tuple behaves as `[]element` for array
    /// operations (index, `for`, `len`, concat); a null result means the
    /// positions differ, so the element type is left permissive — matching the
    /// (untyped) behavior of an un-annotated literal before tuples existed.
    /// Whether a type is the empty struct `struct {}` — the type of an empty
    /// `.{}` literal. Such a value coerces to an empty array of any element type
    /// but otherwise carries no element type to operate on.
    fn isEmptyStructType(self: *TypeChecker, type_expr: *const ast.TypeExpr) bool {
        const t = self.unaliasType(type_expr);
        return t.* == .struct_type and t.struct_type.fields.len == 0;
    }

    fn tupleElementType(self: *TypeChecker, tuple: ast.TypeExpr.TupleType) ?*const ast.TypeExpr {
        if (tuple.elements.len == 0) return null;
        const first = tuple.elements[0];
        for (tuple.elements[1..]) |element| {
            if (!self.pipeTypesEqual(first, element)) return null;
        }
        return first;
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
            .expr => |expr| {
                try self.runExpression(scope, expr);
                // A command argument is serialized to a string, so a whole struct
                // has no valid form here — pass a field instead. (Mirrors the
                // string-interpolation guard; without it the arg would fail at
                // runtime while being materialized.)
                if (try self.resolveExprType(scope, expr)) |t| {
                    const unaliased = self.unaliasType(t);
                    if (unaliased.* == .struct_type and !isExecutionLikeStruct(unaliased.struct_type)) {
                        try self.reportSpanError(
                            expr.span(),
                            Error.TypeMismatch,
                            .@"error",
                            "cannot pass a whole struct as a command argument; pass a field instead (e.g. value.field)",
                            .{},
                        );
                    }
                }
            },
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

    /// The struct type a *named* struct literal constructs (a plain binding, a
    /// generic constructor, or a module-qualified type), read-only, for inferring
    /// nested field-value literal types. Returns null when it can't be resolved to
    /// a plain struct type (an anonymous literal, an error set, unknown name, …).
    fn structTypeOfNamedLiteral(
        self: *TypeChecker,
        scope: *Scope,
        struct_literal: *ast.StructLiteral,
    ) Error!?ast.TypeExpr.StructType {
        if (struct_literal.name.name.len == 0) return null;

        if (struct_literal.object) |object| {
            const obj_raw = (try self.resolveExprType(scope, object)) orelse return null;
            const object_type = self.unaliasType(obj_raw);
            if (object_type.* != .module) return null;
            const module_scope = (try self.requestModuleScope(object_type.module)) orelse return null;
            const member = module_scope.lookup(struct_literal.name.name) orelse return null;
            const member_type = member.type_expr orelse return null;
            const st = self.unaliasType(try self.resolveModuleMemberFields(module_scope, member_type));
            return if (st.* == .struct_type) st.struct_type else null;
        }

        if (self.generic_type_ctors.get(struct_literal.name.name)) |ctor| {
            const resolved = self.unaliasType(try self.resolveGenericCtorBody(scope, ctor));
            return if (resolved.* == .struct_type) resolved.struct_type else null;
        }

        const binding = scope.lookup(struct_literal.name.name) orelse return null;
        const type_expr = self.unaliasType(binding.type_expr orelse return null);
        return if (type_expr.* == .struct_type) type_expr.struct_type else null;
    }

    pub fn runStructLiteral(self: *TypeChecker, scope: *Scope, struct_literal: *ast.StructLiteral) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, struct_literal.span);

        // Inferred nested literals: `Outer{ .inner = .{ .x = 1 } }` takes each
        // anonymous field value's type from that field's declared type, so the
        // inner struct name need not be repeated. Stamp before the fields are
        // checked, so each validates like the named form.
        for (struct_literal.fields) |field| {
            if (exprMayNeedStructInference(field.value)) {
                if (try self.structTypeOfNamedLiteral(scope, struct_literal)) |st| {
                    for (struct_literal.fields) |f| {
                        if (exprMayNeedStructInference(f.value)) {
                            self.stampInferredLiteral(f.value, st.memberType(f.name.name));
                        }
                    }
                }
                break;
            }
        }

        for (struct_literal.fields) |field| try self.runExpression(scope, field.value);

        // An anonymous struct literal `.{ .x = … }` whose name was never inferred
        // from context (no annotation to take a type from). Give a directed error
        // instead of falling through to a lookup of the empty name.
        if (struct_literal.object == null and struct_literal.name.name.len == 0) {
            try self.reportSpanError(
                struct_literal.span,
                Error.UnsupportedExpression,
                .@"error",
                "cannot infer the type of this struct literal; add a type annotation (e.g. `const v: Vector = .{{ … }}`)",
                .{},
            );
            return;
        }

        // Qualified construction `m.Vector3{ … }`: the struct type lives in the
        // module `m`. Struct types can't be `pub` (the parser rejects
        // `pub const X = struct {…}`), so it is looked up in the module's scope
        // regardless of visibility.
        if (struct_literal.object) |object| {
            try self.runExpression(scope, object);
            const object_type = self.unaliasType((try self.resolveExprType(scope, object)) orelse return);
            if (object_type.* != .module) {
                try self.reportSpanError(
                    object.span(),
                    Error.UnsupportedExpression,
                    .@"error",
                    "qualified struct construction requires a module value",
                    .{},
                );
                return;
            }
            const module_scope = (try self.requestModuleScope(object_type.module)) orelse return;
            const member = module_scope.lookup(struct_literal.name.name) orelse {
                try self.reportSpanError(
                    struct_literal.name.span,
                    Error.IdentifierNotFound,
                    .@"error",
                    "module has no type '{s}'",
                    .{struct_literal.name.name},
                );
                return;
            };
            // Resolve the struct's field types in the module's scope so a field
            // type such as `c.Float` (referring to the module's own `import`) is
            // validated where `c` is bound, not in the importer's scope.
            const member_type = member.type_expr orelse return;
            const st = self.unaliasType(try self.resolveModuleMemberFields(module_scope, member_type));
            if (st.* == .struct_type) {
                try self.runStructValueLiteral(scope, st.struct_type, struct_literal);
            } else {
                try self.reportSpanError(
                    struct_literal.name.span,
                    Error.UnsupportedExpression,
                    .@"error",
                    "'{s}' is not a struct type",
                    .{struct_literal.name.name},
                );
            }
            return;
        }

        // A generic constructor (`Box{ … }` for `const Box(T) = struct { … }`, or a
        // comptime type function `Partial{ … }`): validate against the body struct.
        if (self.generic_type_ctors.get(struct_literal.name.name)) |ctor| {
            // Prefer the concrete layout from the *expected* type — the annotation
            // `Partial(Point)`, which `resolveTypeApplication` materializes into
            // real fields (including an `@insert` recipe). This is what lets a
            // `Partial{ .zzz = 1 }` field typo be caught at construction. Fall back
            // to the (parameter-permissive) ctor body when there's no matching
            // annotation to take the instantiation from.
            if (try self.expectedMaterializedStruct(scope, struct_literal.name.name)) |st| {
                try self.runStructValueLiteral(scope, st, struct_literal);
                return;
            }
            const resolved = self.unaliasType(try self.resolveGenericCtorBody(scope, ctor));
            if (resolved.* == .struct_type) try self.runStructValueLiteral(scope, resolved.struct_type, struct_literal);
            return;
        }

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
            .struct_type => |struct_type| try self.runStructValueLiteral(scope, struct_type, struct_literal),
            else => try self.reportSpanError(
                struct_literal.name.span,
                Error.UnsupportedExpression,
                .@"error",
                "'{s}' is not a struct or error set; cannot construct it with {{ … }}",
                .{struct_literal.name.name},
            ),
        }
    }

    /// Validates `Name{ .field = value, … }` against a struct type: every named
    /// field must exist (with a type-compatible value), no duplicates, and every
    /// declared field must be provided (no defaults yet).
    fn runStructValueLiteral(
        self: *TypeChecker,
        scope: *Scope,
        struct_type: ast.TypeExpr.StructType,
        struct_literal: *ast.StructLiteral,
    ) Error!void {
        // A struct carrying an unexpanded `@insert`/`for … @insert` recipe has no
        // resolved fields yet — the IR compiler materializes them per instantiation.
        // The type checker can't fold the recipe, so it validates permissively:
        // check each value expression, but not against field names/count.
        if (struct_type.body_items.len > 0) {
            for (struct_literal.fields) |field| try self.runExpression(scope, field.value);
            return;
        }
        for (struct_literal.fields, 0..) |field, i| {
            // Duplicate field.
            for (struct_literal.fields[0..i]) |prev| {
                if (std.mem.eql(u8, prev.name.name, field.name.name)) {
                    try self.reportSpanError(
                        field.name.span,
                        Error.TypeMismatch,
                        .@"error",
                        "duplicate field '{s}' in struct literal",
                        .{field.name.name},
                    );
                    break;
                }
            }

            const declared = struct_type.memberType(field.name.name) orelse {
                try self.reportSpanError(
                    field.name.span,
                    Error.MemberNotFound,
                    .@"error",
                    "struct has no field '{s}'",
                    .{field.name.name},
                );
                continue;
            };
            const resolved_field = try self.resolveTypeExpr(scope, declared);
            const value_raw = (try self.resolveExprType(scope, field.value)) orelse continue;
            // A member/field access (`e.x`) can surface the field's *raw*
            // declared type from the AST's own resolveType — an unresolved
            // `.identifier` ("Int", or an alias like `c.Int`). Resolve it so it
            // compares against the (already-resolved) declared field type;
            // otherwise `V{ .x = e.x }` fails with "expected Int, actual: Int"
            // (and "actual: c.Int" for a `c.Int`-aliased field).
            const value_type = try self.resolveTypeExpr(scope, value_raw);
            try self.validateTypeAssignment(resolved_field, value_type, .{ .span = field.value.span() });
        }

        // Every declared field must be supplied (no default values yet).
        for (struct_type.fields) |field| {
            var found = false;
            for (struct_literal.fields) |provided| {
                if (std.mem.eql(u8, provided.name.name, field.name.name)) {
                    found = true;
                    break;
                }
            }
            if (!found) try self.reportSpanError(
                struct_literal.span,
                Error.TypeMismatch,
                .@"error",
                "missing field '{s}' in struct literal",
                .{field.name.name},
            );
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
                // A type identifier serializes to its name — a valid String.
                if (self.isTypeIdentifierExpr(scope, expr)) return;
                if (try self.resolveExprType(scope, expr)) |t| {
                    // Interpolating a bare (un-narrowed) sum has no single string
                    // form — narrow it first (with `is`/`==`/match).
                    try self.rejectBareSum(expr, t, "interpolate");
                    // A whole struct has no single string form — interpolate a
                    // field (`${p.x}`), not the struct itself.
                    const unaliased = self.unaliasType(t);
                    if (unaliased.* == .struct_type and !isExecutionLikeStruct(unaliased.struct_type)) {
                        try self.reportSpanError(
                            expr.span(),
                            Error.TypeMismatch,
                            .@"error",
                            "cannot interpolate a whole struct; interpolate a field instead (e.g. ${{value.field}})",
                            .{},
                        );
                    }
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

    /// The Runic type a `std.ffi` C type marshals from/to, used only for
    /// type-checking an `extern fn` and its calls. The C width/ABI distinction
    /// (`c.Int` is 32-bit, Runic `Int` is 64-bit) is irrelevant here — it is
    /// carried by the `ExternFn` AST and applied at marshalling — so several C
    /// types collapse to the same Runic type. `c.Ptr` is an opaque handle,
    /// treated as an `Int` (an address) for the MVP. Returns null for a name
    /// that is not a known C type.
    fn cTypeToRunic(self: *TypeChecker, name: []const u8) Error!?*const ast.TypeExpr {
        const int_names = [_][]const u8{ "Int", "UInt", "Long", "ULong", "Short", "UShort", "Char", "SizeT", "Ptr" };
        for (int_names) |n| {
            if (std.mem.eql(u8, name, n)) return try self.allocTypeExpression(.global(.integer));
        }
        if (std.mem.eql(u8, name, "Float") or std.mem.eql(u8, name, "Double")) {
            return try self.allocTypeExpression(.global(.float));
        }
        if (std.mem.eql(u8, name, "Bool")) return try self.allocTypeExpression(.global(.boolean));
        if (std.mem.eql(u8, name, "Str")) return try self.allocStringType();
        if (std.mem.eql(u8, name, "Void")) return try self.allocTypeExpression(.global(.void));
        return null;
    }

    /// Resolves a C type written in an `extern fn` signature and reports a
    /// diagnostic if it is malformed. It must be a qualified `ns.Name` where
    /// `ns` is in scope (the imported `std.ffi` module) and `Name` is a known C
    /// type. Returns the Runic type it checks as, or a `failed` type on error.
    ///
    /// NOTE: `ns` is only required to be *a* bound identifier, not verified to
    /// be `std.ffi` specifically — a module-value binding does not currently
    /// retain its source path. Tightening this is tracked in future/c-ffi.md.
    /// The scalar C type a struct-field annotation names (`c.Char` → `.char`),
    /// or null if it is not a scalar `c.X` — the marker for a field that cannot
    /// be marshalled in a by-value struct.
    fn cTypeOfFieldAnnotation(type_expr: *const ast.TypeExpr) ?CType {
        if (type_expr.* != .identifier) return null;
        const segments = type_expr.identifier.path.segments;
        if (segments.len == 0) return null;
        return CType.fromName(segments[segments.len - 1].name);
    }

    /// Whether every field of a by-value struct is marshallable — a scalar `c.X`
    /// or (recursively) another by-value struct. Reports the first field that is
    /// not. `depth` guards against a pathological cyclic type.
    fn cStructFieldsMarshallable(
        self: *TypeChecker,
        scope: *Scope,
        struct_type: ast.TypeExpr.StructType,
        struct_name: []const u8,
        depth: usize,
    ) Error!bool {
        if (depth > 32) return false;
        for (struct_type.fields) |field| {
            if (cTypeOfFieldAnnotation(field.type_expr) != null) continue; // scalar
            // A nested by-value struct: a single identifier naming a user struct.
            if (field.type_expr.* == .identifier and
                field.type_expr.identifier.path.segments.len == 1 and
                scope.lookup(field.type_expr.identifier.path.segments[0].name) != null)
            {
                const resolved = try self.resolveTypeExpr(scope, field.type_expr);
                const concrete = self.unaliasType(resolved);
                if (concrete.* == .struct_type) {
                    const nested_name = field.type_expr.identifier.path.segments[0].name;
                    if (try self.cStructFieldsMarshallable(scope, concrete.struct_type, nested_name, depth + 1)) continue;
                    return false; // the nested struct already reported
                }
            }
            try self.reportSpanError(
                field.type_expr.span(),
                Error.UnsupportedExpression,
                .@"error",
                "field `{s}` of struct `{s}` must be a std.ffi C type (like `c.Char`) or a by-value struct",
                .{ field.name.name, struct_name },
            );
            return false;
        }
        return true;
    }

    fn resolveCType(self: *TypeChecker, scope: *Scope, type_expr: *const ast.TypeExpr) Error!*const ast.TypeExpr {
        const failed = try self.allocTypeExpression(.{ .failed = .{ .span = type_expr.span() } });

        // A struct passed by value: a single identifier naming a user struct
        // whose fields are all scalar `c.X` or (recursively) such structs (e.g.
        // raylib's `Color`, `Vector2`, `Camera2D`). Checks as that struct.
        if (type_expr.* == .identifier and
            type_expr.identifier.path.segments.len == 1 and
            scope.lookup(type_expr.identifier.path.segments[0].name) != null)
        {
            const resolved = try self.resolveTypeExpr(scope, type_expr);
            const concrete = self.unaliasType(resolved);
            if (concrete.* == .struct_type) {
                const name = type_expr.identifier.path.segments[0].name;
                if (try self.cStructFieldsMarshallable(scope, concrete.struct_type, name, 0)) return resolved;
                return failed;
            }
            try self.reportSpanError(
                type_expr.span(),
                Error.UnsupportedExpression,
                .@"error",
                "an extern fn type must be a std.ffi C type like `c.Double` or a by-value struct",
                .{},
            );
            return failed;
        }

        if (type_expr.* != .identifier or type_expr.identifier.path.segments.len != 2) {
            try self.reportSpanError(
                type_expr.span(),
                Error.UnsupportedExpression,
                .@"error",
                "an extern fn type must be a std.ffi C type like `c.Double`",
                .{},
            );
            return failed;
        }

        const ns = type_expr.identifier.path.segments[0];
        const cname = type_expr.identifier.path.segments[1].name;

        if (scope.lookup(ns.name) == null) {
            try self.reportSpanError(
                ns.span,
                Error.IdentifierNotFound,
                .@"error",
                "`{s}` is not in scope — `import \"std/ffi.rn\"` for the C types",
                .{ns.name},
            );
            return failed;
        }

        return try self.cTypeToRunic(cname) orelse {
            try self.reportSpanError(
                type_expr.span(),
                Error.UnsupportedExpression,
                .@"error",
                "unknown C type `{s}.{s}`",
                .{ ns.name, cname },
            );
            return failed;
        };
    }

    /// Type-checks a `cimport` block: every `extern fn` parameter and return
    /// type must be a valid `std.ffi` C type. The value's own (module-like)
    /// type is built separately in `buildCImportValueType`.
    pub fn runCImportExpr(self: *TypeChecker, scope: *Scope, cimport: *ast.CImportExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, cimport.span);

        for (cimport.externs) |extern_fn| {
            for (extern_fn.params) |param| {
                if (param.type_annotation) |ann| {
                    _ = try self.resolveCType(scope, ann);
                } else {
                    try self.reportSpanError(
                        extern_fn.name.span,
                        Error.UnsupportedExpression,
                        .@"error",
                        "extern fn `{s}` parameter needs a C type, e.g. `x: c.Double`",
                        .{extern_fn.name.name},
                    );
                }
            }
            _ = try self.resolveCType(scope, extern_fn.return_type);
        }
    }

    /// The Runic type an extern parameter/return annotation checks as, extracted
    /// by its last path segment (`c.Double` → the `Double` C type → `Float`).
    /// Pure (no scope); `runCImportExpr` has already reported any bad type.
    fn cTypeFromAnnotation(self: *TypeChecker, annotation: ?*const ast.TypeExpr) Error!?*const ast.TypeExpr {
        const t = annotation orelse return null;
        if (t.* == .identifier) {
            const segments = t.identifier.path.segments;
            if (segments.len > 0) {
                return try self.cTypeToRunic(segments[segments.len - 1].name);
            }
        }
        return null;
    }

    /// Builds the (module-like) type of a `cimport` value: a struct whose fields
    /// are the declared externs, each typed as a function over the C types'
    /// Runic equivalents. `m.pow` then resolves like any struct-field member and
    /// `m.pow 2.0 10.0` type-checks like any call.
    fn buildCImportValueType(self: *TypeChecker, cimport: ast.CImportExpr) Error!?*const ast.TypeExpr {
        const fields = try self.arena.allocator().alloc(ast.TypeExpr.StructField, cimport.externs.len);
        for (cimport.externs, 0..) |extern_fn, i| {
            const params = try self.arena.allocator().alloc(?*const ast.TypeExpr, extern_fn.params.len);
            for (extern_fn.params, 0..) |param, pi| {
                params[pi] = try self.cTypeFromAnnotation(param.type_annotation);
            }
            const fn_type = try self.allocTypeExpression(.{ .function = .{
                .params = .{ ._non_variadic = params },
                .stdin_type = null,
                .return_type = try self.cTypeFromAnnotation(extern_fn.return_type),
                .span = extern_fn.span,
            } });
            fields[i] = .{
                .name = extern_fn.name,
                .type_expr = fn_type,
                .span = extern_fn.span,
            };
        }

        return self.allocTypeExpression(.{ .struct_type = .{
            .fields = fields,
            .decls = &.{},
            .span = cimport.span,
            .cimport_externs = cimport.externs,
        } });
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

    /// Whether `name`, in a value position, denotes a type (a primitive keyword,
    /// a generic constructor, or a type binding / captured type variable).
    fn isTypeIdentifierName(self: *TypeChecker, scope: *Scope, name: []const u8) bool {
        const primitives = [_][]const u8{ "Int", "String", "Bool", "Float", "Void", "Byte" };
        for (primitives) |p| {
            if (std.mem.eql(u8, name, p)) return true;
        }
        if (self.generic_type_ctors.contains(name)) return true;
        if (scope.lookup(name)) |binding| return binding.is_type;
        return false;
    }

    /// Whether `expr` is a bare type identifier in a value position — an
    /// identifier (or a zero-arg call, which is how a bare name parses) naming a
    /// type. Such an expression serializes to the type's name, so it is a valid
    /// String where a whole struct/error value would not be.
    fn isTypeIdentifierExpr(self: *TypeChecker, scope: *Scope, expr: *const ast.Expression) bool {
        return switch (expr.*) {
            .identifier => |id| self.isTypeIdentifierName(scope, id.name),
            // A bare type name parses as a zero-arg call; a generic application
            // (`Box(Int)`) parses as a call to the constructor with arguments.
            .call => |call| call.callee.* == .identifier and
                (call.arguments.len == 0 or self.generic_type_ctors.contains(call.callee.identifier.name)) and
                self.isTypeIdentifierName(scope, call.callee.identifier.name),
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

        // A generic struct literal (`Box{ … }`) has no type via the AST's own
        // resolveType (the constructor isn't a plain scope binding). Build its
        // type from the constructor body, typing each field by its supplied
        // value so `Box{ .value = 5 }` is `struct { value: Int }` (not `{ T }`).
        if (T == *ast.Expression and expr.* == .struct_literal) {
            if (self.generic_type_ctors.get(expr.struct_literal.name.name)) |ctor| {
                const body = self.unaliasType(try self.resolveGenericCtorBody(scope, ctor));
                if (body.* != .struct_type) return body;
                const new_fields = try self.arena.allocator().alloc(ast.TypeExpr.StructField, body.struct_type.fields.len);
                for (body.struct_type.fields, new_fields) |field, *dst| {
                    dst.* = field;
                    // Prefer the supplied value's type; fall back to the body
                    // field. Resolve it either way, so an unresolved parameter
                    // (`Entry(K, V)`) becomes type variables that match a concrete
                    // declared type permissively.
                    var field_type = field.type_expr;
                    for (expr.struct_literal.fields) |lit_field| {
                        if (std.mem.eql(u8, lit_field.name.name, field.name.name)) {
                            // An empty `.{}` value is an empty struct; keep the
                            // field's declared type (e.g. `[]Entry`) rather than
                            // collapsing the field to `struct {}`.
                            if (try self.resolveExprType(scope, lit_field.value)) |vt| {
                                if (!self.isEmptyStructType(vt)) field_type = vt;
                            }
                            break;
                        }
                    }
                    dst.type_expr = try self.resolveTypeExpr(scope, field_type);
                }
                var new_st = body.struct_type;
                new_st.fields = new_fields;
                return try self.allocTypeExpression(.{ .struct_type = new_st });
            }
        }

        // An empty `.{}` with no element type is an *empty struct* — a value you
        // can pass around but not operate on (no element type to index, iterate,
        // or push). An appendable empty array must say its element type with an
        // annotation: `var xs: []T = .{}` (the empty struct then coerces to the
        // empty array — see `validateTypeAssignmentArray`).
        if (T == *ast.Expression and expr.* == .array and expr.array.elements.len == 0) {
            return try self.allocTypeExpression(.{ .struct_type = .{
                .fields = &.{},
                .decls = &.{},
                .span = expr.array.span,
            } });
        }

        // A *heterogeneous* `.{ … }` literal is typed as a tuple, carrying each
        // element's type by position, so it survives being stored in a variable
        // and later destructured with per-element types. A homogeneous literal
        // keeps the prior permissive (null) typing — it flows as an array (a
        // homogeneous accumulator like `var xs: []T = .{ 1 }`, yields to `[]T`,
        // coercions) exactly as before. When a heterogeneous tuple is assigned to
        // an array `[]T`, the array-coercion check rejects it (see
        // `validateTypeAssignmentArray`), which is where "an array is one type" is
        // enforced. (`ArrayLiteral.resolveType` returns null on its own.)
        if (T == *ast.Expression and expr.* == .array and expr.array.elements.len >= 2) {
            const elements = try self.arena.allocator().alloc(*const ast.TypeExpr, expr.array.elements.len);
            var all_resolved = true;
            for (expr.array.elements, elements) |el, *dst| {
                if (try self.resolveExprType(scope, el)) |t| {
                    dst.* = t;
                } else {
                    all_resolved = false;
                    dst.* = try self.allocTypeExpression(.{ .type_var = .{ .name = "_", .span = el.span() } });
                }
            }
            if (all_resolved) {
                var uniform = true;
                for (elements[1..]) |el| {
                    if (!self.pipeTypesEqual(elements[0], el)) {
                        uniform = false;
                        break;
                    }
                }
                if (!uniform) {
                    return try self.allocTypeExpression(.{ .tuple = .{ .elements = elements, .span = expr.array.span } });
                }
            }
            return null;
        }

        // A cimport value is typed as a struct of its externs (see
        // buildCImportValueType), so `m.pow 2.0 10.0` resolves and checks like
        // any struct-member call. The AST's own resolveType returns null here.
        if (T == *ast.Expression and expr.* == .cimport_expr) {
            return self.buildCImportValueType(expr.cimport_expr);
        }

        // A member access on an imported module value (`m.member`). The AST's
        // own `MemberExpr.resolveType` can't see the module's scope, so it
        // returns null for a `.module` object. Resolve the member's type here
        // from the module scope — including non-`pub` bindings, so an alias to a
        // `cimport` constant (`const rlf = rl.raylib`) is typed as that cimport
        // value (a struct of its externs) rather than null.
        if (T == *ast.Expression and expr.* == .call) {
            // Resolve an overloaded callee before its type (the return type) is
            // read from the — otherwise unbound — original name.
            try self.resolveOverloadedCall(scope, &expr.call);
        }

        if (T == *ast.Expression) {
            const member_access: ?struct { object: *ast.Expression, name: []const u8 } = switch (expr.*) {
                .member => |*m| .{ .object = m.object, .name = m.member.name },
                .binary => |*b| if (b.op == .member and b.right.* == .identifier)
                    .{ .object = b.left, .name = b.right.identifier.name }
                else
                    null,
                else => null,
            };
            if (member_access) |ma| {
                if (try self.resolveExprType(scope, ma.object)) |obj_raw| {
                    const obj_type = self.unaliasType(obj_raw);
                    if (obj_type.* == .module) {
                        if (try self.requestModuleScope(obj_type.module)) |module_scope| {
                            if (module_scope.lookup(ma.name)) |member_binding| {
                                // A type member (`m.Vector3`) is a type reference,
                                // not a value — leave it to the existing path so
                                // `${m.Vector3}` still serializes to the type name.
                                if (!member_binding.is_type) {
                                    if (member_binding.type_expr) |t| return t;
                                }
                            }
                        }
                    }
                }
            }
        }

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

        // A generic type variable on either side unifies with anything (the
        // runtime is dynamic; a generic function is checked permissively). An
        // unresolved `@TypeOf(...)` (reached without a scope to resolve it) is
        // treated the same — its operand's type was validated at its own site.
        if (self.unaliasType(binding_type).* == .type_var or self.unaliasType(assignment_type).* == .type_var) return;
        if (self.unaliasType(binding_type).* == .type_capture or self.unaliasType(assignment_type).* == .type_capture) return;

        try switch (binding_type.*) {
            .failed => {},
            .type_var => {},
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
            // Unreachable: an unresolved `|T|` capture is handled permissively by
            // the early return above; kept for switch exhaustiveness.
            .type_capture => {},
            // A `Box(args…)` application is resolved to a concrete type before it
            // is stored as a binding type, so a raw application should not reach
            // here; kept for exhaustiveness.
            .type_application => {},
            // A `||` merge is resolved to a concrete `error_set` or `sum` before
            // it is stored as a binding type, so a raw merge should not reach here.
            .type_merge => {},
            // Assigning to a `type`-typed target (a `comptime T: type` param, a
            // type-returning result): the value is a type. Permissive for now;
            // type-value typing is refined as comptime evaluation lands.
            .type_type => {},
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

        if (self.unaliasType(assignment_type).* == .null) return;

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

        switch (self.unaliasType(assignment_type).*) {
            .array => |array| {
                try self.validateTypeAssignment(assignee.element, array.element, options);
            },
            // A tuple (`.{ … }` literal) coerces to an array `[]T` only when it is
            // homogeneous — every position must assign to the element type. This
            // is where "an array is a single type" is enforced.
            .tuple => |tuple| {
                for (tuple.elements) |element| {
                    try self.validateTypeAssignment(assignee.element, element, options);
                }
            },
            // An empty `.{}` is typed as an empty struct; it coerces to an empty
            // array of any element type (`var xs: []Int = .{}`). A non-empty
            // struct is not an array.
            .struct_type => |st| if (st.fields.len != 0) try self.reportAssignmentError(assignee, assignment_type, options),
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
        assignee: ast.TypeExpr.TupleType,
        assignment_type: *const ast.TypeExpr,
        options: ValidateTypeAssignmentOptions,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        // A tuple target accepts another tuple of the same arity whose positions
        // are pairwise assignable. (A tuple annotation has no surface syntax yet,
        // so this mainly covers tuple-to-tuple flow the checker infers.)
        switch (self.unaliasType(assignment_type).*) {
            .tuple => |tuple| {
                if (tuple.elements.len == assignee.elements.len) {
                    for (assignee.elements, tuple.elements) |a, v| {
                        try self.validateTypeAssignment(a, v, options);
                    }
                    return;
                }
            },
            else => {},
        }

        try self.reportAssignmentError(assignee, assignment_type, options);
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

        switch (self.unaliasType(assignment_type).*) {
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

        switch (self.unaliasType(assignment_type).*) {
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

        switch (self.unaliasType(assignment_type).*) {
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

        switch (self.unaliasType(assignment_type).*) {
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
    pub const TypeType = ast.TypeExpr{ .type_type = .{ .span = .global } };
    pub fn Array(comptime element: ast.TypeExpr) ast.TypeExpr {
        return .{ .array = .{ .element = &element, .span = .global } };
    }
};

// Builtin function signatures, generated from the shared `builtins` registry so
// a builtin is declared in exactly one place. Each is `fn String name() R`,
// where R is `ParseError!output` for a fallible (parse) builtin and `output`
// otherwise. These are top-level comptime consts, so the `&…[i]` pointers below
// are static and safe to hand to the scope at runtime.
const builtin_output_types: [builtins.all.len]ast.TypeExpr = blk: {
    var arr: [builtins.all.len]ast.TypeExpr = undefined;
    for (builtins.all, 0..) |b, i| arr[i] = b.outputType();
    break :blk arr;
};

const builtin_return_types: [builtins.all.len]ast.TypeExpr = blk: {
    var arr: [builtins.all.len]ast.TypeExpr = undefined;
    for (builtins.all, 0..) |b, i| {
        arr[i] = if (b.fallible())
            ast.TypeExpr{ .error_union = .{
                .err_set = &ast.TypeExpr.parseErrorType,
                .payload = &builtin_output_types[i],
                .span = .global,
            } }
        else
            builtin_output_types[i];
    }
    break :blk arr;
};

const builtin_fn_types: [builtins.all.len]ast.TypeExpr = blk: {
    var arr: [builtins.all.len]ast.TypeExpr = undefined;
    for (builtins.all, 0..) |_, i| {
        arr[i] = ast.TypeExpr{ .function = .{
            .params = .nonVariadic(&.{}),
            .stdin_type = &builtins.string_type,
            .return_type = &builtin_return_types[i],
            .span = .global,
        } };
    }
    break :blk arr;
};

const global_scope_definitions = [_]Definition{
    .init("Void", GlobalTypes.Void),
    .init("Int", GlobalTypes.Int),
    .init("Float", GlobalTypes.Float),
    .init("Bool", GlobalTypes.Boole),
    .init("Boole", GlobalTypes.Boole),
    .init("Boolean", GlobalTypes.Boole),
    .init("Byte", GlobalTypes.Byte),
    .init("type", GlobalTypes.TypeType),
    .init("String", GlobalTypes.Array(GlobalTypes.Byte)),
    .init("ExecutableError", ast.TypeExpr.executableErrorType),
    .init("ParseError", ast.TypeExpr.parseErrorType),
};

/// Builtin value bindings (not types) available in every module's global scope,
/// generated from the shared `builtins` registry.
const global_value_definitions = blk: {
    var defs: [builtins.all.len]Definition = undefined;
    for (builtins.all, 0..) |b, i| {
        defs[i] = .{ .identifier = .{ .name = b.name, .span = .global }, .type_expr = &builtin_fn_types[i] };
    }
    break :blk defs;
};

fn addGlobalScope(allocator: std.mem.Allocator, scope: *Scope) !*Scope {
    const global_scope = try scope.addChild(allocator, scope.span);

    for (global_scope_definitions) |definition| {
        try global_scope.declare(allocator, definition.identifier, definition.type_expr, true, false);
        global_scope.bindings.getPtr(definition.identifier.name).?.is_global = true;
    }

    for (global_value_definitions) |definition| {
        try global_scope.declare(allocator, definition.identifier, definition.type_expr, true, false);
        global_scope.bindings.getPtr(definition.identifier.name).?.is_global = true;
    }

    return global_scope;
}
