const std = @import("std");
const ast = @import("../frontend/ast.zig");
const rainbow = @import("../rainbow.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;

const Scope = @import("scope.zig").Scope;

const logging_name = "TYPE_CHECKER";
const prefix_color = rainbow.beginColor(.blue);
const span_color = rainbow.beginBgColor(.green) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

pub const TypeChecker = struct {
    arena: std.heap.ArenaAllocator,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    logging_enabled: bool,
    document_store: DocumentStore,
    modules: std.StringArrayHashMapUnmanaged(*Scope),

    pub const Error = Scope.Error ||
        std.fs.File.OpenError ||
        std.fs.File.ReadError ||
        std.fs.File.StatError ||
        std.Io.Writer.Error ||
        DocumentStore.Error ||
        error{
            FileTooBig,
            UnsupportedExpression,
            UnsupportedStatement,
            UnsupportedMemberAccess,
            BindingPatternNotSupported,
            IdentifierNotFound,
            MemberObjectTypeUndefined,
            UnresolvedTypeLiteral,
            MemberAccessOnOptional,
            ForSourcesAndBindingsNeedToBeTheSameLength,
            ErrorNotInErrorSet,
            TypeMismatch,
            DocumentNotParsed,
            ModuleNotFound,
        };

    pub const Diagnostic = struct {
        err: Error,
        data: Data,
        message: []const u8,
        severity: Severity,

        pub const Data = union(enum) {
            expr: *ast.Expression,
            span_: ast.Span,

            pub fn span(self: Data) ast.Span {
                return switch (self) {
                    .expr => |expr| expr.span(),
                    .span_ => |span_| span_,
                };
            }
        };

        pub const Severity = enum {
            @"error",
            warning,
            information,
            hint,
        };

        pub fn span(self: Diagnostic) ast.Span {
            return self.data.span();
        }
    };

    pub const Result = union(enum) {
        err: []Diagnostic,
        success,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        document_store: DocumentStore,
    ) TypeChecker {
        const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_" ++ logging_name) catch null;
        defer if (logging_enabled_s) |le| allocator.free(le);
        const logging_enabled = if (logging_enabled_s) |le| std.mem.eql(u8, le, "1") else false;

        return .{
            .arena = .init(allocator),
            .logging_enabled = logging_enabled,
            .document_store = document_store,
            .modules = .empty,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
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
            .data = .{ .span_ = span },
            .message = try std.fmt.allocPrint(
                self.arena.allocator(),
                fmt,
                args,
            ),
            .severity = severity,
        });
    }

    fn reportAssignmentError(
        self: *TypeChecker,
        span: ast.Span,
        expected: anytype,
        actual: anytype,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, span);

        try self.reportSpanError(span, Error.TypeMismatch, .@"error", "expected type {f}, actual: {f}", .{ expected, actual });
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

    pub fn typeCheck(self: *TypeChecker, path: []const u8) Error!Result {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name ++ ": {s}", .{path});

        _ = try self.scopesFromAst(path);

        return self.compileResult();
    }

    fn scopesFromAst(self: *TypeChecker, path: []const u8) Error!*Scope {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name ++ ": {s}", .{path});

        const scope = try self.arena.allocator().create(Scope);
        scope.* = .init();

        try self.modules.put(
            self.arena.allocator(),
            try self.arena.allocator().dupe(u8, path),
            scope,
        );

        var script = try self.document_store.getAst(path) orelse return error.DocumentNotParsed;
        try self.runBlock(scope, &script);

        return scope;
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

        const block_scope = try scope.addChild(self.arena.allocator());
        try self.runBlock(block_scope, block);
    }

    fn runStatement(self: *TypeChecker, scope: *Scope, statement: *ast.Statement) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, statement.span());

        // if (statement.* != .expression) {
        try self.log("<{s}>", .{@tagName(statement.*)});
        try self.logTypeCheckStatement(statement);
        // }

        return switch (statement.*) {
            .binding_decl => |*binding_decl| self.runBindingDecl(scope, binding_decl),
            .fn_decl => |*fn_decl| self.runFnDecl(scope, fn_decl),
            .expression => |*expr_stmt| self.runExpressionStatement(scope, expr_stmt),
            else => error.UnsupportedStatement,
        };
    }

    fn runBindingDecl(
        self: *TypeChecker,
        scope: *Scope,
        binding_decl: *ast.BindingDecl,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, binding_decl.span);

        try self.runExpression(scope, binding_decl.initializer);

        try self.runBindingPattern(
            scope,
            binding_decl.pattern,
            try binding_decl.initializer.resolveType(
                self.arena.allocator(),
                scope,
            ),
        );
    }

    fn runBindingPattern(
        self: *TypeChecker,
        scope: *Scope,
        pattern: *ast.BindingPattern,
        type_expr: ?*ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, pattern.span());

        switch (pattern.*) {
            .identifier => |identifier| {
                scope.declare(
                    self.arena.allocator(),
                    identifier.name,
                    type_expr,
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

        try scope.declare(
            self.arena.allocator(),
            fn_decl.name.name,
            try fn_decl.resolveType(self.arena.allocator(), scope),
        );

        const fn_scope = try scope.addChild(self.arena.allocator());

        for (fn_decl.params) |*param| {
            const param_type = try param.resolveType(
                self.arena.allocator(),
                fn_scope,
            );
            try self.runBindingPattern(
                fn_scope,
                param.pattern,
                param_type,
            );
        }

        switch (fn_decl.body) {
            .expression => |expr| try self.runExpression(fn_scope, expr),
            .block => |*block| try self.runBlock(fn_scope, block),
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
            else => return error.UnsupportedExpression,
        };
    }

    pub fn runForExpr(self: *TypeChecker, scope: *Scope, for_expr: *ast.ForExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, for_expr.span);

        if (for_expr.sources.len != for_expr.capture.bindings.len) {
            return error.ForSourcesAndBindingsNeedToBeTheSameLength;
        }

        const for_scope = try scope.addChild(self.arena.allocator());

        for (for_expr.sources, for_expr.capture.bindings) |source, pattern| {
            try self.runExpression(scope, source);
            const type_expr = try source.resolveType(self.arena.allocator(), scope);
            try self.runBindingPattern(for_scope, pattern, type_expr);
        }

        try self.runBlock(for_scope, &for_expr.body);
    }

    pub fn runIfExpr(self: *TypeChecker, scope: *Scope, if_expr: *ast.IfExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, if_expr.span);

        try self.runExpression(scope, if_expr.condition);
        const then_scope = try scope.addChild(self.arena.allocator());
        try self.runBlock(then_scope, &if_expr.then_block);
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
            .block => |*block| try self.runBlockInNewScope(scope, block),
        }
    }

    pub fn runBinary(self: *TypeChecker, scope: *Scope, binary: *ast.BinaryExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, binary.span);

        try self.runExpression(scope, binary.left);
        try self.runExpression(scope, binary.right);
        const left_type = try binary.left.resolveType(self.arena.allocator(), scope);
        const right_type = try binary.right.resolveType(self.arena.allocator(), scope);
        _ = left_type;
        _ = right_type;

        // TODO: implement all kinds of semantic checks here
    }

    pub fn runMember(self: *TypeChecker, scope: *Scope, member: *ast.MemberExpr) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, member.span);

        try self.runExpression(scope, member.object);
        const object_type = try member.object.resolveType(self.arena.allocator(), scope) orelse {
            return error.MemberObjectTypeUndefined;
        };

        switch (object_type.*) {
            // .identifier => return error.UnresolvedTypeLiteral,
            .optional => return error.MemberAccessOnOptional,
            .promise, .error_union, .error_set, .err, .array, .struct_type, .tuple, .function, .integer, .float, .boolean, .byte => return error.UnsupportedMemberAccess,
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
        const module_scope = try self.requestModuleScope(module) orelse return;
        _ = try self.runIdentifier(module_scope, identifier);
    }

    pub fn runPipeline(self: *TypeChecker, scope: *Scope, pipeline: *ast.Pipeline) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, pipeline.span);

        for (pipeline.stages) |*stage| {
            try self.runPipelineStage(scope, stage);
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

        if (try scope.lookup(identifier.name) == null) {
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

        const module_type = try import.resolveType(self.arena.allocator(), scope) orelse {
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
            try assignment.expr.resolveType(
                self.arena.allocator(),
                scope,
            ),
        );
    }

    fn materializeBindingType(
        self: *TypeChecker,
        scope: *Scope,
        name: []const u8,
        maybe_type_expr: ?*ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});

        const type_expr = maybe_type_expr orelse return;

        const binding_ref = try scope.lookup(name) orelse return error.IdentifierNotFound;
        if (binding_ref.type_expr) |binding_type| {
            try self.validateTypeAssignment(
                binding_type,
                type_expr,
            );
        }

        binding_ref.type_expr = type_expr;
    }

    fn validateTypeAssignment(
        self: *TypeChecker,
        binding_type: *ast.TypeExpr,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        try switch (binding_type.*) {
            // .identifier => return Error.UnresolvedTypeLiteral,
            .optional => |optional| self.validateTypeAssignmentOptional(
                optional,
                assignment_type,
            ),
            .promise => |promise| self.validateTypeAssignmentPromise(
                promise,
                assignment_type,
            ),
            .error_union => |error_union| self.validateTypeAssignmentErrorUnion(
                error_union,
                assignment_type,
            ),
            .error_set => |error_set| self.validateTypeAssignmentErrorSet(
                error_set,
                assignment_type,
            ),
            .err => |err| self.validateTypeAssignmentErrorType(
                err,
                assignment_type,
            ),
            .array => |array| self.validateTypeAssignmentArray(
                array,
                assignment_type,
            ),
            .struct_type => |struct_type| self.validateTypeAssignmentStruct(
                struct_type,
                assignment_type,
            ),
            .module => |module| self.validateTypeAssignmentModule(
                module,
                assignment_type,
            ),
            .tuple => |tuple| self.validateTypeAssignmentTuple(
                tuple,
                assignment_type,
            ),
            .function => |function| self.validateTypeAssignmentFunction(
                function,
                assignment_type,
            ),
            .integer => |*integer| self.validateTypeAssignmentInteger(
                integer,
                assignment_type,
            ),
            .float => |*float| self.validateTypeAssignmentFloat(
                float,
                assignment_type,
            ),
            .boolean => |*boolean| self.validateTypeAssignmentBoolean(
                boolean,
                assignment_type,
            ),
            .byte => |*byte| self.validateTypeAssignmentByte(
                byte,
                assignment_type,
            ),
            // .lazy => |lazy| self.validateTypeAssignmentLazy(
            //     lazy,
            //     assignment_type,
            // ),
        };
    }

    pub fn validateTypeAssignmentOptional(
        self: *TypeChecker,
        assignee: ast.TypeExpr.PrefixType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .optional => |optional| try self.validateTypeAssignment(
                assignee.child,
                optional.child,
            ),
            else => try self.validateTypeAssignment(assignee.child, assignment_type),
        }
    }

    pub fn validateTypeAssignmentPromise(
        self: *TypeChecker,
        assignee: ast.TypeExpr.PrefixType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .promise => |promise| try self.validateTypeAssignment(
                assignee.child,
                promise.child,
            ),
            else => try self.validateTypeAssignment(assignee.child, assignment_type),
        }
    }

    pub fn validateTypeAssignmentErrorUnion(
        self: *TypeChecker,
        assignee: ast.TypeExpr.ErrorUnion,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .error_union => |error_union| {
                try self.validateTypeAssignmentErrorSet(
                    assignee.err_set.error_set,
                    error_union.err_set,
                );
                try self.validateTypeAssignment(
                    assignee.payload,
                    error_union.payload,
                );
            },
            .err => try self.validateErrorInSet(assignee.err_set.error_set, assignment_type),
            else => try self.validateTypeAssignment(assignee.payload, assignment_type),
        }
    }

    pub fn validateTypeAssignmentErrorSet(
        self: *TypeChecker,
        error_set: ast.TypeExpr.ErrorSet,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        for (assignment_type.error_set.error_types) |error_set_type| {
            try self.validateErrorInSet(error_set, error_set_type);
        }
    }

    pub fn validateErrorInSet(
        self: *TypeChecker,
        error_set: ast.TypeExpr.ErrorSet,
        err: *ast.TypeExpr,
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
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        // TODO: see if we need to have a resolve type on assignment_type here
        if (error_type.error_payload == assignment_type) {
            return;
        }

        try self.reportAssignmentError(
            assignment_type.span(),
            error_type,
            assignment_type,
        );
    }

    pub fn validateTypeAssignmentArray(
        self: *TypeChecker,
        assignee: ast.TypeExpr.ArrayType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .array => |array| {
                try self.validateTypeAssignment(assignee.element, array.element);
            },
            else => try self.reportAssignmentError(assignment_type.span(), assignee, assignment_type),
        }
    }

    pub fn validateTypeAssignmentStruct(
        self: *TypeChecker,
        _: ast.TypeExpr.StructType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        // TODO: implement
    }

    pub fn validateTypeAssignmentModule(
        self: *TypeChecker,
        assignee: ast.TypeExpr.ModuleType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .module => |module| if (std.mem.eql(u8, assignee.path, module.path)) return,
            else => {},
        }

        try self.reportAssignmentError(assignment_type.span(), assignee, assignment_type);
    }

    pub fn validateTypeAssignmentTuple(
        self: *TypeChecker,
        _: ast.TypeExpr.TupleType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());
    }

    pub fn validateTypeAssignmentFunction(
        self: *TypeChecker,
        _: ast.TypeExpr.FunctionType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        // TODO: should this even be implement or should we have dynamic function pointers? (no?)
    }

    pub fn validateTypeAssignmentInteger(
        self: *TypeChecker,
        assignee: *ast.TypeExpr.PrimitiveType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .integer => {},
            else => try self.reportAssignmentError(
                assignment_type.span(),
                @as(*ast.TypeExpr, @fieldParentPtr("integer", assignee)),
                assignment_type,
            ),
        }
    }

    pub fn validateTypeAssignmentFloat(
        self: *TypeChecker,
        assignee: *ast.TypeExpr.PrimitiveType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .integer, .float => {},
            else => try self.reportAssignmentError(
                assignment_type.span(),
                @as(*ast.TypeExpr, @fieldParentPtr("float", assignee)),
                assignment_type,
            ),
        }
    }

    pub fn validateTypeAssignmentBoolean(
        self: *TypeChecker,
        assignee: *ast.TypeExpr.PrimitiveType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .boolean => {},
            else => try self.reportAssignmentError(
                assignment_type.span(),
                @as(*ast.TypeExpr, @fieldParentPtr("boolean", assignee)),
                assignment_type,
            ),
        }
    }

    pub fn validateTypeAssignmentByte(
        self: *TypeChecker,
        assignee: *ast.TypeExpr.PrimitiveType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());

        switch (assignment_type.*) {
            .byte => {},
            else => try self.reportAssignmentError(
                assignment_type.span(),
                @as(*ast.TypeExpr, @fieldParentPtr("byte", assignee)),
                assignment_type,
            ),
        }
    }

    pub fn validateTypeAssignmentLazy(
        self: *TypeChecker,
        _: ast.TypeExpr.LazyType,
        assignment_type: *ast.TypeExpr,
    ) Error!void {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.logTypeCheckTrace(@src().fn_name, assignment_type.span());
    }

    fn compileResult(self: *TypeChecker) Error!Result {
        errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
        try self.log(@src().fn_name, .{});

        if (self.diagnostics.items.len > 0) {
            return .{ .err = self.diagnostics.items };
        }

        return .success;
    }
};
