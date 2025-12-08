const std = @import("std");
const ast = @import("../frontend/ast.zig");

const Scope = @import("scope.zig").Scope;

pub const TypeChecker = struct {
    ast: *ast.Script,
    arena: std.heap.ArenaAllocator,
    diagnostics: std.ArrayList(Diagnostic) = .empty,

    pub const Error = Scope.Error || error{
        UnsupportedExpression,
        UnsupportedStatement,
        UnsupportedMemberAccess,
        BindingPatternNotSupported,
        IdentifierNotFound,
        MemberObjectTypeUndefined,
        UnresolvedTypeLiteral,
        MemberAccessOnOptional,
        ForSourcesAndBindingsNeedToBeTheSameLength,
    };

    pub const Diagnostic = struct {
        err: Error,
        expr: *ast.Expression,
        message: []const u8,
        severity: Severity,

        pub const Severity = enum {
            @"error",
            warning,
            information,
            hint,
        };
    };

    pub const Result = union(enum) {
        err: []Diagnostic,
        success,
    };

    pub fn init(allocator: std.mem.Allocator, script: *ast.Script) TypeChecker {
        return .{
            .ast = script,
            .arena = .init(allocator),
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        self.arena.deinit();
    }

    fn reportExprError(
        self: *TypeChecker,
        expr: *ast.Expression,
        err: Error,
        message: []const u8,
        severity: Diagnostic.Severity,
    ) Error!void {
        try self.diagnostics.append(self.arena.allocator(), .{
            .err = err,
            .expr = expr,
            .message = message,
            .severity = severity,
        });
    }

    pub fn typeCheck(self: *TypeChecker) Error!Result {
        var scopes = try self.scopesFromAst();
        try self.resolveLazyTypes(&scopes);
        return self.typeCheckInner(&scopes);
    }

    fn scopesFromAst(self: *TypeChecker) Error!Scope {
        var scope = Scope.init();

        try self.runBlock(&scope, self.ast);

        return scope;
    }

    fn runBlock(self: *TypeChecker, scope: *Scope, block: *ast.Block) Error!void {
        for (block.statements) |statement| {
            try self.runStatement(scope, statement);
        }
    }

    fn runStatement(self: *TypeChecker, scope: *Scope, statement: *ast.Statement) Error!void {
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
        try self.runBindingPattern(
            scope,
            binding_decl.pattern,
            try binding_decl.initializer.resolveType(self.arena.allocator(), scope),
        );
    }

    fn runBindingPattern(
        self: *TypeChecker,
        scope: *Scope,
        pattern: *ast.BindingPattern,
        type_expr: ?*ast.TypeExpr,
    ) Error!void {
        switch (pattern.*) {
            .identifier => |identifier| {
                try scope.declare(
                    self.arena.allocator(),
                    identifier.name,
                    type_expr,
                );
            },
            .discard => {},
            .tuple, .record => return error.BindingPatternNotSupported,
        }
    }

    fn runFnDecl(self: *TypeChecker, scope: *Scope, fn_decl: *ast.FunctionDecl) Error!void {
        try scope.declare(
            self.arena.allocator(),
            fn_decl.name.name,
            try fn_decl.resolveType(self.arena.allocator(), scope),
        );
    }

    fn runExpressionStatement(
        self: *TypeChecker,
        scope: *Scope,
        expr_stmt: *ast.ExpressionStmt,
    ) Error!void {
        try self.runExpression(scope, expr_stmt.expression);
    }

    fn runExpression(
        self: *TypeChecker,
        scope: *Scope,
        expr: *ast.Expression,
    ) Error!void {
        try switch (expr.*) {
            .identifier => |*identifier| self.runIdentifier(scope, identifier),
            .literal => |*literal| self.runLiteral(scope, literal),
            .array => |*array| self.runArray(scope, array),
            .range => |*range| self.runRange(scope, range),
            .pipeline => |*pipeline| self.runPipeline(scope, pipeline),
            .member => |*member| self.runMember(scope, member),
            .binary => |*binary| self.runBinary(scope, binary),
            .block => |*block| self.runBlock(scope, block),
            .if_expr => |*if_expr| self.runIfExpr(scope, if_expr),
            .for_expr => |*for_expr| self.runForExpr(scope, for_expr),
            .import_expr => |*import_expr| self.runImportExpr(scope, import_expr),
            .assignment => |*assignment| self.runAssignment(scope, assignment),
            else => return error.UnsupportedExpression,
        };
    }

    pub fn runForExpr(self: *TypeChecker, scope: *Scope, for_expr: *ast.ForExpr) Error!void {
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
        try self.runExpression(scope, if_expr.condition);
        const then_scope = try scope.addChild(self.arena.allocator());
        try self.runBlock(then_scope, &if_expr.then_block);
        if (if_expr.else_branch) |*else_branch| {
            try self.runElseBranch(then_scope, else_branch);
        }
    }

    pub fn runElseBranch(self: *TypeChecker, scope: *Scope, else_branch: *ast.IfExpr.ElseBranch) Error!void {
        try self.runIfExpr(scope, else_branch.if_expr);
        const else_scope = try scope.addChild(self.arena.allocator());
        try self.runBlock(else_scope, &else_branch.block);
    }

    pub fn runBinary(self: *TypeChecker, scope: *Scope, binary: *ast.BinaryExpr) Error!void {
        try self.runExpression(scope, binary.left);
        try self.runExpression(scope, binary.right);
        const left_type = try binary.left.resolveType(self.arena.allocator(), scope);
        const right_type = try binary.right.resolveType(self.arena.allocator(), scope);
        _ = left_type;
        _ = right_type;

        // TODO: implement all kinds of semantic checks here
    }

    pub fn runMember(self: *TypeChecker, scope: *Scope, member: *ast.MemberExpr) Error!void {
        try self.runExpression(scope, member.object);
        const object_type = try member.object.resolveType(self.arena.allocator(), scope) orelse {
            return error.MemberObjectTypeUndefined;
        };

        switch (object_type.*) {
            .identifier => return error.UnresolvedTypeLiteral,
            .optional => return error.MemberAccessOnOptional,
            .promise, .error_union, .array, .struct_type, .tuple, .function, .integer, .float, .boolean => return error.UnsupportedMemberAccess,
            .lazy => {
                // TODO: Figure out what to do here, do we setup a check after the lazy type has been resolved, or do we always assume we have the types resolved here? <|:)---<
                return error.UnsupportedMemberAccess;
            },
        }

        try self.runIdentifier(scope, member.member);
    }

    pub fn runPipeline(self: *TypeChecker, scope: *Scope, pipeline: *ast.Pipeline) Error!void {
        for (pipeline.stages) |*stage| {
            try self.runPipelineStage(scope, stage);
        }
    }

    pub fn runPipelineStage(
        self: *TypeChecker,
        scope: *Scope,
        stage: *const ast.PipelineStage,
    ) Error!void {
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
        try self.runCommandPart(scope, &command.name);
        for (command.args) |*expr| try self.runCommandPart(scope, expr);
    }

    pub fn runCommandPart(
        self: *TypeChecker,
        scope: *Scope,
        part: *const ast.CommandPart,
    ) Error!void {
        switch (part.*) {
            .expr => |expr| try self.runExpression(scope, expr),
            .word => {},
            .string => |*string_literal| try self.runStringLiteral(scope, string_literal),
        }
    }

    pub fn runRange(self: *TypeChecker, scope: *Scope, range: *ast.RangeLiteral) Error!void {
        try self.runExpression(scope, range.start);
        if (range.end) |end| try self.runExpression(scope, end);
    }

    pub fn runArray(self: *TypeChecker, scope: *Scope, array: *ast.ArrayLiteral) Error!void {
        for (array.elements) |expr| try self.runExpression(scope, expr);
    }

    pub fn runLiteral(self: *TypeChecker, scope: *Scope, literal: *ast.Literal) Error!void {
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
        for (string_literal.segments) |*segment| {
            try self.runStringLiteralSegment(scope, segment);
        }
    }

    pub fn runStringLiteralSegment(
        self: *TypeChecker,
        scope: *Scope,
        segment: *const ast.StringLiteral.Segment,
    ) Error!void {
        switch (segment.*) {
            .interpolation => |expr| try self.runExpression(scope, expr),
            .text => {},
        }
    }

    pub fn runIdentifier(
        _: *TypeChecker,
        scope: *Scope,
        identifier: *ast.Identifier,
    ) Error!void {
        if (try scope.lookup(identifier.name) == null) {
            return error.IdentifierNotFound;
        }
    }

    pub fn runImportExpr(_: *TypeChecker, _: *Scope, _: *ast.ImportExpr) Error!void {
        // TODO: implement module type scopes
        return;
    }

    pub fn runAssignment(self: *TypeChecker, scope: *Scope, assignment: *ast.Assignment) Error!void {
        self.runIdentifier(scope, &assignment.identifier) catch |err| {
            const parent_expr: *ast.Expression = @fieldParentPtr("assignment", assignment);
            try self.reportExprError(
                parent_expr,
                err,
                try std.fmt.allocPrint(self.arena.allocator(), "identifier \"{s}\" not declared", .{assignment.identifier.name}),
                .@"error",
            );
        };
        try self.runExpression(scope, assignment.expr);
    }

    fn resolveLazyTypes(self: *TypeChecker, scope: *Scope) Error!void {
        _ = self;
        _ = scope;
    }

    fn typeCheckInner(self: *TypeChecker, _: *Scope) Error!Result {
        if (self.diagnostics.items.len > 0) {
            return .{ .err = self.diagnostics.items };
        }

        return .success;
    }
};
