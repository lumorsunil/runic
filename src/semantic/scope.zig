const std = @import("std");
const ast = @import("../frontend/ast.zig");

// TODO: add spans to the identifiers so that we can go to where they were defined
pub const Scope = struct {
    bindings: std.StringArrayHashMapUnmanaged(Binding) = .empty,
    children: std.ArrayList(*Scope) = .empty,
    parent: ?*Scope = null,
    span: ast.Span,

    pub const Error =
        std.mem.Allocator.Error ||
        error{
            IdentifierAlreadyDeclared,
            TypeNotFound,
        };

    pub fn init(span: ast.Span) Scope {
        return .{ .span = span };
    }

    pub fn initWithParent(parent: *Scope, span: ast.Span) Scope {
        return .{ .parent = parent, .span = span };
    }

    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.bindings.deinit(allocator);
        for (self.children.items) |child| child.deinit();
        self.children.deinit(allocator);
    }

    pub fn addChild(self: *Scope, allocator: std.mem.Allocator, span: ast.Span) Error!*Scope {
        const child = try allocator.create(Scope);
        child.* = .initWithParent(self, span);
        try self.children.append(allocator, child);
        return self.children.getLast();
    }

    pub fn lookup(self: *Scope, name: []const u8) ?*Binding {
        return self.bindings.getPtr(name) orelse {
            const parent = self.parent orelse return null;
            return parent.lookup(name);
        };
    }

    pub fn declare(
        self: *Scope,
        allocator: std.mem.Allocator,
        identifier: ast.Identifier,
        type_expr: ?*const ast.TypeExpr,
        is_mutable: bool,
    ) Error!void {
        const entry = try self.bindings.getOrPut(allocator, identifier.name);

        if (entry.found_existing) {
            return error.IdentifierAlreadyDeclared;
        }

        entry.value_ptr.* = .{
            .identifier = identifier,
            .type_expr = type_expr,
            .is_mutable = is_mutable,
        };
    }

    pub const Binding = struct {
        identifier: ast.Identifier,
        type_expr: ?*const ast.TypeExpr,
        is_mutable: bool,
    };
};
