const std = @import("std");
const ast = @import("../frontend/ast.zig");

pub const Scope = struct {
    bindings: std.StringArrayHashMapUnmanaged(Binding) = .empty,
    children: std.ArrayList(Scope) = .empty,
    parent: ?*Scope = null,

    pub const Error =
        std.mem.Allocator.Error ||
        error{
            IdentifierAlreadyDeclared,
            TypeNotFound,
        };

    pub fn init() Scope {
        return .{};
    }

    pub fn initWithParent(parent: *Scope) Scope {
        return .{
            .parent = parent,
        };
    }

    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.bindings.deinit(allocator);
        for (self.children.items) |child| child.deinit();
        self.children.deinit(allocator);
    }

    pub fn addChild(self: *Scope, allocator: std.mem.Allocator) Error!*Scope {
        try self.children.append(allocator, .initWithParent(self));
        return &self.children.items[self.children.items.len - 1];
    }

    pub fn lookup(self: *Scope, name: []const u8) Error!?*Binding {
        return self.bindings.getPtr(name) orelse {
            const parent = self.parent orelse return null;
            return parent.lookup(name);
        };
    }

    pub fn declare(
        self: *Scope,
        allocator: std.mem.Allocator,
        name: []const u8,
        type_expr: ?*ast.TypeExpr,
    ) Error!void {
        const entry = try self.bindings.getOrPut(allocator, name);

        if (entry.found_existing) {
            return error.IdentifierAlreadyDeclared;
        }

        entry.value_ptr.* = .{
            .name = name,
            .type_expr = type_expr,
        };
    }

    pub const Binding = struct {
        name: []const u8,
        type_expr: ?*ast.TypeExpr,
    };
};
