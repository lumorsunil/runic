const std = @import("std");
const token = @import("../frontend/token.zig");

pub const Span = token.Span;

/// SymbolKind tags the type of entity stored in the table. Keeping explicit
/// kinds up front lets future semantic passes reserve namespaces for
/// functions, error sets, modules, or loop captures without reworking the
/// data structures.
pub const SymbolKind = enum {
    variable,
    function,
    error_set,
    module,
    parameter,
    capture,
};

/// Metadata tracked for every binding inserted into the table.
pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    is_mutable: bool,
    declared_span: Span,
    scope_depth: usize,

    pub fn allowsAssignment(self: *const Symbol) bool {
        return switch (self.kind) {
            .variable, .parameter, .capture => true,
            else => false,
        };
    }
};

pub const SymbolDesc = struct {
    kind: SymbolKind,
    is_mutable: bool = false,
    span: Span,
};

pub const CaptureBindingDesc = struct {
    name: []const u8,
    span: Span,
    is_mutable: bool = false,
};

const Scope = struct {
    bindings: std.StringHashMapUnmanaged(*Symbol) = .{},

    fn init() Scope {
        return .{ .bindings = .{} };
    }

    fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.bindings.deinit(allocator);
    }
};

pub const Violation = union(enum) {
    duplicate_binding: DuplicateBinding,
    unknown_symbol: UnknownSymbol,
    immutable_reassignment: ImmutableReassignment,

    pub const DuplicateBinding = struct {
        name: []const u8,
        original_span: Span,
        redeclaration_span: Span,
    };

    pub const UnknownSymbol = struct {
        name: []const u8,
        span: Span,
    };

    pub const ImmutableReassignment = struct {
        name: []const u8,
        assignment_span: Span,
        original_span: Span,
    };
};

pub const ResolveResult = struct {
    symbol: *Symbol,
    depth: usize,
};

/// SymbolTable tracks lexical scopes and enforces assignment rules for
/// immutable variables. Symbols live in an arena so downstream analysis can
/// safely hold pointers to metadata without worrying about reallocations.
pub const SymbolTable = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    scopes: std.ArrayListUnmanaged(Scope) = .{},
    last_violation: ?Violation = null,

    pub fn init(backing_allocator: std.mem.Allocator) SymbolTable {
        return .{
            .allocator = backing_allocator,
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .scopes = .{},
            .last_violation = null,
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        for (self.scopes.items) |*scope| {
            scope.deinit(self.scopeAllocator());
        }
        self.scopes.deinit(self.scopeAllocator());
        self.arena.deinit();
    }

    fn scopeAllocator(self: *SymbolTable) std.mem.Allocator {
        return self.allocator;
    }

    fn symbolAllocator(self: *SymbolTable) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn scopeDepth(self: *SymbolTable) usize {
        return self.scopes.items.len;
    }

    fn requireScope(self: *SymbolTable) !*Scope {
        if (self.scopes.items.len == 0) return error.ScopeUnderflow;
        return &self.scopes.items[self.scopes.items.len - 1];
    }

    pub fn enterScope(self: *SymbolTable) !void {
        try self.scopes.append(self.scopeAllocator(), Scope.init());
    }

    pub fn exitScope(self: *SymbolTable) !void {
        if (self.scopes.items.len == 0) return error.ScopeUnderflow;
        const allocator = self.scopeAllocator();
        self.scopes.items[self.scopes.items.len - 1].deinit(allocator);
        _ = self.scopes.pop();
    }

    pub fn declare(self: *SymbolTable, name: []const u8, desc: SymbolDesc) !*Symbol {
        self.last_violation = null;
        var scope = try self.requireScope();

        if (scope.bindings.get(name)) |existing| {
            self.last_violation = .{ .duplicate_binding = .{
                .name = name,
                .original_span = existing.*.declared_span,
                .redeclaration_span = desc.span,
            } };
            return error.DuplicateSymbol;
        }

        const symbol = try self.symbolAllocator().create(Symbol);
        symbol.* = .{
            .name = name,
            .kind = desc.kind,
            .is_mutable = desc.is_mutable,
            .declared_span = desc.span,
            .scope_depth = self.scopeDepth() - 1,
        };

        try scope.bindings.put(self.scopeAllocator(), name, symbol);
        return symbol;
    }

    pub fn resolve(self: *SymbolTable, name: []const u8) ?ResolveResult {
        var depth = self.scopeDepth();
        while (depth > 0) : (depth -= 1) {
            const scope_idx = depth - 1;
            if (self.scopes.items[scope_idx].bindings.get(name)) |symbol| {
                return .{ .symbol = symbol, .depth = scope_idx };
            }
        }
        return null;
    }

    pub fn assign(self: *SymbolTable, name: []const u8, span: Span) !*Symbol {
        self.last_violation = null;
        const resolved = self.resolve(name) orelse {
            self.last_violation = .{ .unknown_symbol = .{
                .name = name,
                .span = span,
            } };
            return error.UnknownSymbol;
        };

        if (!resolved.symbol.allowsAssignment() or !resolved.symbol.*.is_mutable) {
            self.last_violation = .{ .immutable_reassignment = .{
                .name = name,
                .assignment_span = span,
                .original_span = resolved.symbol.*.declared_span,
            } };
            return error.ImmutableBinding;
        }

        return resolved.symbol;
    }

    pub fn violation(self: *SymbolTable) ?Violation {
        return self.last_violation;
    }
};

pub const CaptureScope = struct {
    table: *SymbolTable,
    active: bool = false,

    pub const Error = error{
        AlreadyActive,
        NotActive,
    } || error{ ScopeUnderflow, DuplicateSymbol };

    pub fn init(table: *SymbolTable) CaptureScope {
        return .{ .table = table, .active = false };
    }

    pub fn enter(self: *CaptureScope, bindings: []const CaptureBindingDesc) Error!void {
        if (self.active) return error.AlreadyActive;
        try self.table.enterScope();
        self.active = true;
        errdefer {
            self.active = false;
            self.table.exitScope() catch {};
        }

        for (bindings) |binding| {
            _ = try self.table.declare(binding.name, .{
                .kind = .capture,
                .is_mutable = binding.is_mutable,
                .span = binding.span,
            });
        }
    }

    pub fn exit(self: *CaptureScope) Error!void {
        if (!self.active) return error.NotActive;
        self.active = false;
        try self.table.exitScope();
    }
};

fn fakeSpan(offset: usize) Span {
    return .{
        .start = .{ .line = 1, .column = offset + 1, .offset = offset },
        .end = .{ .line = 1, .column = offset + 2, .offset = offset + 1 },
    };
}

test "symbol table rejects duplicate bindings in the same scope" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    try table.enterScope();

    _ = try table.declare("value", .{ .kind = .variable, .span = fakeSpan(1) });
    try std.testing.expectError(error.DuplicateSymbol, table.declare(
        "value",
        .{ .kind = .variable, .span = fakeSpan(5) },
    ));

    const violation = table.violation().?;
    switch (violation) {
        .duplicate_binding => |dup| {
            try std.testing.expectEqualStrings("value", dup.name);
            try std.testing.expectEqual(@as(usize, 1), dup.original_span.start.offset);
            try std.testing.expectEqual(@as(usize, 5), dup.redeclaration_span.start.offset);
        },
        else => return TestError.UnexpectedViolation,
    }
}

test "scopes shadow outer bindings and enforce mutability" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    try table.enterScope();
    _ = try table.declare("count", .{ .kind = .variable, .span = fakeSpan(0) });

    try table.enterScope();
    _ = try table.declare("count", .{ .kind = .variable, .is_mutable = true, .span = fakeSpan(10) });

    const resolved_inner = table.resolve("count") orelse return TestError.SymbolNotFound;
    try std.testing.expectEqual(@as(usize, 1), resolved_inner.depth);
    try std.testing.expect(resolved_inner.symbol.*.is_mutable);

    _ = try table.assign("count", fakeSpan(11));
    try table.exitScope();

    const resolved_outer = table.resolve("count") orelse return TestError.SymbolNotFound;
    try std.testing.expectEqual(@as(usize, 0), resolved_outer.depth);
    try std.testing.expect(!resolved_outer.symbol.*.is_mutable);

    try std.testing.expectError(error.ImmutableBinding, table.assign("count", fakeSpan(20)));
    const violation = table.violation().?;
    switch (violation) {
        .immutable_reassignment => |info| {
            try std.testing.expectEqualStrings("count", info.name);
            try std.testing.expectEqual(@as(usize, 20), info.assignment_span.start.offset);
            try std.testing.expectEqual(@as(usize, 0), info.original_span.start.offset);
        },
        else => return TestError.UnexpectedViolation,
    }
}

test "assignment to unknown names surfaces a violation" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    try table.enterScope();

    try std.testing.expectError(error.UnknownSymbol, table.assign("missing", fakeSpan(1)));
    const violation = table.violation().?;
    switch (violation) {
        .unknown_symbol => |unknown| {
            try std.testing.expectEqualStrings("missing", unknown.name);
            try std.testing.expectEqual(@as(usize, 1), unknown.span.start.offset);
        },
        else => return TestError.UnexpectedViolation,
    }
}

test "capture scope binds names for optional-aware conditionals" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    try table.enterScope();

    var scope = CaptureScope.init(&table);
    const bindings = [_]CaptureBindingDesc{
        .{ .name = "value", .span = fakeSpan(2) },
    };
    try scope.enter(&bindings);

    const resolved = table.resolve("value") orelse return TestError.SymbolNotFound;
    try std.testing.expectEqual(SymbolKind.capture, resolved.symbol.*.kind);

    try scope.exit();
    try std.testing.expect(!scope.active);
    try std.testing.expect(table.resolve("value") == null);

    try std.testing.expectError(CaptureScope.Error.NotActive, scope.exit());
}

test "capture scope rolls back when declarations fail" {
    var table = SymbolTable.init(std.testing.allocator);
    defer table.deinit();

    try table.enterScope();

    var scope = CaptureScope.init(&table);
    const dup_bindings = [_]CaptureBindingDesc{
        .{ .name = "dup", .span = fakeSpan(4) },
        .{ .name = "dup", .span = fakeSpan(5) },
    };
    try std.testing.expectError(error.DuplicateSymbol, scope.enter(&dup_bindings));
    try std.testing.expect(!scope.active);
    try std.testing.expect(table.resolve("dup") == null);
}

const TestError = error{
    UnexpectedViolation,
    SymbolNotFound,
};
