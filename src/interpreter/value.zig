const std = @import("std");
const command_runner = @import("../runtime/command_runner.zig");
const TypeExpr = @import("../frontend/ast.zig").TypeExpr;
const ScopeStack = @import("../interpreter/scope.zig").ScopeStack;

const ProcessHandle = command_runner.ProcessHandle;

/// Runtime value representation used by the interpreter while evaluating AST
/// nodes. Every variant owns its storage so scopes can safely clone or move
/// bindings between frames.
pub const Value = union(enum) {
    void,
    boolean: bool,
    integer: i64,
    float: f64,
    string: []u8,
    process_handle: ProcessHandle,
    scope: *ScopeStack,

    /// Releases any heap allocations and resets the value to `.void`.
    pub fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |buffer| allocator.free(buffer),
            .process_handle => |*handle| handle.deinit(),
            else => {},
        }
        self.* = .{ .void = {} };
    }

    /// Clones the value into a fresh allocation owned by `allocator`.
    pub fn clone(self: Value, allocator: std.mem.Allocator) !Value {
        return switch (self) {
            .void => .{ .void = {} },
            .boolean => |flag| .{ .boolean = flag },
            .integer => |int| .{ .integer = int },
            .float => |flt| .{ .float = flt },
            .string => |buffer| .{ .string = try allocator.dupe(u8, buffer) },
            .process_handle => |handle| .{ .process_handle = try handle.clone(allocator) },
            .scope => |scope| .{ .scope = scope },
        };
    }

    /// Moves the value out of the union, leaving `.void` in its place so the
    /// caller becomes the new owner of any heap allocations or handles.
    pub fn move(self: *Value) Value {
        const snapshot = self.*;
        self.* = .{ .void = {} };
        return snapshot;
    }
};
