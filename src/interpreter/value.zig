const std = @import("std");
const command_runner = @import("../runtime/command_runner.zig");
const TypeExpr = @import("../frontend/ast.zig").TypeExpr;
const ScopeStack = @import("../interpreter/scope.zig").ScopeStack;
const mem = @import("../mem/root.zig");

const ast = @import("../frontend/ast.zig");
const ProcessHandle = command_runner.ProcessHandle;

/// Runtime value representation used by the interpreter while evaluating AST
/// nodes. Every variant owns its storage so scopes can safely clone or move
/// bindings between frames.
pub const Value = union(enum) {
    void,
    boolean: bool,
    integer: Integer,
    float: Float,
    string: String,
    function: FunctionRef,
    process_handle: ProcessHandle,
    scope: *ScopeStack,

    pub const FunctionRef = struct {
        fn_decl: *const ast.FunctionDecl,
        scope: *ScopeStack,
        closure: *ScopeStack,

        pub fn deinit(self: FunctionRef, allocator: std.mem.Allocator) void {
            self.closure.deinit();
            allocator.destroy(self.closure);
        }
    };

    pub const Integer = i64;
    pub const Float = f64;
    pub const String = mem.RC([]const u8).Ref;

    pub const Error = error{TypeMismatch};

    /// Releases any heap allocations and resets the value to `.void`.
    pub fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |*ref| ref.release(),
            .process_handle => |*handle| handle.deinit(),
            .function => |f| f.deinit(allocator),
            else => {},
        }
        self.* = .{ .void = {} };
    }

    /// Clones the value into a fresh allocation owned by `allocator`.
    pub fn clone(self: Value, allocator: std.mem.Allocator) !Value {
        return switch (self) {
            .void, .boolean, .integer, .float, .function => self,
            .string => |ref| .{ .string = try ref.ref() },
            .process_handle => |handle| .{ .process_handle = try handle.clone(allocator) },
            // TODO: Investigate if we need to clone this (hint: everytime we use a Value in the evaluator, we seem to clone it and deinitialize)
            .scope => self,
        };
    }

    /// Moves the value out of the union, leaving `.void` in its place so the
    /// caller becomes the new owner of any heap allocations or handles.
    pub fn move(self: *Value) Value {
        const snapshot = self.*;
        self.* = .{ .void = {} };
        return snapshot;
    }

    pub fn reassign(self: *Value, allocator: std.mem.Allocator, newValue: *Value) Error!void {
        if (std.meta.activeTag(self.*) != std.meta.activeTag(newValue.*)) return Error.TypeMismatch;
        self.deinit(allocator);
        self.* = newValue.move();
    }

    pub fn isNumeric(self: Value) bool {
        return self == .integer or self == .float;
    }
};
