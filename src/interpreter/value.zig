//! Module containing Value that represents a value in runic runtime.

const std = @import("std");
const command_runner = @import("../runtime/command_runner.zig");
const TypeExpr = @import("../frontend/ast.zig").TypeExpr;
const ScopeStack = @import("../interpreter/scope.zig").ScopeStack;
const mem = @import("../mem/root.zig");
const Stream = @import("../stream.zig").Stream;

const ast = @import("../frontend/ast.zig");
const ExitCode = command_runner.ExitCode;

/// Runtime value representation used by the interpreter while evaluating AST
/// nodes. Every variant owns its storage so scopes can safely clone or move
/// bindings between frames.
pub const Value = union(enum) {
    void,
    boolean: bool,
    integer: Integer,
    float: Float,
    string: String,
    range: Range,
    array: Array,
    function: FunctionRef,
    scope: ScopeRef,
    stream: *Stream(Value),
    process: std.process.Child.Id,
    execution: ExecutionRef,
    exit_code: ?ExitCode,

    pub const FunctionRef = mem.RC(FunctionRefInner).Ref;

    const FunctionRefInner = struct {
        fn_decl: *const ast.FunctionDecl,
        closure: ?*ScopeStack,

        pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            const closure = self.closure orelse return;
            self.closure = null;
            closure.deinit();
            allocator.destroy(self);
        }

        pub fn deinitMain(self: *@This(), allocator: std.mem.Allocator) void {
            const closure = self.closure orelse return;
            self.closure = null;
            closure.deinitMain();
            allocator.destroy(self);
        }
    };

    pub const Integer = i64;
    pub const Float = f64;
    pub const String = mem.RC([]u8).Ref;
    pub const Array = mem.RC(std.ArrayList(Value)).Ref;

    pub const Range = struct {
        start: Integer,
        end: ?Integer,
        inclusive_end: bool,

        pub fn usizeStart(self: Range) !usize {
            if (self.start < 0) return error.NegativeLength;
            return @intCast(self.start);
        }

        pub fn usizeEnd(self: Range) !?usize {
            const end_i64 = self.end orelse return null;
            if (end_i64 < 0) return error.NegativeLength;
            return @intCast(end_i64);
        }

        pub fn getLen(self: Range) !?usize {
            const end = try self.usizeEnd() orelse return null;
            const start = try self.usizeStart();
            const inclusive: usize = if (self.inclusive_end) 1 else 0;
            return end - start + inclusive;
        }
    };

    pub const ScopeRef = mem.RC(ScopeStack).Ref;

    pub const ExecutionRef = mem.RC(ExecutionRefInner).Ref;

    const ExecutionRefInner = struct {
        stdout: Value.String,
        stderr: Value.String,
        exit_code: ?ExitCode,

        pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            self.stdout.deinit(.{});
            self.stderr.deinit(.{});
            allocator.destroy(self);
        }
    };

    pub const Error = error{TypeMismatch};

    /// Releases any heap allocations and resets the value to `.void`.
    pub fn deinit(self: *Value) void {
        switch (self.*) {
            .string => |*ref| ref.deinit(.{}),
            .function => |*f| f.deinit(.{
                .deinit_child_fn = .withAllocator(FunctionRefInner.deinit),
            }),
            .array => |*arr| arr.deinit(.{}),
            .scope => |*scope| scope.deinit(.{
                .deinit_child_fn = .withoutAllocator(ScopeStack.deinit),
            }),
            .stream => |stream| stream.deinit(),
            .execution => |*execution| execution.deinit(
                .{ .deinit_child_fn = .withAllocator(ExecutionRefInner.deinit) },
            ),
            else => {},
        }
        self.* = .{ .void = {} };
    }

    /// Releases any heap allocations and resets the value to `.void`.
    pub fn deinitMain(self: *Value) void {
        switch (self.*) {
            .string => |*ref| ref.deinit(.{}),
            .function => |*f| f.deinit(.{
                .deinit_child_fn = .withAllocator(FunctionRefInner.deinitMain),
            }),
            .array => |*array_ref| {
                // <|:)--<
                array_ref.deinit(.{
                    .deinit_array_child_fn = .withoutAllocator(Value.deinitMain),
                });
            },
            .scope => |*scope| scope.deinit(.{
                .deinit_child_fn = .withoutAllocator(ScopeStack.deinitMain),
            }),
            .stream => |stream| stream.deinit(),
            .execution => |*execution| execution.deinit(
                .{ .deinit_child_fn = .withAllocator(ExecutionRefInner.deinit) },
            ),
            else => {},
        }
        self.* = .{ .void = {} };
    }

    /// Clones the value into a fresh allocation owned by `allocator`.
    pub fn clone(self: Value, _: std.mem.Allocator, options: mem.RCInitOptions) !Value {
        return switch (self) {
            .void, .boolean, .integer, .float, .range, .process => self,
            .function => |ref| .{ .function = try ref.ref(options) },
            .string => |ref| .{ .string = try ref.ref(options) },
            .array => |ref| .{ .array = try ref.ref(options) },
            // TODO: Investigate if we need to clone this (hint: everytime we use a Value in the evaluator, we seem to clone it and deinitialize)
            .scope => |scope| .{ .scope = try scope.ref(options) },
            .stream => |stream| .{ .stream = try stream.clone() },
            .execution => |execution| .{ .execution = brk: {
                const clone_ = try execution.clone(options);
                const inner = try clone_.getPtr();
                inner.stdout = try inner.stdout.ref(options);
                inner.stderr = try inner.stderr.ref(options);
                break :brk clone_;
            } },
            // TODO: implement error values
            .exit_code => self,
        };
    }

    /// Moves the value out of the union, leaving `.void` in its place so the
    /// caller becomes the new owner of any heap allocations or handles.
    pub fn move(self: *Value) Value {
        const snapshot = self.*;
        self.* = .{ .void = {} };
        return snapshot;
    }

    pub fn reassign(self: *Value, newValue: *Value) Error!void {
        if (std.meta.activeTag(self.*) != std.meta.activeTag(newValue.*)) return Error.TypeMismatch;
        self.deinit();
        self.* = newValue.move();
    }

    pub fn isNumeric(self: Value) bool {
        return self == .integer or self == .float;
    }

    pub fn format(self: Value, writer: *std.Io.Writer) !void {
        switch (self) {
            .void => try writer.writeAll("void"),
            inline .boolean, .integer, .float => |v| try writer.print("{}", .{v}),
            inline .string => |v| {
                if (v.get()) |string| {
                    try writer.print("{f}", .{std.json.fmt(string, .{})});
                } else |err| {
                    try writer.print("RCError:{}", .{err});
                }
            },
            .range => |r| {
                if (r.end) |end| return try writer.print("{}..{}", .{ r.start, end });
                try writer.print("{}..", .{r.start});
            },
            .array => try writer.writeAll("<array>"),
            .function => try writer.writeAll("<fn>"),
            .scope => |scope_ref| try writer.print("<module:{?*}>", .{scope_ref.getPtr() catch null}),
            .stream => |stream| try writer.print("<stream:{*}>", .{stream}),
            .process => |pid| try writer.print("<process:{any}>", .{pid}),
            .execution => try writer.print("<execution>", .{}),
            .exit_code => |exit_code| try writer.print("<exit_code:{?f}>", .{exit_code}),
        }
    }
};
