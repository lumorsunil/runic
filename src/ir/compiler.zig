const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("ir.zig");
const ast = @import("../frontend/ast.zig");
const Value = @import("../interpreter/value.zig").Value;

pub const Error =
    Allocator.Error ||
    error{
        UnsupportedExpression,
        UnsupportedBindingPattern,
        UnsupportedLiteral,
        DataTooLargeToFitInPage,
    };

pub const Result = union(enum) {
    location: ir.Location,
    value: Value,

    pub fn fromValue(value: Value) @This() {
        return .{ .value = value };
    }
};

const page_size = 1024 * 4;

pub const IRData = struct {
    data: std.ArrayList(Page) = .empty,

    pub const Page = std.Io.Writer;

    pub fn init() @This() {
        return .{};
    }

    fn addPage(self: *IRData, allocator: Allocator) Error!usize {
        try self.data.append(allocator, .fixed(try allocator.alloc(u8, page_size)));
        return self.data.items.len - 1;
    }

    fn ensureDataCapacity(self: *IRData, allocator: Allocator, len: usize) Error!usize {
        if (len > page_size) {
            return Error.DataTooLargeToFitInPage;
        }

        if (self.data.items.len == 0) {
            return self.addPage(allocator);
        }

        const current_page = self.data.items.len - 1;

        if (self.data.items[current_page].unusedCapacityLen() >= len) {
            return current_page;
        } else {
            return self.addPage(allocator);
        }
    }

    fn getPageWriter(self: *IRData, page: usize) *Page {
        return &self.data.items[page];
    }

    pub fn addData(self: *IRData, allocator: Allocator, data: []const u8) Error!ir.Location {
        const page = try self.ensureDataCapacity(allocator, data.len);
        const page_writer = self.getPageWriter(page);
        const addr = page_writer.end;
        try page_writer.writeAll(data);
        return .{ .data = .init(page, addr) };
    }
};

const Scope = struct {
    frames: std.ArrayList(Frame) = .empty,

    pub const Frame = union(enum) {
        bindings: std.StringArrayHashMapUnmanaged(Binding),
    };

    pub const Binding = struct {
        is_mutable: bool,
        result: Result,
    };

    pub fn init() @This() {
        return .{};
    }

    pub fn getCurrentFrame(self: *Scope, tag: std.meta.Tag(Frame)) *Frame {
        for (0..self.frames.items.len) |i| {
            const index = self.frames.items.len - 1 - i;
            const frame = &self.frames.items[index];

            if (frame.* == tag) {
                return frame;
            }
        }
    }

    pub fn declare(self: *Scope, allocator: Allocator, name: []const u8, result: Result, is_mutable: bool) Error!void {
        const frame = self.getCurrentFrame(.bindings);
        return frame.bindings.put(allocator, name, .{ .is_mutable = is_mutable, .result = result });
    }
};

pub const IRCompiler = struct {
    allocator: Allocator,
    script: *ast.Script,
    scopes: Scope = .init(),
    data: IRData,
    instructions: std.ArrayList(ir.Instruction) = .empty,

    pub fn init(allocator: Allocator, script: *ast.Script) @This() {
        return .{
            .allocator = allocator,
            .script = script,
        };
    }

    pub fn addData(self: *IRCompiler, data: []const u8) Error!ir.Location {
        return self.data.addData(self.allocator, data);
    }

    pub fn addInstruction(self: *IRCompiler, instruction: ir.Instruction) Error!void {
        try self.instructions.append(self.allocator, instruction);
    }

    pub fn toIRContext(self: *IRCompiler) Error!ir.IRContext {
        return .{
            .read_only = .{
                .data = try self.data.toOwnedSlice(self.allocator),
                .instructions = try self.instructions.toOwnedSlice(self.allocator),
            },
        };
    }

    pub fn compile(self: *IRCompiler) Error!ir.IRContext {
        for (self.script.statements) |stmt| {
            try self.compileStatement(stmt);
        }

        return self.toIRContext();
    }

    fn compileStatement(self: *IRCompiler, stmt: *ast.Statement) Error!Result {
        return switch (stmt.*) {
            .type_binding_decl => {},
            .binding_decl => |b| self.compileBindingDecl(b),
            .expression => |expr| self.compileExpression(expr),
            else => Error.UnsupportedExpression,
        };
    }

    fn compileBindingDecl(self: *IRCompiler, binding_decl: *ast.BindingDecl) Error!Result {
        return self.compileBinding(binding_decl.pattern, binding_decl.initializer, binding_decl.is_mutable);
    }

    fn compileBinding(self: *IRCompiler, pattern: *ast.BindingPattern, expr: *ast.Expression, is_mutable: bool) Error!Result {
        return switch (pattern.*) {
            .discard => {
                _ = try self.compileExpression(expr);
                return .fromValue(.void);
            },
            .identifier => |identifier| {
                const result = try self.compileExpression(expr);
                self.scopes.declare(identifier.name, result, is_mutable);
                self.addInstruction(.init());
                return .fromValue(.void);
            },
            else => Error.UnsupportedBindingPattern,
        };
    }

    fn compileExpression(self: *IRCompiler, expr: *ast.Expression) Error!Result {
        return switch (expr.*) {
            .literal => |literal| self.compileLiteral(literal),
            else => Error.UnsupportedExpression,
        };
    }

    fn compileLiteral(self: *IRCompiler, literal: ast.Literal) Error!Result {
        return switch (literal) {
            .integer => |integer| self.addData(integer.text),
            .float => |float| self.addData(float.text),
            .bool => |boolean| .fromValue(.{ .boolean = boolean }),
            .string => Error.UnsupportedLiteral,
            else => Error.UnsupportedLiteral,
        };
    }
};
