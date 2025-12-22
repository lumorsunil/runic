const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("ir.zig");
const ast = @import("../frontend/ast.zig");

pub const Error =
    Allocator.Error ||
    std.Io.Writer.Error ||
    error{
        UnsupportedExpression,
        UnsupportedBindingPattern,
        UnsupportedLiteral,
        DataTooLargeToFitInPage,
    };

pub const Result = union(enum) {
    value: ir.Value,

    pub fn fromLocation(location: ir.Location) @This() {
        return .fromValue(.fromLocation(location));
    }

    pub fn fromValue(value: ir.Value) @This() {
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

    pub fn deinit(self: *IRData, allocator: Allocator) void {
        for (self.data.items) |*item| {
            item.end = 0;
            allocator.free(item.buffer);
        }
        self.data.deinit(allocator);
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

    pub fn toOwnedSlice(self: *IRData, allocator: Allocator) ![]const []const u8 {
        const owned = try allocator.alloc([]const u8, self.data.items.len);
        for (owned, self.data.items) |*o, item| o.* = try allocator.dupe(
            u8,
            item.buffered(),
        );
        self.deinit(allocator);
        return owned;
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

    pub fn pushBindings(self: *Scope, allocator: Allocator) !void {
        return self.frames.append(allocator, .{ .bindings = .empty });
    }

    pub fn popFrame(self: *Scope) void {
        _ = self.frames.pop();
    }

    pub fn getCurrentFrame(self: *Scope, tag: std.meta.Tag(Frame)) *Frame {
        for (0..self.frames.items.len) |i| {
            const index = self.frames.items.len - 1 - i;
            const frame = &self.frames.items[index];

            if (frame.* == tag) {
                return frame;
            }
        }

        @panic("shouldn't happen <|:)-|--<");
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
    data: IRData = .init(),
    instructions: std.ArrayList(ir.Instruction) = .empty,

    pub fn init(allocator: Allocator, script: *ast.Script) @This() {
        return .{
            .allocator = allocator,
            .script = script,
        };
    }

    pub fn addData(self: *IRCompiler, data: []const u8) Error!ir.Value {
        return .fromLocation(try self.data.addData(self.allocator, data));
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
            .registers = .{},
        };
    }

    pub fn compile(self: *IRCompiler) Error!ir.IRContext {
        try self.scopes.pushBindings(self.allocator);
        defer self.scopes.popFrame();

        for (self.script.statements) |stmt| {
            _ = try self.compileStatement(stmt);
        }

        return self.toIRContext();
    }

    fn compileStatement(self: *IRCompiler, stmt: *ast.Statement) Error!Result {
        return switch (stmt.*) {
            .type_binding_decl => .fromValue(.void),
            .binding_decl => |*b| self.compileBindingDecl(.{ .stmt = stmt }, b),
            .expression => |expr| self.compileExpression(expr.expression),
            else => Error.UnsupportedExpression,
        };
    }

    fn compileBindingDecl(
        self: *IRCompiler,
        source: ir.Instruction.Source,
        binding_decl: *ast.BindingDecl,
    ) Error!Result {
        return self.compileBinding(
            source,
            binding_decl.pattern,
            binding_decl.initializer,
            binding_decl.is_mutable,
        );
    }

    fn compileBinding(
        self: *IRCompiler,
        source: ir.Instruction.Source,
        pattern: *ast.BindingPattern,
        expr: *ast.Expression,
        is_mutable: bool,
    ) Error!Result {
        return switch (pattern.*) {
            .discard => {
                _ = try self.compileExpression(expr);
                return .fromValue(.void);
            },
            .identifier => |identifier| {
                const result = try self.compileExpression(expr);
                try self.scopes.declare(
                    self.allocator,
                    identifier.name,
                    result,
                    is_mutable,
                );
                try self.addInstruction(.init(source, .{ .push = result.value }));
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
            .integer => |integer| .fromValue(try self.addData(integer.text)),
            .float => |float| .fromValue(try self.addData(float.text)),
            .bool => |boolean| .fromValue(.{ .boolean = boolean.value }),
            .string => Error.UnsupportedLiteral,
            else => Error.UnsupportedLiteral,
        };
    }
};
