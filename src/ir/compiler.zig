const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("ir.zig");
const ast = @import("../frontend/ast.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;

pub const Error =
    Allocator.Error ||
    std.Io.Writer.Error ||
    std.fmt.ParseIntError ||
    GetFrameError ||
    error{
        UnsupportedExpression,
        UnsupportedBindingPattern,
        UnsupportedLiteral,
        UnsupportedCalleeExpression,
        UnsupportedExitCodeExpression,
        DataTooLargeToFitInPage,
        ScopeNotFound,
    };

pub const GetFrameError = error{
    FrameStartDepthTooHigh,
};

pub const Result = union(enum) {
    value: ir.Value,

    pub fn fromLocation(location: ir.Location) @This() {
        return .fromValue(.fromLocation(location));
    }

    pub fn fromValue(value: ir.Value) @This() {
        return .{ .value = value };
    }

    pub fn executable(name: []const u8) @This() {
        return .fromValue(.{ .executable = name });
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
        const loc = try self.allocData(allocator, data.len);
        const page_writer = self.getPageWriter(loc.data.page);
        try page_writer.writeAll(data);
        return loc;
    }

    pub fn allocData(self: *IRData, allocator: Allocator, len: usize) Error!ir.Location {
        const page = try self.ensureDataCapacity(allocator, len);
        const page_writer = self.getPageWriter(page);
        const addr = page_writer.end;
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

    pub fn getFrameByTag(
        self: *Scope,
        tag: std.meta.Tag(Frame),
        start_depth: usize,
    ) GetFrameError!?struct { usize, *Frame } {
        if (start_depth >= self.frames.items.len) return Error.FrameStartDepthTooHigh;
        for (start_depth..self.frames.items.len) |i| {
            const index = self.frames.items.len - 1 - i;
            const frame = &self.frames.items[index];

            if (frame.* == tag) {
                return .{ i, frame };
            }
        }

        return null;
    }

    pub fn declare(
        self: *Scope,
        allocator: Allocator,
        name: []const u8,
        result: Result,
        is_mutable: bool,
    ) Error!void {
        _, const frame = try self.getFrameByTag(.bindings, 0) orelse return Error.ScopeNotFound;
        return frame.bindings.put(allocator, name, .{ .is_mutable = is_mutable, .result = result });
    }

    pub fn lookup(self: *Scope, name: []const u8) ?*Binding {
        var depth: usize = 0;
        while (true) {
            const new_depth, const frame = self.getFrameByTag(
                .bindings,
                depth,
            ) catch |err| switch (err) {
                GetFrameError.FrameStartDepthTooHigh => return null,
            } orelse return null;
            depth = new_depth + 1;
            return frame.bindings.getPtr(name) orelse continue;
        }
    }
};

const InstructionSet = struct {
    instructions: std.ArrayList(ir.Instruction) = .empty,

    pub fn init() @This() {
        return .{};
    }

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        self.instructions.deinit(allocator);
    }

    pub fn add(self: *@This(), allocator: Allocator, instruction: ir.Instruction) Allocator.Error!void {
        try self.instructions.append(allocator, instruction);
    }

    pub fn toOwnedSlice(self: *@This(), allocator: Allocator) Allocator.Error![]ir.Instruction {
        return self.instructions.toOwnedSlice(allocator);
    }
};

const Refs = struct {
    current_ref: usize = 0,

    pub fn init() @This() {
        return .{};
    }

    pub fn new(self: *Refs) ir.Ref {
        defer self.current_ref += 1;
        return self.current_ref;
    }
};

const Labels = struct {
    map: std.AutoArrayHashMapUnmanaged(ir.Value.Addr.LabelKey, ?ir.Value.Addr) = .empty,

    pub fn init() @This() {
        return .{};
    }

    pub fn new(
        self: *Labels,
        allocator: Allocator,
        name: []const u8,
        addr: ?ir.Value.Addr,
    ) Allocator.Error!ir.Value.Addr.LabelKey {
        const label: ir.Value.Addr.LabelKey = .{ self.map.count(), name };
        try self.set(allocator, label, addr);
        return label;
    }

    pub fn set(
        self: *Labels,
        allocator: Allocator,
        label: ir.Value.Addr.LabelKey,
        addr: ?ir.Value.Addr,
    ) Allocator.Error!void {
        try self.map.put(allocator, label, addr);
    }
};

pub const IRCompiler = struct {
    allocator: Allocator,
    script: *ast.Script,
    scopes: Scope = .init(),
    data: IRData = .init(),
    instructions: InstructionSet = .init(),
    refs: Refs = .init(),
    labels: Labels = .init(),

    pub fn init(allocator: Allocator, script: *ast.Script) @This() {
        return .{
            .allocator = allocator,
            .script = script,
        };
    }

    pub fn addData(self: *IRCompiler, data: []const u8) Error!ir.Location {
        return try self.data.addData(self.allocator, data);
    }

    pub fn addSlice(self: *IRCompiler, data: []const u8) Error!ir.Value {
        const loc = try self.addData(data);
        return .{ .slice = .{ .location = loc, .len = data.len } };
    }

    pub fn addInstruction(self: *@This(), instruction: ir.Instruction) Allocator.Error!void {
        try self.instructions.add(self.allocator, instruction);
    }

    pub fn currentAddr(self: @This()) ir.Value.Addr {
        const abs = self.instructions.instructions.items.len;
        return .{ .abs = abs };
    }

    const SetLabelAddr = union(enum) { unknown, abs, rel: isize };

    fn getAddr(self: @This(), addr: SetLabelAddr) ?ir.Value.Addr {
        return switch (addr) {
            .unknown => null,
            .abs => self.currentAddr(),
            .rel => |rel| .{ .rel = rel },
        };
    }

    pub fn newLabel(self: *@This(), name: []const u8, addr: SetLabelAddr,) Allocator.Error!ir.Value.Addr {
        const label_addr = self.getAddr(addr);
        return .{ .label = try self.labels.new(self.allocator, name, label_addr) };
    }

    pub fn setLabel(self: *@This(), label: ir.Value.Addr.LabelKey, addr: SetLabelAddr,) Allocator.Error!void {
        const label_addr = self.getAddr(addr);
        return self.labels.set(self.allocator, label, label_addr);
    }

    pub fn declare(
        self: *IRCompiler,
        name: []const u8,
        result: Result,
        is_mutable: bool,
    ) Error!void {
        return self.scopes.declare(self.allocator, name, result, is_mutable);
    }

    pub fn lookup(self: *IRCompiler, name: []const u8) ?*Scope.Binding {
        return self.scopes.lookup(name);
    }

    pub fn push(
        self: *IRCompiler,
        source: anytype,
        value: ir.Value,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .push_(value)));
    }

    pub fn pop(
        self: *IRCompiler,
        source: anytype,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .pop));
    }

    pub fn jmp(
        self: *IRCompiler,
        source: anytype,
        condition: Result,
        jump_if: bool,
        destination: ir.Value.Addr,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .jmp = .{
                .cond = condition.value,
                .jump_if = jump_if,
                .dest = destination,
            },
        }));
    }

    pub fn call_(
        self: *IRCompiler,
        source: anytype,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .call));
    }

    pub fn exit_(
        self: *IRCompiler,
        source: anytype,
        value: Result,
    ) Error!void {
        const exit_code: ExitCode = switch (value.value) {
            .uinteger => |x| .fromByte(@intCast(@mod(x, 256))),
            .exit_code => |exit_code| exit_code,
            else => return Error.UnsupportedExitCodeExpression,
        };
        return self.addInstruction(.init(.from(source), .exit_(exit_code)));
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

        try self.addInstruction(.init(null, .exit_(.success)));

        return self.toIRContext();
    }

    fn compileStatement(self: *IRCompiler, stmt: *ast.Statement) Error!Result {
        return switch (stmt.*) {
            .type_binding_decl => .fromValue(.void),
            .binding_decl => |*b| self.compileBindingDecl(b),
            .return_stmt => |r| self.compileReturn(stmt, r),
            .expression => |expr| self.compileExpression(expr.expression),
            else => Error.UnsupportedExpression,
        };
    }

    fn compileReturn(self: *IRCompiler, source: *ast.Statement, r: ast.ReturnStmt) Error!Result {
        const result: Result = if (r.value) |value| try self.compileExpression(value) else .fromValue(.{ .exit_code = .success });
        try self.exit_(source, result);
        return .fromValue(.void);
    }

    fn compileBindingDecl(
        self: *IRCompiler,
        binding_decl: *ast.BindingDecl,
    ) Error!Result {
        return self.compileBinding(
            binding_decl.pattern,
            binding_decl.initializer,
            binding_decl.is_mutable,
        );
    }

    fn compileBinding(
        self: *IRCompiler,
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
                return .fromValue(.void);
            },
            else => Error.UnsupportedBindingPattern,
        };
    }

    fn compileExpression(self: *IRCompiler, expr: *ast.Expression) Error!Result {
        return switch (expr.*) {
            .literal => |literal| self.compileLiteral(literal),
            .identifier => |identifier| self.compileIdentifier(identifier),
            .call => |call| self.compileCall(expr, call),
            .if_expr => |if_expr| self.compileIf(expr, if_expr),
            else => Error.UnsupportedExpression,
        };
    }

    fn compileLiteral(self: *IRCompiler, literal: ast.Literal) Error!Result {
        return switch (literal) {
            // .integer => |integer| .fromValue(try self.addSlice(integer.text)),
            .integer => |integer| .fromValue(try parseInt(integer.text)),
            .float => |float| .fromValue(try self.addSlice(float.text)),
            .bool => |boolean| .fromValue(.{ .boolean = boolean.value }),
            .string => |string| {
                if (string.segments.len != 1) return Error.UnsupportedLiteral;
                return .fromValue(try self.addSlice(string.segments[0].text.payload));
            },
            else => Error.UnsupportedLiteral,
        };
    }

    fn parseInt(text: []const u8) std.fmt.ParseIntError!ir.Value {
        return .{ .uinteger = try std.fmt.parseInt(usize, text, 10) };
    }

    fn compileIdentifier(self: *IRCompiler, identifier: ast.Identifier) Result {
        const binding = self.lookup(identifier.name) orelse return .executable(identifier.name);
        return binding.result;
    }

    fn compileCall(
        self: *IRCompiler,
        source: *ast.Expression,
        call: ast.CallExpr,
    ) Error!Result {
        const callee = try self.compileExpression(call.callee);

        switch (callee.value) {
            .executable => |executable| return self.compileExecutableCall(
                source,
                executable,
                call.arguments,
            ),
            else => return Error.UnsupportedCalleeExpression,
        }
    }

    fn compileExecutableCall(
        self: *IRCompiler,
        source: *ast.Expression,
        executable: []const u8,
        arguments: []const *ast.Expression,
    ) Error!Result {
        var it = std.mem.reverseIterator(arguments);
        while (it.next()) |arg| {
            const result = try self.compileExpression(arg);
            try self.push(source, result.value);
        }
        const executable_loc = try self.addSlice(executable);
        try self.push(source, executable_loc);
        try self.push(source, .{ .uinteger = arguments.len });
        try self.call_(source);
        // for (0..arguments.len + 2) |_| {
        //     try self.pop(source);
        // }
        return .fromValue(.void);
    }

    // TODO: figure out how to be able to pass the result of a if expression to a result location
    fn compileIf(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
    ) Error!Result {
        const condition = try self.compileExpression(if_expr.condition);
        const after_addr = try self.newLabel("if_after", .unknown);
        const else_addr = try self.newLabel("if_then", .unknown);
        try self.jmp(source, condition, false, else_addr);
        const then = try self.compileExpression(if_expr.then_expr);
        try self.jmp(source, condition, false, after_addr);
        try self.setLabel(else_addr.label, .abs);
        const else_ = try if (if_expr.else_branch) |e| switch (e) {
            .expr => |expr_| self.compileExpression(expr_),
            .if_expr => |if_expr_| self.compileIf(source, if_expr_.*),
        };
        try self.setLabel(after_addr.label, .abs);
    }
};
