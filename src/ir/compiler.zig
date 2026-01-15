const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir.zig");
const ast = @import("../frontend/ast.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;
const rainbow = @import("../rainbow.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;
const Stream = @import("../stream.zig").Stream;
const RCError = @import("../mem/rc.zig").RCError;
const evaluateArithmetic = ir.evaluator.IREvaluator.evaluateArithmetic;
const evaluateLogical = ir.evaluator.IREvaluator.evaluateLogical;
const evaluateCompare = ir.evaluator.IREvaluator.evaluateCompare;
const page_size = ir.context.page_size;
const stack_start = ir.context.stack_start;

const logging_name = "COMPILER";
const prefix_color = rainbow.beginColor(.blue);
const span_color = rainbow.beginBgColor(.green) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

pub const Error =
    Allocator.Error ||
    std.Io.Writer.Error ||
    std.fmt.ParseIntError ||
    std.fmt.ParseFloatError ||
    GetFrameError ||
    ir.Location.Error ||
    ir.Value.ToStreamError ||
    DocumentStore.Error ||
    RCError ||
    error{
        UnsupportedExpression,
        UnsupportedBindingPattern,
        UnsupportedLiteral,
        UnsupportedCalleeExpression,
        UnsupportedExitCodeExpression,
        UnsupportedAddrType,
        UnsupportedBinaryOperation,
        DataTooLargeToFitInPage,
        ScopeNotFound,
        LabelAddrNotSet,
        StructTypeNotFound,
    };

pub const GetFrameError = error{
    FrameStartDepthTooHigh,
};

pub const Diagnostic = struct {
    err: Error,
    _span: ?ast.Span,
    message: []const u8,
    _severity: Severity,

    pub const Severity = enum {
        @"error",
        warning,
        information,
        hint,
    };

    pub fn span(self: Diagnostic) ?ast.Span {
        return self._span;
    }

    pub fn severity(self: Diagnostic) []const u8 {
        return @tagName(self._severity);
    }

    pub fn path(self: Diagnostic) ?[]const u8 {
        const span_ = self.span() orelse return null;
        return span_.start.file;
    }
};

pub const CompilationResult = union(enum) {
    err: struct {
        _diagnostics: []Diagnostic,

        pub fn diagnostics(self: @This()) []const Diagnostic {
            return self._diagnostics;
        }
    },
    success: ir.context.IRSharedContext,
};

pub const Result = union(enum) {
    value: ir.Value,

    pub fn from(v: anytype) !@This() {
        if (@TypeOf(v) == ir.Location) {
            return .fromLocation(v);
        }
        if (@TypeOf(v) == ir.Value) {
            return .fromValue(v);
        }

        @compileError("Unsupported Result type: " ++ @typeName(@TypeOf(v)));
    }

    pub fn fromLocation(location: ir.Location) ir.Location.Error!@This() {
        return switch (location.abs) {
            .register => |reg| .fromValue(.{ .dereference = .{ .register = .init(reg, location.mod) } }),
            else => .fromValue(.fromAddr(try location.toAddr())),
        };
    }

    pub fn fromValue(value: ir.Value) @This() {
        return .{ .value = value };
    }

    pub fn executable(name: []const u8) @This() {
        return .fromValue(.{ .slice = name });
    }
};

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
        try self.data.append(allocator, .fixed(
            try allocator.alloc(u8, page_size),
        ));
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
        const page_writer = self.getPageWriter(loc.abs.data.page);
        try page_writer.writeAll(data);
        return loc;
    }

    pub fn allocData(self: *IRData, allocator: Allocator, len: usize) Error!ir.Location {
        const page = try self.ensureDataCapacity(allocator, len);
        const page_writer = self.getPageWriter(page);
        const addr = page_writer.end;
        return .initAbs(.{ .data = .init(page, addr) });
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
    frames: std.ArrayList(StackFrame) = .empty,

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

const StackFrame = struct {
    rel_stack_counter: usize = 0,

    pub fn init() @This() {
        return .{};
    }
};

fn internalStructTypes(
    allocator: Allocator,
) Allocator.Error![]ir.Value.Struct.Type {
    const executableCallContext = ir.Value.Struct.Type{
        .name = "ExecutableCallContext",
        .fields = try .init(
            allocator,
            &.{"argv"},
            &.{.{ .slice = ir.Value.Slice.size() }},
        ),
        .decls = .empty,
    };

    return allocator.dupe(ir.Value.Struct.Type, &.{
        executableCallContext,
    });
}

pub const IRCompiler = struct {
    allocator: Allocator,
    script: *ast.Script,
    scopes: Scope = .init(),
    data: IRData = .init(),
    instruction_sets: std.ArrayList(InstructionSet) = .empty,
    current_instruction_set: usize = 0,
    labels: ir.Labels = .init(),
    struct_types: std.ArrayList(ir.Value.Struct.Type) = .empty,
    document_store: *DocumentStore,
    logging_enabled: bool,
    diagnostics: std.ArrayList(Diagnostic) = .empty,

    pub fn init(
        allocator: Allocator,
        document_store: *DocumentStore,
        script: *ast.Script,
    ) Allocator.Error!@This() {
        const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_" ++ logging_name) catch null;
        const logging_enabled = if (logging_enabled_s) |le| std.mem.eql(u8, le, "1") else false;

        return .{
            .allocator = allocator,
            .script = script,
            .struct_types = .fromOwnedSlice(try internalStructTypes(allocator)),
            .document_store = document_store,
            .logging_enabled = logging_enabled,
        };
    }

    fn reportSourceError(
        self: *@This(),
        source: anytype,
        err: Error,
        severity: Diagnostic.Severity,
        comptime fmt: []const u8,
        args: anytype,
    ) Error!void {
        try self.diagnostics.append(self.allocator, .{
            .err = err,
            ._span = if (@typeInfo(@TypeOf(source)) == .optional) if (source) |s| s.span() else null else source.span(),
            .message = try std.fmt.allocPrint(self.allocator, fmt, args),
            ._severity = severity,
        });
    }

    fn getStructType(self: IRCompiler, name: []const u8) Error!usize {
        for (self.struct_types.items, 0..) |st, i| {
            if (std.mem.eql(u8, st.name, name)) {
                return i;
            }
        }

        return Error.StructTypeNotFound;
    }

    pub fn addData(self: *IRCompiler, data: []const u8) Error!ir.Location {
        return try self.data.addData(self.allocator, data);
    }

    pub fn addDataValue(self: *IRCompiler, value: ir.Value) Error!ir.Location {
        var buffer: [1024]u8 = undefined;
        var writer = std.Io.Writer.fixed(&buffer);
        try writer.print("{f}", .{value});
        return self.addData(writer.buffered());
    }

    pub fn addSlice(
        self: *IRCompiler,
        element_size: usize,
        data: []const u8,
    ) Error!ir.Value {
        const loc = try self.addData(data);
        return .{ .slice = .{
            .addr = try loc.toAddr(),
            .element_size = element_size,
            .len = @divExact(data.len, element_size),
        } };
    }

    fn currentInstrSet(self: *@This()) *InstructionSet {
        return &self.instruction_sets.items[self.current_instruction_set];
    }

    fn pushFrame(self: *@This(), source: anytype) Error!void {
        try self.addInstruction(.init(.from(source), .{ .push = .{
            .register = .initAbs(.sf),
        } }));
        try self.set(source, .initAbs(.{ .register = .sf }), .{ .register = .initAbs(.sc) });
        return self.currentInstrSet().frames.append(self.allocator, .init());
    }

    fn popFrame(self: *@This(), source: anytype) !void {
        try self.push(source, .{ .register = .initAbs(.sf) });
        try self.addInstruction(.init(.from(source), .{ .set = .{
            .location = .initAbs(.{ .register = .sf }),
            .value = .{ .dereference = .{ .register = .initSub(.sf, 1) } },
        } }));
        try self.addInstruction(.init(.from(source), .{ .set = .{
            .location = .initAbs(.{ .register = .sc }),
            .value = .{ .dereference = .{ .register = .initSub(.sc, 1) } },
        } }));
        try self.addInstruction(.init(.from(source), .{ .set = .{
            .location = .initAbs(.{ .register = .sc }),
            .value = .{ .register = .initSub(.sc, 1) },
        } }));
        const instr_set = self.currentInstrSet();
        const popped = instr_set.frames.pop() orelse return;
        if (instr_set.frames.items.len > 0) {
            const frame = &instr_set.frames.items[instr_set.frames.items.len - 1];
            frame.rel_stack_counter += popped.rel_stack_counter;
        }
    }

    fn currentFrame(self: *@This()) *StackFrame {
        const instr_set = self.currentInstrSet();
        return &instr_set.frames.items[instr_set.frames.items.len - 1];
    }

    // If you change this, make sure to fix the std...StreamSet functions as well
    pub fn addInstructionSet(self: *@This(), source: anytype) Error!usize {
        const new_instr_set = try self.addInstructionSetNoPushFrame();
        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = new_instr_set;
        try self.pushFrame(source);
        self.current_instruction_set = orig_instr_set;
        return new_instr_set;
    }

    pub fn addInstructionSetNoPushFrame(self: *@This()) Allocator.Error!usize {
        try self.instruction_sets.append(self.allocator, .init());
        return self.instruction_sets.items.len - 1;
    }

    pub fn addInstruction(
        self: *@This(),
        instruction: ir.Instruction,
    ) Allocator.Error!void {
        try self.instruction_sets.items[self.current_instruction_set].add(
            self.allocator,
            instruction,
        );
    }

    pub fn currentAddr(self: @This()) ir.ResolvedInstructionAddr {
        const abs = self.instruction_sets.items[self.current_instruction_set].instructions.items.len;
        return .init(self.current_instruction_set, abs);
    }

    const SetLabelAddr = union(enum) { unknown, abs };

    fn getAddr(self: @This(), addr: SetLabelAddr) ?ir.ResolvedInstructionAddr {
        return switch (addr) {
            .unknown => null,
            .abs => self.currentAddr(),
        };
    }

    fn getLocalAddr(self: @This(), addr: SetLabelAddr) ?usize {
        const addr_ = self.getAddr(addr) orelse return null;
        return addr_.local_addr;
    }

    pub fn newLabel(
        self: *@This(),
        name: []const u8,
        addr: SetLabelAddr,
    ) Allocator.Error!ir.InstructionAddr {
        const label_addr = self.getLocalAddr(addr);
        return .initLabel(
            self.current_instruction_set,
            try self.labels.new(self.allocator, name, label_addr),
        );
    }

    pub fn setLabel(
        self: *@This(),
        label: ir.InstructionAddr.LabelKey,
        addr: SetLabelAddr,
    ) Allocator.Error!void {
        const label_addr = self.getLocalAddr(addr);
        return self.labels.set(self.allocator, label, label_addr);
    }

    pub fn newRef(
        self: *@This(),
        source: anytype,
        name: []const u8,
    ) Allocator.Error!ir.Location {
        defer self.currentFrame().rel_stack_counter += 1;
        try self.addInstruction(.init(.from(source), .{ .ref = name }));

        return .initAbs(.{ .ref = .{
            .name = name,
            .rel_stack_addr = self.currentFrame().rel_stack_counter,
        } });
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
        self.currentFrame().rel_stack_counter += 1;
        return self.addInstruction(.init(.from(source), .push_(value)));
    }

    pub fn pop(
        self: *IRCompiler,
        source: anytype,
    ) Error!ir.Location {
        self.currentFrame().rel_stack_counter -= 1;
        try self.addInstruction(.init(.from(source), .pop));
        return .initAbs(.{ .register = .r });
    }

    pub fn set(
        self: *IRCompiler,
        source: anytype,
        location: ir.Location,
        value: ir.Value,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .set = .{
                .location = location,
                .value = value,
            },
        }));
    }

    pub fn pipe(
        self: *IRCompiler,
        source: anytype,
        location: ir.Location,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .pipe = .{ .result = location },
        }));
    }

    pub fn pipeOpt(
        self: *IRCompiler,
        source: anytype,
        handle: ir.Location,
        option: ir.Instruction.PipeOption.OptionType,
        value: ir.Value,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .pipe_opt = .{
                .handle = handle,
                .option = option,
                .value = value,
            },
        }));
    }

    pub fn pipeFwd(
        self: *IRCompiler,
        source: anytype,
        pipe_source: ir.Location,
        pipe_destination: ir.Location,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .pipe_fwd = .{
                .source = pipe_source,
                .destination = pipe_destination,
            },
        }));
    }

    pub fn jmp(
        self: *IRCompiler,
        source: anytype,
        condition: ?Result,
        jump_if: bool,
        destination: ir.InstructionAddr,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .jmp = .{
                .cond = if (condition) |cond| cond.value else null,
                .jump_if = jump_if,
                .dest = destination,
            },
        }));
    }

    pub fn exec_(
        self: *IRCompiler,
        source: anytype,
        sync: bool,
    ) Error!ir.Location {
        self.currentFrame().rel_stack_counter -= 1;
        try self.addInstruction(.init(.from(source), .{ .exec = .{
            .sync = sync,
        } }));
        return .initAbs(.{ .register = .r });
    }

    pub fn fork(
        self: *IRCompiler,
        source: anytype,
        dest: ir.InstructionAddr,
        stdin: ir.Location,
        stdout: ir.Location,
        stderr: ir.Location,
    ) Error!ir.Location {
        try self.addInstruction(.init(.from(source), .fork_(
            dest,
            stdin,
            stdout,
            stderr,
        )));

        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = dest.instr_set;

        self.currentFrame().rel_stack_counter += 3;

        self.current_instruction_set = orig_instr_set;

        return .initAbs(.{ .register = .r });
    }

    pub fn forkInherit(
        self: *IRCompiler,
        source: anytype,
        dest: ir.InstructionAddr,
    ) Error!ir.Location {
        return try self.fork(
            source,
            dest,
            self.threadStdin(),
            self.threadStdout(),
            self.threadStderr(),
        );
    }

    pub fn wait(self: *IRCompiler, source: anytype, waitee: ir.Location) Error!void {
        return self.addInstruction(.init(.from(source), .wait_(waitee)));
    }

    pub fn stream(self: *IRCompiler, source: anytype, streamee: ir.Location) Error!void {
        return self.addInstruction(.init(.from(source), .stream_(streamee)));
    }

    pub fn exit_(
        self: *IRCompiler,
        source: anytype,
        value: Result,
    ) Error!void {
        const exit_code: ExitCode = switch (value.value) {
            .uinteger => |x| .fromByte(@intCast(@mod(x, 256))),
            .exit_code => |exit_code| exit_code,
            else => {
                return self.reportSourceError(source, Error.UnsupportedExitCodeExpression, .@"error", "value type \"{t}\" cannot be coerced into an exit code", .{value.value});
            },
        };
        return self.addInstruction(.init(.from(source), .exit_(exit_code)));
    }

    fn labelLessThan(_: *IRCompiler, a: ir.Label, b: ir.Label) bool {
        return a.addr < b.addr;
    }

    fn validateNoUnknownLabels(self: *IRCompiler) Error!void {
        for (self.labels.map.values()) |value| {
            if (value == null) return Error.LabelAddrNotSet;
        }
    }

    pub fn toIRContext(self: *IRCompiler) Error!ir.context.IRSharedContext {
        try self.validateNoUnknownLabels();
        self.labels.sort();

        const instructions = try self.allocator.alloc([]const ir.Instruction, self.instruction_sets.items.len);
        for (instructions, self.instruction_sets.items) |*dest, *src| {
            dest.* = try src.toOwnedSlice(self.allocator);
        }

        return .{
            .data = try self.data.toOwnedSlice(self.allocator),
            .instructions = instructions,
            .labels = .{ .map = self.labels.map.move() },
            .struct_types = try self.struct_types.toOwnedSlice(self.allocator),
        };
    }

    pub fn compile(self: *IRCompiler) Error!CompilationResult {
        self.current_instruction_set = try self.addInstructionSetNoPushFrame();

        try self.scopes.pushBindings(self.allocator);
        defer self.scopes.popFrame();

        try self.compileInitial();

        for (self.script.statements) |stmt| {
            _ = try self.compileStatement(stmt);
        }

        try self.addInstruction(.init(null, .exit_(.success)));

        if (self.diagnostics.items.len > 0) {
            return .{ .err = .{ ._diagnostics = self.diagnostics.items } };
        }

        return .{ .success = try self.toIRContext() };
    }

    fn compileInitial(self: *@This()) Error!void {
        try self.addInstruction(.init(null, .fwd_stdio));
        try self.pushFrame(null);

        const stdin_set = try self.addInstructionSet(null);
        const stdout_set = try self.addInstructionSet(null);
        const stderr_set = try self.addInstructionSet(null);

        const prev_instr_set = self.current_instruction_set;

        self.current_instruction_set = stdin_set;
        try self.stream(null, self.threadStdin());

        self.current_instruction_set = stdout_set;
        try self.stream(null, self.threadStdout());

        self.current_instruction_set = stderr_set;
        try self.stream(null, self.threadStderr());

        self.current_instruction_set = prev_instr_set;

        _ = try self.fork(null, .initAbs(stdin_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr());
        _ = try self.fork(null, .initAbs(stdout_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr());
        _ = try self.fork(null, .initAbs(stderr_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr());
    }

    fn compileStatement(
        self: *IRCompiler,
        stmt: *ast.Statement,
    ) Error!Result {
        const result: Result = try switch (stmt.*) {
            .type_binding_decl => Result.fromValue(.void),
            .binding_decl => |*b| self.compileBindingDecl(stmt, b),
            .return_stmt => |r| self.compileReturn(stmt, r),
            .expression => |expr| self.compileExpression(expr.expression),
            else => {
                try self.reportSourceError(stmt, Error.UnsupportedExpression, .@"error", "statement type \"{t}\" not yet supported", .{stmt.*});
                return .fromValue(.void);
            },
        };

        if (isWaitable(result)) |loc| {
            try self.wait(stmt, loc);
        }

        return result;
    }

    fn isWaitable(result: Result) ?ir.Location {
        return switch (result.value) {
            .register => |r| .init(.{ .register = r.abs }, r.mod),
            .dereference => |d| switch (d) {
                .addr => null,
                .register => |r| .init(.{ .register = r.abs }, r.mod),
            },
            else => null,
        };
    }

    fn compileReturn(
        self: *IRCompiler,
        source: *ast.Statement,
        r: ast.ReturnStmt,
    ) Error!Result {
        const result: Result = if (r.value) |value| try self.compileExpression(value) else .fromValue(.{ .exit_code = .success });
        try self.exit_(source, result);
        return .fromValue(.void);
    }

    fn compileBindingDecl(
        self: *IRCompiler,
        source: *ast.Statement,
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
        source: anytype,
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
            else => {
                try self.reportSourceError(
                    source,
                    Error.UnsupportedBindingPattern,
                    .@"error",
                    "binding pattern type \"{t}\" not yet supported",
                    .{pattern.*},
                );
                return .fromValue(.void);
            },
        };
    }

    fn compileExpression(
        self: *IRCompiler,
        expr: *ast.Expression,
    ) Error!Result {
        return switch (expr.*) {
            .literal => |literal| self.compileLiteral(expr, literal),
            .identifier => |identifier| self.compileIdentifier(identifier),
            .call => |call| self.compileCall(expr, call),
            .if_expr => |if_expr| self.compileIf(expr, if_expr),
            .pipeline => |pipeline| self.compilePipeline(expr, pipeline),
            .block => |block| self.compileBlock(expr, block),
            .fn_decl => |fn_decl| self.compileFnDecl(expr, fn_decl),
            .binary => |binary| self.compileBinary(expr, binary),
            else => {
                try self.reportSourceError(expr, Error.UnsupportedExpression, .@"error", "expression type \"{t}\" not yet supported", .{expr.*});
                return .fromValue(.void);
            },
        };
    }

    fn compileLiteral(
        self: *IRCompiler,
        source: anytype,
        literal: ast.Literal,
    ) Error!Result {
        return switch (literal) {
            // .integer => |integer| .fromValue(try self.addSlice(integer.text)),
            .integer => |integer| .from(try parseInt(integer.text)),
            .float => |float| .from(try parseFloat(float.text)),
            .bool => |boolean| .fromValue(.{ .exit_code = .fromBoolean(boolean.value) }),
            .string => |string| self.compileStringLiteral(string),
            else => {
                try self.reportSourceError(source, Error.UnsupportedLiteral, .@"error", "literal type \"{t}\" not yet supported", .{literal});
                return .fromValue(.void);
            },
        };
    }

    fn compileStringLiteral(
        self: *IRCompiler,
        string_literal: ast.StringLiteral,
    ) Error!Result {
        if (string_literal.segments.len == 1) {
            return .from(try self.addSlice(1, string_literal.segments[0].text.payload));
        }

        const s_tream = try self.allocator.alloc(ir.Value, string_literal.segments.len);

        // TODO: find a way to make this instruction-based instead because of volatile result register, compileExpression in a loop won't cut it

        for (string_literal.segments, 0..) |segment, i| switch (segment) {
            .text => |text| {
                s_tream[i] = try self.addSlice(1, text.payload);
            },
            .interpolation => |interp| {
                s_tream[i] = (try self.compileExpression(interp)).value;
            },
        };

        return .fromValue(.{ .stream = s_tream });
    }

    fn parseInt(text: []const u8) std.fmt.ParseIntError!ir.Value {
        return .{ .uinteger = try std.fmt.parseInt(usize, text, 10) };
    }

    fn parseFloat(text: []const u8) std.fmt.ParseFloatError!ir.Value {
        return .{ .float = try std.fmt.parseFloat(f64, text) };
    }

    fn compileIdentifier(
        self: *IRCompiler,
        identifier: ast.Identifier,
    ) Error!Result {
        const binding = self.lookup(identifier.name) orelse {
            const executable = try self.addSlice(1, identifier.name);
            return .from(executable);
        };
        return binding.result;
    }

    fn compileCall(
        self: *IRCompiler,
        source: *ast.Expression,
        call: ast.CallExpr,
    ) Error!Result {
        const callee = try self.compileExpression(call.callee);

        return switch (callee.value) {
            .slice => self.compileExecutableCall(
                source,
                callee.value,
                call.arguments,
            ),
            .fn_ref => self.compileFunctionCall(source, callee.value, call.arguments),
            .stream, .addr, .void, .uinteger, .float, .strct, .exit_code, .pipe, .thread, .closeable, .register, .dereference => .from(callee.value),
        };
    }

    fn compileExpressions(
        self: *IRCompiler,
        exprs: []const *ast.Expression,
    ) Error![]ir.Value {
        const values = try self.allocator.alloc(ir.Value, exprs.len);
        errdefer self.allocator.free(values);

        for (exprs, values) |expr, *value| {
            const result = try self.compileExpression(expr);
            value.* = result.value;
        }

        return values;
    }

    fn allocExecutableCallContextFields(
        self: *IRCompiler,
        executable: ir.Value,
        args: []const ir.Value,
    ) Error![]ir.Value {
        var buffer: [1024]u8 = undefined;
        var buffer_w = std.Io.Writer.fixed(&buffer);
        try (try executable.toStream(self.allocator)).serialize(&buffer_w);
        for (args) |arg| try (try arg.toStream(self.allocator)).serialize(&buffer_w);
        const argv = try self.addSlice(@sizeOf([]ir.Value), buffer_w.buffered());
        return self.allocator.dupe(ir.Value, &.{argv});
    }

    fn threadStdin(_: *IRCompiler) ir.Location {
        return .initAbs(.{ .stack = 0 });
    }

    fn threadStdout(_: *IRCompiler) ir.Location {
        return .initAbs(.{ .stack = 1 });
    }

    fn threadStderr(_: *IRCompiler) ir.Location {
        return .initAbs(.{ .stack = 2 });
    }

    fn stdinStreamSet(_: *IRCompiler) ir.InstructionAddr {
        return .initAbs(1, 0);
    }

    fn stdoutStreamSet(_: *IRCompiler) ir.InstructionAddr {
        return .initAbs(2, 0);
    }

    fn stderrStreamSet(_: *IRCompiler) ir.InstructionAddr {
        return .initAbs(3, 0);
    }

    fn compileExecutableCall(
        self: *IRCompiler,
        source: *ast.Expression,
        executable: ir.Value,
        arguments: []const *ast.Expression,
    ) Error!Result {
        // const args_temp = try self.compileExpressions(arguments);
        // defer self.allocator.free(args_temp);
        // const fields = try self.allocExecutableCallContextFields(executable, args_temp);
        // errdefer self.allocator.free(fields);

        const exec_instr_set = try self.addInstructionSet(source);

        const prev_instr_set = self.current_instruction_set;
        self.current_instruction_set = exec_instr_set;

        var it = std.mem.reverseIterator(arguments);
        while (it.next()) |arg_expr| {
            const arg = try self.compileExpression(arg_expr);
            try self.push(source, arg.value);
        }
        try self.push(source, executable);
        try self.push(source, .{ .uinteger = arguments.len });

        // try self.push(source, .{ .strct = .{
        //     .type = try self.getStructType("ExecutableCallContext"),
        //     .fields = fields,
        // } });
        const exec_handle = try self.exec_(source, false);
        try self.wait(source, exec_handle);

        self.current_instruction_set = prev_instr_set;

        const thread_handle = try self.forkInherit(source, .initAbs(exec_instr_set, 0));

        return .from(thread_handle);
    }

    fn compileFunctionCall(
        self: *IRCompiler,
        source: *ast.Expression,
        fn_ref_value: ir.Value,
        arguments: []const *ast.Expression,
    ) Error!Result {
        _ = arguments;
        const fn_ref = fn_ref_value.fn_ref;
        const fn_addr = fn_ref.fn_addr;

        const handle = try self.forkInherit(source, fn_addr);

        return .fromLocation(handle);
    }

    fn compileIf(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
    ) Error!Result {
        if (if_expr.else_branch) |else_branch| {
            return try self.compileIfElse(source, if_expr, else_branch);
        } else {
            return try self.compileIfNoElse(source, if_expr);
        }
    }

    fn compileIfElse(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
        else_branch: ast.IfExpr.ElseBranch,
    ) Error!Result {
        const condition = try self.compileExpression(if_expr.condition);
        if (condition.value == .exit_code) {
            const c = condition.value.exit_code.toBoolean();

            if (c) return self.compileExpression(if_expr.then_expr);
            return switch (else_branch) {
                .expr => |expr_| self.compileExpression(expr_),
                .if_expr => |if_expr_| self.compileIf(source, if_expr_.*),
            };
        }

        const result = try self.newRef(source, "if_result");
        const after_addr = try self.newLabel("if_after", .unknown);
        const else_addr = try self.newLabel("if_else", .unknown);

        try self.jmp(source, condition, false, else_addr);
        const then = try self.compileExpression(if_expr.then_expr);
        try self.set(source, result, then.value);
        try self.jmp(source, null, false, after_addr);
        try self.setLabel(else_addr.local_addr.label, .abs);
        const else_ = try switch (else_branch) {
            .expr => |expr_| self.compileExpression(expr_),
            .if_expr => |if_expr_| self.compileIf(source, if_expr_.*),
        };
        try self.set(source, result, else_.value);
        try self.setLabel(after_addr.local_addr.label, .abs);

        const result_loc = try self.pop(source);

        return .from(result_loc);
    }

    fn compileIfNoElse(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
    ) Error!Result {
        try self.pushFrame(source);

        const condition = try self.compileExpression(if_expr.condition);
        if (condition.value == .exit_code) {
            const c = condition.value.exit_code.toBoolean();

            if (c) return self.compileExpression(if_expr.then_expr);
            return .fromValue(.void);
        }

        const result = try self.newRef(source, "if_result");

        const after_addr = try self.newLabel("if_after", .unknown);

        try self.jmp(source, condition, false, after_addr);

        const then = try self.compileExpression(if_expr.then_expr);
        try self.set(source, result, then.value);

        try self.setLabel(after_addr.local_addr.label, .abs);

        const result_loc = try self.pop(source);

        try self.popFrame(source);

        return .from(result_loc);
    }

    fn refLocation(self: *IRCompiler, ref_def: RefDef) ir.Location {
        return .fromRef(
            ref_def.name,
            self.currentFrame().rel_stack_counter - ref_def.rel_stack_addr,
        );
    }

    fn compilePipeline(
        self: *IRCompiler,
        source: *ast.Expression,
        pipeline: ast.Pipeline,
    ) Error!Result {
        const refs = try self.allocator.alloc(ir.Location, pipeline.stages.len - 1);
        defer self.allocator.free(refs);

        for (refs) |*ref| {
            ref.* = try self.newRef(source, "pipe");
            try self.pipe(source, ref.*);
            try self.pipeOpt(source, ref.*, .keep_open, .fromBoolean(true));
        }

        const orig_instr_set = self.current_instruction_set;
        const stage_sets = try self.allocator.alloc(usize, pipeline.stages.len - 1);
        defer self.allocator.free(stage_sets);
        for (stage_sets, pipeline.stages[0 .. pipeline.stages.len - 1]) |*stage_set, stage_expr| {
            stage_set.* = try self.addInstructionSet(source);
            self.current_instruction_set = stage_set.*;
            const result = try self.compileExpression(stage_expr);

            if (isWaitable(result)) |_| {
                try self.push(source, result.value);
                _ = try self.forkInherit(source, self.stdoutStreamSet());
                const result_stack = try self.pop(source);
                try self.wait(source, result_stack);
                try self.pipeOpt(source, self.threadStdout(), .keep_open, .fromBoolean(false));
            } else {
                return Error.UnsupportedExpression;
            }
        }
        self.current_instruction_set = orig_instr_set;

        _ = try self.fork(source, .initAbs(stage_sets[0], 0), self.threadStdin(), refs[0], self.threadStderr());

        for (refs[0 .. refs.len - 1], refs[1..], stage_sets[1..]) |prev, curr, stage_set| {
            _ = try self.fork(source, .initAbs(stage_set, 0), prev, curr, self.threadStderr());
        }

        const last_set = try self.addInstructionSet(source);

        self.current_instruction_set = last_set;
        const result = try self.compileExpression(pipeline.stages[pipeline.stages.len - 1]);
        if (isWaitable(result)) |loc| {
            try self.wait(source, loc);
        }
        self.current_instruction_set = orig_instr_set;

        const last_fork_handle = try self.fork(
            source,
            .initAbs(last_set, 0),
            refs[refs.len - 1],
            self.threadStdout(),
            self.threadStderr(),
        );

        return .from(last_fork_handle);
    }

    fn compileBlock(
        self: *IRCompiler,
        source: *ast.Expression,
        block: ast.Block,
    ) Error!Result {
        const block_stdout = try self.newRef(source, "block_stdout_pipe");

        try self.pipe(source, block_stdout);
        try self.pipeOpt(
            source,
            block_stdout,
            .disconnect_destination,
            .fromBoolean(false),
        );
        try self.pipeOpt(
            source,
            block_stdout,
            .close_destination,
            .fromBoolean(false),
        );
        try self.pipeOpt(
            source,
            block_stdout,
            .keep_open,
            .fromBoolean(true),
        );
        try self.pipeFwd(
            source,
            block_stdout,
            self.threadStdout(),
        );
        const block_set = try self.addInstructionSet(source);

        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = block_set;

        const block_stdout_thread = try self.forkInherit(source, self.stdoutStreamSet());
        try self.push(source, .{ .register = .init(block_stdout_thread.abs.register, block_stdout_thread.mod) });

        for (block.statements) |stmt| {
            // TODO: figure out what to do with these results
            _ = try self.compileStatement(stmt);
        }
        // const pipe_opts_set = try self.addInstructionSet(source);
        // try self.atomic(source, pipe_opts_set);
        // self.current_instruction_set = pipe_opts_set;
        try self.pipeOpt(
            source,
            self.threadStdout(),
            .keep_open,
            .fromBoolean(false),
        );
        // try self.pipeOpt(source, self.threadStdout(), .disconnect_destination, true);
        _ = try self.pop(source);
        try self.wait(source, block_stdout_thread);

        self.current_instruction_set = orig_instr_set;

        const block_result = try self.fork(
            source,
            .initAbs(block_set, 0),
            self.threadStdin(),
            block_stdout,
            self.threadStderr(),
        );

        return .from(block_result);
    }

    fn compileFnDecl(
        self: *IRCompiler,
        source: *ast.Expression,
        fn_decl: ast.FunctionDecl,
    ) Error!Result {
        const instr_set = try self.addInstructionSet(source);
        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = instr_set;

        // TODO: figure out how to be able to call async functions multiple times and have the result not be overwritten in a ref
        const result = try self.compileExpression(fn_decl.body);
        // TODO: figure out how to make this async
        if (isWaitable(result)) |loc| {
            try self.wait(source, loc);
        }

        self.current_instruction_set = orig_instr_set;
        const fn_ref = ir.Value{
            .fn_ref = .{ .fn_addr = ir.InstructionAddr.initAbs(instr_set, 0) },
        };
        if (fn_decl.name) |name| {
            try self.scopes.declare(self.allocator, name.name, try .from(fn_ref), false);
        }

        return .from(fn_ref);
    }

    fn compileBinary(
        self: *IRCompiler,
        source: *ast.Expression,
        binary: ast.BinaryExpr,
    ) Error!Result {
        switch (binary.op) {
            .add, .subtract, .multiply, .divide, .remainder => {
                const left = try self.compileExpression(binary.left);
                const right = try self.compileExpression(binary.right);

                if (evaluateArithmetic(.from(binary.op), left.value, right.value)) |comptime_result| {
                    return .from(comptime_result);
                }

                const ref = try self.newRef(source, "ath_result");

                try self.addInstruction(.init(.from(source), .{ .ath = .{
                    .op = .from(binary.op),
                    .a = left.value,
                    .b = right.value,
                    .result = ref,
                } }));

                return .from(ref);
            },
            .logical_and, .logical_or => {
                const left = try self.compileExpression(binary.left);

                if (evaluateLogical(.from(binary.op), left.value)) |comptime_result| {
                    switch (comptime_result) {
                        .left => return left,
                        .right => return self.compileExpression(binary.right),
                    }
                }

                const right = try self.compileExpression(binary.right);
                const ref = try self.newRef(source, "logical_result");

                try self.addInstruction(.init(.from(source), .{ .log = .{
                    .op = .from(binary.op),
                    .a = left.value,
                    .b = right.value,
                    .result = ref,
                } }));

                return .from(ref);
            },
            .greater, .greater_equal, .less, .less_equal, .equal, .not_equal => {
                const left = try self.compileExpression(binary.left);
                const right = try self.compileExpression(binary.right);

                if (evaluateCompare(.from(binary.op), left.value, right.value)) |comptime_result| {
                    return .from(comptime_result);
                }

                const ref = try self.newRef(source, "cmp_result");

                try self.addInstruction(.init(.from(source), .{ .cmp = .{
                    .op = .from(binary.op),
                    .a = left.value,
                    .b = right.value,
                    .result = ref,
                } }));

                return .from(ref);
            },
            .apply, .pipe => {
                try self.log(@src().fn_name ++ ": error, encountered {t} binary expression", .{binary.op});
                try self.logEvaluateSpan(binary.span);
            },
            else => {},
        }

        try self.reportSourceError(source, Error.UnsupportedBinaryOperation, .@"error", "binary operator \"{t}\" not yet supported", .{binary.op});
        return .fromValue(.void);
    }

    pub fn log(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        if (@hasField(@This(), "logging_enabled")) {
            if (!self.logging_enabled) return;
        }

        var stderr = std.fs.File.stderr().writer(&.{});
        const writer = &stderr.interface;

        try writer.print("[{s}{*}{s}]\n", .{ prefix_color, self, end_color });
        // try writer.print("{s}:\n", .{self.path});
        try writer.print(fmt ++ "\n", args);
    }

    pub fn logWithoutPrefix(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        if (@hasField(@This(), "logging_enabled")) {
            if (!self.logging_enabled) return;
        }

        var stderr = std.fs.File.stderr().writer(&.{});
        const writer = &stderr.interface;

        try writer.print(fmt, args);
    }

    pub fn logEvaluationTrace(self: *@This(), label: []const u8) !void {
        try self.log("{s}", .{label});
    }

    pub fn logEvaluateSpan(self: *@This(), span: ast.Span) !void {
        if (@hasField(@This(), "logging_enabled")) {
            if (!self.logging_enabled) return;
        }

        if (span.isGlobal()) {
            try self.logWithoutPrefix("{s}\n", .{span.start.file});
            return;
        }

        const source = try self.document_store.getSource(span.start.file);
        var lineIt = std.mem.splitScalar(u8, source, '\n');
        var i: usize = 0;
        while (lineIt.next()) |line| : (i += 1) {
            if (i >= span.start.line -| 3 and i <= span.end.line +| 3) {
                if (span.start.line == i + 1 and span.end.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        line[0 .. span.start.column - 1],
                        span_color,
                        line[span.start.column - 1 ..],
                        end_color,
                    });
                } else if (span.end.line == i + 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line[0 .. span.end.column - 1],
                        end_color,
                        line[span.end.column - 1 ..],
                    });
                } else if (span.start.line - 1 <= i and i <= span.end.line - 1) {
                    try self.logWithoutPrefix("{:>4}:{s}{s}{s}\n", .{
                        i + 1,
                        span_color,
                        line,
                        end_color,
                    });
                } else {
                    try self.logWithoutPrefix("{:>4}:{s}\n", .{ i + 1, line });
                }
            }
        }
    }
};

const RefDef = struct {
    name: []const u8,
    rel_stack_addr: usize,
};
