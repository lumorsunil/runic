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

// TODO: change to proper types
pub const execution_handles_struct_type = ast.TypeExpr{ .struct_type = .{
    .span = .global,
    .decls = &.{},
    .fields = &.{ .{
        .name = .global("thread"),
        .type_expr = &.{ .integer = .{ .span = .global } },
        .span = .global,
    }, .{
        .name = .global("closeable"),
        .type_expr = &.{ .integer = .{ .span = .global } },
        .span = .global,
    } },
} };

pub const execution_result_struct_type = ast.TypeExpr{ .struct_type = .{
    .span = .global,
    .decls = &.{},
    .fields = &.{ .{
        .name = .global("stdout"),
        .type_expr = &.{ .integer = .{ .span = .global } },
        .span = .global,
    }, .{
        .name = .global("stderr"),
        .type_expr = &.{ .integer = .{ .span = .global } },
        .span = .global,
    }, .{
        .name = .global("closeable"),
        .type_expr = &.{ .integer = .{ .span = .global } },
        .span = .global,
    } },
} };

pub const thread_type = ast.TypeExpr.global(.thread);

pub fn array_type(element: *const ast.TypeExpr) ast.TypeExpr {
    return .{ .array = .{
        .element = element,
        .span = .global,
    } };
}

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
        UnsupportedValueType,
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
    source: ir.ValueSource,

    pub fn from(v: anytype) !@This() {
        if (@TypeOf(v) == ir.Location) {
            return .fromLocation(v);
        }
        if (@TypeOf(v) == ir.Value) {
            return .fromValue(v);
        }

        @compileError("Unsupported Result type: " ++ @typeName(@TypeOf(v)));
    }

    pub fn fromLocation(location: ir.Location) @This() {
        return .{ .source = .{ .location = location } };
    }

    pub fn fromValue(value: ir.Value) @This() {
        return .{ .source = .{ .value = value } };
    }

    pub fn executable(name: []const u8) @This() {
        return .fromValue(.{ .slice = name });
    }

    pub fn dereference(self: @This()) @This() {
        if (self.source == .location) {
            var copy = self;
            copy.source = copy.source.dereference();
            return copy;
        } else {
            return self;
        }
    }

    pub fn typed(self: @This(), maybe_type_expr: ?ast.TypeExpr) @This() {
        const type_expr = maybe_type_expr orelse return self;
        if (self.source == .location) {
            var copy = self;
            copy.source.location = copy.source.location.typed(type_expr);
            return copy;
        } else {
            return self;
        }
    }

    pub fn typeExpr(self: @This()) ?ast.TypeExpr {
        return self.source.typeExpr();
    }

    pub fn isType(self: @This(), type_expr: ast.TypeExpr) bool {
        return self.source == .location and self.source.location.isType(type_expr);
    }

    pub fn isFunctionRef(self: @This()) bool {
        return self.source == .value and self.source.value == .fn_ref;
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
        return .initAbs(.{ .data = .init(page, addr) }, .{});
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

    pub fn size(self: IRData) usize {
        if (self.data.items.len == 0) return 0;
        return (self.data.items.len - 1) * page_size + self.data.getLast().buffered().len;
    }
};

const Scope = struct {
    frames: std.ArrayList(Frame) = .empty,

    pub const Frame = struct {
        bindings: std.StringArrayHashMapUnmanaged(Binding) = .empty,
        closure: std.ArrayList(ClosureBinding) = .empty,

        pub fn declare(
            self: *Frame,
            allocator: Allocator,
            name: []const u8,
            result: Result,
            is_mutable: bool,
        ) Error!void {
            return self.bindings.put(allocator, name, .{
                .is_mutable = is_mutable,
                .result = result,
            });
        }
    };

    pub const Binding = struct {
        is_mutable: bool,
        result: Result,
    };

    pub const ClosureBinding = struct {
        depth: usize,
        type: enum { outer, mutable },
        identifier: ast.Identifier,

        pub fn outer(identifier: ast.Identifier) @This() {
            return .{
                .type = .outer,
                .identifier = identifier,
                .depth = 1,
            };
        }

        pub fn mutable(identifier: ast.Identifier) @This() {
            return .{
                .type = .mutable,
                .identifier = identifier,
                .depth = 0,
            };
        }
    };

    pub fn init() @This() {
        return .{};
    }

    pub fn push(self: *Scope, allocator: Allocator) !void {
        return self.frames.append(allocator, .{});
    }

    pub fn pop(self: *Scope) void {
        _ = self.frames.pop();
    }

    pub fn getFrame(
        self: *Scope,
        depth: usize,
    ) GetFrameError!*Frame {
        if (depth >= self.frames.items.len) return Error.FrameStartDepthTooHigh;
        const index = self.frames.items.len - 1 - depth;
        const frame = &self.frames.items[index];
        return frame;
    }

    pub fn declare(
        self: *Scope,
        allocator: Allocator,
        name: []const u8,
        result: Result,
        is_mutable: bool,
    ) Error!void {
        const frame = try self.getFrame(0);
        return frame.declare(allocator, name, result, is_mutable);
    }

    pub const LookupOptions = struct {
        shallow: bool = false,
        initial_depth: usize = 0,
    };

    pub fn lookup(self: *Scope, name: []const u8, options: LookupOptions) ?*Binding {
        var depth: usize = options.initial_depth;
        while (true) {
            const frame = self.getFrame(depth) catch |err| switch (err) {
                GetFrameError.FrameStartDepthTooHigh => return null,
            };
            depth += 1;
            return frame.bindings.getPtr(name) orelse {
                if (options.shallow) {
                    return null;
                } else {
                    continue;
                }
            };
        }
    }
};

const InstructionSet = struct {
    instructions: std.ArrayList(ir.Instruction) = .empty,
    frames: std.ArrayList(StackFrame) = .empty,
    closure_slot_count: usize = 0,
    closure_captures: []const ClosureCapture = &.{},

    pub fn init() @This() {
        return .{};
    }

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        self.instructions.deinit(allocator);
    }

    pub fn add(self: *@This(), allocator: Allocator, instruction: ir.Instruction) Allocator.Error!void {
        try self.instructions.append(allocator, instruction);
    }

    pub fn insertSlice(self: *@This(), allocator: Allocator, index: usize, instructions: []ir.Instruction) Allocator.Error!void {
        try self.instructions.insertSlice(allocator, index, instructions);
    }

    pub fn toOwnedSlice(self: *@This(), allocator: Allocator) Allocator.Error![]ir.Instruction {
        return self.instructions.toOwnedSlice(allocator);
    }
};

const ClosureCapture = struct {
    identifier: ast.Identifier,
    slot: usize,
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
    result_counter: usize = 0,

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
        try self.addInstruction(.init(
            .from(source),
            .push_(.fromLocation(.initRegister(.sf))),
        ));
        try self.set(source, .initRegister(.sf), .fromLocation(.initRegister(.sc)));
        return self.pushFrameNoInstructions();
    }

    fn pushFrameNoInstructions(self: *@This()) Error!void {
        return self.currentInstrSet().frames.append(self.allocator, .init());
    }

    fn popFrame(self: *@This(), source: anytype) !void {
        try self.push(source, .fromLocation(.initRegister(.sf)));
        try self.addInstruction(.init(.from(source), .{ .set = .{
            .destination = .initRegister(.sf),
            .source = .fromLocation(.initSub(.{ .register = .sf }, 1, .{ .dereference = true })),
        } }));
        try self.addInstruction(.init(.from(source), .{ .set = .{
            .destination = .initRegister(.sc),
            .source = .fromLocation(.initSub(.{ .register = .sc }, 1, .{ .dereference = true })),
        } }));
        try self.addInstruction(.init(.from(source), .{ .set = .{
            .destination = .initRegister(.sc),
            .source = .fromLocation(.initSub(.{ .register = .sc }, 1, .{})),
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
    /// assumes that we always have stdin, stdout and stderr as the first three things on the stack
    pub fn addInstructionSet(self: *@This()) Error!usize {
        const new_instr_set = try self.addInstructionSetNoPushFrame();
        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = new_instr_set;
        try self.pushFrameNoInstructions();
        self.currentFrame().rel_stack_counter += 4;
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
        } }, .{});
    }

    pub fn declare(
        self: *IRCompiler,
        name: []const u8,
        result: Result,
        is_mutable: bool,
    ) Error!void {
        return self.scopes.declare(self.allocator, name, result, is_mutable);
    }

    pub fn lookup(
        self: *IRCompiler,
        name: []const u8,
        options: Scope.LookupOptions,
    ) ?*Scope.Binding {
        return self.scopes.lookup(name, options);
    }

    pub fn push(
        self: *IRCompiler,
        source: anytype,
        value_source: ir.ValueSource,
    ) Error!void {
        self.currentFrame().rel_stack_counter += 1;
        return self.addInstruction(.init(.from(source), .push_(value_source)));
    }

    pub fn pop(
        self: *IRCompiler,
        source: anytype,
    ) Error!ir.Location {
        self.currentFrame().rel_stack_counter -= 1;
        try self.addInstruction(.init(.from(source), .pop));
        return .initAbs(.{ .register = .r }, .{});
    }

    pub fn set(
        self: *IRCompiler,
        source: anytype,
        location: ir.Location,
        value_source: ir.ValueSource,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .set = .{
                .destination = location,
                .source = value_source,
            },
        }));
    }

    pub fn inc(
        self: *IRCompiler,
        source: anytype,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .inc));
    }

    pub fn dec(
        self: *IRCompiler,
        source: anytype,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .dec));
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
        value_source: ir.ValueSource,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .pipe_opt = .{
                .handle = handle,
                .option = option,
                .source = value_source,
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
                .cond = if (condition) |cond| cond.source else null,
                .jump_if = jump_if,
                .dest = destination,
            },
        }));
    }

    pub fn exec_(
        self: *IRCompiler,
        source: anytype,
        args: usize,
        sync: bool,
    ) Error!ir.Location {
        self.currentFrame().rel_stack_counter -= args + 2;
        try self.addInstruction(.init(.from(source), .{ .exec = .{
            .sync = sync,
        } }));
        return .initAbs(.{ .register = .r }, .{});
    }

    pub fn fork(
        self: *IRCompiler,
        source: anytype,
        dest: ir.InstructionAddr,
        stdin: ir.Location,
        stdout: ir.Location,
        stderr: ir.Location,
        closure: ir.Location,
    ) Error!ir.Location {
        try self.addInstruction(.init(.from(source), .fork_(
            dest,
            stdin,
            stdout,
            stderr,
            closure,
        )));

        // Moved this logic to addInstructionSet, all sets now are assumed to be called using forks
        //
        // const orig_instr_set = self.current_instruction_set;
        // self.current_instruction_set = dest.instr_set;
        //
        // // TODO: issue here when we are forking multiple times to the same set since the frame is connected to the set and not the thread, the rel_stack_counter will point to the wrong place in the stack
        // self.currentFrame().rel_stack_counter += 3;
        //
        // self.current_instruction_set = orig_instr_set;

        return .initAbs(.{ .register = .r }, .{ .type_expr = thread_type });
    }

    pub fn forkInherit(
        self: *IRCompiler,
        source: anytype,
        dest: ir.InstructionAddr,
        closure: ir.Location,
    ) Error!ir.Location {
        return try self.fork(
            source,
            dest,
            self.threadStdin(),
            self.threadStdout(),
            self.threadStderr(),
            closure,
        );
    }

    pub fn wait(self: *IRCompiler, source: anytype, waitee: ir.Location) Error!void {
        return self.addInstruction(.init(.from(source), .wait_(waitee)));
    }

    pub fn stream(self: *IRCompiler, source: anytype, streamee: ir.Location) Error!void {
        return self.addInstruction(.init(.from(source), .stream_(streamee)));
    }

    pub fn alloc(self: *IRCompiler, source: anytype, size: usize) Error!void {
        return self.addInstruction(.init(.from(source), .{ .alloc = size }));
    }

    pub fn exit_(
        self: *IRCompiler,
        source: anytype,
        value: Result,
    ) Error!void {
        const exit_code: ExitCode = @as(?ExitCode, switch (value.source) {
            .value => |v| switch (v) {
                .uinteger => |x| .fromByte(@intCast(@mod(x, 256))),
                .exit_code => |exit_code| exit_code,
                else => null,
            },
            else => null,
        }) orelse {
            return self.reportSourceError(source, Error.UnsupportedExitCodeExpression, .@"error", "value type \"{t}\" cannot be coerced into an exit code", .{value.source});
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

        const data_size = self.data.size();

        return .{
            .data = try self.data.toOwnedSlice(self.allocator),
            .instructions = instructions,
            .labels = .{ .map = self.labels.map.move() },
            .struct_types = try self.struct_types.toOwnedSlice(self.allocator),
            .current_heap_addr = data_size,
        };
    }

    pub fn compile(self: *IRCompiler) Error!CompilationResult {
        self.current_instruction_set = try self.addInstructionSetNoPushFrame();

        try self.scopes.push(self.allocator);
        defer self.scopes.pop();

        const main_closure = try self.compileInitial();

        for (self.script.statements) |stmt| {
            _ = try self.compileStatement(stmt);
        }

        try self.compileMainClosureInitialization(null, main_closure);

        try self.addInstruction(.init(null, .exit_(.success)));

        if (self.diagnostics.items.len > 0) {
            return .{ .err = .{ ._diagnostics = self.diagnostics.items } };
        }

        return .{ .success = try self.toIRContext() };
    }

    fn compileInitial(self: *@This()) Error!MainClosureContext {
        try self.pushFrameNoInstructions();
        try self.addInstruction(.init(null, .fwd_stdio));
        self.currentFrame().rel_stack_counter += 4;
        const closure = try self.compileCreateMainClosure();

        const stdin_set = try self.addInstructionSet();
        const stdout_set = try self.addInstructionSet();
        const stderr_set = try self.addInstructionSet();

        const prev_instr_set = self.current_instruction_set;

        self.current_instruction_set = stdin_set;
        try self.stream(null, self.threadStdin());

        self.current_instruction_set = stdout_set;
        try self.stream(null, self.threadStdout());

        self.current_instruction_set = stderr_set;
        try self.stream(null, self.threadStderr());

        self.current_instruction_set = prev_instr_set;

        _ = try self.fork(null, .initAbs(stdin_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr(), .noll);
        _ = try self.fork(null, .initAbs(stdout_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr(), .noll);
        _ = try self.fork(null, .initAbs(stderr_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr(), .noll);

        return closure;
    }

    fn comment(self: *IRCompiler, comptime fmt: []const u8, args: anytype) !void {
        _ = self;
        _ = fmt;
        _ = args;
        return;
        // const comment_message = try std.fmt.allocPrint(self.allocator, fmt, args);
        // try self.addInstruction(.{ .type = .{ .comment = comment_message } });
    }

    fn compileStatement(
        self: *IRCompiler,
        stmt: *ast.Statement,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(stmt.span()), @src().fn_name });

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
            try self.comment("wait from {s}", .{@src().fn_name});
            try self.wait(stmt, loc);
        }

        return result;
    }

    fn isWaitable(result: Result) ?ir.Location {
        return switch (result.source) {
            .location => |loc| if (loc.isType(execution_handles_struct_type)) {
                return loc.dereference();
            } else if (loc.isType(thread_type)) {
                return loc;
            } else null,
            else => null,
        };
    }

    fn compileReturn(
        self: *IRCompiler,
        source: *ast.Statement,
        r: ast.ReturnStmt,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const result: Result = if (r.value) |value| try self.compileExpression(value) else .fromValue(.{ .exit_code = .success });
        try self.exit_(source, result);
        return .fromValue(.void);
    }

    fn compileBindingDecl(
        self: *IRCompiler,
        source: *ast.Statement,
        binding_decl: *ast.BindingDecl,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

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
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        return switch (pattern.*) {
            .discard => {
                _ = try self.compileExpressionWithCapture(source, expr);
                return .fromValue(.void);
            },
            .identifier => |identifier| {
                const result = try self.compileExpressionWithCapture(source, expr);
                try self.compileIdentifierBinding(source, identifier, result.source, is_mutable);
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

    fn compileIdentifierBinding(
        self: *IRCompiler,
        source: anytype,
        identifier: ast.Identifier,
        value: ir.ValueSource,
        is_mutable: bool,
    ) Error!void {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        var result: Result = .{ .source = value };

        if (result.source.isRegister(.r)) {
            const result_ref = try self.newRef(source, "identifier_ref");
            try self.set(source, result_ref, .fromLocation(.initRegister(.r)));
            result = try .from(result_ref.dereference().typed(value.typeExpr()));
        }
        if (is_mutable) {
            try self.compileMutableVariable(source, identifier, result.source);
        } else {
            try self.scopes.declare(
                self.allocator,
                identifier.name,
                result,
                is_mutable,
            );
        }
    }

    fn compileMutableVariable(
        self: *IRCompiler,
        source: anytype,
        identifier: ast.Identifier,
        value: ir.ValueSource,
    ) Error!void {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const closure_location = try self.declareClosureValue(
            .mutable(identifier),
            0,
            true,
            value.typeExpr(),
        );
        try self.alloc(source, 1);
        try self.set(
            source,
            .initAbs(.{ .register = .r }, .{ .dereference = true }),
            value,
        );
        try self.set(
            source,
            closure_location.undereference(),
            .fromLocation(.initRegister(.r)),
        );
    }

    const InlineSpanFormatter = union(enum) {
        success: struct {
            span: ast.Span,
            source: []const u8,
        },
        err,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            switch (self) {
                .success => |success| {
                    try writer.writeAll(success.span.sliceFrom(success.source));
                },
                .err => try writer.writeAll("<error getting source>"),
            }
        }
    };

    fn formatInlineSpan(self: *IRCompiler, span: ast.Span) InlineSpanFormatter {
        const source = self.document_store.getSource(span.start.file) catch return .err;
        return .{ .success = .{ .span = span, .source = source } };
    }

    fn compileExpression(
        self: *IRCompiler,
        expr: *ast.Expression,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(expr.span()), @src().fn_name });

        return switch (expr.*) {
            .literal => |literal| self.compileLiteral(expr, literal),
            .identifier => |identifier| self.compileIdentifier(expr, identifier),
            .call => |call| self.compileCall(expr, call),
            .if_expr => |if_expr| self.compileIf(expr, if_expr),
            .pipeline => |pipeline| self.compilePipeline(expr, pipeline),
            .block => |block| self.compileBlock(expr, block),
            .fn_decl => |fn_decl| self.compileFnDecl(expr, fn_decl),
            .binary => |binary| self.compileBinary(expr, binary),
            .array => |array| self.compileArray(expr, array),
            else => {
                try self.reportSourceError(expr, Error.UnsupportedExpression, .@"error", "expression type \"{t}\" not yet supported", .{expr.*});
                return .fromValue(.void);
            },
        };
    }

    fn compileExpressionWithCapture(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
    ) Error!Result {
        const expr_effects = self.analyzeExpressionEffects(expr);
        if (expr_effects.needs_stdio_capture) {
            // TODO: We need something that cleans up the pipes because otherwise they will get stuck if they are not used
            // Suggestion: Maybe we can have a nested variable structure for inner threads that will set to true when the thread is done processing. We can then continuously check that variable from the outermost scope (here), and whenever it is set to true, we would set the pipes to be closed.
            const stdout_pipe_ref = try self.newRef(source, "stdout_pipe");
            try self.pipe(source, stdout_pipe_ref);
            try self.pipeOpt(source, stdout_pipe_ref.dereference(), .complete_after_source_closed, .fromValue(.fromBoolean(true)));
            const stderr_pipe_ref = try self.newRef(source, "stderr_pipe");
            try self.pipe(source, stderr_pipe_ref);
            try self.pipeOpt(source, stderr_pipe_ref.dereference(), .complete_after_source_closed, .fromValue(.fromBoolean(true)));
            _ = try self.fork(source, self.stdoutStreamSet(), self.threadStdin(), stdout_pipe_ref.dereference(), self.threadStderr(), .noll);
            _ = try self.fork(source, self.stderrStreamSet(), self.threadStdin(), self.threadStdout(), stderr_pipe_ref.dereference(), .noll);
            var result = try self.compileWithContext(source, .{
                .out = stdout_pipe_ref.dereference(),
                .err = stderr_pipe_ref.dereference(),
            }, expr);

            // if (isWaitable(result)) |waitable| {
            //     try self.wait(source, waitable);
            if (result.isType(execution_handles_struct_type)) {
                // alloc 3 # create execution result
                try self.alloc(source, 3);
                // set [%r+0] = @@pipe_stdout
                try self.set(source, .initAdd(.{ .register = .r }, 0, .{ .dereference = true }), .from(stdout_pipe_ref.dereference()));
                // set [%r+1] = @@pipe_stderr
                try self.set(source, .initAdd(.{ .register = .r }, 1, .{ .dereference = true }), .from(stderr_pipe_ref.dereference()));
                // set [%r+2] = [@@execution_handles+0]
                try self.set(source, .initRegister(.r2), result.source.dereference());
                try self.set(source, .initAdd(.{ .register = .r }, 2, .{ .dereference = true }), .fromLocation(.initAdd(.{ .register = .r2 }, 1, .{ .dereference = true })));
                // set [C+?] = %r
                result = .fromLocation(.initRegister(.r));
                result = result.typed(execution_result_struct_type);
            }
            // }
            return result.dereference();
        } else {
            return try self.compileExpression(expr);
        }
    }

    fn compileLiteral(
        self: *IRCompiler,
        source: anytype,
        literal: ast.Literal,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        return switch (literal) {
            // .integer => |integer| .fromValue(try self.addSlice(integer.text)),
            .integer => |integer| .from(try parseInt(integer.text)),
            .float => |float| .from(try parseFloat(float.text)),
            .bool => |boolean| .fromValue(.{ .exit_code = .fromBoolean(boolean.value) }),
            .string => |string| self.compileStringLiteral(source, string),
            else => {
                try self.reportSourceError(source, Error.UnsupportedLiteral, .@"error", "literal type \"{t}\" not yet supported", .{literal});
                return .fromValue(.void);
            },
        };
    }

    fn field(
        addr: usize,
        struct_type: ir.Value.Struct.Type,
        field_name: []const u8,
    ) ir.Location {
        return .initAdd(.{ .heap = addr }, struct_type.fields.getIndex(field_name).?);
    }

    fn compileStringLiteral(
        self: *IRCompiler,
        source: anytype,
        string_literal: ast.StringLiteral,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (string_literal.segments.len == 1) {
            return .from(try self.addSlice(1, string_literal.segments[0].text.payload));
        }

        const ref = try self.newRef(source, "string_literal");
        try self.alloc(source, string_literal.segments.len + 1);
        try self.set(
            source,
            .initAbs(.{ .register = .r }, .{ .dereference = true }),
            .fromValue(.{ .uinteger = string_literal.segments.len }),
        );
        try self.set(source, ref, .fromLocation(.initRegister(.r)));
        // const s_tream = try self.allocator.alloc(ir.Value, string_literal.segments.len);

        for (string_literal.segments, 0..) |segment, i| switch (segment) {
            .text => |text| {
                const result = try self.addSlice(1, text.payload);
                try self.set(source, .initRegister(.r), .from(ref.dereference()));
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, i + 1, .{ .dereference = true }),
                    .from(result),
                );
                // s_tream[i] = try self.addSlice(1, text.payload);
            },
            .interpolation => |interp| {
                // TODO: make sure to compile this in a context because it might be "echo hello"
                var result = (try self.compileExpression(interp)).source;
                const is_result_reg_r = result.isRegister(.r);
                if (is_result_reg_r) {
                    const segment_ref = try self.newRef(source, "segment");
                    try self.set(source, segment_ref, result);
                    result = .from(segment_ref.dereference());
                }
                try self.set(source, .initRegister(.r), .from(ref.dereference()));
                // TODO: handle array coercion
                if (result == .location and result.location.isType(execution_result_struct_type)) {
                    try self.set(source, .initRegister(.r2), result.undereference());
                    result = .fromLocation(.initAdd(.{ .register = .r2 }, 0, .{ .dereference = true }));
                }
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, i + 1, .{ .dereference = true }),
                    result,
                );
                if (is_result_reg_r) {
                    _ = try self.pop(source);
                }
                // s_tream[i] = (try self.compileExpression(interp)).source;
            },
        };

        // return .fromValue(.{ .stream = s_tream });
        // return .fromValue(.{ .stream = undefined });
        const result = try self.pop(source);
        return .from(result);
    }

    fn parseInt(text: []const u8) std.fmt.ParseIntError!ir.Value {
        return .{ .uinteger = try std.fmt.parseInt(usize, text, 10) };
    }

    fn parseFloat(text: []const u8) std.fmt.ParseFloatError!ir.Value {
        return .{ .float = try std.fmt.parseFloat(f64, text) };
    }

    fn declareClosureValue(
        self: *IRCompiler,
        binding: Scope.ClosureBinding,
        depth: usize,
        is_mutable: bool,
        type_expr: ?ast.TypeExpr,
    ) Error!ir.Location {
        const frame = try self.scopes.getFrame(depth);
        const index = frame.closure.items.len;
        try frame.closure.append(self.allocator, binding);
        var location: ir.Location = .initAdd(.closure, index, .{});
        if (type_expr) |te| location = location.typed(te);
        try frame.declare(
            self.allocator,
            binding.identifier.name,
            try .from(location),
            is_mutable,
        );

        return location;
    }

    fn compileIdentifier(
        self: *IRCompiler,
        source: anytype,
        identifier: ast.Identifier,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const source_binding = self.lookup(identifier.name, .{ .shallow = false }) orelse {
            const executable = try self.addSlice(1, identifier.name);
            return .fromValue(.{ .executable = executable.slice });
        };

        if (!source_binding.is_mutable and source_binding.result.isFunctionRef()) {
            return source_binding.result;
        }

        if (self.lookup(identifier.name, .{ .shallow = true })) |local_binding| {
            if (local_binding.is_mutable) {
                return local_binding.result.dereference();
            }
            return local_binding.result;
        }

        const closure_value_location = try self.declareClosureValue(
            .outer(identifier),
            0,
            source_binding.is_mutable,
            source_binding.result.typeExpr(),
        );

        var i: usize = 1;
        while (self.lookup(identifier.name, .{ .shallow = true, .initial_depth = i }) == null) : (i += 1) {
            _ = try self.declareClosureValue(
                .outer(identifier),
                i,
                source_binding.is_mutable,
                source_binding.result.typeExpr(),
            );
        }

        if (source_binding.is_mutable) {
            return .from(closure_value_location.dereference());
        }
        return .from(closure_value_location);
    }

    fn compileCall(
        self: *IRCompiler,
        source: *ast.Expression,
        call: ast.CallExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const callee = try self.compileExpression(call.callee);

        return switch (callee.source) {
            .value => |v| switch (v) {
                .executable => self.compileExecutableCall(source, v, call.arguments),
                .fn_ref => self.compileFunctionCall(source, v, call.arguments),
                .slice, .stream, .addr, .void, .uinteger, .float, .strct, .exit_code, .pipe, .thread, .closeable => .from(v),
                .zig_string => Error.UnsupportedValueType,
            },
            .location => |loc| .from(loc),
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
            value.* = result.source;
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
        return .initAbs(.{ .stack = 0 }, .{ .dereference = true });
    }

    fn threadStdout(_: *IRCompiler) ir.Location {
        return .initAbs(.{ .stack = 1 }, .{ .dereference = true });
    }

    fn threadStderr(_: *IRCompiler) ir.Location {
        return .initAbs(.{ .stack = 2 }, .{ .dereference = true });
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

    const MainClosureContext = struct {
        index: usize,
    };

    const ClosureContext = struct {
        saved_r: ir.Location,
        closure_ref: ir.Location,
        index: usize,
    };

    fn compileCreateMainClosure(self: *IRCompiler) Error!MainClosureContext {
        const index = self.currentInstrSet().instructions.items.len;

        return .{
            .index = index,
        };
    }

    fn compileCreateClosure(self: *IRCompiler, source: anytype) Error!ClosureContext {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const saved_r = try self.newRef(source, "closure_saved_r");
        const closure_ref = try self.newRef(source, "closure");
        // self.currentFrame().rel_stack_counter -= 2;
        const index = self.currentInstrSet().instructions.items.len;

        return .{
            .saved_r = saved_r,
            .closure_ref = closure_ref,
            .index = index,
        };
    }

    fn consume(
        self: *IRCompiler,
        source: anytype,
        result: Result,
    ) Error!void {
        if (result.source.isStackLocation()) {
            _ = try self.pop(source);
        }
    }

    pub const Stdio = struct {
        in: ?ir.Location = null,
        out: ?ir.Location = null,
        err: ?ir.Location = null,
    };

    fn compileWithContext(
        self: *IRCompiler,
        source: anytype,
        stdio: Stdio,
        expr: *ast.Expression,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        // 3. create a instruction set for the expression compilation
        const instr_set = try self.addInstructionSet();
        // 2. declare a binding for the result of the execution of the expression
        const result_identifier = try self.compileResultVariable(source, .fromValue(.void));
        // 8. fork into new instruction set using the stdio context
        const stdin = stdio.in orelse self.threadStdin();
        const stdout = stdio.out orelse self.threadStdout();
        const stderr = stdio.err orelse self.threadStderr();
        // 1. create a closure for a new fork
        const closure = try self.compileCreateClosure(source);
        const fork_handle = try self.fork(
            source,
            .initAbs(instr_set, 0),
            stdin,
            stdout,
            stderr,
            closure.closure_ref.dereference(),
        );
        // 4. switch to new instruction set
        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = instr_set;
        try self.scopes.push(self.allocator);
        //   5. compile expression into the new instruction set
        const result = try self.compileExpression(expr);
        //   6. set result binding to result of compilation
        const source_binding = self.lookup(result_identifier.name, .{ .shallow = false }).?;
        source_binding.result = source_binding.result.typed(result.source.typeExpr());
        const result_variable = try self.compileIdentifier(source, result_identifier);
        try self.set(source, result_variable.dereference().source.location, result.source);
        // 7. switch back to original instruction set
        try self.setClosureIdentifiers();
        self.current_instruction_set = orig_instr_set;
        try self.compileClosureInitialization(source, closure);
        self.scopes.pop();
        // 9. wait for fork
        try self.wait(source, fork_handle);
        // 10. return binding
        return self.compileIdentifier(source, result_identifier);
    }

    fn setClosureIdentifiers(self: *IRCompiler) !void {
        const frame = try self.scopes.getFrame(0);
        self.currentInstrSet().closure_slot_count = frame.closure.items.len;

        var outer_count: usize = 0;
        for (frame.closure.items) |binding| {
            if (binding.type == .outer) outer_count += 1;
        }

        const closure_captures = try self.allocator.alloc(ClosureCapture, outer_count);
        var i: usize = 0;
        for (frame.closure.items, 0..) |binding, slot| {
            if (binding.type != .outer) continue;
            closure_captures[i] = .{
                .identifier = binding.identifier,
                .slot = slot,
            };
            i += 1;
        }
        self.currentInstrSet().closure_captures = closure_captures;
    }

    fn compileExecutableCall(
        self: *IRCompiler,
        source: *ast.Expression,
        executable: ir.Value,
        arguments: []const *ast.Expression,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const exec_instr_set = try self.addInstructionSet();

        const execution_handles = try self.newRef(source, "execution_handles");
        try self.alloc(source, 2);
        try self.set(source, execution_handles, .fromLocation(.initRegister(.r)));
        const result_variable_identifier = try self.compileResultVariable(
            source,
            .from(execution_handles.dereference()),
        );

        const closure = try self.compileCreateClosure(source);
        const thread_handle = try self.forkInherit(
            source,
            .initAbs(exec_instr_set, 0),
            closure.closure_ref.dereference(),
        );

        const prev_instr_set = self.current_instruction_set;
        self.current_instruction_set = exec_instr_set;
        try self.scopes.push(self.allocator);

        var it = std.mem.reverseIterator(arguments);
        while (it.next()) |arg_expr| {
            const arg = try self.compileExpression(arg_expr);
            try self.push(source, arg.source);
        }
        try self.push(source, .from(executable));
        try self.push(source, .fromValue(.{ .uinteger = arguments.len }));

        const exec_handle = try self.exec_(source, arguments.len, false);
        const exec_handle_ref = try self.newRef(source, "exec_handle");
        try self.set(source, exec_handle_ref, .from(exec_handle));
        const result_variable = try self.compileIdentifier(source, result_variable_identifier);
        try self.set(source, .initRegister(.r), result_variable.dereference().source);
        try self.set(
            source,
            .initAdd(.{ .register = .r }, 1, .{ .dereference = true }),
            .from(exec_handle_ref.dereference()),
        );
        try self.comment("wait from {s}", .{@src().fn_name});
        try self.wait(source, exec_handle_ref.dereference());

        try self.setClosureIdentifiers();
        self.current_instruction_set = prev_instr_set;
        try self.compileClosureInitialization(source, closure);
        self.scopes.pop();

        const thread_handle_ref = try self.newRef(source, "thread_handle");
        try self.set(source, thread_handle_ref, .from(thread_handle));
        try self.set(source, .initRegister(.r), .from(execution_handles.dereference()));
        try self.set(source, .initAdd(.{ .register = .r }, 0, .{ .dereference = true }), .from(thread_handle_ref.dereference()));
        _ = try self.pop(source);

        const execution_handles_r = try self.pop(source);
        return .from(execution_handles_r.typed(execution_handles_struct_type));
    }

    fn compileResultVariable(
        self: *IRCompiler,
        source: anytype,
        value: ir.ValueSource,
    ) Error!ast.Identifier {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const name = try std.fmt.allocPrint(
            self.allocator,
            "@result_{}",
            .{self.result_counter},
        );
        const identifier = ast.Identifier.global(name);
        self.result_counter += 1;
        try self.compileIdentifierBinding(source, identifier, value, true);
        return identifier;
    }

    fn compileMainClosureInitialization(
        self: *IRCompiler,
        source: anytype,
        closure: MainClosureContext,
    ) Error!void {
        const frame = try self.scopes.getFrame(0);
        const cl = frame.closure.items.len;
        var instructions: std.ArrayList(ir.Instruction) = try .initCapacity(self.allocator, 4 + cl);

        // alloc 2
        instructions.appendAssumeCapacity(.init(.from(source), .{ .alloc = cl }));
        // set S3 = %r
        instructions.appendAssumeCapacity(.init(.from(source), .{ .set = .{
            .destination = .initAbs(.{ .stack = 3 }, .{}),
            .source = .fromLocation(.initRegister(.r)),
        } }));

        try self.currentInstrSet().insertSlice(self.allocator, closure.index, try instructions.toOwnedSlice(self.allocator));
    }

    fn compileClosureInitialization(
        self: *IRCompiler,
        source: anytype,
        closure: ClosureContext,
    ) Error!void {
        const frame = try self.scopes.getFrame(0);
        const cl = frame.closure.items.len;
        var pre_instructions: std.ArrayList(ir.Instruction) = try .initCapacity(self.allocator, 5 + cl);

        const comment_message = try std.fmt.allocPrint(self.allocator, "{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });
        pre_instructions.appendAssumeCapacity(.init(null, .{ .comment = comment_message }));
        // set @@saved_r = %r
        pre_instructions.appendAssumeCapacity(.init(.from(source), .{ .set = .{
            .destination = closure.saved_r,
            .source = .fromLocation(.initRegister(.r)),
        } }));
        // alloc 2
        pre_instructions.appendAssumeCapacity(.init(.from(source), .{ .alloc = cl }));
        // set @@closure = %r
        pre_instructions.appendAssumeCapacity(.init(.from(source), .{ .set = .{
            .destination = closure.closure_ref,
            .source = .fromLocation(.initRegister(.r)),
        } }));
        // set [%r+0] = x
        // set [%r+1] = y
        for (0..cl) |i| pre_instructions.appendAssumeCapacity(.init(.from(source), .{ .set = .{
            .destination = .initAdd(.{ .register = .r }, i, .{ .dereference = true }),
            .source = self.lookup(frame.closure.items[i].identifier.name, .{
                .shallow = true,
                .initial_depth = frame.closure.items[i].depth,
            }).?.result.source,
        } }));
        // set %r = @@saved_r
        pre_instructions.appendAssumeCapacity(.init(.from(source), .{ .set = .{
            .destination = .initRegister(.r),
            .source = .fromLocation(closure.saved_r.dereference()),
        } }));
        // fork <5:0> [S0] [S1] [S2] @@closure
        var post_instructions: std.ArrayList(ir.Instruction) = try .initCapacity(self.allocator, 4);
        post_instructions.appendAssumeCapacity(.init(null, .{ .comment = comment_message }));
        // set @@saved_r = %r
        post_instructions.appendAssumeCapacity(.init(.from(source), .{ .set = .{
            .destination = closure.saved_r,
            .source = .fromLocation(.initRegister(.r)),
        } }));
        // pop
        self.currentFrame().rel_stack_counter -= 1;
        post_instructions.appendAssumeCapacity(.init(.from(source), .pop));
        // pop
        self.currentFrame().rel_stack_counter -= 1;
        post_instructions.appendAssumeCapacity(.init(.from(source), .pop));

        try self.currentInstrSet().insertSlice(self.allocator, closure.index + 1, try post_instructions.toOwnedSlice(self.allocator));
        try self.currentInstrSet().insertSlice(self.allocator, closure.index, try pre_instructions.toOwnedSlice(self.allocator));
    }

    fn compileFunctionCall(
        self: *IRCompiler,
        source: *ast.Expression,
        fn_ref_value: ir.Value,
        arguments: []const *ast.Expression,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const fn_ref = fn_ref_value.fn_ref;
        const fn_addr = fn_ref.fn_addr;

        // Manual closure compilation
        // TODO: Add closure variables as well (we need to extend the function reference value to be able to understand what closure variables are needed)
        const closure_captures = self.instruction_sets.items[fn_ref.fn_addr.instr_set].closure_captures;
        const closure_size = self.instruction_sets.items[fn_ref.fn_addr.instr_set].closure_slot_count;
        try self.alloc(source, closure_size);
        const closure_ref = try self.newRef(source, "closure");
        try self.set(source, closure_ref, .fromLocation(.initRegister(.r)));

        try self.alloc(source, closure_size);
        const closure_dumb_ref = try self.newRef(source, "closure_dumb");
        try self.set(source, closure_dumb_ref, .fromLocation(.initRegister(.r)));

        for (arguments, 0..) |arg, i| {
            const arg_result = try self.compileExpression(arg);
            const arg_ref = try self.newRef(source, "arg");
            try self.set(source, arg_ref, arg_result.source);
            try self.set(source, .initRegister(.r2), .from(closure_dumb_ref.dereference()));
            try self.set(source, .initAdd(.{ .register = .r2 }, i, .{ .dereference = true }), arg_result.source);
            try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
            try self.set(source, .initAdd(.{ .register = .r }, i, .{ .dereference = true }), .fromLocation(.initAdd(.{ .register = .r2 }, i, .{})));
            try self.set(source, .initRegister(.r2), .fromLocation(.initRegister(.r)));
            try self.consume(source, try .from(arg_ref));
        }

        try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
        try self.set(source, .initRegister(.r2), .from(closure_dumb_ref.dereference()));

        for (closure_captures) |capture| {
            // NOTE: Guard against refering to internal identifiers
            if (self.lookup(capture.identifier.name, .{ .shallow = false }) == null) continue;

            const identifier_result = try self.compileIdentifier(source, capture.identifier);

            if (identifier_result.source == .location and identifier_result.source.location.abs == .data) {
                try self.set(
                    source,
                    .initAdd(.{ .register = .r2 }, capture.slot, .{ .dereference = true }),
                    identifier_result.source.undereference(),
                );
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, capture.slot, .{ .dereference = true }),
                    .fromLocation(.initAdd(.{ .register = .r2 }, capture.slot, .{ .dereference = true })),
                );
            } else if (identifier_result.source == .location) {
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, capture.slot, .{ .dereference = true }),
                    identifier_result.source.undereference(),
                );
            } else {
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, capture.slot, .{ .dereference = true }),
                    identifier_result.source,
                );
            }
        }

        const handle = try self.forkInherit(
            source,
            fn_addr,
            closure_ref.dereference(),
        );
        try self.set(source, .initRegister(.r2), .from(handle));
        try self.consume(source, try .from(closure_ref));
        try self.set(source, .initRegister(.r), .fromLocation(.initRegister(.r2)));

        return .fromLocation(
            ir.Location.initRegister(.r).typed(handle.options.type_expr),
        );
    }

    fn compileIf(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

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
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const condition = try self.compileExpression(if_expr.condition);
        if (condition.source.isValueTag(.exit_code)) {
            const c = condition.source.value.exit_code.toBoolean();

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
        try self.set(source, result, then.source);
        try self.jmp(source, null, false, after_addr);
        try self.setLabel(else_addr.local_addr.label, .abs);
        const else_ = try switch (else_branch) {
            .expr => |expr_| self.compileExpression(expr_),
            .if_expr => |if_expr_| self.compileIf(source, if_expr_.*),
        };
        try self.set(source, result, else_.source);
        try self.setLabel(after_addr.local_addr.label, .abs);

        const result_loc = try self.pop(source);

        return .from(result_loc);
    }

    fn compileIfNoElse(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        try self.pushFrame(source);

        const condition = try self.compileExpression(if_expr.condition);
        if (condition.source.isValueTag(.exit_code)) {
            const c = condition.source.value.exit_code.toBoolean();

            if (c) return self.compileExpression(if_expr.then_expr);
            return .fromValue(.void);
        }

        const result = try self.newRef(source, "if_result");

        const after_addr = try self.newLabel("if_after", .unknown);

        try self.jmp(source, condition, false, after_addr);

        const then = try self.compileExpression(if_expr.then_expr);
        try self.set(source, result, then.source);

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
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const refs = try self.allocator.alloc(ir.Location, pipeline.stages.len - 1);
        defer self.allocator.free(refs);

        for (refs) |*ref| {
            ref.* = try self.newRef(source, "pipe");
            try self.pipe(source, ref.dereference());
            try self.pipeOpt(
                source,
                ref.dereference(),
                .keep_open,
                .fromValue(.fromBoolean(true)),
            );
        }

        const orig_instr_set = self.current_instruction_set;
        const stage_sets = try self.allocator.alloc(usize, pipeline.stages.len - 1);
        const stage_closures = try self.allocator.alloc(ClosureContext, pipeline.stages.len - 1);
        defer self.allocator.free(stage_sets);
        defer self.allocator.free(stage_closures);
        for (stage_sets) |*stage_set| {
            stage_set.* = try self.addInstructionSet();
        }
        const last_set = try self.addInstructionSet();

        try self.comment("first stage", .{});
        stage_closures[0] = try self.compileCreateClosure(source);
        _ = try self.fork(
            source,
            .initAbs(stage_sets[0], 0),
            self.threadStdin(),
            refs[0].dereference(),
            self.threadStderr(),
            stage_closures[0].closure_ref.dereference(),
        );
        // TODO: Handle this more generically that also covers the other use cases for compiling closures. Right now this is handled in the compileClosureInitialization function. The following line (and the other lines following the same pattern in this function) is a special handling since we are compiling multiple closures at the same place consecutively.
        self.currentFrame().rel_stack_counter -= 2;

        for (refs[0 .. refs.len - 1], refs[1..], stage_sets[1..], 1..) |prev, curr, stage_set, i| {
            try self.comment("stage {}", .{i});
            stage_closures[i] = try self.compileCreateClosure(source);
            _ = try self.fork(
                source,
                .initAbs(stage_set, 0),
                prev.dereference(),
                curr.dereference(),
                self.threadStderr(),
                stage_closures[i].closure_ref.dereference(),
            );
            // TODO: As stated above, we need special handling here for the rel_stack_counter.
            self.currentFrame().rel_stack_counter -= 2;
        }

        try self.comment("last stage", .{});
        var last_closure = try self.compileCreateClosure(source);
        const last_fork_handle = try self.fork(
            source,
            .initAbs(last_set, 0),
            refs[refs.len - 1].dereference(),
            self.threadStdout(),
            self.threadStderr(),
            last_closure.closure_ref.dereference(),
        );
        // TODO: As stated above, we need special handling here for the rel_stack_counter.
        self.currentFrame().rel_stack_counter -= 2;

        var n_instructions_diff: usize = 0;

        for (stage_sets, stage_closures, pipeline.stages[0 .. pipeline.stages.len - 1], 0..) |*stage_set, *stage_closure, stage_expr, i| {
            self.current_instruction_set = stage_set.*;
            try self.scopes.push(self.allocator);
            const result = try self.compileExpression(stage_expr);

            if (isWaitable(result)) |loc| {
                try self.push(source, result.source);
                _ = try self.forkInherit(source, self.stdoutStreamSet(), .noll);
                const result_stack = (try self.pop(source)).typed(loc.options.type_expr);
                if (isWaitable(try .from(result_stack))) |loc_| {
                    try self.comment("wait from {s}", .{@src().fn_name});
                    try self.wait(source, loc_);
                }
                try self.pipeOpt(
                    source,
                    self.threadStdout(),
                    .keep_open,
                    .fromValue(.fromBoolean(false)),
                );
            } else {
                return Error.UnsupportedExpression;
            }

            try self.setClosureIdentifiers();
            self.current_instruction_set = orig_instr_set;
            stage_closure.index += n_instructions_diff;
            const n_instructions_before = self.instruction_sets.items[self.current_instruction_set].instructions.items.len;
            try self.comment("closure initialization stage {}: {}", .{ i, stage_closure.index });
            try self.compileClosureInitialization(source, stage_closure.*);
            const n_instructions_after = self.instruction_sets.items[self.current_instruction_set].instructions.items.len;
            n_instructions_diff += n_instructions_after - n_instructions_before - 1;
            // TODO: As stated above, we need special handling here for the rel_stack_counter.
            self.currentFrame().rel_stack_counter += 2;
            self.scopes.pop();
        }
        self.current_instruction_set = orig_instr_set;

        self.current_instruction_set = last_set;
        try self.scopes.push(self.allocator);
        const result = try self.compileExpression(pipeline.stages[pipeline.stages.len - 1]);
        if (isWaitable(result)) |loc| {
            try self.comment("wait from {s} (last stage)", .{@src().fn_name});
            try self.wait(source, loc);
        }
        try self.setClosureIdentifiers();
        self.current_instruction_set = orig_instr_set;
        last_closure.index += n_instructions_diff;
        try self.comment("closure initialization last stage: {}", .{last_closure.index});
        try self.compileClosureInitialization(source, last_closure);
        // TODO: As stated above, we need special handling here for the rel_stack_counter.
        self.currentFrame().rel_stack_counter += 2;
        self.scopes.pop();

        return .from(last_fork_handle);
    }

    fn compileBlock(
        self: *IRCompiler,
        source: *ast.Expression,
        block: ast.Block,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const block_stdout = try self.newRef(source, "block_stdout_pipe");

        try self.pipe(source, block_stdout.dereference());
        try self.pipeOpt(
            source,
            block_stdout.dereference(),
            .disconnect_destination,
            .fromValue(.fromBoolean(false)),
        );
        try self.pipeOpt(
            source,
            block_stdout.dereference(),
            .close_destination,
            .fromValue(.fromBoolean(false)),
        );
        try self.pipeOpt(
            source,
            block_stdout.dereference(),
            .keep_open,
            .fromValue(.fromBoolean(true)),
        );
        try self.pipeFwd(
            source,
            block_stdout.dereference(),
            self.threadStdout(),
        );
        const block_set = try self.addInstructionSet();

        const closure = try self.compileCreateClosure(source);
        const block_result = try self.fork(
            source,
            .initAbs(block_set, 0),
            self.threadStdin(),
            block_stdout.dereference(),
            self.threadStderr(),
            closure.closure_ref.dereference(),
        );

        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = block_set;
        try self.scopes.push(self.allocator);

        const block_stdout_thread = try self.forkInherit(source, self.stdoutStreamSet(), .noll);
        const block_stdout_thread_ref = try self.newRef(source, "block_stdout_thread");
        try self.set(source, block_stdout_thread_ref, .from(block_stdout_thread));
        // try self.push(source, .from(block_stdout_thread));

        for (block.statements) |stmt| {
            // TODO: figure out what to do with these results
            _ = try self.compileStatement(stmt);
        }
        // const pipe_opts_set = try self.addInstructionSet();
        // try self.atomic(source, pipe_opts_set);
        // self.current_instruction_set = pipe_opts_set;
        try self.pipeOpt(
            source,
            self.threadStdout(),
            .keep_open,
            .fromValue(.fromBoolean(false)),
        );
        // try self.pipeOpt(source, self.threadStdout(), .disconnect_destination, true);
        // _ = try self.pop(source);
        try self.wait(source, block_stdout_thread_ref.dereference());

        try self.setClosureIdentifiers();
        self.current_instruction_set = orig_instr_set;
        try self.compileClosureInitialization(source, closure);
        self.scopes.pop();

        return .from(block_result);
    }

    fn compileFnDecl(
        self: *IRCompiler,
        source: *ast.Expression,
        fn_decl: ast.FunctionDecl,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const instr_set = try self.addInstructionSet();
        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = instr_set;
        try self.scopes.push(self.allocator);

        for (fn_decl.params._non_variadic) |param| {
            switch (param.pattern.*) {
                .discard => {},
                .identifier => |identifier| {
                    // TODO: types?
                    _ = try self.declareClosureValue(.mutable(identifier), 0, true, null);
                },
                .tuple, .record => unreachable,
            }
        }

        // TODO: closure bindings, how do we manage them (non-parameters)?
        // TODO: figure out how to be able to call async functions multiple times and have the result not be overwritten in a ref
        const result = try self.compileExpression(fn_decl.body);
        // TODO: figure out how to make this async
        if (isWaitable(result)) |loc| {
            try self.wait(source, loc);
        }

        try self.setClosureIdentifiers();
        self.current_instruction_set = orig_instr_set;
        self.scopes.pop();
        const fn_ref = ir.Value{
            .fn_ref = .{ .fn_addr = ir.InstructionAddr.initAbs(instr_set, 0) },
        };
        if (fn_decl.name) |name| {
            try self.scopes.declare(self.allocator, name.name, try .from(fn_ref), false);
        }

        return .from(fn_ref);
    }

    fn compileResultSaveR(
        self: *IRCompiler,
        source: *ast.Expression,
        result: Result,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (result.source.isRegister(.r)) {
            const ref = try self.newRef(source, "saved_r");
            try self.set(source, ref, .fromLocation(.initRegister(.r)));
            return .from(ref);
        }

        return result;
    }

    fn compileBinary(
        self: *IRCompiler,
        source: *ast.Expression,
        binary: ast.BinaryExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        switch (binary.op) {
            .add, .subtract, .multiply, .divide, .remainder => {
                const left = (try self.compileExpression(binary.left)).dereference();
                const right = (try self.compileExpression(binary.right)).dereference();

                if (evaluateArithmetic(.from(binary.op), left.source, right.source)) |comptime_result| {
                    return .from(comptime_result);
                }

                const ref = try self.newRef(source, "ath_result");

                try self.addInstruction(.init(.from(source), .{ .ath = .{
                    .op = .from(binary.op),
                    .a = left.source,
                    .b = right.source,
                    .result = ref,
                } }));

                return .from(ref.dereference());
            },
            .logical_and, .logical_or => {
                const left = try self.compileExpression(binary.left);

                if (evaluateLogical(.from(binary.op), left.source)) |comptime_result| {
                    switch (comptime_result) {
                        .left => return left,
                        .right => return self.compileExpression(binary.right),
                    }
                }

                const right = try self.compileExpression(binary.right);
                const ref = try self.newRef(source, "logical_result");

                try self.addInstruction(.init(.from(source), .{ .log = .{
                    .op = .from(binary.op),
                    .a = left.source,
                    .b = right.source,
                    .result = ref,
                } }));

                return .from(ref.dereference());
            },
            .greater, .greater_equal, .less, .less_equal, .equal, .not_equal => {
                const left = try self.compileExpression(binary.left);
                const right = try self.compileExpression(binary.right);

                if (evaluateCompare(.from(binary.op), left.source, right.source)) |comptime_result| {
                    return .from(comptime_result);
                }

                const ref = try self.newRef(source, "cmp_result");

                try self.addInstruction(.init(.from(source), .{ .cmp = .{
                    .op = .from(binary.op),
                    .a = left.source,
                    .b = right.source,
                    .result = ref,
                } }));

                return .from(ref.dereference());
            },
            .assign => {
                const left = try self.compileExpression(binary.left);
                const right = try self.compileExpression(binary.right);
                try self.set(source, left.source.location, right.source);

                return .from(left.source.location);
            },
            .add_assign, .minus_assign, .mul_assign, .div_assign, .rem_assign => {
                const right = try self.allocator.create(ast.Expression);
                right.* = .{ .binary = .{
                    .op = binary.op.unwrapAssign(),
                    .left = binary.left,
                    .right = binary.right,
                    .span = binary.span,
                } };
                return self.compileBinary(source, .{
                    .op = .assign,
                    .left = binary.left,
                    .right = right,
                    .span = binary.span,
                });
            },
            .array_access => {
                const array_access_ref = try self.newRef(source, "array_access_ref");

                const left = try self.compileExpression(binary.left);
                const left_ref = try self.newRef(source, "array_access_left_ref");
                try self.set(source, left_ref, left.source);
                const right = try self.compileExpression(binary.right);

                try self.addInstruction(.init(.from(source), .{ .ath = .{
                    .op = .add,
                    .a = left.source,
                    .b = right.source,
                    .result = .initRegister(.r2),
                } }));
                try self.inc(source);

                try self.set(
                    source,
                    array_access_ref,
                    .fromLocation(.initAbs(.{ .register = .r2 }, .{ .dereference = true })),
                );

                return .from(array_access_ref.dereference());
            },
            .apply, .pipe => {
                try self.log(@src().fn_name ++ ": error, encountered {t} binary expression", .{binary.op});
                try self.logEvaluateSpan(binary.span);
            },
            .member => {},
        }

        try self.reportSourceError(source, Error.UnsupportedBinaryOperation, .@"error", "binary operator \"{t}\" not yet supported", .{binary.op});
        return .fromValue(.void);
    }

    fn compileArray(
        self: *IRCompiler,
        source: *ast.Expression,
        array: ast.ArrayLiteral,
    ) Error!Result {
        try self.alloc(source, array.elements.len + 1);
        try self.set(source, .initAbs(.{ .register = .r }, .{ .dereference = true }), .fromValue(.{ .uinteger = array.elements.len }));
        const array_ref = try self.newRef(source, "array");
        try self.set(source, array_ref, .fromLocation(.initRegister(.r)));
        var element_type: ?ast.TypeExpr = null;
        for (array.elements, 1..) |element, i| {
            const result = try self.compileExpressionWithCapture(source, element);
            element_type = result.typeExpr();
            try self.set(source, .initRegister(.r2), .from(array_ref.dereference()));
            try self.set(source, .initAdd(.{ .register = .r2 }, i, .{ .dereference = true }), result.source);
        }
        const resolved_element_type = try self.allocator.create(ast.TypeExpr);
        resolved_element_type.* = element_type orelse .global(.void);
        const array_result = try self.pop(source);
        return .from(array_result.typed(array_type(resolved_element_type)));
    }

    fn analyzeExpressionEffects(
        self: *IRCompiler,
        expr: *ast.Expression,
    ) ExprEffects {
        return switch (expr.*) {
            .call => .{ .needs_stdio_capture = true }, // conservative
            .pipeline => .{ .needs_stdio_capture = true }, // definitely stdio-heavy
            .block => .{ .needs_stdio_capture = true }, // may emit output
            .if_expr => |if_expr| self.analyzeIfExpressionEffects(if_expr),
            .array => |_| .{ .needs_stdio_capture = false },
            else => .{},
        };
    }

    fn analyzeArrayExpressionEffects(
        self: *IRCompiler,
        array: ast.ArrayLiteral,
    ) ExprEffects {
        var result = ExprEffects.empty;
        for (array.elements) |e| result.merge(self.analyzeExpressionEffects(e));
        return result;
    }

    fn analyzeIfExpressionEffects(
        self: *IRCompiler,
        if_expr: ast.IfExpr,
    ) ExprEffects {
        // TODO: there is a bug here if one of the branches does need stdio capture, but the other branch(es) does not, and the runtime goes into one of the branches that does NOT require capture, then it would hang because the pipes are not cleaned up properly
        var out: ExprEffects = .{};
        const c = self.analyzeExpressionEffects(if_expr.condition);
        const t = self.analyzeExpressionEffects(if_expr.then_expr);
        out.needs_stdio_capture = c.needs_stdio_capture or t.needs_stdio_capture;
        if (if_expr.else_branch) |e| switch (e) {
            .expr => |ee| out.needs_stdio_capture = out.needs_stdio_capture or
                self.analyzeExpressionEffects(ee).needs_stdio_capture,
            .if_expr => |ie| out.needs_stdio_capture = out.needs_stdio_capture or
                self.analyzeIfExpressionEffects(ie.*).needs_stdio_capture,
        };
        return out;
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

const ExprEffects = struct {
    // Needs forked context with custom stdout/stderr capture pipes.
    needs_stdio_capture: bool = false,

    pub const empty: @This() = .{};

    pub fn merge(self: *@This(), other: @This()) void {
        self.needs_stdio_capture = self.needs_stdio_capture or other.needs_stdio_capture;
    }
};

const RefDef = struct {
    name: []const u8,
    rel_stack_addr: usize,
};
