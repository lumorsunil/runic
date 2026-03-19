const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir.zig");
const ast = @import("../frontend/ast.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;
const rainbow = @import("../rainbow.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;
const Stream = @import("../stream.zig").Stream;
const RCError = @import("../mem/rc.zig").RCError;
const FrontendDocumentStore = @import("../frontend/document_store.zig").FrontendDocumentStore;
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
        .name = .global("merged"),
        .type_expr = &.{ .integer = .{ .span = .global } },
        .span = .global,
    }, .{
        .name = .global("closeable"),
        .type_expr = &.{ .integer = .{ .span = .global } },
        .span = .global,
    }, .{
        .name = .global("completion_is_thread"),
        .type_expr = &.{ .boolean = .{ .span = .global } },
        .span = .global,
    } },
} };

pub const thread_type = ast.TypeExpr.global(.thread);
pub const string_element_type = ast.TypeExpr.global(.byte);
pub const string_type = ast.TypeExpr{ .array = .{
    .element = &string_element_type,
    .span = .global,
} };
pub const optional_string_type = ast.TypeExpr{ .optional = .{
    .child = &string_type,
    .span = .global,
} };

pub const ExecutionHandlesField = enum {
    thread,
    closeable,
};

pub const ExecutionResultField = enum {
    stdout,
    stderr,
    merged,
    closeable,
    completion_is_thread,
};

pub fn executionHandlesFieldOffset(field: ExecutionHandlesField) usize {
    return switch (field) {
        .thread => 0,
        .closeable => 1,
    };
}

pub fn executionResultFieldOffset(field: ExecutionResultField) usize {
    return switch (field) {
        .stdout => 0,
        .stderr => 1,
        .merged => 2,
        .closeable => 3,
        .completion_is_thread => 4,
    };
}

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
        InternalInvariantViolation,
        DataTooLargeToFitInPage,
        ScopeNotFound,
        LabelAddrNotSet,
        StructTypeNotFound,
        NotImplemented,
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

fn mergedResultType(a: Result, b: Result) ?ast.TypeExpr {
    const a_type = a.typeExpr() orelse return null;
    const b_type = b.typeExpr() orelse return null;
    if (!std.meta.eql(a_type, b_type)) return null;
    return a_type;
}

fn stableResultSource(result: Result) ir.ValueSource {
    return result.source;
}

const capture_temp_ref_count = 5;

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

    pub const ScopeType = enum { lexical, closure };

    pub const Frame = struct {
        bindings: std.StringArrayHashMapUnmanaged(Binding) = .empty,
        closure_bindings: std.ArrayList(ClosureBinding) = .empty,
        scope_type: ScopeType,

        pub const lexical = Frame{ .scope_type = .lexical };
        pub const closure = Frame{ .scope_type = .closure };

        pub fn declare(
            self: *Frame,
            allocator: Allocator,
            name: []const u8,
            result: Result,
            type_expr: ?ast.TypeExpr,
            is_mutable: bool,
            kind: Binding.Kind,
        ) Error!void {
            return self.bindings.put(allocator, name, .{
                .is_mutable = is_mutable,
                .result = result,
                .type_expr = type_expr,
                .kind = kind,
            });
        }
    };

    pub const Binding = struct {
        pub const Kind = enum {
            normal,
            env_var,
        };

        is_mutable: bool,
        result: Result,
        type_expr: ?ast.TypeExpr = null,
        kind: Kind = .normal,
    };

    pub const ClosureBinding = struct {
        depth: usize,
        type: enum { outer, mutable },
        identifier: ast.Identifier,
        kind: Binding.Kind = .normal,

        pub fn outer(
            identifier: ast.Identifier,
            depth: usize,
            kind: Binding.Kind,
        ) @This() {
            return .{
                .type = .outer,
                .identifier = identifier,
                .depth = depth,
                .kind = kind,
            };
        }

        pub fn mutable(
            identifier: ast.Identifier,
            kind: Binding.Kind,
        ) @This() {
            return .{
                .type = .mutable,
                .identifier = identifier,
                .depth = 0,
                .kind = kind,
            };
        }
    };

    pub fn init() @This() {
        return .{};
    }

    pub fn push(self: *Scope, allocator: Allocator, frame: Frame) !void {
        return self.frames.append(allocator, frame);
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
        type_expr: ?ast.TypeExpr,
        is_mutable: bool,
        kind: Binding.Kind,
    ) Error!void {
        const frame = try self.getFrame(0);
        return frame.declare(allocator, name, result, type_expr, is_mutable, kind);
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
    script_args: []const []const u8,
    env: ?*const std.process.EnvMap = null,
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
        script_args: []const []const u8,
        env: ?*const std.process.EnvMap,
    ) Allocator.Error!@This() {
        const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_" ++ logging_name) catch null;
        const logging_enabled = if (logging_enabled_s) |le| std.mem.eql(u8, le, "1") else false;

        return .{
            .allocator = allocator,
            .script = script,
            .script_args = script_args,
            .env = env,
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
        const local_addr = self.getLocalAddr(addr);
        return .initLabel(
            self.current_instruction_set,
            try self.labels.new(self.allocator, name, if (local_addr) |a| .init(self.current_instruction_set, a) else null),
        );
    }

    pub fn setLabel(
        self: *@This(),
        label: ir.InstructionAddr.LabelKey,
        addr: SetLabelAddr,
    ) Allocator.Error!void {
        const local_addr = self.getLocalAddr(addr);
        return self.labels.set(self.allocator, label, if (local_addr) |a| .init(self.current_instruction_set, a) else null);
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
        return self.scopes.declare(
            self.allocator,
            name,
            result,
            result.typeExpr(),
            is_mutable,
            .normal,
        );
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

    pub fn pipeWrite(
        self: *IRCompiler,
        source: anytype,
        pipe_handle: ir.Location,
        value_source: ir.ValueSource,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .pipe_write = .{
                .pipe = pipe_handle,
                .source = value_source,
            },
        }));
    }

    pub fn pipeFile(
        self: *IRCompiler,
        source: anytype,
        pipe_location: ir.Location,
        target: ir.ValueSource,
        mode: ast.RedirectionMode,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .pipe_file = .{
                .pipe = pipe_location,
                .target = target,
                .mode = mode,
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

    pub fn ath(
        self: *IRCompiler,
        source: anytype,
        op: ast.BinaryOp,
        left: ir.ValueSource,
        right: ir.ValueSource,
        result: ir.Location,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{ .ath = .{
            .op = .from(op),
            .a = left,
            .b = right,
            .result = result,
        } }));
    }

    pub fn cmp(
        self: *IRCompiler,
        source: anytype,
        op: ast.BinaryOp,
        left: ir.ValueSource,
        right: ir.ValueSource,
        result: ir.Location,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{ .cmp = .{
            .op = .from(op),
            .a = left,
            .b = right,
            .result = result,
        } }));
    }

    pub fn neg(
        self: *IRCompiler,
        source: anytype,
        operand: ir.Location,
        result: ir.Location,
    ) Error!ir.Location {
        try self.addInstruction(.init(.from(source), .{ .neg = .{
            .operand = operand,
            .result = result,
        } }));
        return result.typed(operand.options.type_expr);
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
        subshell: ir.Instruction.Fork.Subshell,
    ) Error!ir.Location {
        try self.addInstruction(.init(.from(source), .fork_(
            dest,
            stdin,
            stdout,
            stderr,
            closure,
            subshell,
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
            .inherit,
        );
    }

    pub fn wait(self: *IRCompiler, source: anytype, waitee: ir.Location) Error!void {
        return self.addInstruction(.init(.from(source), .wait_(waitee)));
    }

    pub fn stream(self: *IRCompiler, source: anytype, streamee: ir.Location) Error!void {
        return self.addInstruction(.init(.from(source), .stream_(streamee)));
    }

    pub fn setEnv(
        self: *IRCompiler,
        source: anytype,
        name: []const u8,
        value: ir.ValueSource,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .set_env = .{
                .name = name,
                .value = value,
            },
        }));
    }

    pub fn getEnv(
        self: *IRCompiler,
        source: anytype,
        name: []const u8,
        result: ir.Location,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{
            .get_env = .{
                .name = name,
                .result = result,
            },
        }));
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

    pub fn exitWith(
        self: *IRCompiler,
        source: anytype,
        value: Result,
    ) Error!void {
        return self.addInstruction(.init(.from(source), .{ .exit_with = value.source }));
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

        try self.scopes.push(self.allocator, .closure);
        defer self.scopes.pop();

        const main_closure = try self.compileInitial();

        if (self.script.signature) |signature| {
            switch (signature.params) {
                ._non_variadic => |params| {
                    const accepts_script_args_array = params.len == 1 and self.isStringArrayParam(params[0]);

                    if (!accepts_script_args_array and params.len != self.script_args.len) {
                        const diag_source: ?*ast.Statement = if (self.script.statements.len > 0)
                            self.script.statements[0]
                        else
                            null;
                        try self.reportSourceError(
                            diag_source,
                            Error.UnsupportedExpression,
                            .@"error",
                            "expected {} script arguments, got {}",
                            .{ params.len, self.script_args.len },
                        );
                    } else if (accepts_script_args_array) {
                        const param = params[0];
                        switch (param.pattern.*) {
                            .discard => {},
                            .identifier => |identifier| {
                                const value = try self.compileScriptArgsArray(param.type_annotation, self.script_args);
                                try self.compileIdentifierBinding(
                                    null,
                                    identifier,
                                    value,
                                    param.type_annotation,
                                    false,
                                    .normal,
                                );
                            },
                            .tuple, .record => return Error.UnsupportedBindingPattern,
                        }
                    } else {
                        for (params, self.script_args) |param, arg| {
                            switch (param.pattern.*) {
                                .discard => {},
                                .identifier => |identifier| {
                                    const value = try self.addSlice(1, arg);
                                    try self.compileIdentifierBinding(
                                        null,
                                        identifier,
                                        .fromValue(value),
                                        param.type_annotation,
                                        false,
                                        .normal,
                                    );
                                },
                                .tuple, .record => return Error.UnsupportedBindingPattern,
                            }
                        }
                    }
                },
                ._variadic => return Error.UnsupportedExpression,
            }
        }

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

    fn isStringArrayParam(self: *IRCompiler, param: *const ast.Parameter) bool {
        _ = self;
        const type_annotation = param.type_annotation orelse return false;
        return switch (type_annotation.*) {
            .array => |array| switch (array.element.*) {
                .identifier => |identifier| identifier.path.segments.len == 1 and std.mem.eql(u8, identifier.path.segments[0].name, "String"),
                else => false,
            },
            else => false,
        };
    }

    fn compileScriptArgsArray(
        self: *IRCompiler,
        type_annotation: ?*const ast.TypeExpr,
        args: []const []const u8,
    ) Error!ir.ValueSource {
        try self.alloc(null, args.len + 1);
        try self.set(null, .initAbs(.{ .register = .r }, .{ .dereference = true }), .fromValue(.{ .uinteger = args.len }));

        const array_ref = try self.newRef(null, "script_args");
        try self.set(null, array_ref, .fromLocation(.initRegister(.r)));

        for (args, 1..) |arg, i| {
            const value = try self.addSlice(1, arg);
            try self.set(null, .initRegister(.r2), .from(array_ref.dereference()));
            try self.set(null, .initAdd(.{ .register = .r2 }, i, .{ .dereference = true }), .fromValue(value));
        }

        var location = array_ref.dereference();
        if (type_annotation) |annotation| {
            location = location.typed(annotation.*);
        }
        return .fromLocation(location);
    }

    fn compileInitial(self: *@This()) Error!MainClosureContext {
        try self.pushFrameNoInstructions();
        try self.addInstruction(.init(null, .fwd_stdio));
        self.currentFrame().rel_stack_counter += 4;

        const stdin_set = try self.addInstructionSet();
        const stdout_set = try self.addInstructionSet();
        const stderr_set = try self.addInstructionSet();

        const closure = try self.compileCreateMainClosure();

        const prev_instr_set = self.current_instruction_set;

        self.current_instruction_set = stdin_set;
        try self.stream(null, self.threadStdin());

        self.current_instruction_set = stdout_set;
        try self.stream(null, self.threadStdout());

        self.current_instruction_set = stderr_set;
        try self.stream(null, self.threadStderr());

        self.current_instruction_set = prev_instr_set;

        _ = try self.fork(null, .initAbs(stdin_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr(), .noll, .inherit);
        _ = try self.fork(null, .initAbs(stdout_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr(), .noll, .inherit);
        _ = try self.fork(null, .initAbs(stderr_set, 0), self.threadStdin(), self.threadStdout(), self.threadStderr(), .noll, .inherit);

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

        return switch (stmt.*) {
            .type_binding_decl => Result.fromValue(.void),
            .binding_decl => |*b| self.compileBindingDecl(stmt, b),
            .return_stmt => |r| self.compileReturn(stmt, r),
            .exit_stmt => |e| self.compileExit(stmt, e),
            .expression => |expr| self.compileExpressionStatement(stmt, expr.expression),
            else => {
                try self.reportSourceError(stmt, Error.UnsupportedExpression, .@"error", "statement type \"{t}\" not yet supported", .{stmt.*});
                return .fromValue(.void);
            },
        };
    }

    fn compileExpressionStatement(
        self: *IRCompiler,
        source: *ast.Statement,
        expr: *ast.Expression,
    ) Error!Result {
        switch (expr.*) {
            .binary => |binary| switch (binary.op) {
                .logical_and, .logical_or => return self.compileLogicalBinary(source, binary, .statement),
                else => {},
            },
            else => {},
        }

        const result = try self.compileExpression(expr);
        try self.finalizeStatementResult(source, result);
        return .fromValue(.void);
    }

    fn finalizeStatementResult(
        self: *IRCompiler,
        source: anytype,
        result: Result,
    ) Error!void {
        if (isWaitable(result)) |loc| {
            try self.comment("wait from {s}", .{@src().fn_name});
            try self.wait(source, loc);
        }
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

    fn compileExit(
        self: *IRCompiler,
        source: *ast.Statement,
        exit_stmt: ast.ExitStmt,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const result: Result = if (exit_stmt.value) |value|
            try self.compileExpression(value)
        else
            .fromValue(.{ .exit_code = .success });
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
            binding_decl.annotation,
            binding_decl.is_mutable,
        );
    }

    fn compileBinding(
        self: *IRCompiler,
        source: anytype,
        pattern: *ast.BindingPattern,
        expr: *ast.Expression,
        annotation: ?*const ast.TypeExpr,
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
                try self.compileIdentifierBinding(
                    source,
                    identifier,
                    result.source,
                    annotation,
                    is_mutable,
                    .normal,
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

    fn compileIdentifierBinding(
        self: *IRCompiler,
        source: anytype,
        identifier: ast.Identifier,
        value: ir.ValueSource,
        annotation: ?*const ast.TypeExpr,
        is_mutable: bool,
        kind: Scope.Binding.Kind,
    ) Error!void {
        const T = @TypeOf(source);
        if (T == @TypeOf(null)) {
            try self.comment("<script-arg> -> {s}", .{@src().fn_name});
        } else {
            try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });
        }

        var result: Result = .{ .source = value };
        const annotated_type = if (annotation) |ann| ann.* else null;
        const needs_annotated_storage = if (annotated_type) |annotation_type|
            if (result.typeExpr()) |result_type|
                !std.meta.eql(result_type, annotation_type)
            else
                true
        else
            false;

        if (result.source.isRegister(.r)) {
            const result_ref = try self.newRef(source, "identifier_ref");
            try self.set(source, result_ref, .fromLocation(.initRegister(.r)));
            result = try .from(result_ref.dereference().typed(annotated_type orelse value.typeExpr()));
        } else if (needs_annotated_storage) {
            const result_ref = try self.newRef(source, "identifier_ref");
            try self.set(source, result_ref, value);
            result = try .from(result_ref.dereference().typed(annotated_type));
        }
        if (is_mutable) {
            try self.compileMutableVariable(source, identifier, result.source, kind);
            const binding = self.lookup(identifier.name, .{ .shallow = true }).?;
            binding.kind = kind;
        } else {
            try self.scopes.declare(
                self.allocator,
                identifier.name,
                result,
                annotated_type orelse result.typeExpr(),
                is_mutable,
                kind,
            );
        }
    }

    fn compileMutableVariable(
        self: *IRCompiler,
        source: anytype,
        identifier: ast.Identifier,
        value: ir.ValueSource,
        kind: Scope.Binding.Kind,
    ) Error!void {
        const T = @TypeOf(source);
        if (T == @TypeOf(null)) {
            try self.comment("<script-arg> -> {s}", .{@src().fn_name});
        } else {
            try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });
        }

        const current_frame = try self.scopes.getFrame(0);
        const storage_depth = if (current_frame.scope_type == .closure) @as(usize, 0) else try self.nearestClosureDepth();
        const closure_location = if (storage_depth == 0)
            try self.declareClosureValue(
                .mutable(identifier, kind),
                0,
                true,
                value.typeExpr(),
            )
        else blk: {
            const location = try self.allocClosureValue(
                .mutable(identifier, kind),
                storage_depth,
                value.typeExpr(),
            );
            try self.scopes.declare(
                self.allocator,
                identifier.name,
                try .from(location),
                value.typeExpr(),
                true,
                kind,
            );
            break :blk location;
        };
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
            .env_var => |env_var| self.compileEnvVar(expr, env_var),
            .call => |call| self.compileCall(expr, call),
            .member => |member| self.compileMember(expr, member),
            .if_expr => |if_expr| self.compileIf(expr, if_expr),
            .pipeline => |pipeline| self.compilePipeline(expr, pipeline),
            .block => |block| self.compileBlock(expr, block),
            .fn_decl => |fn_decl| self.compileFnDecl(expr, fn_decl),
            .binary => |binary| self.compileBinary(expr, binary),
            .unary => |unary| self.compileUnary(expr, unary),
            .array => |array| self.compileArray(expr, array),
            .for_expr => |for_expr| self.compileForLoop(expr, for_expr),
            .subshell => |subshell| self.compileSubshell(expr, subshell),
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
            try self.pipeOpt(source, stdout_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(true)));
            try self.pipeOpt(source, stdout_pipe_ref.dereference(), .close_destination, .fromValue(.fromBoolean(false)));
            try self.pipeOpt(source, stdout_pipe_ref.dereference(), .disconnect_destination, .fromValue(.fromBoolean(false)));
            const stderr_pipe_ref = try self.newRef(source, "stderr_pipe");
            try self.pipe(source, stderr_pipe_ref);
            try self.pipeOpt(source, stderr_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(true)));
            try self.pipeOpt(source, stderr_pipe_ref.dereference(), .close_destination, .fromValue(.fromBoolean(false)));
            try self.pipeOpt(source, stderr_pipe_ref.dereference(), .disconnect_destination, .fromValue(.fromBoolean(false)));
            const merged_pipe_ref = try self.newRef(source, "merged_pipe");
            try self.pipe(source, merged_pipe_ref);
            try self.pipeFwd(source, stdout_pipe_ref.dereference(), merged_pipe_ref.dereference());
            try self.pipeFwd(source, stderr_pipe_ref.dereference(), merged_pipe_ref.dereference());
            const stdout_stream_thread_ref = try self.newRef(source, "stdout_stream_thread");
            try self.set(
                source,
                stdout_stream_thread_ref,
                .from(try self.fork(source, self.stdoutStreamSet(), self.threadStdin(), stdout_pipe_ref.dereference(), self.threadStderr(), .noll, .inherit)),
            );
            const stderr_stream_thread_ref = try self.newRef(source, "stderr_stream_thread");
            try self.set(
                source,
                stderr_stream_thread_ref,
                .from(try self.fork(source, self.stderrStreamSet(), self.threadStdin(), self.threadStdout(), stderr_pipe_ref.dereference(), .noll, .inherit)),
            );
            var result = try self.compileWithContext(source, .{
                .out = stdout_pipe_ref.dereference(),
                .err = stderr_pipe_ref.dereference(),
            }, expr);
            result = try self.compileResultSaveR(source, result);
            if (result.isType(thread_type)) {
                try self.wait(source, result.source.location);
            } else if (result.isType(execution_handles_struct_type)) {
                try self.set(source, .initRegister(.r2), stableResultSource(result));
                try self.wait(
                    source,
                    ir.Location.initAdd(
                        .{ .register = .r2 },
                        executionHandlesFieldOffset(.thread),
                        .{ .dereference = true },
                    ).typed(thread_type),
                );
            }
            try self.pipeOpt(source, stdout_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(false)));
            try self.pipeOpt(source, stderr_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(false)));
            try self.wait(source, stdout_stream_thread_ref.dereference());
            try self.wait(source, stderr_stream_thread_ref.dereference());
            if (result.isType(execution_handles_struct_type)) {
                // alloc 5 # create execution result
                try self.alloc(source, 5);
                // set [%r+0] = @@pipe_stdout
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.stdout), .{ .dereference = true }),
                    .from(stdout_pipe_ref.dereference()),
                );
                // set [%r+1] = @@pipe_stderr
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.stderr), .{ .dereference = true }),
                    .from(stderr_pipe_ref.dereference()),
                );
                // set [%r+2] = @@pipe_merged
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.merged), .{ .dereference = true }),
                    .from(merged_pipe_ref.dereference()),
                );
                // set [%r+3] = [@@execution_handles+0]
                try self.set(source, .initRegister(.r2), stableResultSource(result));
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.closeable), .{ .dereference = true }),
                    .fromLocation(.initAdd(
                        .{ .register = .r2 },
                        executionHandlesFieldOffset(.closeable),
                        .{ .dereference = true },
                    )),
                );
                // set [%r+4] = false
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.completion_is_thread), .{ .dereference = true }),
                    .fromValue(.fromBoolean(false)),
                );
                // set [C+?] = %r
                result = .fromLocation(.initRegister(.r));
                result = result.typed(execution_result_struct_type);
            } else if (result.isType(thread_type)) {
                // alloc 5 # create execution result for thread-backed capture
                try self.alloc(source, 5);
                // set [%r+0] = @@pipe_stdout
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.stdout), .{ .dereference = true }),
                    .from(stdout_pipe_ref.dereference()),
                );
                // set [%r+1] = @@pipe_stderr
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.stderr), .{ .dereference = true }),
                    .from(stderr_pipe_ref.dereference()),
                );
                // set [%r+2] = @@pipe_merged
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.merged), .{ .dereference = true }),
                    .from(merged_pipe_ref.dereference()),
                );
                // set [%r+3] = thread handle
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.closeable), .{ .dereference = true }),
                    stableResultSource(result),
                );
                // set [%r+4] = true
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, executionResultFieldOffset(.completion_is_thread), .{ .dereference = true }),
                    .fromValue(.fromBoolean(true)),
                );
                // set [C+?] = %r
                result = .fromLocation(.initRegister(.r));
                result = result.typed(execution_result_struct_type);
            }
            return result;
        } else {
            return try self.compileExpression(expr);
        }
    }

    fn compileStableExpression(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
        comptime ref_name: []const u8,
    ) Error!Result {
        const expr_effects = self.analyzeExpressionEffects(expr);
        if (!expr_effects.needs_stdio_capture) {
            return try self.compileExpression(expr);
        }

        const saved_ref = try self.newRef(source, ref_name);
        const result = try self.compileExpressionWithCapture(source, expr);
        if (result.isType(execution_result_struct_type)) {
            try self.set(source, .initRegister(.r2), stableResultSource(result));
            try self.set(source, saved_ref, .fromLocation(.initRegister(.r2)));
        } else {
            try self.set(source, saved_ref, stableResultSource(result));
        }
        var i: usize = 0;
        while (i < capture_temp_ref_count) : (i += 1) {
            _ = try self.pop(source);
        }
        return try .from(saved_ref.dereference().typed(result.typeExpr()));
    }

    fn compileStableExpressionIntoRef(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
        destination: ir.Location,
    ) Error!Result {
        const expr_effects = self.analyzeExpressionEffects(expr);
        if (!expr_effects.needs_stdio_capture) {
            const result = try self.compileExpression(expr);
            try self.set(source, destination, stableResultSource(result));
            return .from(destination.dereference().typed(result.typeExpr()));
        }

        const result = try self.compileExpressionWithCapture(source, expr);
        if (result.isType(execution_result_struct_type)) {
            try self.set(source, .initRegister(.r2), stableResultSource(result));
            try self.set(source, destination, .fromLocation(.initRegister(.r2)));
        } else {
            try self.set(source, destination, stableResultSource(result));
        }
        var i: usize = 0;
        while (i < capture_temp_ref_count) : (i += 1) {
            _ = try self.pop(source);
        }
        return .from(destination.dereference().typed(result.typeExpr()));
    }

    fn compileTransientExpression(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
    ) Error!Result {
        const expr_effects = self.analyzeExpressionEffects(expr);
        if (!expr_effects.needs_stdio_capture) {
            return try self.compileExpression(expr);
        }

        const result = try self.compileExpressionWithCapture(source, expr);
        try self.set(source, .initRegister(.r2), stableResultSource(result));
        var i: usize = 0;
        while (i < capture_temp_ref_count) : (i += 1) {
            _ = try self.pop(source);
        }
        return .fromLocation(ir.Location.initRegister(.r2).typed(result.typeExpr()));
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
            .null => .fromValue(.null),
            .string => |string| self.compileStringLiteral(source, string),
        };
    }

    fn compileMember(
        self: *IRCompiler,
        source: *ast.Expression,
        member: ast.MemberExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (std.mem.eql(u8, member.member.name, "?")) {
            return self.compileOptionalUnwrap(source, member.object);
        }

        const object = try self.compileExpression(member.object);
        const object_type = object.typeExpr() orelse {
            try self.reportSourceError(
                source,
                Error.NotImplemented,
                .@"error",
                "member access requires a typed internal struct object",
                .{},
            );
            return .fromValue(.void);
        };

        const struct_type = switch (object_type) {
            .array => {
                if (!std.mem.eql(u8, member.member.name, "len")) {
                    try self.reportSourceError(
                        source,
                        Error.NotImplemented,
                        .@"error",
                        "member \"{s}\" not found on array value",
                        .{member.member.name},
                    );
                    return .fromValue(.void);
                }

                if (object.source != .location) {
                    try self.reportSourceError(
                        source,
                        Error.NotImplemented,
                        .@"error",
                        "member access requires an address-backed array value",
                        .{},
                    );
                    return .fromValue(.void);
                }

                const object_ref = try self.newRef(source, "array_member_object_ref");
                try self.set(source, object_ref, object.source);
                try self.set(source, .initRegister(.r2), .from(object_ref.dereference()));

                return .fromLocation(.initAbs(
                    .{ .register = .r2 },
                    .{
                        .dereference = true,
                        .type_expr = .global(.integer),
                    },
                ));
            },
            .struct_type => |struct_type| struct_type,
            else => {
                try self.reportSourceError(
                    source,
                    Error.NotImplemented,
                    .@"error",
                    "member access is only supported for internal struct types in IR",
                    .{},
                );
                return .fromValue(.void);
            },
        };

        if (object.source != .location) {
            try self.reportSourceError(
                source,
                Error.NotImplemented,
                .@"error",
                "member access requires an address-backed struct value",
                .{},
            );
            return .fromValue(.void);
        }

        if (std.mem.eql(u8, member.member.name, "exit_code")) {
            const result_ref = try self.newRef(source, "exit_code_result");
            try self.addInstruction(.init(.from(source), .{ .resolve_exit_code = .{
                .source = object.source.location,
                .result = result_ref,
            } }));
            return .fromLocation(result_ref.dereference().typed(.global(.integer)));
        }

        const field_ = struct_type.fieldLayout(member.member.name) catch |err| switch (err) {
            error.FieldNotFound => {
                try self.reportSourceError(
                    source,
                    Error.NotImplemented,
                    .@"error",
                    "member \"{s}\" not found on internal struct",
                    .{member.member.name},
                );
                return .fromValue(.void);
            },
            error.UnsupportedLayout => {
                try self.reportSourceError(
                    source,
                    Error.NotImplemented,
                    .@"error",
                    "member access is not supported for this internal struct layout in IR",
                    .{},
                );
                return .fromValue(.void);
            },
        };
        const object_ref = try self.newRef(source, "member_object_ref");
        try self.set(source, object_ref, object.source);
        try self.set(source, .initRegister(.r2), .from(object_ref.dereference()));

        return .fromLocation(.initAdd(
            .{ .register = .r2 },
            field_.offset,
            .{
                .dereference = true,
                .type_expr = field_.type_expr,
            },
        ));
    }

    fn compileOptionalUnwrap(
        self: *IRCompiler,
        source: *ast.Expression,
        object_expr: *ast.Expression,
    ) Error!Result {
        const object_ref = try self.newRef(source, "optional_unwrap_object");
        const object = try self.compileStableExpressionIntoRef(source, object_expr, object_ref);

        const object_type = object.typeExpr() orelse {
            try self.reportSourceError(
                source,
                Error.UnsupportedExpression,
                .@"error",
                "optional unwrap requires an optional value",
                .{},
            );
            return .fromValue(.void);
        };

        const child_type = switch (object_type) {
            .optional => |optional| optional.child.*,
            else => {
                try self.reportSourceError(
                    source,
                    Error.UnsupportedExpression,
                    .@"error",
                    "optional unwrap requires an optional value",
                    .{},
                );
                return .fromValue(.void);
            },
        };

        const is_null_ref = try self.newRef(source, "optional_unwrap_is_null");
        try self.cmp(
            source,
            .equal,
            .from(object_ref.dereference()),
            .fromValue(.null),
            is_null_ref,
        );

        const after_addr = try self.newLabel("optional_unwrap_after", .unknown);
        try self.jmp(source, try .from(is_null_ref.dereference()), false, after_addr);
        try self.exit_(source, .fromValue(.fromBoolean(false)));
        try self.setLabel(after_addr.local_addr.label, .abs);

        return .from(object_ref.dereference().typed(child_type));
    }

    fn compileMemberBinary(
        self: *IRCompiler,
        source: *ast.Expression,
        binary: ast.BinaryExpr,
    ) Error!Result {
        const member_name = switch (binary.right.*) {
            .identifier => |identifier| identifier,
            else => {
                try self.reportSourceError(
                    source,
                    Error.NotImplemented,
                    .@"error",
                    "member access expects an identifier on the right-hand side",
                    .{},
                );
                return .fromValue(.void);
            },
        };

        return self.compileMember(source, .{
            .object = binary.left,
            .member = member_name,
            .span = binary.span,
        });
    }

    fn compileStringLiteral(
        self: *IRCompiler,
        source: anytype,
        string_literal: ast.StringLiteral,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (string_literal.segments.len == 1) {
            const decoded = try self.decodeStringLiteralText(string_literal.segments[0].text.payload);
            defer self.allocator.free(decoded);
            return .from(try self.addSlice(1, decoded));
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
                const decoded = try self.decodeStringLiteralText(text.payload);
                defer self.allocator.free(decoded);
                const result = try self.addSlice(1, decoded);
                try self.set(source, .initRegister(.r), .from(ref.dereference()));
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, i + 1, .{ .dereference = true }),
                    .from(result),
                );
                // s_tream[i] = try self.addSlice(1, text.payload);
            },
            .interpolation => |interp| {
                var result = (try self.compileTransientExpression(source, interp)).source;
                const is_result_reg_r = result.isRegister(.r);
                if (is_result_reg_r) {
                    const segment_ref = try self.newRef(source, "segment");
                    try self.set(source, segment_ref, result);
                    result = .from(segment_ref.dereference());
                }
                try self.set(source, .initRegister(.r), .from(ref.dereference()));
                // TODO: handle array coercion
                if (result == .location and result.location.isType(execution_result_struct_type)) {
                    try self.set(source, .initRegister(.r2), result);
                    result = .fromLocation(.initAdd(
                        .{ .register = .r2 },
                        executionResultFieldOffset(.merged),
                        .{ .dereference = true },
                    ));
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
        return .from(ref.dereference());
    }

    fn decodeStringLiteralText(self: *IRCompiler, encoded: []const u8) Allocator.Error![]u8 {
        var decoded = std.ArrayList(u8).empty;
        defer decoded.deinit(self.allocator);

        var i: usize = 0;
        while (i < encoded.len) : (i += 1) {
            const rest = encoded[i..];
            const ch = encoded[i];

            if (rest.len >= 2) {
                if (std.mem.eql(u8, rest[0..2], "\\n")) {
                    try decoded.append(self.allocator, '\n');
                    i += 1;
                    continue;
                }
                if (std.mem.eql(u8, rest[0..2], "\\t")) {
                    try decoded.append(self.allocator, '\t');
                    i += 1;
                    continue;
                }
                if (std.mem.eql(u8, rest[0..2], "\\r")) {
                    try decoded.append(self.allocator, '\r');
                    i += 1;
                    continue;
                }
                if (std.mem.eql(u8, rest[0..2], "\\\"")) {
                    try decoded.append(self.allocator, '"');
                    i += 1;
                    continue;
                }
            }

            try decoded.append(self.allocator, ch);
        }

        return try decoded.toOwnedSlice(self.allocator);
    }

    fn nearestClosureDepth(self: *IRCompiler) Error!usize {
        var depth: usize = 0;
        while (true) : (depth += 1) {
            const frame = try self.scopes.getFrame(depth);
            if (frame.scope_type == .closure) return depth;
        }
    }

    fn allocClosureValue(
        self: *IRCompiler,
        binding: Scope.ClosureBinding,
        depth: usize,
        type_expr: ?ast.TypeExpr,
    ) Error!ir.Location {
        const frame = try self.scopes.getFrame(depth);
        const index = frame.closure_bindings.items.len;
        try frame.closure_bindings.append(self.allocator, binding);
        var location: ir.Location = .initAdd(.closure, index, .{});
        if (type_expr) |te| location = location.typed(te);
        return location;
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
        const location = try self.allocClosureValue(binding, depth, type_expr);
        if (!frame.bindings.contains(binding.identifier.name)) {
            try frame.declare(
                self.allocator,
                binding.identifier.name,
                try .from(location),
                type_expr,
                is_mutable,
                binding.kind,
            );
        }

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

        // Lexical scopes share the same runtime closure/frame. Only capture once we cross
        // an actual closure boundary.
        const current_frame = try self.scopes.getFrame(0);
        var crossed_closure_boundary = current_frame.scope_type == .closure;
        var depth: usize = 1;
        while (true) : (depth += 1) {
            const frame = self.scopes.getFrame(depth) catch break;
            if (frame.bindings.getPtr(identifier.name)) |binding| {
                if (!crossed_closure_boundary) {
                    if (binding.is_mutable) {
                        return binding.result.dereference();
                    }
                    return binding.result;
                }
                break;
            }
            if (frame.scope_type == .closure) {
                crossed_closure_boundary = true;
            }
        }

        var binding_depth: ?usize = null;
        depth = 1;
        while (true) : (depth += 1) {
            const frame = self.scopes.getFrame(depth) catch break;
            if (frame.bindings.getPtr(identifier.name) != null) {
                binding_depth = depth;
                break;
            }
        }

        const target_depth = binding_depth orelse {
            try self.reportSourceError(
                source,
                Error.InternalInvariantViolation,
                .@"error",
                "failed to resolve capture depth for identifier \"{s}\"",
                .{identifier.name},
            );
            return .fromValue(.void);
        };
        var closure_depth = if (current_frame.scope_type == .closure) @as(usize, 0) else @as(usize, 1);
        while ((try self.scopes.getFrame(closure_depth)).scope_type != .closure) : (closure_depth += 1) {}

        var closure_value_location: ?ir.Location = null;
        var source_depth = target_depth;
        var depth_cursor = target_depth;
        while (depth_cursor > closure_depth) : (depth_cursor -= 1) {
            const frame = try self.scopes.getFrame(depth_cursor - 1);
            if (frame.scope_type != .closure) continue;

            const next_source_depth = source_depth - (depth_cursor - 1);
            const location = try self.declareClosureValue(
                .outer(identifier, next_source_depth, source_binding.kind),
                depth_cursor - 1,
                source_binding.is_mutable,
                source_binding.result.typeExpr(),
            );
            closure_value_location = location;
            source_depth = depth_cursor - 1;
        }

        const closure_location = closure_value_location orelse {
            try self.reportSourceError(
                source,
                Error.InternalInvariantViolation,
                .@"error",
                "failed to materialize closure capture for identifier \"{s}\"",
                .{identifier.name},
            );
            return .fromValue(.void);
        };

        if (source_binding.is_mutable) {
            return .from(closure_location.dereference());
        }
        return .from(closure_location);
    }

    fn compileCall(
        self: *IRCompiler,
        source: *ast.Expression,
        call: ast.CallExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (call.callee.* == .block and call.redirects.len > 0) {
            return self.compileBlockCallWithRedirects(source, call.callee.*.block, call.redirects);
        }

        if (call.callee.* == .identifier) {
            const name = call.callee.identifier.name;
            if (std.mem.eql(u8, name, "cd") and self.lookup(name, .{ .shallow = false }) == null) {
                return self.compileBuiltinCd(source, call.arguments);
            }
        }

        const callee = try self.compileExpression(call.callee);

        return switch (callee.source) {
            .value => |v| switch (v) {
                .executable => self.compileExecutableCall(source, v, call.arguments, call.redirects),
                .fn_ref => self.compileFunctionCall(source, v, call.arguments),
                .slice, .stream, .addr, .void, .null, .uinteger, .float, .strct, .exit_code, .pipe, .thread, .closeable => .from(v),
                .zig_string => Error.UnsupportedValueType,
            },
            .location => |loc| .from(loc),
        };
    }

    fn compileBlockCallWithRedirects(
        self: *IRCompiler,
        source: *ast.Expression,
        block: ast.Block,
        redirects: []const ast.Redirection,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const block_instr_set = try self.addInstructionSet();

        var exec_stdout = self.threadStdout();
        var exec_stderr = self.threadStderr();

        for (redirects) |redirect| {
            switch (redirect.target) {
                .path => |path_target| {
                    const redirect_target = try self.compileStringLiteral(source, path_target.value);
                    const redirect_pipe_ref = try self.newRef(source, "stdout_redirect_pipe");
                    try self.pipe(source, redirect_pipe_ref);
                    try self.pipeFile(
                        source,
                        redirect_pipe_ref.dereference(),
                        stableResultSource(redirect_target),
                        redirect.mode,
                    );
                    switch (redirect.stream) {
                        .stdout => exec_stdout = redirect_pipe_ref.dereference(),
                        .stderr => exec_stderr = redirect_pipe_ref.dereference(),
                        else => {},
                    }
                },
                .fd => |target_fd| {
                    const target_loc: ir.Location = switch (target_fd) {
                        0 => self.threadStdin(),
                        1 => self.threadStdout(),
                        2 => self.threadStderr(),
                        else => continue,
                    };
                    switch (redirect.stream) {
                        .stdout => exec_stdout = target_loc,
                        .stderr => exec_stderr = target_loc,
                        else => {},
                    }
                },
            }
        }

        const spawned = try self.spawnClosure(
            source,
            .initAbs(block_instr_set, 0),
            self.threadStdin(),
            exec_stdout,
            exec_stderr,
        );

        const prev_instr_set = self.current_instruction_set;
        self.current_instruction_set = block_instr_set;
        try self.scopes.push(self.allocator, .closure);

        _ = try self.compileBlock(source, block);
        try self.exitWith(source, .fromValue(.fromBoolean(true)));

        try self.setClosureIdentifiers();
        self.current_instruction_set = prev_instr_set;
        self.scopes.pop();

        return .fromLocation(spawned.thread_handle);
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
        instr_set: usize,
        return_addr: ir.InstructionAddr,
    };

    const ClosureContext = struct {
        closure_ref: ir.Location,
        instr_set: usize,
        return_addr: ir.InstructionAddr,
    };

    fn compileCreateMainClosure(self: *IRCompiler) Error!MainClosureContext {
        // const index = self.currentInstrSet().instructions.items.len;
        const instr_set = try self.addInstructionSet();
        try self.jmp(null, null, true, .initAbs(instr_set, 0));
        const return_addr = try self.newLabel("main_closure_return", .abs);

        return .{
            // .index = index,
            .instr_set = instr_set,
            .return_addr = return_addr,
        };
    }

    fn compileCreateClosure(self: *IRCompiler, source: anytype) Error!ClosureContext {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const closure_ref = try self.newRef(source, "closure");
        const instr_set = try self.addInstructionSet();
        try self.jmp(null, null, true, .initAbs(instr_set, 0));
        const return_addr = try self.newLabel("closure_return", .abs);

        return .{
            .closure_ref = closure_ref,
            .return_addr = return_addr,
            .instr_set = instr_set,
        };
    }

    const SpawnedClosure = struct {
        closure: ClosureContext,
        thread_handle: ir.Location,
    };

    fn spawnClosure(
        self: *IRCompiler,
        source: anytype,
        dest: ir.InstructionAddr,
        stdin: ir.Location,
        stdout: ir.Location,
        stderr: ir.Location,
    ) Error!SpawnedClosure {
        const closure = try self.compileCreateClosure(source);
        const thread_handle = try self.fork(
            source,
            dest,
            stdin,
            stdout,
            stderr,
            closure.closure_ref.dereference(),
            .inherit,
        );
        try self.compileClosurePostFork(source);
        return .{
            .closure = closure,
            .thread_handle = thread_handle,
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
        const spawned = try self.spawnClosure(
            source,
            .initAbs(instr_set, 0),
            stdin,
            stdout,
            stderr,
        );
        // 4. switch to new instruction set
        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = instr_set;
        try self.scopes.push(self.allocator, .closure);
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
        try self.compileClosureInitialization(source, spawned.closure);
        self.scopes.pop();
        // 9. wait for fork
        try self.wait(source, spawned.thread_handle);
        // 10. return binding
        return self.compileIdentifier(source, result_identifier);
    }

    fn setClosureIdentifiers(self: *IRCompiler) !void {
        const frame = try self.scopes.getFrame(0);
        self.currentInstrSet().closure_slot_count = frame.closure_bindings.items.len;

        var outer_count: usize = 0;
        for (frame.closure_bindings.items) |binding| {
            if (binding.type == .outer) outer_count += 1;
        }

        const closure_captures = try self.allocator.alloc(ClosureCapture, outer_count);
        var i: usize = 0;
        for (frame.closure_bindings.items, 0..) |binding, slot| {
            if (binding.type != .outer) continue;
            closure_captures[i] = .{
                .identifier = binding.identifier,
                .slot = slot,
            };
            i += 1;
        }
        self.currentInstrSet().closure_captures = closure_captures;
    }

    fn compileBuiltinCd(
        self: *IRCompiler,
        source: *ast.Expression,
        arguments: []const *ast.Expression,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        // No argument: evaluator will use HOME from process environment
        const path: ir.ValueSource = if (arguments.len == 0)
            .fromValue(.void)
        else blk: {
            const compiled = try self.compileExpression(arguments[0]);
            break :blk stableResultSource(compiled);
        };

        try self.addInstruction(.init(.from(source), .{ .cd = path }));
        return .fromLocation(.initRegister(.r));
    }

    fn compileSubshell(
        self: *IRCompiler,
        source: *ast.Expression,
        subshell: ast.SubshellExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        try self.addInstruction(.init(.from(source), .enter_subshell));
        const result = try self.compileExpressionWithCapture(source, subshell.child);
        try self.addInstruction(.init(.from(source), .exit_subshell));
        return result;
    }

    fn compileEnvVar(
        self: *IRCompiler,
        source: *ast.Expression,
        env_var: ast.EnvVarExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const result_ref = try self.newRef(source, "env_var");
        try self.getEnv(source, env_var.identifier.name, result_ref);
        return .from(result_ref.dereference().typed(optional_string_type));
    }

    fn compileExecutableCall(
        self: *IRCompiler,
        source: *ast.Expression,
        executable: ir.Value,
        arguments: []const *ast.Expression,
        redirects: []const ast.Redirection,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const exec_instr_set = try self.addInstructionSet();
        const redirected_stdout_slot = 2;
        var has_stdout_file_redirect = false;
        var has_stderr_file_redirect = false;
        for (redirects) |redirect| {
            if (redirect.target != .path) continue;
            const stream_fd_: u8 = switch (redirect.stream) {
                .stdin => 0,
                .stdout => 1,
                .stderr => 2,
                .descriptor => |fd| fd,
            };
            if (stream_fd_ == 1) has_stdout_file_redirect = true;
            if (stream_fd_ == 2) has_stderr_file_redirect = true;
        }

        const execution_handles = try self.newRef(source, "execution_handles");
        try self.alloc(source, if (has_stdout_file_redirect) 3 else 2);
        try self.set(source, execution_handles, .fromLocation(.initRegister(.r)));
        const result_variable_identifier = try self.compileResultVariable(
            source,
            .from(execution_handles.dereference()),
        );

        var exec_stdout = self.threadStdout();
        var exec_stderr = self.threadStderr();

        for (redirects) |redirect| {
            switch (redirect.target) {
                .path => |path_target| {
                    const stream_fd: u8 = switch (redirect.stream) {
                        .stdin => 0,
                        .stdout => 1,
                        .stderr => 2,
                        .descriptor => |fd| fd,
                    };
                    if (stream_fd != 1 and stream_fd != 2) continue;
                    const redirect_target = try self.compileStringLiteral(source, path_target.value);
                    const redirect_pipe_ref = try self.newRef(source, if (stream_fd == 1) "stdout_redirect_pipe" else "stderr_redirect_pipe");
                    try self.pipe(source, redirect_pipe_ref);
                    try self.pipeFile(
                        source,
                        redirect_pipe_ref.dereference(),
                        stableResultSource(redirect_target),
                        redirect.mode,
                    );
                    if (stream_fd == 1) {
                        try self.set(source, .initRegister(.r2), .from(execution_handles.dereference()));
                        try self.set(
                            source,
                            .initAdd(
                                .{ .register = .r2 },
                                redirected_stdout_slot,
                                .{ .dereference = true },
                            ),
                            .from(redirect_pipe_ref.dereference()),
                        );
                        exec_stdout = redirect_pipe_ref.dereference();
                    } else {
                        exec_stderr = redirect_pipe_ref.dereference();
                    }
                },
                .fd => |target_fd| {
                    const target_loc: ir.Location = switch (target_fd) {
                        0 => self.threadStdin(),
                        1 => self.threadStdout(),
                        2 => self.threadStderr(),
                        else => continue, // unsupported fd target
                    };
                    const source_fd: u8 = switch (redirect.stream) {
                        .stdin => 0,
                        .stdout => 1,
                        .stderr => 2,
                        .descriptor => |fd| fd,
                    };
                    switch (source_fd) {
                        1 => exec_stdout = target_loc,
                        2 => exec_stderr = target_loc,
                        else => {}, // stdin redirect not supported here
                    }
                },
            }
        }

        const spawned = try self.spawnClosure(
            source,
            .initAbs(exec_instr_set, 0),
            self.threadStdin(),
            exec_stdout,
            exec_stderr,
        );
        try self.set(source, .initRegister(.r2), .from(execution_handles.dereference()));
        try self.set(
            source,
            .initAdd(
                .{ .register = .r2 },
                executionHandlesFieldOffset(.thread),
                .{ .dereference = true },
            ),
            .from(spawned.thread_handle),
        );

        const prev_instr_set = self.current_instruction_set;
        self.current_instruction_set = exec_instr_set;
        try self.scopes.push(self.allocator, .closure);

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
            .initAdd(
                .{ .register = .r },
                executionHandlesFieldOffset(.closeable),
                .{ .dereference = true },
            ),
            .from(exec_handle_ref.dereference()),
        );
        try self.comment("wait from {s}", .{@src().fn_name});
        try self.wait(source, exec_handle_ref.dereference());

        try self.setClosureIdentifiers();
        self.current_instruction_set = prev_instr_set;
        try self.compileClosureInitialization(source, spawned.closure);
        self.scopes.pop();

        if (has_stdout_file_redirect) {
            try self.set(source, .initRegister(.r2), .from(execution_handles.dereference()));
            const redirect_stream_thread = try self.fork(
                source,
                self.stdoutStreamSet(),
                self.threadStdin(),
                .initAdd(
                    .{ .register = .r2 },
                    redirected_stdout_slot,
                    .{ .dereference = true },
                ),
                self.threadStderr(),
                .noll,
                .inherit,
            );
            try self.wait(source, redirect_stream_thread);
            try self.push(source, .from(execution_handles.dereference()));
            const execution_handles_r = try self.pop(source);
            return .from(execution_handles_r.typed(execution_handles_struct_type));
        }

        const thread_handle_ref = try self.newRef(source, "thread_handle");
        try self.set(source, thread_handle_ref, .from(spawned.thread_handle));
        try self.set(source, .initRegister(.r), .from(execution_handles.dereference()));
        try self.set(
            source,
            .initAdd(
                .{ .register = .r },
                executionHandlesFieldOffset(.thread),
                .{ .dereference = true },
            ),
            .from(thread_handle_ref.dereference()),
        );
        _ = try self.pop(source);

        if (has_stderr_file_redirect) {
            _ = try self.pop(source);
        }

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
        try self.compileIdentifierBinding(source, identifier, value, null, true, .normal);
        return identifier;
    }

    fn compileMainClosureInitialization(
        self: *IRCompiler,
        source: anytype,
        closure: MainClosureContext,
    ) Error!void {
        const frame = try self.scopes.getFrame(0);
        const cl = frame.closure_bindings.items.len;

        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = closure.instr_set;

        try self.alloc(source, cl);
        try self.set(source, .initAbs(.{ .stack = 3 }, .{}), .fromLocation(.initRegister(.r)));
        try self.jmp(source, null, true, closure.return_addr);

        self.current_instruction_set = orig_instr_set;

        // var instructions: std.ArrayList(ir.Instruction) = try .initCapacity(self.allocator, 2);
        //
        // // alloc 2
        // instructions.appendAssumeCapacity(.init(.from(source), .{ .alloc = cl }));
        // // set S3 = %r
        // instructions.appendAssumeCapacity(.init(.from(source), .{ .set = .{
        //     .destination = .initAbs(.{ .stack = 3 }, .{}),
        //     .source = .fromLocation(.initRegister(.r)),
        // } }));
        //
        // try self.currentInstrSet().insertSlice(self.allocator, closure.index, try instructions.toOwnedSlice(self.allocator));
    }

    fn compileClosurePostFork(
        self: *IRCompiler,
        source: anytype,
    ) Error!void {
        const comment_message = try std.fmt.allocPrint(self.allocator, "{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });
        try self.comment("{s}", .{comment_message});
        try self.set(source, .initRegister(.r2), .fromLocation(.initRegister(.r)));
        _ = try self.pop(source);
        try self.set(source, .initRegister(.r), .fromLocation(.initRegister(.r2)));
    }

    fn compileClosureInitialization(
        self: *IRCompiler,
        source: anytype,
        closure: ClosureContext,
    ) Error!void {
        const frame = try self.scopes.getFrame(0);
        const cl = frame.closure_bindings.items.len;

        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = closure.instr_set;

        const comment_message = try std.fmt.allocPrint(self.allocator, "{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });
        try self.comment("{s}", .{comment_message});

        try self.set(source, .initRegister(.r2), .fromLocation(.initRegister(.r)));
        try self.alloc(source, cl);
        try self.set(source, closure.closure_ref, .fromLocation(.initRegister(.r)));

        for (0..cl) |i| {
            const binding = frame.closure_bindings.items[i];
            const slot_value: ir.ValueSource = switch (binding.type) {
                .mutable => .fromValue(.void),
                .outer => blk: {
                    const source_binding = self.lookup(binding.identifier.name, .{
                        .shallow = true,
                        .initial_depth = binding.depth,
                    }) orelse return Error.ScopeNotFound;
                    break :blk source_binding.result.source;
                },
            };
            try self.set(
                source,
                .initAdd(.{ .register = .r }, i, .{ .dereference = true }),
                slot_value,
            );
        }

        try self.set(source, .initRegister(.r), .fromLocation(.initRegister(.r2)));
        try self.jmp(source, null, true, closure.return_addr);

        self.current_instruction_set = orig_instr_set;
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
        const is_self_recursive = self.current_instruction_set == fn_addr.instr_set;
        const self_closure_depth = if (is_self_recursive) blk: {
            const frame = try self.scopes.getFrame(0);
            break :blk if (frame.scope_type == .closure) @as(usize, 0) else try self.nearestClosureDepth();
        } else 0;

        // Manual closure compilation
        // TODO: Add closure variables as well (we need to extend the function reference value to be able to understand what closure variables are needed)
        const closure_captures = self.instruction_sets.items[fn_ref.fn_addr.instr_set].closure_captures;
        const closure_size = if (is_self_recursive)
            (try self.scopes.getFrame(self_closure_depth)).closure_bindings.items.len
        else
            self.instruction_sets.items[fn_ref.fn_addr.instr_set].closure_slot_count;
        try self.alloc(source, closure_size);
        const closure_ref = try self.newRef(source, "closure");
        try self.set(source, closure_ref, .fromLocation(.initRegister(.r)));

        if (is_self_recursive) {
            try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
            for (0..closure_size) |slot| {
                try self.set(
                    source,
                    .initAdd(.{ .register = .r }, slot, .{ .dereference = true }),
                    .fromLocation(.initAdd(.closure, slot, .{})),
                );
            }
        }

        for (arguments, 0..) |arg, i| {
            const arg_result = try self.compileResultSaveR(source, try self.compileExpression(arg));
            try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
            try self.set(
                source,
                .initAdd(.{ .register = .r }, i, .{ .dereference = true }),
                arg_result.source,
            );
            try self.consume(source, arg_result);
        }

        try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));

        if (!is_self_recursive) {
            for (closure_captures) |capture| {
                // NOTE: Guard against refering to internal identifiers
                if (self.lookup(capture.identifier.name, .{ .shallow = false }) == null) continue;

                const identifier_result = try self.compileIdentifier(source, capture.identifier);

                if (identifier_result.source == .location and identifier_result.source.location.abs == .data) {
                    try self.set(
                        source,
                        .initAdd(.{ .register = .r }, capture.slot, .{ .dereference = true }),
                        identifier_result.source.undereference(),
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

        if (self.hasMixedIfCaptureRequirements(if_expr)) {
            try self.reportSourceError(
                source,
                Error.NotImplemented,
                .@"error",
                "if expressions with mixed stdio-capture branches are not yet supported in IR",
                .{},
            );
            return .fromValue(.void);
        }

        if (if_expr.else_branch) |else_branch| {
            return try self.compileIfElse(source, if_expr, else_branch);
        } else {
            return try self.compileIfNoElse(source, if_expr);
        }
    }

    fn hasMixedIfCaptureRequirements(
        self: *IRCompiler,
        if_expr: ast.IfExpr,
    ) bool {
        const else_branch = if_expr.else_branch orelse return false;
        const then_capture = self.analyzeExpressionEffects(if_expr.then_expr).needs_stdio_capture;
        const else_capture = switch (else_branch) {
            .expr => |expr_| self.analyzeExpressionEffects(expr_).needs_stdio_capture,
            .if_expr => |if_expr_| self.analyzeIfExpressionEffects(if_expr_.*).needs_stdio_capture,
            .condition => false,
        };

        return then_capture != else_capture;
    }

    fn compileIfElse(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
        else_branch: ast.IfExpr.ElseBranch,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const if_condition = try self.compileIfCondition(source, if_expr);
        const condition = if_condition.condition;
        if (condition.source.isValueTag(.exit_code)) {
            const c = condition.source.value.exit_code.toBoolean();

            if (c) return self.compileIfBranchExpression(source, if_expr.then_expr, if_condition.capture_binding);
            return switch (else_branch) {
                .expr => |expr_| self.compileExpression(expr_),
                .if_expr => |if_expr_| self.compileIf(source, if_expr_.*),
                .condition => condition,
            };
        }

        const result = try self.newRef(source, "if_result");
        if (else_branch == .condition) {
            try self.set(source, result, stableResultSource(condition));
        }
        const after_addr = try self.newLabel("if_after", .unknown);
        const else_addr = try self.newLabel("if_else", .unknown);

        try self.jmp(source, condition, false, else_addr);
        const then = try self.compileIfBranchExpression(source, if_expr.then_expr, if_condition.capture_binding);
        try self.set(source, result, stableResultSource(then));
        try self.jmp(source, null, false, after_addr);
        try self.setLabel(else_addr.local_addr.label, .abs);
        var result_type = if (else_branch == .condition) mergedResultType(condition, then) else null;
        switch (else_branch) {
            .expr => |expr_| {
                const else_ = try self.compileExpression(expr_);
                try self.set(source, result, stableResultSource(else_));
                result_type = mergedResultType(then, else_);
            },
            .if_expr => |if_expr_| {
                const else_ = try self.compileIf(source, if_expr_.*);
                try self.set(source, result, stableResultSource(else_));
                result_type = mergedResultType(then, else_);
            },
            .condition => {},
        }
        try self.setLabel(after_addr.local_addr.label, .abs);
        try self.set(source, .initRegister(.r2), .from(result.dereference()));

        return .fromLocation(ir.Location.initRegister(.r2).typed(result_type));
    }

    fn compileIfNoElse(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const if_condition = try self.compileIfCondition(source, if_expr);
        const condition = if_condition.condition;
        if (condition.source.isValueTag(.exit_code)) {
            const c = condition.source.value.exit_code.toBoolean();

            if (c) return self.compileIfBranchExpression(source, if_expr.then_expr, if_condition.capture_binding);
            return .fromValue(.void);
        }
        const after_addr = try self.newLabel("if_after", .unknown);
        try self.jmp(source, condition, false, after_addr);
        const then_result = try self.compileIfBranchExpression(source, if_expr.then_expr, if_condition.capture_binding);
        if (isWaitable(then_result)) |loc| {
            try self.wait(source, loc);
        }
        try self.setLabel(after_addr.local_addr.label, .abs);
        return .fromValue(.void);
    }

    const IfCaptureBinding = struct {
        pattern: *ast.BindingPattern,
        value: ir.ValueSource,
    };

    const IfCondition = struct {
        condition: Result,
        capture_binding: ?IfCaptureBinding = null,
    };

    fn compileIfCondition(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
    ) Error!IfCondition {
        const condition = try self.compileTransientExpression(source, if_expr.condition);
        const capture = if_expr.capture;

        const condition_type = blk: {
            if (condition.typeExpr()) |type_expr| break :blk type_expr;
            if (if_expr.condition.* == .identifier) {
                const identifier = if_expr.condition.identifier;
                if (self.lookup(identifier.name, .{ .shallow = false })) |binding| {
                    if (binding.result.typeExpr()) |type_expr| break :blk type_expr;
                }
            }
            break :blk null;
        };
        const is_literal_null = condition.source.isValueTag(.null) or
            (if_expr.condition.* == .literal and if_expr.condition.literal == .null);

        if (condition_type) |condition_type_| switch (condition_type_) {
            .optional => |optional| {
                const is_present_ref = try self.newRef(source, "if_optional_present");
                try self.cmp(
                    source,
                    .not_equal,
                    stableResultSource(condition),
                    .fromValue(.null),
                    is_present_ref,
                );

                return .{
                    .condition = try .from(is_present_ref.dereference()),
                    .capture_binding = if (capture) |capture_clause| blk: {
                        if (capture_clause.bindings.len != 1) {
                            try self.reportSourceError(
                                source,
                                Error.UnsupportedBindingPattern,
                                .@"error",
                                "if capture clauses currently require exactly one binding",
                                .{},
                            );
                            return .{ .condition = .fromValue(.void) };
                        }

                        var capture_value = condition.source;
                        if (capture_value == .location) {
                            capture_value.location = capture_value.location.typed(optional.child.*);
                        }

                        break :blk .{
                            .pattern = capture_clause.bindings[0],
                            .value = capture_value,
                        };
                    } else null,
                };
            },
            else => {},
        };

        if (is_literal_null) {
            if (capture) |_| {
                try self.reportSourceError(
                    source,
                    Error.UnsupportedExpression,
                    .@"error",
                    "if capture requires an optional condition",
                    .{},
                );
                return .{ .condition = .fromValue(.void) };
            }

            return .{ .condition = .fromValue(.fromBoolean(false)) };
        }

        if (capture) |_| {
            try self.reportSourceError(
                source,
                Error.UnsupportedExpression,
                .@"error",
                "if capture requires an optional condition",
                .{},
            );
            return .{ .condition = .fromValue(.void) };
        }

        return .{ .condition = condition };
    }

    fn compileIfBranchExpression(
        self: *IRCompiler,
        source: *ast.Expression,
        expr: *ast.Expression,
        capture_binding: ?IfCaptureBinding,
    ) Error!Result {
        try self.scopes.push(self.allocator, .lexical);
        defer self.scopes.pop();

        if (capture_binding) |binding| {
            switch (binding.pattern.*) {
                .discard => {},
                .identifier => |identifier| try self.compileIdentifierBinding(
                    source,
                    identifier,
                    binding.value,
                    null,
                    false,
                    .normal,
                ),
                else => {
                    try self.reportSourceError(
                        source,
                        Error.UnsupportedBindingPattern,
                        .@"error",
                        "if capture binding pattern not yet supported",
                        .{},
                    );
                    return .fromValue(.void);
                },
            }
        }

        var result = try self.compileExpression(expr);
        switch (result.source) {
            .location => |loc| if (loc.abs == .ref) {
                try self.set(source, .initRegister(.r2), stableResultSource(result));
                result = .fromLocation(ir.Location.initRegister(.r2).typed(result.typeExpr()));
            },
            else => {},
        }

        return result;
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
        const first_spawned = try self.spawnClosure(
            source,
            .initAbs(stage_sets[0], 0),
            self.threadStdin(),
            refs[0].dereference(),
            self.threadStderr(),
        );
        stage_closures[0] = first_spawned.closure;

        for (refs[0 .. refs.len - 1], refs[1..], stage_sets[1..], 1..) |prev, curr, stage_set, i| {
            try self.comment("stage {}", .{i});
            const spawned = try self.spawnClosure(
                source,
                .initAbs(stage_set, 0),
                prev.dereference(),
                curr.dereference(),
                self.threadStderr(),
            );
            stage_closures[i] = spawned.closure;
        }

        try self.comment("last stage", .{});
        const last_spawned = try self.spawnClosure(
            source,
            .initAbs(last_set, 0),
            refs[refs.len - 1].dereference(),
            self.threadStdout(),
            self.threadStderr(),
        );

        for (stage_sets, stage_closures, pipeline.stages[0 .. pipeline.stages.len - 1], 0..) |*stage_set, *stage_closure, stage_expr, i| {
            self.current_instruction_set = stage_set.*;
            try self.scopes.push(self.allocator, .closure);
            const stdout_stream_thread_ref = try self.newRef(source, "pipeline_stdout_stream_thread");
            try self.set(
                source,
                stdout_stream_thread_ref,
                .from(try self.forkInherit(source, self.stdoutStreamSet(), .noll)),
            );
            const result = try self.compileExpression(stage_expr);

            if (isWaitable(result)) |loc| {
                try self.comment("wait from {s}", .{@src().fn_name});
                try self.wait(source, loc);
                try self.pipeOpt(
                    source,
                    self.threadStdout(),
                    .keep_open,
                    .fromValue(.fromBoolean(false)),
                );
            } else {
                try self.pipeWrite(source, self.threadStdout(), result.source);
                try self.pipeOpt(
                    source,
                    self.threadStdout(),
                    .keep_open,
                    .fromValue(.fromBoolean(false)),
                );
            }

            try self.wait(source, stdout_stream_thread_ref.dereference());

            try self.setClosureIdentifiers();
            self.current_instruction_set = orig_instr_set;
            try self.comment("closure initialization stage {}: {f}", .{ i, stage_closure.return_addr });
            try self.compileClosureInitialization(source, stage_closure.*);
            self.scopes.pop();
        }
        self.current_instruction_set = orig_instr_set;

        self.current_instruction_set = last_set;
        try self.scopes.push(self.allocator, .closure);
        const result = try self.compileExpression(pipeline.stages[pipeline.stages.len - 1]);
        if (isWaitable(result)) |loc| {
            try self.comment("wait from {s} (last stage)", .{@src().fn_name});
            try self.wait(source, loc);
        }
        try self.exitWith(source, result);
        try self.setClosureIdentifiers();
        self.current_instruction_set = orig_instr_set;
        try self.comment("closure initialization last stage: {f}", .{last_spawned.closure.return_addr});
        try self.compileClosureInitialization(source, last_spawned.closure);
        self.scopes.pop();

        return .from(last_spawned.thread_handle);
    }

    fn compileBlock(
        self: *IRCompiler,
        source: *ast.Expression,
        block: ast.Block,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });
        try self.scopes.push(self.allocator, .lexical);

        var result: Result = .fromValue(.void);
        for (block.statements[0..block.statements.len -| 1]) |stmt| {
            result = try self.compileStatement(stmt);
        }
        if (block.statements.len > 0) {
            const last_stmt = block.statements[block.statements.len - 1];
            result = switch (last_stmt.*) {
                .expression => |expr| try self.compileExpression(expr.expression),
                else => try self.compileStatement(last_stmt),
            };
        }

        switch (result.source) {
            .location => |loc| if (loc.abs == .ref) {
                try self.set(source, .initRegister(.r2), stableResultSource(result));
                result = .fromLocation(ir.Location.initRegister(.r2).typed(result.typeExpr()));
            },
            else => {},
        }

        self.scopes.pop();

        return result;
    }

    fn compileFnDecl(
        self: *IRCompiler,
        source: *ast.Expression,
        fn_decl: ast.FunctionDecl,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const instr_set = try self.addInstructionSet();
        const fn_ref = ir.Value{
            .fn_ref = .{ .fn_addr = ir.InstructionAddr.initAbs(instr_set, 0) },
        };
        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = instr_set;
        try self.scopes.push(self.allocator, .closure);

        if (fn_decl.name) |name| {
            try self.scopes.declare(
                self.allocator,
                name.name,
                try .from(fn_ref),
                null,
                false,
                .normal,
            );
        }

        for (fn_decl.params._non_variadic) |param| {
            switch (param.pattern.*) {
                .discard => {},
                .identifier => |identifier| {
                    _ = try self.declareClosureValue(
                        .mutable(identifier, .normal),
                        0,
                        false,
                        if (param.type_annotation) |type_annotation| type_annotation.* else null,
                    );
                },
                .tuple, .record => {
                    try self.reportSourceError(
                        source,
                        Error.UnsupportedBindingPattern,
                        .@"error",
                        "function parameter destructuring is not yet supported in IR",
                        .{},
                    );
                    return .fromValue(.void);
                },
            }
        }

        // TODO: closure bindings, how do we manage them (non-parameters)?
        // TODO: figure out how to be able to call async functions multiple times and have the result not be overwritten in a ref
        const result = try self.compileExpression(fn_decl.body);
        // TODO: figure out how to make this async
        if (isWaitable(result)) |loc| {
            try self.wait(source, loc);
        }
        if (result.source.isValueTag(.void)) {
            try self.exitWith(source, .fromValue(.fromBoolean(true)));
        } else {
            try self.exitWith(source, result);
        }

        try self.setClosureIdentifiers();
        self.current_instruction_set = orig_instr_set;
        self.scopes.pop();
        if (fn_decl.name) |name| {
            try self.scopes.declare(
                self.allocator,
                name.name,
                try .from(fn_ref),
                null,
                false,
                .normal,
            );
        }

        return .from(fn_ref);
    }

    fn compileResultSaveR(
        self: *IRCompiler,
        source: anytype,
        result: Result,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (result.source.isRegister(.r)) {
            const ref = try self.newRef(source, "saved_r");
            try self.set(source, ref, .fromLocation(.initRegister(.r)));
            return .from(ref.dereference().typed(result.source.typeExpr()));
        }

        return result;
    }

    fn compileUnary(
        self: *IRCompiler,
        source: anytype,
        unary: ast.UnaryExpr,
    ) Error!Result {
        switch (unary.op) {
            .logical_not => {
                const result = try self.compileTransientExpression(source, unary.operand);
                if (result.source.isValueTag(.exit_code)) {
                    return .fromValue(.fromBoolean(!result.source.value.exit_code.toBoolean()));
                }
                const negated = try self.neg(source, result.source.location, .initRegister(.r));
                return .from(negated.typed(.{ .boolean = .{ .span = .global } }));
            },
        }
    }

    fn compileBinary(
        self: *IRCompiler,
        source: anytype,
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

                try self.ath(source, binary.op, left.source, right.source, ref);

                return .from(ref.dereference());
            },
            .logical_and, .logical_or => {
                return self.compileLogicalBinary(source, binary, .value);
            },
            .@"orelse" => {
                return self.compileOrelseBinary(source, binary);
            },
            .greater, .greater_equal, .less, .less_equal, .equal, .not_equal => {
                const left = try self.compileExpression(binary.left);
                const right = try self.compileExpression(binary.right);

                if (evaluateCompare(.from(binary.op), left.source, right.source)) |comptime_result| {
                    return .from(comptime_result);
                }

                const ref = try self.newRef(source, "cmp_result");

                try self.cmp(source, binary.op, left.source, right.source, ref);

                return .from(ref.dereference());
            },
            .append_redirect, .redirect_fd => return Error.UnsupportedBinaryOperation,
            .assign => {
                if (binary.left.* == .env_var) {
                    const env_var = binary.left.env_var;
                    const right = try self.compileExpression(binary.right);
                    try self.setEnv(source, env_var.identifier.name, right.source);
                    const result_ref = try self.newRef(source, "env_var_assign");
                    try self.set(source, result_ref, right.source);
                    return .from(result_ref.dereference().typed(optional_string_type));
                }

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
                const left_type = left.typeExpr() orelse return Error.UnsupportedBinaryOperation;
                if (left_type != .array) return Error.UnsupportedBinaryOperation;
                const element_type = left_type.array.element.*;
                const left_ref = try self.newRef(source, "array_access_left_ref");
                try self.set(source, left_ref, left.source);
                const right = try self.compileExpression(binary.right);
                try self.set(source, .initRegister(.r2), .from(left_ref.dereference()));

                try self.addInstruction(.init(.from(source), .{ .ath = .{
                    .op = .add,
                    .a = .fromLocation(.initRegister(.r2)),
                    .b = right.source,
                    .result = .initRegister(.r2),
                } }));
                try self.inc(source);

                try self.set(
                    source,
                    array_access_ref,
                    .fromLocation(.initAbs(.{ .register = .r2 }, .{ .dereference = true })),
                );

                return .from(array_access_ref.dereference().typed(element_type));
            },
            .apply, .pipe => {
                try self.log(@src().fn_name ++ ": error, encountered {t} binary expression", .{binary.op});
                try self.logEvaluateSpan(binary.span);
            },
            .member => return self.compileMemberBinary(source, binary),
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
        return .from(array_ref.dereference().typed(array_type(resolved_element_type)));
    }

    const ForSource = struct {
        kind: enum { array, range },
        value_type: ?ast.TypeExpr,
        base_ref: ?ir.Location = null,
        start_ref: ?ir.Location = null,
        len_ref: ?ir.Location = null,
    };

    fn compileForSource(
        self: *IRCompiler,
        source: *ast.Expression,
        for_source: *ast.Expression,
        index: usize,
    ) Error!ForSource {
        switch (for_source.*) {
            .range => |range| {
                const start_result = try self.compileExpression(range.start);
                const start_ref = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}_range_start", .{index}));
                try self.set(source, start_ref, start_result.source);

                var len_ref: ?ir.Location = null;
                if (range.end) |end_expr| {
                    const end_result = try self.compileExpression(end_expr);
                    const end_ref = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}_range_end", .{index}));
                    try self.set(source, end_ref, end_result.source);

                    const range_len_ref = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}_range_len", .{index}));
                    try self.ath(
                        source,
                        .subtract,
                        .from(end_ref.dereference()),
                        .from(start_ref.dereference()),
                        range_len_ref,
                    );
                    if (range.inclusive_end) {
                        try self.set(source, .initRegister(.r2), .from(range_len_ref.dereference()));
                        try self.inc(source);
                        try self.set(source, range_len_ref, .fromLocation(.initRegister(.r2)));
                    }
                    len_ref = range_len_ref;
                }

                return .{
                    .kind = .range,
                    .start_ref = start_ref,
                    .len_ref = len_ref,
                    .value_type = ast.TypeExpr.global(.integer),
                };
            },
            else => {
                const source_result = try self.compileExpression(for_source);

                if (source_result.source != .location or source_result.source.location.options.type_expr != .array) {
                    try self.reportSourceError(source, Error.NotImplemented, .@"error", "for loops with source type \"{t}\" not yet implemented", .{for_source.*});
                    return .{
                        .kind = .array,
                        .value_type = null,
                    };
                }

                const source_ref = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}", .{index}));
                try self.set(source, source_ref, source_result.source);

                const len_ref = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}_len", .{index}));
                try self.set(source, .initRegister(.r2), .fromLocation(source_ref.dereference()));
                try self.set(source, len_ref, .fromLocation(.initAbs(.{ .register = .r2 }, .{ .dereference = true })));

                return .{
                    .kind = .array,
                    .base_ref = source_ref,
                    .len_ref = len_ref,
                    .value_type = source_result.typeExpr().?.array.element.*,
                };
            },
        }
    }

    const LogicalCompileMode = enum {
        value,
        statement,
    };

    fn compileLogicalBinary(
        self: *IRCompiler,
        source: anytype,
        binary: ast.BinaryExpr,
        mode: LogicalCompileMode,
    ) Error!Result {
        switch (mode) {
            .statement => {
                const left = try self.compileExpression(binary.left);
                try self.finalizeStatementResult(source, left);

                if (evaluateLogical(.from(binary.op), left.source)) |comptime_result| {
                    return switch (comptime_result) {
                        .left => .fromValue(.void),
                        .right => self.compileLogicalRightStatement(source, binary.right),
                    };
                }

                const after_addr = try self.newLabel("logical_stmt_after", .unknown);
                switch (binary.op) {
                    .logical_and => try self.jmp(source, left, false, after_addr),
                    .logical_or => try self.jmp(source, left, true, after_addr),
                    else => {
                        try self.reportSourceError(
                            source,
                            Error.UnsupportedBinaryOperation,
                            .@"error",
                            "operator \"{t}\" is not supported in logical statement lowering",
                            .{binary.op},
                        );
                        return .fromValue(.void);
                    },
                }

                _ = try self.compileLogicalRightStatement(source, binary.right);
                try self.setLabel(after_addr.local_addr.label, .abs);
                return .fromValue(.void);
            },
            .value => {
                const left_expr_effects = self.analyzeExpressionEffects(binary.left);
                const right_expr_effects = self.analyzeExpressionEffects(binary.right);

                if (left_expr_effects.needs_stdio_capture or right_expr_effects.needs_stdio_capture) {
                    const result = try self.newRef(source, "logical_result");
                    const left = try self.compileStableExpressionIntoRef(source, binary.left, result);

                    if (evaluateLogical(.from(binary.op), left.source)) |comptime_result| {
                        return switch (comptime_result) {
                            .left => left,
                            .right => try self.compileStableExpression(source, binary.right, "logical_right"),
                        };
                    }

                    const after_addr = try self.newLabel("logical_after", .unknown);
                    switch (binary.op) {
                        .logical_and => try self.jmp(source, left, false, after_addr),
                        .logical_or => try self.jmp(source, left, true, after_addr),
                        else => {
                            try self.reportSourceError(
                                source,
                                Error.UnsupportedBinaryOperation,
                                .@"error",
                                "operator \"{t}\" is not supported in logical value lowering",
                                .{binary.op},
                            );
                            return .fromValue(.void);
                        },
                    }

                    const right = try self.compileStableExpressionIntoRef(source, binary.right, result);
                    try self.setLabel(after_addr.local_addr.label, .abs);

                    return .from(result.dereference().typed(mergedResultType(left, right)));
                }

                const left = try self.compileExpression(binary.left);

                if (evaluateLogical(.from(binary.op), left.source)) |comptime_result| {
                    return switch (comptime_result) {
                        .left => left,
                        .right => try self.compileExpression(binary.right),
                    };
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
        }
    }

    fn compileLogicalRightStatement(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
    ) Error!Result {
        switch (expr.*) {
            .binary => |binary| switch (binary.op) {
                .logical_and, .logical_or => return self.compileLogicalBinary(source, binary, .statement),
                else => {},
            },
            else => {},
        }
        const result = try self.compileExpression(expr);
        try self.finalizeStatementResult(source, result);
        return .fromValue(.void);
    }

    fn compileOrelseBinary(
        self: *IRCompiler,
        source: anytype,
        binary: ast.BinaryExpr,
    ) Error!Result {
        const left_is_literal_null = binary.left.* == .literal and binary.left.literal == .null;
        const result_ref = try self.newRef(source, "orelse_result");
        const left = try self.compileStableExpressionIntoRef(source, binary.left, result_ref);

        const result_type = if (left_is_literal_null or left.source.isValueTag(.null)) null else if (left.typeExpr()) |left_type| switch (left_type) {
            .optional => |optional| optional.child.*,
            .null => null,
            else => {
                try self.reportSourceError(
                    source,
                    Error.UnsupportedBinaryOperation,
                    .@"error",
                    "left side of orelse must be an optional",
                    .{},
                );
                return .fromValue(.void);
            },
        } else null;

        if (left_is_literal_null or left.source.isValueTag(.null)) {
            const right = try self.compileStableExpressionIntoRef(source, binary.right, result_ref);
            return .from(result_ref.dereference().typed(result_type orelse right.typeExpr()));
        }

        if (left.source == .value) {
            return .from(result_ref.dereference().typed(result_type orelse left.typeExpr()));
        }

        const is_null_ref = try self.newRef(source, "orelse_is_null");
        try self.cmp(
            source,
            .equal,
            .from(result_ref.dereference()),
            .fromValue(.null),
            is_null_ref,
        );

        const after_addr = try self.newLabel("orelse_after", .unknown);
        try self.jmp(source, try .from(is_null_ref.dereference()), false, after_addr);
        const right = try self.compileStableExpressionIntoRef(source, binary.right, result_ref);
        try self.setLabel(after_addr.local_addr.label, .abs);

        return .from(result_ref.dereference().typed(result_type orelse right.typeExpr()));
    }

    fn compileForBindingValue(
        self: *IRCompiler,
        source: *ast.Expression,
        for_source: ForSource,
        counter_ref: ir.Location,
        binding: ast.Identifier,
        capture_ref: ir.Location,
    ) Error!void {
        switch (for_source.kind) {
            .array => {
                const source_ref = for_source.base_ref.?;
                try self.set(
                    source,
                    .initRegister(.r2),
                    .fromLocation(source_ref.dereference()),
                );
                try self.ath(
                    source,
                    .add,
                    .fromLocation(.initRegister(.r2)),
                    .from(counter_ref.dereference()),
                    .initRegister(.r2),
                );
                try self.inc(source);
                try self.set(source, capture_ref, .fromLocation(.initAbs(.{ .register = .r2 }, .{ .dereference = true })));
            },
            .range => {
                try self.ath(
                    source,
                    .add,
                    .from(for_source.start_ref.?.dereference()),
                    .from(counter_ref.dereference()),
                    capture_ref,
                );
            },
        }

        try self.compileIdentifierBinding(
            source,
            binding,
            .from(capture_ref.dereference().typed(for_source.value_type)),
            null,
            false,
            .normal,
        );
    }

    fn compileForIterationsRef(
        self: *IRCompiler,
        source: *ast.Expression,
        for_sources: []const ForSource,
    ) Error!?ir.Location {
        var iterations_ref: ?ir.Location = null;

        for (for_sources, 0..) |for_source, i| {
            const len_ref = for_source.len_ref orelse continue;
            if (iterations_ref == null) {
                iterations_ref = try self.newRef(source, "for_iterations");
                try self.set(source, iterations_ref.?, .from(len_ref.dereference()));
                continue;
            }

            try self.cmp(
                source,
                .less,
                .from(len_ref.dereference()),
                .from(iterations_ref.?.dereference()),
                .initRegister(.r2),
            );
            const keep_iterations_label = try self.newLabel(
                try std.fmt.allocPrint(self.allocator, "for_keep_iterations_{}", .{i}),
                .unknown,
            );
            try self.jmp(source, .fromLocation(.initRegister(.r2)), false, keep_iterations_label);
            try self.set(source, iterations_ref.?, .from(len_ref.dereference()));
            try self.setLabel(keep_iterations_label.local_addr.label, .abs);
        }

        return iterations_ref;
    }

    fn compileForLoop(
        self: *IRCompiler,
        source: *ast.Expression,
        for_expr: ast.ForExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });
        const for_sources = try self.allocator.alloc(ForSource, for_expr.sources.len);
        defer self.allocator.free(for_sources);
        for (for_expr.sources, 0..) |for_source_expr, i| {
            for_sources[i] = try self.compileForSource(source, for_source_expr, i);
        }
        const capture_refs = try self.allocator.alloc(?ir.Location, for_expr.capture.bindings.len);
        defer self.allocator.free(capture_refs);
        for (for_expr.capture.bindings, 0..) |capture, i| {
            capture_refs[i] = switch (capture.*) {
                .identifier => try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_capture_{}", .{i})),
                .discard => null,
                .tuple, .record => {
                    try self.reportSourceError(
                        source,
                        Error.UnsupportedBindingPattern,
                        .@"error",
                        "for-loop destructuring captures are not yet supported in IR",
                        .{},
                    );
                    return .fromValue(.void);
                },
            };
        }

        const counter_ref = try self.newRef(source, "for_counter");
        try self.set(source, counter_ref, .fromValue(.{ .uinteger = 0 }));
        const iterations_ref = try self.compileForIterationsRef(source, for_sources) orelse {
            try self.reportSourceError(source, Error.NotImplemented, .@"error", "for loops require at least one finite source", .{});
            return .fromValue(.void);
        };

        const after_label = try self.newLabel("for_after", .unknown);
        const for_label = try self.newLabel("for", .abs);

        try self.cmp(source, .less, .from(counter_ref.dereference()), .from(iterations_ref.dereference()), .initRegister(.r2));
        try self.jmp(source, .fromLocation(.initRegister(.r2)), false, after_label);

        // 1. Create new bindings scope for for body

        try self.scopes.push(self.allocator, .lexical);

        // 2. Declare bindings for source captures

        for (for_expr.capture.bindings, for_sources, 0..) |capture, for_source, i| {
            switch (capture.*) {
                .discard => {},
                .identifier => |identifier| try self.compileForBindingValue(
                    source,
                    for_source,
                    counter_ref,
                    identifier,
                    capture_refs[i].?,
                ),
                .tuple, .record => {
                    try self.reportSourceError(
                        source,
                        Error.UnsupportedBindingPattern,
                        .@"error",
                        "for-loop destructuring captures are not yet supported in IR",
                        .{},
                    );
                    return .fromValue(.void);
                },
            }
        }

        // 4. Compile for body as statement

        const stack_before_body = self.currentFrame().rel_stack_counter;
        const for_body_result = try self.compileExpression(for_expr.body);

        if (isWaitable(for_body_result)) |loc| {
            try self.wait(source, loc);
        }

        // Pop any refs that were allocated during the body but not cleaned up.
        // Without this, the runtime stack grows each iteration and ref lookups
        // based on compile-time rel_stack_addr become incorrect on iteration 2+.
        const body_extra_refs = self.currentFrame().rel_stack_counter - stack_before_body;
        for (0..body_extra_refs) |_| {
            _ = try self.pop(source);
        }

        // 5. Pop bindings scope

        self.scopes.pop();

        try self.set(source, .initRegister(.r2), .from(counter_ref.dereference()));
        try self.inc(source);
        try self.set(source, counter_ref, .fromLocation(.initRegister(.r2)));
        try self.jmp(source, null, true, for_label);

        try self.setLabel(after_label.local_addr.label, .abs);

        // TODO: return something like the block compilation is doing
        return .fromValue(.void);
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
            .binary => |binary| brk: {
                var result = self.analyzeExpressionEffects(binary.left);
                result.merge(self.analyzeExpressionEffects(binary.right));
                break :brk result;
            },
            .unary => |unary| self.analyzeExpressionEffects(unary.operand),
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
            .condition => {},
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

fn compileInlineForTest(
    allocator: Allocator,
    source: []const u8,
    script_args: []const []const u8,
) !CompilationResult {
    var document_store = FrontendDocumentStore.init(allocator);
    defer document_store.deinit();

    const path = ":compiler-test";
    const document = try document_store.putDocument(path, source);
    const parse_result = document.parser.parseScript(path);
    const script = switch (parse_result) {
        .success => |script| script,
        .err => |err| {
            std.debug.print("unexpected parse failure in compiler test:\n", .{});
            for (err.diagnostics()) |diag| {
                std.debug.print("{s}\n", .{diag.message});
            }
            return error.UnexpectedToken;
        },
    };

    document.ast = script;

    var compiler = try IRCompiler.init(
        allocator,
        &document_store.document_store,
        &document.ast.?,
        script_args,
        null,
    );
    return try compiler.compile();
}

fn expectCompilerDiagnostic(
    allocator: Allocator,
    source: []const u8,
    expected_err: Error,
    message_substring: []const u8,
) !void {
    const result = try compileInlineForTest(allocator, source, &.{});
    switch (result) {
        .success => return error.TestUnexpectedSuccess,
        .err => |err| {
            try std.testing.expect(err.diagnostics().len > 0);
            try std.testing.expectEqual(expected_err, err.diagnostics()[0].err);
            try std.testing.expect(
                std.mem.indexOf(u8, err.diagnostics()[0].message, message_substring) != null,
            );
        },
    }
}

fn expectCompilerDiagnosticWithArgs(
    allocator: Allocator,
    source: []const u8,
    script_args: []const []const u8,
    expected_err: Error,
    message_substring: []const u8,
) !void {
    const result = try compileInlineForTest(allocator, source, script_args);
    switch (result) {
        .success => return error.TestUnexpectedSuccess,
        .err => |err| {
            try std.testing.expect(err.diagnostics().len > 0);
            try std.testing.expectEqual(expected_err, err.diagnostics()[0].err);
            try std.testing.expect(
                std.mem.indexOf(u8, err.diagnostics()[0].message, message_substring) != null,
            );
        },
    }
}

test "compiler diagnoses mixed stdio-capture if else without panicking" {
    const allocator = std.testing.allocator;
    try expectCompilerDiagnostic(
        allocator,
        \\const verbose = false
        \\if (verbose) {
        \\  echo "captured"
        \\} else {
        \\  1
        \\}
    ,
        Error.NotImplemented,
        "mixed stdio-capture branches",
    );
}

test "compiler diagnoses missing internal struct member without panicking" {
    const allocator = std.testing.allocator;
    try expectCompilerDiagnostic(
        allocator,
        \\const result = echo "hello"
        \\echo "${result.nope}"
    ,
        Error.NotImplemented,
        "member \"nope\" not found on internal struct",
    );
}

test "compiler diagnoses unsupported array member without panicking" {
    const allocator = std.testing.allocator;
    try expectCompilerDiagnostic(
        allocator,
        \\const arr = .{ 1, 2 }
        \\echo "${arr.nope}"
    ,
        Error.NotImplemented,
        "member \"nope\" not found on array value",
    );
}

test "compiler diagnoses unsupported for source type without panicking" {
    const allocator = std.testing.allocator;
    try expectCompilerDiagnostic(
        allocator,
        \\for ("hello") |c| echo "${c}"
    ,
        Error.NotImplemented,
        "for loops with source type",
    );
}

test "compiler diagnoses infinite-only for loop without panicking" {
    const allocator = std.testing.allocator;
    try expectCompilerDiagnostic(
        allocator,
        \\for (0..) |i| echo "${i}"
    ,
        Error.NotImplemented,
        "for loops require at least one finite source",
    );
}

test "compiler diagnoses script arg arity mismatch without panicking" {
    const allocator = std.testing.allocator;
    try expectCompilerDiagnosticWithArgs(
        allocator,
        \\fn Void @(file: String) String
        \\echo "${file}"
    ,
        &.{},
        Error.UnsupportedExpression,
        "expected 1 script arguments, got 0",
    );
}
