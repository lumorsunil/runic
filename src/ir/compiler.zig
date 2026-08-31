const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir.zig");
const ast = @import("../frontend/ast.zig");
const ExitCode = @import("../runtime/exit_code.zig").ExitCode;
const rainbow = @import("../rainbow.zig");
const DocumentStore = @import("../document_store.zig").DocumentStore;
const Stream = @import("../stream.zig").Stream;
const RCError = @import("../mem/rc.zig").RCError;
const FrontendDocumentStore = @import("../frontend/document_store.zig").FrontendDocumentStore;
const resolveModulePath = @import("../frontend/document_store.zig").resolveModulePath;
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
const int_element_type = ast.TypeExpr.global(.integer);
pub const int_array_type = ast.TypeExpr{ .array = .{
    .element = &int_element_type,
    .span = .global,
} };
const push_fallback_element = ast.TypeExpr{ .void = .{ .span = .global } };
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

/// Returns true when `type_expr` denotes the given primitive type name, whether
/// it is already a resolved primitive tag or an unresolved `.identifier`/`.alias`
/// referring to that name (e.g. `Int`). Used for type-directed `&0` parsing
/// where the compiler only has the raw declared stdin type.
fn typeExprIsNamed(type_expr: ast.TypeExpr, name: []const u8) bool {
    return switch (type_expr) {
        .integer => std.mem.eql(u8, name, "Int"),
        .float => std.mem.eql(u8, name, "Float"),
        .boolean => std.mem.eql(u8, name, "Bool"),
        .byte => std.mem.eql(u8, name, "Byte"),
        .alias => |alias| std.mem.eql(u8, alias.name, name),
        .identifier => |named| blk: {
            const segments = named.path.segments;
            if (segments.len == 0) break :blk false;
            break :blk std.mem.eql(u8, segments[segments.len - 1].name, name);
        },
        else => false,
    };
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
        is_pub: bool = false,
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
        if (depth >= self.frames.items.len) return GetFrameError.FrameStartDepthTooHigh;
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
    /// Number of declared parameters (for `_non_variadic`). Lets a zero-arg
    /// reference to a parameter-taking function resolve to a function *value*
    /// (`const f = dbl`) instead of being called with no arguments.
    param_count: usize = 0,
    closure_slot_count: usize = 0,
    closure_captures: []const ClosureCapture = &.{},
    pub_exports: []const PubExport = &.{},

    pub const PubExport = struct {
        name: []const u8,
        slot: usize,
        type_expr: ?ast.TypeExpr = null,
        /// For pub fn exports: the compile-time fn_ref value so callers can emit a direct call.
        fn_ref_value: ?ir.Value = null,
    };

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
    io: std.Io,
    allocator: Allocator,
    script: *ast.Script,
    script_args: []const []const u8,
    env_map: *std.process.Environ.Map,
    scopes: Scope = .init(),
    data: IRData = .init(),
    instruction_sets: std.ArrayList(InstructionSet) = .empty,
    current_instruction_set: usize = 0,
    labels: ir.Labels = .init(),
    struct_types: std.ArrayList(ir.Value.Struct.Type) = .empty,
    /// Top-level error set declarations, keyed by name, so `compileMember` can
    /// recognize `MyError.Variant` as error-value construction.
    error_sets: std.StringHashMapUnmanaged(ast.TypeExpr.ErrorSet) = .empty,
    /// Top-level user struct type declarations, keyed by name, so
    /// `compileStructLiteral` can build a struct value with the right layout.
    user_struct_types: std.StringHashMapUnmanaged(ast.TypeExpr.StructType) = .empty,
    document_store: *DocumentStore,
    logging_enabled: bool,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    result_counter: usize = 0,
    allow_simple_exec: bool = false,
    compiled_modules: std.StringHashMapUnmanaged(usize) = .empty,
    loading_set: std.StringHashMapUnmanaged(void) = .empty,
    /// Stack of enclosing functions' declared stdin types, so `&0` can be
    /// typed correctly for coercions. Top is the innermost function.
    stdin_type_stack: std.ArrayList(?ast.TypeExpr) = .empty,

    /// Comptime evaluation state. `comptime_forcing > 0` while lowering a
    /// `comptime` expression, which enables the comptime folder to reduce pure
    /// function calls (off by default so ordinary calls stay runtime). Maps a
    /// function's instruction-set index to its AST declaration so the folder can
    /// interpret its body, and bounds recursion so a non-terminating comptime
    /// call fails to compile instead of hanging the compiler.
    comptime_forcing: usize = 0,
    comptime_fn_decls: std.AutoHashMapUnmanaged(usize, *const ast.FunctionDecl) = .empty,
    comptime_depth: usize = 0,

    /// Monomorphization. `fn_decl_sources` maps a function's instruction set to
    /// its declaration expression (so a specialization can recompile its body);
    /// `specializations` caches a compiled specialization per "instr_set|T=Int…"
    /// key; `specializing` is set while compiling one, so `compileFnDecl` skips
    /// the outer name declaration and re-registration.
    fn_decl_sources: std.AutoHashMapUnmanaged(usize, *ast.Expression) = .empty,
    specializations: std.StringHashMapUnmanaged(usize) = .empty,
    specializing: bool = false,
    specialization_depth: usize = 0,
    /// The generic instruction set currently being specialized, passed from
    /// `maybeSpecialize` to `compileFnDecl` so it can register the (generic →
    /// specialization) mapping once the specialization's set is created.
    specializing_generic: ?usize = null,
    /// Stack of in-flight specializations; a self-recursive call inside a
    /// specialization body (its callee resolves to the generic set) is
    /// redirected to the specialization so it recurses at the same concrete type.
    active_specializations: std.ArrayListUnmanaged(struct { generic: usize, spec: usize }) = .empty,

    /// Concrete types bound by `|T|` captures in binding positions (`const x:
    /// []|T| = …` → `T` = the element type). A later `: T` resolves through here.
    type_captures: std.StringHashMapUnmanaged(ast.TypeExpr) = .empty,
    /// A generic type constructor's type parameters, by name — so an application
    /// `Box(Int)` can substitute its arguments into the registered struct body.
    generic_ctor_params: std.StringHashMapUnmanaged([]const ast.Identifier) = .empty,
    /// Recursion-depth cap for comptime call interpretation. Kept well below the
    /// point where the interpreter's own (native) call stack would overflow, so
    /// a non-terminating `comptime` recursion fails to compile instead of
    /// crashing. Deeply recursive comptime algorithms beyond this are rejected.
    const comptime_max_depth: usize = 128;
    /// Cap on nested specialization compilation, so type-changing recursion can't
    /// specialize forever (same-type recursion is redirected, not re-specialized).
    const specialization_max_depth: usize = 64;

    pub fn init(
        io: std.Io,
        allocator: Allocator,
        document_store: *DocumentStore,
        script: *ast.Script,
        script_args: []const []const u8,
        env_map: *std.process.Environ.Map,
    ) Allocator.Error!@This() {
        const logging_enabled_s = env_map.get("RUNIC_LOG_" ++ logging_name) orelse null;
        const logging_enabled = if (logging_enabled_s) |le| std.mem.eql(u8, le, "1") else false;

        return .{
            .io = io,
            .allocator = allocator,
            .script = script,
            .script_args = script_args,
            .env_map = env_map,
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

    pub fn simpleExec(
        self: *IRCompiler,
        source: anytype,
        executable: ir.ValueSource,
        arguments: []const ir.ValueSource,
    ) Error!void {
        try self.addInstruction(.init(.from(source), .{ .simple_exec = .{
            .executable = executable,
            .arguments = arguments,
        } }));
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
        const exit_code = try self.resolveExitCode(source, value) orelse return;
        return self.addInstruction(.init(.from(source), .exit_(exit_code)));
    }

    /// The user-facing `exit` statement: terminates the whole program from any
    /// thread (a plain `.exit` from a forked function thread would only close
    /// that thread and let the script continue).
    pub fn processExit_(
        self: *IRCompiler,
        source: anytype,
        value: Result,
    ) Error!void {
        const exit_code = try self.resolveExitCode(source, value) orelse return;
        return self.addInstruction(.init(.from(source), .processExit_(exit_code)));
    }

    fn resolveExitCode(self: *IRCompiler, source: anytype, value: Result) Error!?ExitCode {
        const code: ?ExitCode = switch (value.source) {
            .value => |v| switch (v) {
                .integer => |x| .fromByte(@intCast(@mod(x, 256))),
                .exit_code => |exit_code| exit_code,
                else => null,
            },
            else => null,
        };
        if (code) |c| return c;
        try self.reportSourceError(source, Error.UnsupportedExitCodeExpression, .@"error", "value type \"{t}\" cannot be coerced into an exit code", .{value.source});
        return null;
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

    /// Records every top-level `const Name = error { ... }` declaration so that
    /// `MyError.Variant` member access can be compiled as error-value construction.
    fn registerErrorSets(self: *IRCompiler) Allocator.Error!void {
        // Builtin error sets available in every script.
        try self.error_sets.put(self.allocator, "ExecutableError", ast.TypeExpr.executableErrorSet);
        try self.error_sets.put(self.allocator, "ParseError", ast.TypeExpr.parseErrorSet);

        try self.registerTypeDecls(self.script.statements);
    }

    /// Records the struct / generic-constructor / error-set type declarations in
    /// `statements` (the main script or an imported module), so their
    /// construction, member access, and applications resolve.
    fn registerTypeDecls(self: *IRCompiler, statements: []const *ast.Statement) Allocator.Error!void {
        for (statements) |stmt| {
            if (stmt.* != .type_binding_decl) continue;
            const decl = stmt.type_binding_decl;
            switch (decl.type_expr.*) {
                .error_set => |es| try self.error_sets.put(self.allocator, decl.identifier.name, es),
                .struct_type => |st| {
                    try self.user_struct_types.put(self.allocator, decl.identifier.name, st);
                    // Record a generic constructor's type parameters so an
                    // application (`Box(Int)`) can substitute the arguments.
                    if (decl.params.len > 0) {
                        try self.generic_ctor_params.put(self.allocator, decl.identifier.name, decl.params);
                    }
                },
                else => {},
            }
        }
    }

    pub fn compile(self: *IRCompiler) Error!CompilationResult {
        try self.registerErrorSets();

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
        try self.set(null, .initAbs(.{ .register = .r }, .{ .dereference = true }), .fromValue(.{ .integer = @as(i64, @intCast(args.len)) }));

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
            .exit_stmt => |e| self.compileExit(stmt, e),
            .yield_stmt => |y| self.compileYield(stmt, y),
            .while_stmt => |w| self.compileWhileLoop(stmt, w),
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
        return self.compileExpressionAsStatement(source, expr);
    }

    fn compileExpressionAsStatement(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
    ) Error!Result {
        switch (expr.*) {
            .binary => |binary| switch (binary.op) {
                .logical_and, .logical_or, .sequence => return self.compileLogicalBinary(source, binary, .statement),
                // A command-left `>` is a redirect; rewrite to the redirected
                // call here so it takes the normal command-statement path
                // (direct-executable handling, etc.) rather than being compiled
                // as an expression.
                .greater => if (try self.greaterRedirectCall(binary)) |call_expr| {
                    return self.compileExpressionAsStatement(source, call_expr);
                },
                else => {},
            },
            else => {},
        }

        if (isBackgroundExecutionExpression(expr)) {
            _ = try self.compileDetachedExpressionStatement(source, expr);
            return .fromValue(.void);
        }

        if (expr.* == .block) {
            return self.compileBlockAsStatement(expr, expr.block);
        }

        if (self.allow_simple_exec and expr.* == .call and self.isDirectExecutableStatementCall(expr.call)) {
            return self.compileDirectExecutableStatement(expr, expr.call);
        }

        const result = try self.compileExpression(expr);

        // Auto-call fn_ref values from member access used as statements (e.g., `m.greeting`).
        // Mirrors how bare identifier calls work, but only for member-access expressions so
        // fn declarations and identifier references are not inadvertently called.
        const is_member_access = expr.* == .binary and expr.binary.op == .member;
        if (is_member_access and result.source == .value and result.source.value == .fn_ref) {
            const call_result = try self.compileFunctionCall(expr, result.source.value, &.{}, &.{}, null);
            try self.finalizeStatementResult(source, call_result);
            return .fromValue(.void);
        }

        try self.finalizeStatementResult(source, result);
        return .fromValue(.void);
    }

    fn compileBlockAsStatement(
        self: *IRCompiler,
        source: *ast.Expression,
        block: ast.Block,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });
        try self.scopes.push(self.allocator, .lexical);
        defer self.scopes.pop();

        for (block.statements) |stmt| {
            _ = try self.compileStatement(stmt);
        }

        return .fromValue(.void);
    }

    fn isDirectExecutableStatementCall(self: *IRCompiler, call: ast.CallExpr) bool {
        if (call.background or call.redirects.len != 0) return false;
        if (call.callee.* != .identifier) return false;

        const name = call.callee.identifier.name;
        if (std.mem.eql(u8, name, "cd")) return false;
        // `run` computes its executable from the first argument; it must go
        // through the general call path, not the static-name direct exec.
        if (std.mem.eql(u8, name, "run")) return false;
        for (call.arguments) |arg| {
            if (!isSimpleExecArgument(arg)) return false;
        }
        return self.lookup(name, .{ .shallow = false }) == null;
    }

    fn isSimpleExecArgument(expr: *ast.Expression) bool {
        return switch (expr.*) {
            .identifier, .env_var => true,
            .literal => |literal| switch (literal) {
                .string => |string| blk: {
                    for (string.segments) |segment| {
                        if (segment != .text) break :blk false;
                    }
                    break :blk true;
                },
                else => true,
            },
            else => false,
        };
    }

    fn compileDirectExecutableStatement(
        self: *IRCompiler,
        source: *ast.Expression,
        call: ast.CallExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const stack_before_args = self.currentFrame().rel_stack_counter;
        const args = try self.allocator.alloc(ir.ValueSource, call.arguments.len);
        defer self.allocator.free(args);
        for (call.arguments, 0..) |arg_expr, i| {
            const arg = try self.compileExpression(arg_expr);
            args[i] = arg.source;
        }

        const executable = ir.ValueSource.fromValue(.{
            .executable = (try self.addSlice(1, call.callee.identifier.name)).slice,
        });
        try self.simpleExec(source, executable, try self.allocator.dupe(ir.ValueSource, args));

        const extra_refs = self.currentFrame().rel_stack_counter - stack_before_args;
        for (0..extra_refs) |_| {
            _ = try self.pop(source);
        }
        return .fromValue(.void);
    }

    fn isBackgroundExecutionExpression(expr: *const ast.Expression) bool {
        return switch (expr.*) {
            .call => |call| call.background,
            .pipeline => |pipeline| pipeline.background,
            .block => |block| block.background,
            else => false,
        };
    }

    fn compileDetachedExpressionStatement(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
    ) Error!Result {
        _ = source;
        return self.compileBackgroundExpression(expr, expr);
    }

    fn compileBackgroundExpression(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
    ) Error!Result {
        return self.compileBackgroundExpressionValue(source, expr.*);
    }

    fn compileBackgroundExpressionValue(
        self: *IRCompiler,
        source: anytype,
        expr: ast.Expression,
    ) Error!Result {
        const instr_set = try self.addInstructionSet();
        const spawned = try self.spawnClosure(
            source,
            .initAbs(instr_set, 0),
            self.threadStdin(),
            self.threadStdout(),
            self.threadStderr(),
        );

        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = instr_set;
        try self.scopes.push(self.allocator, .closure);

        const result = switch (expr) {
            .call => |call| try self.compileCall(source, call),
            .pipeline => |pipeline| try self.compilePipeline(source, pipeline),
            .block => |block| try self.compileBlock(source, block),
            else => try self.compileExpression(source),
        };
        try self.finalizeStatementResult(source, result);
        try self.exit_(source, .fromValue(.{ .exit_code = .success }));

        try self.setClosureIdentifiers();
        self.current_instruction_set = orig_instr_set;
        try self.compileClosureInitialization(source, spawned.closure);
        self.scopes.pop();

        return .fromLocation(spawned.thread_handle);
    }

    fn finalizeStatementResult(
        self: *IRCompiler,
        source: anytype,
        result: Result,
    ) Error!void {
        if (result.isType(execution_handles_struct_type) and result.source.isRegister(.r)) {
            try self.comment("wait from {s}", .{@src().fn_name});
            try self.wait(source, ir.Location.initRegister(.r).typed(thread_type));
            return;
        }

        const stable = if (result.source.isRegister(.r) and result.isType(thread_type))
            try self.compileResultSaveR(source, result)
        else
            result;
        if (isWaitable(stable)) |loc| {
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

    /// Whether a compiled operand is (or may hold) an error value. Used by the
    /// logical-or lowering to choose error-discard semantics over the exit-code
    /// path — a `jmp`/`log` on an error value would misread it as an exit code.
    fn resultIsErrorLike(result: Result) bool {
        if (result.source.isValueTag(.err)) return true;
        const result_type = result.typeExpr() orelse return false;
        return switch (result_type) {
            .error_union, .error_set, .err => true,
            else => false,
        };
    }

    /// `yield expr` writes the value of `expr` to the thread's stdout stream
    /// (the program stdout, or the inter-stage pipe when used as a pipeline
    /// stage). Unlike `return`, it does not exit the function.
    fn compileYield(
        self: *IRCompiler,
        source: *ast.Statement,
        y: ast.YieldStmt,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const target = switch (y.fd) {
            1 => self.threadStdout(),
            2 => self.threadStderr(),
            else => {
                try self.reportSourceError(source, Error.UnsupportedExpression, .@"error", "cannot yield to &{d}; only &1 (stdout) and &2 (stderr) are writable", .{y.fd});
                return .fromValue(.void);
            },
        };

        // Clean up only the temporaries that compiling the yielded value
        // pushed. A bare `yield v` where `v` is a binding/loop capture returns a
        // *borrowed* reference and pushes nothing, so there is nothing to pop;
        // popping it (as the old `consume`'s isStackLocation check did) would
        // corrupt the stack and, inside a loop, make the body net-pop each
        // iteration — underflowing the per-iteration ref accounting. Measuring
        // the frame counter across compilation distinguishes owned temporaries
        // from borrowed references regardless of where they sit in the frame.
        const stack_before_value = self.currentFrame().rel_stack_counter;
        const value = try self.compileResultSaveR(source, try self.compileExpression(y.value));
        try self.pipeWrite(source, target, value.source);
        const pushed = self.currentFrame().rel_stack_counter -| stack_before_value;
        for (0..pushed) |_| _ = try self.pop(source);
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
        try self.processExit_(source, result);
        return .fromValue(.void);
    }

    fn compileBindingDecl(
        self: *IRCompiler,
        source: *ast.Statement,
        binding_decl: *ast.BindingDecl,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const result = try self.compileBinding(
            source,
            binding_decl.pattern,
            binding_decl.initializer,
            binding_decl.annotation,
            binding_decl.is_mutable,
        );

        if (binding_decl.is_pub) {
            switch (binding_decl.pattern.*) {
                .identifier => |identifier| {
                    if (self.lookup(identifier.name, .{ .shallow = true })) |binding| {
                        binding.is_pub = true;
                    }
                },
                else => {},
            }
        }

        return result;
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
                var result = try self.compileExpressionWithCapture(source, expr);
                // A command bound where an error union is expected becomes
                // `ExecutableError!String`: exit 0 → output, else an error.
                if (annotation) |ann| {
                    if (ann.* == .error_union and result.isType(execution_result_struct_type)) {
                        result = try self.compileExecutionToErrorUnion(source, result, ann.*);
                    }
                }
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
        // Normalize the annotation (String→[]Byte). When it carries a `|T|`
        // capture, unify it against the initializer's concrete type — recording
        // the captured names — and store that concrete type for this binding so
        // member access and later `: T` references resolve.
        const annotated_type = if (annotation) |ann| blk: {
            // Bind whatever captures structurally match the initializer, then
            // normalize the annotation: matched captures resolve to the concrete
            // type, unmatched ones (e.g. `?|T| = null`) stay type variables.
            if (hasTypeCapture(ann.*)) {
                // Seed every capture name as a permissive type variable first, so
                // an unmatched one (`?|T| = null`) still resolves to its name;
                // then bind the ones that structurally match to a concrete type.
                self.registerParamTypeVars(ann.*);
                // Literals don't carry a type on their Result; fall back to the
                // value's tag so a capture like `const x: |T| = 5` binds T=Int.
                const init_type = result.typeExpr() orelse
                    (if (result.source == .value) valueTypeExpr(result.source.value) else null);
                if (init_type) |it| self.bindTypeCaptures(ann.*, it);
            }
            break :blk self.normalizeStringTypes(ann.*);
        } else null;
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

        if (try self.evalComptimeExpression(expr)) |comptime_result| {
            if (!(comptime_result.source == .value and comptime_result.source.value == .zig_string)) {
                return comptime_result;
            }
        }

        return switch (expr.*) {
            .literal => |literal| self.compileLiteral(expr, literal),
            .identifier => |identifier| self.compileIdentifier(expr, identifier),
            .env_var => |env_var| self.compileEnvVar(expr, env_var),
            .call => |call| if (call.background) blk: {
                var call_copy = call;
                call_copy.background = false;
                break :blk self.compileBackgroundExpressionValue(expr, .{ .call = call_copy });
            } else self.compileCall(expr, call),
            .member => |member| self.compileMember(expr, member, .read),
            .if_expr => |if_expr| self.compileIf(expr, if_expr),
            .match_expr => |match_expr| self.compileMatch(expr, match_expr),
            .pipeline => |pipeline| if (pipeline.background) blk: {
                var pipeline_copy = pipeline;
                pipeline_copy.background = false;
                break :blk self.compileBackgroundExpressionValue(expr, .{ .pipeline = pipeline_copy });
            } else self.compilePipeline(expr, pipeline),
            .block => |block| if (block.background) blk: {
                var block_copy = block;
                block_copy.background = false;
                break :blk self.compileBackgroundExpressionValue(expr, .{ .block = block_copy });
            } else self.compileBlock(expr, block),
            .fn_decl => |fn_decl| self.compileFnDecl(expr, fn_decl),
            .import_expr => |import_expr| self.compileImportExpr(expr, import_expr),
            .binary => |binary| self.compileBinary(expr, binary),
            .unary => |unary| self.compileUnary(expr, unary),
            .array => |array| self.compileArray(expr, array),
            .struct_literal => |struct_literal| self.compileStructLiteral(expr, struct_literal),
            .catch_expr => |catch_expr| self.compileCatch(expr, catch_expr),
            .try_expr => |try_expr| self.compileTry(expr, try_expr),
            .is_expr => |is_expr| self.compileIs(expr, is_expr),
            .for_expr => |for_expr| self.compileForLoop(expr, for_expr),
            .comptime_expr => |comptime_expr| self.compileComptimeExpr(expr, comptime_expr),
            .subshell => |subshell| self.compileSubshell(expr, subshell),
            .fd => |fd_expr| self.compileFd(expr, fd_expr),
            else => {
                try self.reportSourceError(expr, Error.UnsupportedExpression, .@"error", "expression type \"{t}\" not yet supported", .{expr.*});
                return .fromValue(.void);
            },
        };
    }

    /// `&0` reads the function's stdin as a typed value (the former `@stdin`).
    /// `&1`/`&2` are write-only streams and are not readable here.
    fn compileFd(
        self: *IRCompiler,
        source: *ast.Expression,
        fd_expr: ast.FdExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (fd_expr.fd != 0) {
            try self.reportSourceError(source, Error.UnsupportedExpression, .@"error", "&{d} is a write-only stream; use `yield`", .{fd_expr.fd});
            return .fromValue(.void);
        }

        // Type the read with the enclosing function's declared stdin type so
        // coercions like T→?T work (e.g. `&0 orelse "default"` in a ?String
        // function). Fall back to String when no declared type is available.
        const declared_type: ?ast.TypeExpr = if (self.stdin_type_stack.items.len > 0)
            self.stdin_type_stack.items[self.stdin_type_stack.items.len - 1]
        else
            null;
        try self.addInstruction(.init(.from(source), .collect_stdin));
        // Type-directed parsing: an Int-typed stdin parses the collected bytes
        // into an Int value so arithmetic on `&0` works.
        if (declared_type) |dt| {
            if (typeExprIsNamed(dt, "Int")) {
                try self.addInstruction(.init(.from(source), .parse_int));
                return try self.stabilizeRegisterResult(source, "stdin_value", .global(.integer));
            }
        }
        return try self.stabilizeRegisterResult(source, "stdin_value", declared_type orelse string_type);
    }

    fn evalComptimeLiteral(self: *IRCompiler, literal: ast.Literal) Error!?Result {
        _ = self;
        return switch (literal) {
            .integer => |integer| .fromValue(try parseInt(integer.text)),
            .float => |float| .fromValue(try parseFloat(float.text)),
            .bool => |boolean| .fromValue(.{ .exit_code = .fromBoolean(boolean.value) }),
            .null => .fromValue(.null),
            .string => |string| blk: {
                if (string.segments.len != 1 or string.segments[0] != .text) break :blk null;
                break :blk .fromValue(.{ .zig_string = string.segments[0].text.payload });
            },
        };
    }

    fn evalComptimeBlock(
        self: *IRCompiler,
        block: ast.Block,
    ) Error!?Result {
        if (block.statements.len != 1) return null;
        const stmt = block.statements[0];
        if (stmt.* != .expression) return null;
        return self.evalComptimeExpression(stmt.expression.expression);
    }

    fn comptimeValueEql(self: *IRCompiler, left: Result, right: Result) bool {
        _ = self;
        if (left.source != .value or right.source != .value) return false;
        return switch (left.source.value) {
            .null => right.source.value == .null,
            .integer => |l| right.source.value == .integer and l == right.source.value.integer,
            .float => |l| right.source.value == .float and l == right.source.value.float,
            .exit_code => |l| right.source.value == .exit_code and l.toBoolean() == right.source.value.exit_code.toBoolean(),
            .zig_string => |l| right.source.value == .zig_string and std.mem.eql(u8, l, right.source.value.zig_string),
            else => false,
        };
    }

    fn comptimeConditionTruth(
        self: *IRCompiler,
        expr: *ast.Expression,
        result: Result,
    ) ?bool {
        if (result.source != .value) return null;

        const condition_type = blk: {
            if (result.typeExpr()) |type_expr| break :blk type_expr;
            if (expr.* == .identifier) {
                if (self.lookup(expr.identifier.name, .{ .shallow = false })) |binding| {
                    if (binding.type_expr) |type_expr| break :blk type_expr;
                }
            }
            break :blk null;
        };

        if (condition_type) |type_expr| switch (type_expr) {
            .optional => return result.source.value != .null,
            else => {},
        };

        return switch (result.source.value) {
            .exit_code => |exit_code| exit_code.toBoolean(),
            .null => false,
            else => null,
        };
    }

    fn evalComptimeExpression(
        self: *IRCompiler,
        expr: *ast.Expression,
    ) Error!?Result {
        return switch (expr.*) {
            .literal => |literal| try self.evalComptimeLiteral(literal),
            .identifier => |identifier| blk: {
                const binding = self.lookup(identifier.name, .{ .shallow = false }) orelse break :blk null;
                if (binding.is_mutable or binding.result.source != .value) break :blk null;
                break :blk binding.result;
            },
            .unary => |unary| blk: {
                const operand = (try self.evalComptimeExpression(unary.operand)) orelse break :blk null;
                break :blk switch (unary.op) {
                    .logical_not => if (operand.source.isValueTag(.exit_code))
                        Result.fromValue(.fromBoolean(!operand.source.value.exit_code.toBoolean()))
                    else
                        null,
                    .negate => if (evaluateArithmetic(.sub, .fromValue(.{ .integer = 0 }), operand.source)) |folded|
                        Result.fromValue(folded)
                    else
                        null,
                };
            },
            .binary => |binary| blk: {
                switch (binary.op) {
                    .logical_and, .logical_or => {
                        const left = (try self.evalComptimeExpression(binary.left)) orelse break :blk null;
                        if (evaluateLogical(.from(binary.op), left.source)) |logical_result| {
                            break :blk switch (logical_result) {
                                .left => left,
                                .right => (try self.evalComptimeExpression(binary.right)) orelse break :blk null,
                            };
                        }
                        break :blk null;
                    },
                    .@"orelse" => {
                        const left = (try self.evalComptimeExpression(binary.left)) orelse break :blk null;
                        if (left.source.isValueTag(.null)) {
                            break :blk (try self.evalComptimeExpression(binary.right)) orelse break :blk null;
                        }
                        break :blk left;
                    },
                    .add, .subtract, .multiply, .divide, .remainder => {
                        const left = (try self.evalComptimeExpression(binary.left)) orelse break :blk null;
                        const right = (try self.evalComptimeExpression(binary.right)) orelse break :blk null;
                        break :blk if (evaluateArithmetic(.from(binary.op), left.source, right.source)) |result|
                            .fromValue(result)
                        else
                            null;
                    },
                    .greater, .greater_equal, .less, .less_equal, .equal, .not_equal => {
                        const left = (try self.evalComptimeExpression(binary.left)) orelse break :blk null;
                        const right = (try self.evalComptimeExpression(binary.right)) orelse break :blk null;
                        if (evaluateCompare(.from(binary.op), left.source, right.source)) |result| {
                            break :blk .fromValue(result);
                        }
                        if (binary.op == .equal or binary.op == .not_equal) {
                            const equal = self.comptimeValueEql(left, right);
                            break :blk .fromValue(.fromBoolean(if (binary.op == .equal) equal else !equal));
                        }
                        break :blk null;
                    },
                    else => break :blk null,
                }
            },
            .if_expr => |if_expr| blk: {
                const condition = (try self.evalComptimeExpression(if_expr.condition)) orelse break :blk null;
                const truth = self.comptimeConditionTruth(if_expr.condition, condition) orelse break :blk null;
                if (truth) {
                    break :blk (try self.evalComptimeExpression(if_expr.then_expr)) orelse break :blk null;
                }
                if (if_expr.else_branch) |else_branch| switch (else_branch) {
                    .expr => |expr_| break :blk (try self.evalComptimeExpression(expr_)) orelse break :blk null,
                    .if_expr => |if_expr_| {
                        const else_expr = try self.allocExpression(.{ .if_expr = if_expr_.* });
                        break :blk (try self.evalComptimeExpression(else_expr)) orelse break :blk null;
                    },
                    .condition => break :blk condition,
                };
                break :blk .fromValue(.void);
            },
            .match_expr => |match_expr| blk: {
                const subject = (try self.evalComptimeExpression(match_expr.subject)) orelse break :blk null;
                for (match_expr.cases) |case| {
                    switch (case.pattern) {
                        .wildcard => break :blk (try self.evalComptimeBlock(case.body)) orelse break :blk null,
                        .literal => |literal| {
                            const pattern = (try self.evalComptimeLiteral(literal)) orelse break :blk null;
                            if (!self.comptimeValueEql(subject, pattern)) continue;
                            break :blk (try self.evalComptimeBlock(case.body)) orelse break :blk null;
                        },
                        else => break :blk null,
                    }
                }
                break :blk null;
            },
            .comptime_expr => |comptime_expr| blk: {
                // Nested `comptime` (or the operand of a top-level one): fold the
                // operand with call-folding enabled.
                self.comptime_forcing += 1;
                defer self.comptime_forcing -= 1;
                break :blk try self.evalComptimeExpression(comptime_expr.operand);
            },
            .call => |call| if (self.comptime_forcing > 0)
                try self.evalComptimeCall(call)
            else
                null,
            else => null,
        };
    }

    /// Comptime-evaluates a pure function call (`comptime fib 10`). Resolves the
    /// callee to a known function's AST, folds each argument, binds the
    /// parameters in a fresh scope, and interprets the body. Returns null (not
    /// comptime-foldable) if the callee isn't a plain function, an argument or
    /// the body can't be folded, or the arity doesn't match.
    fn evalComptimeCall(self: *IRCompiler, call: ast.CallExpr) Error!?Result {
        if (call.redirects.len != 0 or call.background) return null;
        if (call.callee.* != .identifier) return null;

        const binding = self.lookup(call.callee.identifier.name, .{ .shallow = false }) orelse return null;
        if (binding.is_mutable or binding.result.source != .value) return null;

        // A bare identifier parses as a zero-arg call (`n` -> `n()`); when it
        // names an immutable value (e.g. a bound parameter), fold to that value.
        if (binding.result.source.value != .fn_ref) {
            return if (call.arguments.len == 0) binding.result else null;
        }

        const instr_set = binding.result.source.value.fn_ref.fn_addr.instr_set;
        const fn_decl = self.comptime_fn_decls.get(instr_set) orelse return null;

        // Only non-variadic, identifier-parameter functions are interpretable.
        if (fn_decl.params != ._non_variadic) return null;
        const params = fn_decl.params._non_variadic;
        if (params.len != call.arguments.len) return null;

        // Fold arguments in the *caller's* scope, before binding parameters.
        const arg_values = try self.allocator.alloc(Result, call.arguments.len);
        defer self.allocator.free(arg_values);
        for (call.arguments, arg_values) |arg_expr, *slot| {
            slot.* = (try self.evalComptimeExpression(arg_expr)) orelse return null;
        }

        if (self.comptime_depth >= comptime_max_depth) {
            return null;
        }
        self.comptime_depth += 1;
        defer self.comptime_depth -= 1;

        // A fresh scope holds the parameter bindings; the function name stays
        // visible from the enclosing scope so recursion resolves.
        try self.scopes.push(self.allocator, .lexical);
        defer self.scopes.pop();

        for (params, arg_values) |param, arg_value| {
            switch (param.pattern.*) {
                .identifier => |identifier| try self.scopes.declare(
                    self.allocator,
                    identifier.name,
                    arg_value,
                    arg_value.typeExpr(),
                    false,
                    .normal,
                ),
                .discard => {},
                else => return null,
            }
        }

        return switch (try self.evalComptimeBody(fn_decl.body)) {
            .value => |result| result,
            // A body that never yields a value isn't comptime-foldable here.
            .fell_through, .not_foldable => null,
        };
    }

    /// The outcome of interpreting a comptime statement/body: a `yield`ed return
    /// value, a fall-through (statement completed without returning), or a
    /// non-foldable construct (the whole comptime evaluation gives up).
    const ComptimeFlow = union(enum) {
        value: Result,
        fell_through,
        not_foldable,
    };

    /// Interprets a function/branch body for comptime evaluation. A `yield`
    /// anywhere is the return value; a bare `if`/`match` whose taken branch
    /// yields returns through it; local `const` bindings extend the scope.
    fn evalComptimeBody(self: *IRCompiler, body: *ast.Expression) Error!ComptimeFlow {
        switch (body.*) {
            .block => |block| {
                for (block.statements) |stmt| {
                    switch (try self.evalComptimeStatement(stmt)) {
                        .value => |result| return .{ .value = result },
                        .fell_through => {},
                        .not_foldable => return .not_foldable,
                    }
                }
                return .fell_through;
            },
            // A bare-expression body (`fn … n * 2`) folds directly.
            else => return if (try self.evalComptimeExpression(body)) |result|
                .{ .value = result }
            else
                .not_foldable,
        }
    }

    fn evalComptimeStatement(self: *IRCompiler, stmt: *ast.Statement) Error!ComptimeFlow {
        switch (stmt.*) {
            .yield_stmt => |yield_stmt| {
                if (yield_stmt.fd != 1) return .not_foldable;
                const value = (try self.evalComptimeExpression(yield_stmt.value)) orelse return .not_foldable;
                return .{ .value = value };
            },
            .binding_decl => |binding_decl| {
                if (binding_decl.is_mutable) return .not_foldable;
                const value = (try self.evalComptimeExpression(binding_decl.initializer)) orelse return .not_foldable;
                switch (binding_decl.pattern.*) {
                    .identifier => |identifier| self.scopes.declare(
                        self.allocator,
                        identifier.name,
                        value,
                        value.typeExpr(),
                        false,
                        .normal,
                    ) catch return .not_foldable,
                    .discard => {},
                    else => return .not_foldable,
                }
                return .fell_through;
            },
            .expression => |expr_stmt| return self.evalComptimeControlStatement(expr_stmt.expression),
            else => return .not_foldable,
        }
    }

    /// A statement-position expression during comptime interpretation. An `if`
    /// or `match` here may yield (return) through its taken branch; anything
    /// else with no return value simply falls through if it folds.
    fn evalComptimeControlStatement(self: *IRCompiler, expr: *ast.Expression) Error!ComptimeFlow {
        switch (expr.*) {
            .if_expr => |if_expr| {
                const condition = (try self.evalComptimeExpression(if_expr.condition)) orelse return .not_foldable;
                const truth = self.comptimeConditionTruth(if_expr.condition, condition) orelse return .not_foldable;
                if (truth) return self.evalComptimeBody(if_expr.then_expr);
                if (if_expr.else_branch) |else_branch| switch (else_branch) {
                    .expr => |else_expr| return self.evalComptimeBody(else_expr),
                    .if_expr => |else_if| {
                        const else_expr = try self.allocExpression(.{ .if_expr = else_if.* });
                        return self.evalComptimeControlStatement(else_expr);
                    },
                    .condition => return .fell_through,
                };
                return .fell_through;
            },
            // A non-control statement expression that folds is a discarded value
            // (falls through); one that can't fold gives up.
            else => return if (try self.evalComptimeExpression(expr)) |_|
                .fell_through
            else
                .not_foldable,
        }
    }

    /// In-process typed capture of a function call that returns a structured
    /// value — an error union (`E!T`) or an optional (`?T`). Such a function
    /// `yield`s the value (an error value / `null` / the payload); a byte
    /// capture would flatten an error to text (`"E.Bad"`) or drop `null`,
    /// losing the discriminant. Instead run the function with a `typed` stdout
    /// pipe (so `yield` enqueues the value), wait, and dequeue it — returning
    /// the value typed as the return type so `catch`/`try`/`match`/`if` operate
    /// on the real value. Returns null (caller falls back to the byte path)
    /// when not applicable.
    const CapturableCall = struct {
        /// The function to call.
        method_name: []const u8,
        /// Full arguments (for a UFCS call `recv.m a b`, the receiver is arg 0).
        arguments: []const *ast.Expression,
        redirects: []const ast.Redirection,
    };

    /// Extracts the function + arguments of a value-capturable call: a plain
    /// `name(args…)`, or a UFCS method access `recv.method` / `recv.method args…`
    /// (receiver prepended). Returns null for anything else.
    fn capturableCallInfo(self: *IRCompiler, expr: *ast.Expression) Error!?CapturableCall {
        switch (expr.*) {
            .call => |call| {
                if (call.background) return null;
                switch (call.callee.*) {
                    .identifier => |id| return .{ .method_name = id.name, .arguments = call.arguments, .redirects = call.redirects },
                    .binary => |b| if (b.op == .member and b.right.* == .identifier)
                        return .{ .method_name = b.right.identifier.name, .arguments = try self.prependReceiver(b.left, call.arguments), .redirects = call.redirects }
                    else
                        return null,
                    else => return null,
                }
            },
            // Bare `recv.method` (no args) — a UFCS call with the receiver alone.
            .binary => |b| if (b.op == .member and b.right.* == .identifier)
                return .{ .method_name = b.right.identifier.name, .arguments = try self.prependReceiver(b.left, &.{}), .redirects = &.{} }
            else
                return null,
            else => return null,
        }
    }

    fn prependReceiver(self: *IRCompiler, receiver: *ast.Expression, args: []const *ast.Expression) Error![]const *ast.Expression {
        const full = try self.allocator.alloc(*ast.Expression, args.len + 1);
        full[0] = receiver;
        for (args, 0..) |a, i| full[i + 1] = a;
        return full;
    }

    /// Whether a function's return type should be captured as a typed value
    /// (rather than byte-serialized). Structured values (error union / optional /
    /// sum / struct) must survive intact; scalar numbers/bools must keep their
    /// runtime tag so the caller can do arithmetic. String returns stay on the
    /// byte path (their serialized form *is* the value).
    fn isTypedCaptureReturn(self: *IRCompiler, t: *const ast.TypeExpr) bool {
        return switch (t.*) {
            .error_union, .optional, .sum, .type_merge, .struct_type, .integer, .float, .boolean => true,
            // A generic result (`type_var`) is captured by value — preserve
            // whatever it is. A real array (`[]T`, element not `Byte`) is captured
            // as the array itself; a String (`[]Byte`) stays on the byte path.
            .type_var => true,
            .array => |a| a.element.* != .byte,
            // A generic struct application (`Box(Int)`) is captured by value like
            // any struct, so it isn't byte-serialized (which would misread it).
            .type_application => |app| self.user_struct_types.contains(app.name.name),
            .identifier => |named| blk: {
                const name = named.path.segments[named.path.segments.len - 1].name;
                if (self.user_struct_types.contains(name)) break :blk true;
                break :blk std.mem.eql(u8, name, "Int") or std.mem.eql(u8, name, "Float") or std.mem.eql(u8, name, "Bool");
            },
            else => false,
        };
    }

    fn tryCompileTypedValueCapture(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
    ) Error!?Result {
        // A call through a module member (`m.fn args`, `std.list.map xs f`) whose
        // pub fn declares a typed-capturable return. The fn_ref and its return
        // type come from the member's `fn_ref_type` (no receiver is prepended —
        // unlike UFCS, a module member is not a method on its object).
        if (try self.moduleMemberFnRef(expr)) |mm| {
            if (self.isTypedCaptureReturn(mm.return_type)) {
                return try self.emitTypedValueCapture(source, expr, mm.fn_ref_value, mm.arguments, &.{}, mm.return_type);
            }
            return null;
        }

        const info = (try self.capturableCallInfo(expr)) orelse return null;
        if (info.redirects.len != 0) return null;

        // The callee must be a known function (a direct fn_ref binding — closure-
        // captured functions fall back) returning a typed-capturable value.
        const binding = self.lookup(info.method_name, .{ .shallow = false }) orelse return null;
        if (!binding.result.isFunctionRef()) return null;
        const fn_type = binding.type_expr orelse return null;
        if (fn_type != .function) return null;
        const return_type_ptr = fn_type.function.return_type orelse return null;
        if (!self.isTypedCaptureReturn(return_type_ptr)) return null;
        const fn_ref_value = binding.result.source.value;
        // A zero-arg reference to a function that declares parameters is a
        // function *value* (`const f = dbl`), not a call to capture.
        if (info.arguments.len == 0 and
            self.instruction_sets.items[fn_ref_value.fn_ref.fn_addr.instr_set].param_count > 0)
        {
            return null;
        }
        // pub exports change the call's result shape (a struct, already waited);
        // keep those on the existing path.
        if (self.instruction_sets.items[fn_ref_value.fn_ref.fn_addr.instr_set].pub_exports.len != 0) return null;

        return try self.emitTypedValueCapture(source, expr, fn_ref_value, info.arguments, info.redirects, return_type_ptr);
    }

    /// Emits an in-process typed value capture of a function call: fork with a
    /// `typed` stdout pipe, wait, then dequeue the single yielded value into %r
    /// typed as `return_type_ptr`. Shared by the local-binding and module-member
    /// capture paths.
    fn emitTypedValueCapture(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
        fn_ref_value: ir.Value,
        arguments: []const *ast.Expression,
        redirects: []const ast.Redirection,
        return_type_ptr: *const ast.TypeExpr,
    ) Error!Result {
        // Mirror the byte-capture path's stack contract: leave exactly
        // `capture_temp_ref_count` temporary refs on top and return the result
        // in %r, so every caller (pop-5 or binding) stays balanced.
        const stack_before = self.currentFrame().rel_stack_counter;

        const pipe_ref = try self.newRef(source, "typed_capture_pipe");
        try self.pipe(source, pipe_ref.dereference());
        try self.pipeOpt(source, pipe_ref.dereference(), .typed, .fromValue(.fromBoolean(true)));

        const call_result = try self.compileFunctionCall(
            expr,
            fn_ref_value,
            arguments,
            redirects,
            pipe_ref.dereference(),
        );

        // Wait for the function to finish so its yielded value is enqueued.
        const thread_ref = try self.newRef(source, "typed_capture_thread");
        try self.set(source, thread_ref, stableResultSource(call_result));
        try self.wait(source, thread_ref.dereference().typed(thread_type));

        // Pad so the net refs created here match the byte path's count; the
        // dequeue (below) runs before the caller pops them, and the result
        // value lives in %r, which `.ref`/`.pop` never touch.
        while (self.currentFrame().rel_stack_counter -| stack_before < capture_temp_ref_count) {
            _ = try self.newRef(source, "typed_capture_pad");
        }

        // Dequeue the single yielded value (the error union) into %r.
        try self.addInstruction(.init(.from(source), .{ .pipe_dequeue = pipe_ref.dereference() }));
        return try .from(ir.Location.initRegister(.r).typed(return_type_ptr.*));
    }

    const ModuleMemberFnRef = struct {
        fn_ref_value: ir.Value,
        return_type: *const ast.TypeExpr,
        arguments: []const *ast.Expression,
    };

    /// If `expr` is a call whose callee is a module member naming a pub fn (a
    /// `fn_ref_type` field with a known return type) — `m.fn args` or a nested
    /// `std.list.map xs f` — returns its fn_ref, return type, and the *raw*
    /// arguments (no receiver prepended). Returns null otherwise. Resolves the
    /// callee's type statically (via bindings/fields), emitting no instructions.
    fn moduleMemberFnRef(self: *IRCompiler, expr: *ast.Expression) Error!?ModuleMemberFnRef {
        const callee: *ast.Expression, const arguments: []const *ast.Expression = switch (expr.*) {
            .call => |call| blk: {
                if (call.background or call.redirects.len != 0) return null;
                break :blk .{ call.callee, call.arguments };
            },
            .binary => .{ expr, &.{} },
            else => return null,
        };
        if (callee.* != .binary or callee.binary.op != .member or callee.binary.right.* != .identifier) return null;

        const callee_type = self.resolveStaticType(callee) orelse return null;
        if (callee_type != .fn_ref_type) return null;
        const frt = callee_type.fn_ref_type;
        const return_type = frt.return_type orelse return null;

        // A zero-arg reference to a fn that declares parameters is a function
        // *value* (`const f = m.fn`), not a call — leave it to the value path.
        if (arguments.len == 0 and (frt.param_count orelse 0) > 0) return null;

        return .{
            .fn_ref_value = .{ .fn_ref = .{ .fn_addr = ir.InstructionAddr.initAbs(frt.instr_set, 0) } },
            .return_type = return_type,
            .arguments = arguments,
        };
    }

    /// Whether `object`'s static type is a struct (a module value or user
    /// struct) that declares a field named `name` — i.e. `object.name` is a real
    /// member, so a same-named builtin (string op, `push`) must not shadow it.
    fn memberIsStructField(self: *IRCompiler, object: *ast.Expression, name: []const u8) bool {
        return self.memberFieldType(object, name) != null;
    }

    /// The static type of `object.name` when `object` resolves to a struct that
    /// declares a field `name`; null otherwise. No instructions emitted.
    fn memberFieldType(self: *IRCompiler, object: *ast.Expression, name: []const u8) ?ast.TypeExpr {
        var t = self.resolveStaticType(object) orelse return null;
        while (t == .alias) t = t.alias.type_expr.*;
        if (t != .struct_type) return null;
        for (t.struct_type.fields) |field| {
            if (std.mem.eql(u8, field.name.name, name)) return field.type_expr.*;
        }
        return null;
    }

    /// Best-effort compile-time type of an expression, resolved purely from
    /// binding declarations and struct field layouts (no instructions emitted).
    /// Handles the shapes module-member resolution needs: a bound identifier and
    /// nested `object.member` field access. Returns null when the type is not
    /// statically known this way.
    fn resolveStaticType(self: *IRCompiler, expr: *ast.Expression) ?ast.TypeExpr {
        switch (expr.*) {
            .identifier => |id| {
                const binding = self.lookup(id.name, .{ .shallow = false }) orelse return null;
                return binding.type_expr;
            },
            // A bare identifier parses as a zero-arg call (`p` -> `p()`); resolve
            // to the referenced binding's type (e.g. the operand of `@TypeOf(p)`).
            .call => |call| {
                if (call.arguments.len != 0 or call.callee.* != .identifier) return null;
                const binding = self.lookup(call.callee.identifier.name, .{ .shallow = false }) orelse return null;
                return binding.type_expr;
            },
            .binary => |b| {
                if (b.op != .member or b.right.* != .identifier) return null;
                var object_type = self.resolveStaticType(b.left) orelse return null;
                while (object_type == .alias) object_type = object_type.alias.type_expr.*;
                if (object_type != .struct_type) return null;
                for (object_type.struct_type.fields) |field| {
                    if (std.mem.eql(u8, field.name.name, b.right.identifier.name)) return field.type_expr.*;
                }
                return null;
            },
            else => return null,
        }
    }

    fn compileExpressionWithCapture(
        self: *IRCompiler,
        source: anytype,
        expr: *ast.Expression,
    ) Error!Result {
        if (try self.tryCompileTypedValueCapture(source, expr)) |captured| return captured;

        const background_capture = isBackgroundExecutionExpression(expr);
        const expr_effects = self.analyzeExpressionEffects(expr);
        if (expr_effects.needs_stdio_capture or background_capture) {
            // A pipeline whose final stage yields an error union (e.g.
            // `… | parseInt`) is captured by *value*: mark its stdout pipe
            // `typed` so the final `yield` enqueues the structured value, then
            // dequeue it below (instead of flattening it to bytes). `catch`/`try`
            // then operate on the real error value.
            const typed_result_type = try self.pipelineCaptureErrorUnionType(expr);

            // TODO: We need something that cleans up the pipes because otherwise they will get stuck if they are not used
            // Suggestion: Maybe we can have a nested variable structure for inner threads that will set to true when the thread is done processing. We can then continuously check that variable from the outermost scope (here), and whenever it is set to true, we would set the pipes to be closed.
            const stdout_pipe_ref = try self.newRef(source, "stdout_pipe");
            try self.pipe(source, stdout_pipe_ref);
            if (typed_result_type != null) {
                try self.pipeOpt(source, stdout_pipe_ref.dereference(), .typed, .fromValue(.fromBoolean(true)));
            }
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
            if (!background_capture and result.isType(thread_type)) {
                try self.wait(source, result.source.location);
            } else if (!background_capture and result.isType(execution_handles_struct_type)) {
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
            if (!background_capture) {
                try self.pipeOpt(source, stdout_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(false)));
                try self.pipeOpt(source, stderr_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(false)));
                try self.wait(source, stdout_stream_thread_ref.dereference());
                try self.wait(source, stderr_stream_thread_ref.dereference());
            }
            // Typed value capture: the final stage enqueued its structured value
            // on the (typed) stdout pipe; dequeue it as the captured result.
            if (typed_result_type) |t| {
                try self.addInstruction(.init(.from(source), .{ .pipe_dequeue = stdout_pipe_ref.dereference() }));
                return .from(ir.Location.initRegister(.r).typed(t));
            }
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
            } else if (blk: {
                const rtype = result.typeExpr() orelse break :blk false;
                break :blk rtype == .struct_type;
            }) {
                // Merged struct from compileFunctionCall with pub exports — patch in the
                // captured pipe references for stdout/stderr/merged.
                try self.set(source, .initRegister(.r2), stableResultSource(result));
                try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.stdout), .{ .dereference = true }), .from(stdout_pipe_ref.dereference()));
                try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.stderr), .{ .dereference = true }), .from(stderr_pipe_ref.dereference()));
                try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.merged), .{ .dereference = true }), .from(merged_pipe_ref.dereference()));
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

    /// Compiles `MyError.Variant` into a payload-less error value. Payloaded
    /// construction (`MyError{ .Variant = payload }`) is Phase 3b.
    fn compileErrorVariant(
        self: *IRCompiler,
        source: *ast.Expression,
        set_name: []const u8,
        error_set: ast.TypeExpr.ErrorSet,
        variant_ident: ast.Identifier,
    ) Error!Result {
        const variant = error_set.variant(variant_ident.name) orelse {
            try self.reportSourceError(
                source,
                Error.NotImplemented,
                .@"error",
                "error set '{s}' has no variant '{s}'",
                .{ set_name, variant_ident.name },
            );
            return .fromValue(.void);
        };

        if (variant.payload != null) {
            try self.reportSourceError(
                source,
                Error.NotImplemented,
                .@"error",
                "error variant '{s}.{s}' requires a payload; use {s}{{ .{s} = <value> }} (not yet implemented)",
                .{ set_name, variant_ident.name, set_name, variant_ident.name },
            );
            return .fromValue(.void);
        }

        return .fromValue(.{ .err = .{ .set = set_name, .variant = variant_ident.name } });
    }

    /// Compiles `MyError{ .Variant = payload }` into an error value carrying a
    /// boxed payload. Phase 3b currently supports constant payloads (the
    /// compiled value must be a `.value` source).
    fn compileStructLiteral(
        self: *IRCompiler,
        source: *ast.Expression,
        struct_literal: ast.StructLiteral,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        // User-defined struct construction: allocate the struct's slots and
        // populate each field at its layout offset (like the execution-result
        // struct build). Field values are compiled into refs first so they
        // don't clobber the alloc base register.
        if (self.user_struct_types.get(struct_literal.name.name)) |struct_type| {
            return self.compileStructValueLiteral(source, struct_type, struct_literal);
        }

        const error_set = self.error_sets.get(struct_literal.name.name) orelse {
            try self.reportSourceError(
                source,
                Error.NotImplemented,
                .@"error",
                "'{s}' is not a struct or error set; cannot construct it with {{ … }}",
                .{struct_literal.name.name},
            );
            return .fromValue(.void);
        };

        if (struct_literal.fields.len != 1) {
            try self.reportSourceError(source, Error.NotImplemented, .@"error", "error value construction requires exactly one variant field", .{});
            return .fromValue(.void);
        }

        const field = struct_literal.fields[0];
        const variant = error_set.variant(field.name.name) orelse {
            try self.reportSourceError(source, Error.NotImplemented, .@"error", "error set '{s}' has no variant '{s}'", .{ struct_literal.name.name, field.name.name });
            return .fromValue(.void);
        };

        if (variant.payload == null) {
            try self.reportSourceError(source, Error.NotImplemented, .@"error", "error variant '{s}.{s}' has no payload; use {s}.{s}", .{ struct_literal.name.name, field.name.name, struct_literal.name.name, field.name.name });
            return .fromValue(.void);
        }

        // Construct the error value at runtime so the payload may be any value
        // (constant or runtime), boxed by the `make_err` instruction.
        const payload_result = try self.compileExpression(field.value);
        const result_ref = try self.newRef(source, "error_value");
        try self.addInstruction(.init(.from(source), .{ .make_err = .{
            .set = struct_literal.name.name,
            .variant = field.name.name,
            .payload = payload_result.source,
            .result = result_ref,
        } }));
        return .from(result_ref.dereference());
    }

    /// Builds a user struct value: compile each field's value into a ref (so the
    /// alloc base register isn't clobbered), allocate the struct's slots, then
    /// write each field at its layout offset. Returns the base address typed as
    /// the struct, so field access (`p.x`) reads `[base + offset]`.
    fn compileStructValueLiteral(
        self: *IRCompiler,
        source: *ast.Expression,
        struct_type: ast.TypeExpr.StructType,
        struct_literal: ast.StructLiteral,
    ) Error!Result {
        const FieldSlot = struct { offset: usize, ref: ir.Location };
        var slots = std.ArrayList(FieldSlot).empty;
        defer slots.deinit(self.allocator);

        // Rebuild the struct type with each field typed by its supplied value
        // (rather than the declared type, which may be a generic parameter `T`),
        // so a `|T|` capture through this literal binds to the concrete type.
        const concrete_fields = try self.allocator.alloc(ast.TypeExpr.StructField, struct_type.fields.len);

        var total: usize = 0;
        for (struct_type.fields, 0..) |field, i| {
            const layout = struct_type.fieldLayout(field.name.name) catch return .fromValue(.void);
            total += field.type_expr.slotSize() catch 1;

            concrete_fields[i] = field;

            // The value supplied for this field (the type checker already
            // verified presence/typing).
            const value: Result = blk: {
                for (struct_literal.fields) |lit_field| {
                    if (std.mem.eql(u8, lit_field.name.name, field.name.name)) {
                        if (self.argTypeExpr(lit_field.value)) |vt| {
                            const ft = try self.allocator.create(ast.TypeExpr);
                            ft.* = vt;
                            concrete_fields[i].type_expr = ft;
                        }
                        break :blk try self.compileExpression(lit_field.value);
                    }
                }
                break :blk .fromValue(.void);
            };
            const field_ref = try self.newRef(source, "struct_field");
            try self.set(source, field_ref, value.source);
            try slots.append(self.allocator, .{ .offset = layout.offset, .ref = field_ref.dereference() });
        }

        try self.alloc(source, total); // %r = base of the struct's slots
        for (slots.items) |slot| {
            try self.set(
                source,
                .initAdd(.{ .register = .r }, slot.offset, .{ .dereference = true }),
                .from(slot.ref),
            );
        }
        var concrete_type = struct_type;
        concrete_type.fields = concrete_fields;
        return .fromLocation(ir.Location.initRegister(.r).typed(.{ .struct_type = concrete_type }));
    }

    /// Converts a finished command (`ExecutionResult`) into an
    /// `ExecutableError!String` value: exit 0 → the captured output (ok), any
    /// other exit → `ExecutableError.NonZeroExit(code)`.
    fn compileExecutionToErrorUnion(
        self: *IRCompiler,
        source: anytype,
        exec: Result,
        result_type: ?ast.TypeExpr,
    ) Error!Result {
        const exec_ref = try self.newRef(source, "exec_value");
        try self.set(source, exec_ref, stableResultSource(exec));

        const exit_code_ref = try self.newRef(source, "exec_exit_code");
        try self.addInstruction(.init(.from(source), .{ .resolve_exit_code = .{
            .source = exec_ref.dereference().typed(execution_result_struct_type),
            .result = exit_code_ref,
        } }));

        const result_ref = try self.newRef(source, "exec_error_union");
        const err_addr = try self.newLabel("exec_error", .unknown);
        const after_addr = try self.newLabel("exec_after", .unknown);

        // Non-zero exit (falsy exit code) → error branch.
        try self.jmp(source, try .from(exit_code_ref.dereference()), false, err_addr);

        // Success: the ok value is the captured (merged) output.
        try self.set(source, .initRegister(.r2), .from(exec_ref.dereference()));
        try self.set(source, result_ref, .fromLocation(.initAdd(
            .{ .register = .r2 },
            executionResultFieldOffset(.merged),
            .{ .dereference = true },
        )));
        try self.jmp(source, null, false, after_addr);

        // Failure: ExecutableError.NonZeroExit(code).
        try self.setLabel(err_addr.local_addr.label, .abs);
        try self.addInstruction(.init(.from(source), .{ .make_err = .{
            .set = "ExecutableError",
            .variant = "NonZeroExit",
            .payload = .from(exit_code_ref.dereference()),
            .result = result_ref,
        } }));

        try self.setLabel(after_addr.local_addr.label, .abs);
        return .from(result_ref.dereference().typed(result_type));
    }

    /// `expr catch <default>` / `expr catch |err| <handler>`. Evaluates the
    /// subject; if it is an error value, runs the handler (with `|err|` bound to
    /// the error), otherwise uses the subject's ok value.
    fn compileCatch(
        self: *IRCompiler,
        source: *ast.Expression,
        catch_expr: ast.CatchExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const subject_ref = try self.newRef(source, "catch_subject");
        var subject = try self.compileStableExpressionIntoRef(source, catch_expr.subject, subject_ref);
        // A command subject is treated as `ExecutableError!String`.
        if (subject.isType(execution_result_struct_type)) {
            const error_union = try self.compileExecutionToErrorUnion(source, subject, null);
            try self.set(source, subject_ref, stableResultSource(error_union));
            subject = try .from(subject_ref.dereference().typed(error_union.typeExpr()));
        }

        const is_err_ref = try self.newRef(source, "catch_is_err");
        try self.addInstruction(.init(.from(source), .{ .is_err = .{
            .operand = subject_ref.dereference(),
            .result = is_err_ref,
        } }));

        const result = try self.newRef(source, "catch_result");
        const handler_addr = try self.newLabel("catch_handler", .unknown);
        const after_addr = try self.newLabel("catch_after", .unknown);

        // Jump to the handler when the subject is an error.
        try self.jmp(source, try .from(is_err_ref.dereference()), true, handler_addr);

        // Ok path: the result is the subject's ok value.
        try self.set(source, result, .from(subject_ref.dereference()));
        try self.jmp(source, null, false, after_addr);

        // Error path: run the handler (with the optional `|err|` capture).
        try self.setLabel(handler_addr.local_addr.label, .abs);
        const handler = try self.compileCatchHandler(source, catch_expr, subject_ref);
        // Drain a forked handler (e.g. `catch |err| echo "…"`) here: the result
        // is typed as the payload, so the statement-level wait would miss it and
        // the handler's capture fork could race program exit.
        if (isWaitable(handler)) |loc| try self.wait(source, loc);
        try self.set(source, result, stableResultSource(handler));

        try self.setLabel(after_addr.local_addr.label, .abs);

        // Result type: the error union's payload when known, else the handler's
        // type (a pure error value always yields the handler).
        const result_type: ?ast.TypeExpr = blk: {
            if (subject.typeExpr()) |subject_type| {
                if (subject_type == .error_union) break :blk subject_type.error_union.payload.*;
            }
            break :blk handler.typeExpr();
        };
        return .from(result.dereference().typed(result_type));
    }

    /// `try expr` — if the subject is an error, propagate it out of the
    /// enclosing function (`exit_with`); otherwise evaluate to the ok value.
    fn compileTry(
        self: *IRCompiler,
        source: *ast.Expression,
        try_expr: ast.TryExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const subject_ref = try self.newRef(source, "try_subject");
        var subject = try self.compileStableExpressionIntoRef(source, try_expr.subject, subject_ref);
        // A command subject is treated as `ExecutableError!String`.
        if (subject.isType(execution_result_struct_type)) {
            const error_union = try self.compileExecutionToErrorUnion(source, subject, null);
            try self.set(source, subject_ref, stableResultSource(error_union));
            subject = try .from(subject_ref.dereference().typed(error_union.typeExpr()));
        }

        const is_err_ref = try self.newRef(source, "try_is_err");
        try self.addInstruction(.init(.from(source), .{ .is_err = .{
            .operand = subject_ref.dereference(),
            .result = is_err_ref,
        } }));

        const after_addr = try self.newLabel("try_after", .unknown);
        // Skip propagation when the subject is not an error.
        try self.jmp(source, try .from(is_err_ref.dereference()), false, after_addr);
        // Error: propagate it as the enclosing function's result by yielding the
        // error value to stdout — the same channel a `yield <error>` uses — so a
        // capturing caller (`catch`/`try`/`match`) observes the real error and it
        // chains through nested propagation and pipeline stages. Then halt the
        // function so the statements after the `try` do not run.
        try self.pipeWrite(source, self.threadStdout(), stableResultSource(subject));
        try self.exitWith(source, try .from(subject_ref.dereference()));
        try self.setLabel(after_addr.local_addr.label, .abs);

        const payload_type: ?ast.TypeExpr = switch (subject.typeExpr() orelse @as(ast.TypeExpr, .global(.void))) {
            .error_union => |error_union| error_union.payload.*,
            else => null,
        };
        return .from(subject_ref.dereference().typed(payload_type));
    }

    /// `x is T` — compile the subject, then test its runtime value's tag against
    /// `T`. Evaluates to `Bool`.
    fn compileIs(
        self: *IRCompiler,
        source: *ast.Expression,
        is_expr: ast.IsExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const tag = typeTagOf(is_expr.type_expr) orelse {
            try self.reportSourceError(source, Error.UnsupportedExpression, .@"error", "`is` is not supported for type \"{f}\" yet", .{is_expr.type_expr});
            return .fromValue(.fromBoolean(false));
        };

        const subject_ref = try self.newRef(source, "is_subject");
        _ = try self.compileStableExpressionIntoRef(source, is_expr.subject, subject_ref);

        const result_ref = try self.newRef(source, "is_result");
        try self.addInstruction(.init(.from(source), .{ .is_type = .{
            .operand = subject_ref.dereference(),
            .tag = tag,
            .result = result_ref,
        } }));

        return .from(result_ref.dereference().typed(.{ .boolean = .{ .span = source.span() } }));
    }

    /// Maps a type expression to its runtime `TypeTag`, or null when the type
    /// isn't yet testable by `is`. The `is T` operand is unresolved AST, so the
    /// builtin primitives arrive as identifiers (`Int`, `String`, …); resolved
    /// primitive forms are also accepted.
    fn typeTagOf(type_expr: *const ast.TypeExpr) ?ir.Instruction.TypeTag {
        return switch (type_expr.*) {
            .identifier => |named| typeTagForName(named.path.segments[named.path.segments.len - 1].name),
            .integer => .int,
            .float => .float,
            .boolean => .boolean,
            // `String` is `[]Byte`.
            .array => |array| if (array.element.* == .byte) .string else null,
            .alias => |alias| typeTagOf(alias.type_expr),
            else => null,
        };
    }

    /// The runtime `TypeTag` for a builtin primitive type name, or null.
    fn typeTagForName(name: []const u8) ?ir.Instruction.TypeTag {
        if (std.mem.eql(u8, name, "Int")) return .int;
        if (std.mem.eql(u8, name, "Float")) return .float;
        if (std.mem.eql(u8, name, "Bool")) return .boolean;
        if (std.mem.eql(u8, name, "String")) return .string;
        return null;
    }

    fn compileCatchHandler(
        self: *IRCompiler,
        source: *ast.Expression,
        catch_expr: ast.CatchExpr,
        subject_ref: ir.Location,
    ) Error!Result {
        const stack_base = self.currentFrame().rel_stack_counter;

        try self.scopes.push(self.allocator, .lexical);
        defer self.scopes.pop();

        if (catch_expr.capture) |capture| {
            if (capture.bindings.len == 1) {
                switch (capture.bindings[0].*) {
                    .discard => {},
                    .identifier => |identifier| try self.compileIdentifierBinding(
                        source,
                        identifier,
                        .from(subject_ref.dereference()),
                        null,
                        false,
                        .normal,
                    ),
                    else => {
                        try self.reportSourceError(
                            source,
                            Error.UnsupportedBindingPattern,
                            .@"error",
                            "catch capture binding pattern not yet supported",
                            .{},
                        );
                        return .fromValue(.void);
                    },
                }
            }
        }

        const result = try self.compileExpression(catch_expr.handler);
        // Balance the capture-binding slot (result carried in r2) so the runtime
        // stack matches the counter — see the same discipline in compileMatchCaseBody.
        try self.set(source, .initRegister(.r2), stableResultSource(result));
        while (self.currentFrame().rel_stack_counter > stack_base) {
            _ = try self.pop(source);
        }
        return .fromLocation(ir.Location.initRegister(.r2).typed(result.typeExpr()));
    }

    /// How a struct field access should be compiled: as a read (the field's
    /// value is materialized into a stable ref, so it survives a later field
    /// access clobbering the base register) or as an assignment target (the raw
    /// `[base + offset]` slot, written to immediately by the caller).
    const MemberMode = enum { read, lvalue };

    fn compileMember(
        self: *IRCompiler,
        source: *ast.Expression,
        member: ast.MemberExpr,
        mode: MemberMode,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (std.mem.eql(u8, member.member.name, "?")) {
            return self.compileOptionalUnwrap(source, member.object);
        }

        if (std.mem.eql(u8, member.member.name, "wait")) {
            return self.compileWaitMember(source, member.object);
        }

        // `MyError.Variant` — construct a (payload-less) error value when the
        // object is an identifier naming a known error set.
        if (member.object.* == .identifier) {
            if (self.error_sets.get(member.object.identifier.name)) |error_set| {
                return self.compileErrorVariant(
                    source,
                    member.object.identifier.name,
                    error_set,
                    member.member,
                );
            }
        }

        const object = try self.compileExpression(member.object);

        // String builtins with no arguments (`s.len`, `s.upper`, `s.trim`, …).
        // `len` also names an array's length, so it only applies to a string
        // receiver; the transform/predicate names apply to any (materialized)
        // value. A string may be untyped (a bare literal), so a missing type is
        // treated as a string here.
        if (stringBuiltin(member.member.name)) |sb| {
            if (sb.arity == 0) {
                const obj_is_string = if (object.typeExpr()) |t| self.typeIsString(t) else true;
                if (!sb.string_only_receiver or obj_is_string) {
                    return self.compileStrOp(source, object, sb, &.{});
                }
            }
        }

        // No-argument Float builtins (`x.sqrt`, `x.floor`, …). Skipped when the
        // receiver is a struct declaring that member (so a module member wins).
        if (floatBuiltin(member.member.name)) |fb| {
            if (fb.arity == 0 and !self.memberIsStructField(member.object, member.member.name)) {
                return self.compileFloatOp(source, object, fb, &.{});
            }
        }

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

                // The length lives at slot 0 of the heap array, addressed through
                // %r2 — a volatile register. Copy it into a fresh stable ref so a
                // later access (or arithmetic on the bound length, `xs.len - 1`)
                // doesn't read through a clobbered %r2. Mirrors the struct-field
                // read below.
                try self.set(source, .initRegister(.r2), object.source);
                const len_ref = try self.newRef(source, "array_len");
                try self.set(source, len_ref, .fromLocation(.initAbs(
                    .{ .register = .r2 },
                    .{ .dereference = true, .type_expr = .global(.integer) },
                )));
                return .fromLocation(len_ref.dereference().typed(.global(.integer)));
            },
            .struct_type => |struct_type| struct_type,
            // A binding annotated with a user struct's name carries the type as
            // an identifier; resolve it to the struct's layout.
            .identifier => |named| self.user_struct_types.get(named.path.segments[named.path.segments.len - 1].name) orelse {
                try self.reportSourceError(
                    source,
                    Error.NotImplemented,
                    .@"error",
                    "member access is only supported for struct types in IR",
                    .{},
                );
                return .fromValue(.void);
            },
            // A captured generic struct return (`const e = wrap "x" 5` where
            // `wrap` yields `Entry(K, V)`) carries its type as an application;
            // resolve the constructor to its (param-substituted) layout.
            .type_application => |app| blk: {
                const resolved = self.resolveTypeApplication(app) orelse {
                    try self.reportSourceError(
                        source,
                        Error.NotImplemented,
                        .@"error",
                        "member access is only supported for struct types in IR",
                        .{},
                    );
                    return .fromValue(.void);
                };
                if (resolved != .struct_type) {
                    try self.reportSourceError(
                        source,
                        Error.NotImplemented,
                        .@"error",
                        "member access is only supported for struct types in IR",
                        .{},
                    );
                    return .fromValue(.void);
                }
                break :blk resolved.struct_type;
            },
            else => {
                try self.reportSourceError(
                    source,
                    Error.NotImplemented,
                    .@"error",
                    "member access is only supported for struct types in IR",
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
                // Not a field — try UFCS: `recv.method` ≡ `method(recv)` when a
                // function named `method` is in scope (field access wins, which
                // is why this is only reached after fieldLayout fails).
                if (try self.tryUfcsRewrite(source, member.object, member.member.name, &.{}, &.{})) |r| return r;
                try self.reportSourceError(
                    source,
                    Error.NotImplemented,
                    .@"error",
                    "member \"{s}\" not found on struct (no field or method)",
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
        // For fn_ref_type fields, the fn_ref is a compile-time constant; return it as a value
        // directly so call compilation can emit a direct fork without runtime indirection.
        if (field_.type_expr == .fn_ref_type) {
            const frt = field_.type_expr.fn_ref_type;
            const fn_ref_value = ir.Value{ .fn_ref = .{
                .fn_addr = ir.InstructionAddr.initAbs(frt.instr_set, 0),
            } };
            // A *nullary* module member (`m.cwd`, `m.greet`) is a call, like a bare
            // nullary identifier — auto-call it in a value context so `m.f` and
            // `${m.f}` yield its result, not the fn_ref. (A member with parameters
            // is a function value, left for a later call with arguments.) The
            // instruction set carries the authoritative parameter count.
            if (mode == .read and self.instruction_sets.items[frt.instr_set].param_count == 0) {
                return try self.compileFunctionCall(source, fn_ref_value, &.{}, &.{}, null);
            }
            return .from(fn_ref_value);
        }

        const object_ref = try self.newRef(source, "member_object_ref");
        try self.set(source, object_ref, object.source);
        try self.set(source, .initRegister(.r2), .from(object_ref.dereference()));

        // The field slot addresses through %r2, a volatile register: a second
        // field access (e.g. `p.x + q.x`) would reset %r2 and make both operands
        // read the same struct. So for a read, copy the field's value into a
        // fresh stable ref and return that. For an assignment target the caller
        // writes immediately, before anything clobbers %r2, so the raw slot is
        // fine (and must be returned so the write lands in the struct).
        const slot = ir.Location.initAdd(
            .{ .register = .r2 },
            field_.offset,
            .{ .dereference = true, .type_expr = field_.type_expr },
        );
        switch (mode) {
            .lvalue => return .fromLocation(slot),
            .read => {
                const field_ref = try self.newRef(source, "member_field");
                try self.set(source, field_ref, .fromLocation(slot));
                return .fromLocation(field_ref.dereference().typed(field_.type_expr));
            },
        }
    }

    fn compileWaitMember(
        self: *IRCompiler,
        source: *ast.Expression,
        object_expr: *ast.Expression,
    ) Error!Result {
        const object_ref = try self.newRef(source, "wait_member_object");
        const object = try self.compileStableExpressionIntoRef(source, object_expr, object_ref);

        if (object.isType(thread_type)) {
            try self.wait(source, object_ref.dereference().typed(thread_type));
            return .from(object_ref.dereference().typed(thread_type));
        }

        if (object.isType(execution_result_struct_type)) {
            try self.set(source, .initRegister(.r2), .from(object_ref.dereference()));

            const completion_is_thread = try self.newRef(source, "wait_member_completion_is_thread");
            try self.set(
                source,
                completion_is_thread,
                .fromLocation(.initAdd(
                    .{ .register = .r2 },
                    executionResultFieldOffset(.completion_is_thread),
                    .{ .dereference = true, .type_expr = .global(.boolean) },
                )),
            );

            const wait_closeable = try self.newLabel("wait_member_closeable", .unknown);
            const wait_done = try self.newLabel("wait_member_done", .unknown);
            try self.jmp(source, try .from(completion_is_thread.dereference()), true, wait_closeable);
            try self.wait(
                source,
                .initAdd(
                    .{ .register = .r2 },
                    executionResultFieldOffset(.closeable),
                    .{ .dereference = true },
                ),
            );
            try self.jmp(source, null, false, wait_done);
            try self.setLabel(wait_closeable.local_addr.label, .abs);
            try self.wait(
                source,
                .initAdd(
                    .{ .register = .r2 },
                    executionResultFieldOffset(.closeable),
                    .{ .dereference = true, .type_expr = thread_type },
                ),
            );
            try self.setLabel(wait_done.local_addr.label, .abs);
            try self.pipeOpt(
                source,
                .initAdd(
                    .{ .register = .r2 },
                    executionResultFieldOffset(.stdout),
                    .{ .dereference = true },
                ),
                .keep_open,
                .fromValue(.fromBoolean(false)),
            );
            try self.pipeOpt(
                source,
                .initAdd(
                    .{ .register = .r2 },
                    executionResultFieldOffset(.stderr),
                    .{ .dereference = true },
                ),
                .keep_open,
                .fromValue(.fromBoolean(false)),
            );
            try self.pipeOpt(
                source,
                .initAdd(
                    .{ .register = .r2 },
                    executionResultFieldOffset(.merged),
                    .{ .dereference = true },
                ),
                .keep_open,
                .fromValue(.fromBoolean(false)),
            );
            return .from(object_ref.dereference().typed(execution_result_struct_type));
        }

        if (!object.isType(thread_type)) {
            try self.reportSourceError(
                source,
                Error.UnsupportedExpression,
                .@"error",
                "wait member requires an execution or thread-backed background handle",
                .{},
            );
            return .fromValue(.void);
        }
        unreachable;
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
        mode: MemberMode,
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
        }, mode);
    }

    fn compileStringLiteral(
        self: *IRCompiler,
        source: anytype,
        string_literal: ast.StringLiteral,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        // Empty literal (`""`) — no segments; still a String, not a 0-length
        // struct (the multi-segment path below would mistype it).
        if (string_literal.segments.len == 0) {
            return .from(try self.addSlice(1, ""));
        }

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
            .fromValue(.{ .integer = @as(i64, @intCast(string_literal.segments.len)) }),
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
        return .{ .integer = try std.fmt.parseInt(i64, text, 10) };
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

    /// Copies the value currently in `%r` into a fresh ref slot and returns a
    /// dereferenced location for it. Value-producing builtins (`&0`,
    /// `parseInt`) leave their result directly in `%r`, but the rest of the
    /// compiler expects value expressions to yield a `ref.dereference()`
    /// location (so callers like arithmetic operands can `.dereference()` it).
    fn stabilizeRegisterResult(
        self: *IRCompiler,
        source: anytype,
        comptime name: []const u8,
        type_expr: ?ast.TypeExpr,
    ) Error!Result {
        const ref = try self.newRef(source, name);
        try self.set(source, ref, .fromLocation(.initRegister(.r)));
        return .fromLocation(ref.dereference().typed(type_expr));
    }

    /// Compiles a parsing pipeline builtin (`parseInt`/`parseFloat`) as a stage
    /// that *maps* over its stdin stream: it reads each value with
    /// `collect_stdin`, stops at EOF (`.null`), parses it in place with
    /// `parse_instr`, and yields the typed result — so it composes with both a
    /// single value and a framed multi-value stream (e.g. `lines | parseInt`).
    fn compileParseMapStage(
        self: *IRCompiler,
        source: anytype,
        comptime name: []const u8,
        parse_instr: ir.Instruction.Type,
        result_type: ast.TypeExpr,
    ) Error!void {
        const value_ref = try self.newRef(source, name ++ "_in");
        const is_eof_ref = try self.newRef(source, name ++ "_eof");
        const after_label = try self.newLabel(name ++ "_after", .unknown);
        const loop_label = try self.newLabel(name ++ "_loop", .abs);

        try self.addInstruction(.init(.from(source), .collect_stdin));
        try self.set(source, value_ref, .fromLocation(.initRegister(.r)));
        try self.cmp(source, .equal, .from(value_ref.dereference()), .fromValue(.null), is_eof_ref);
        try self.jmp(source, try .from(is_eof_ref.dereference()), true, after_label);

        // %r still holds the collected value; parse it in place and yield.
        try self.addInstruction(.init(.from(source), parse_instr));
        try self.pipeWrite(source, self.threadStdout(), .fromLocation(ir.Location.initRegister(.r).typed(result_type)));
        try self.jmp(source, null, true, loop_label);
        try self.setLabel(after_label.local_addr.label, .abs);
    }

    fn compileIdentifier(
        self: *IRCompiler,
        source: anytype,
        identifier: ast.Identifier,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (std.mem.eql(u8, identifier.name, "@src")) {
            const path = self.script.span.start.file;
            const value = try self.addSlice(1, path);
            return .fromValue(value);
        }

        // parseInt builtin: a pipeline stage with type `fn String parseInt() Int`.
        // It maps each input value on stdin to an `Int` and yields it, so it
        // composes with a framed multi-value stream (e.g. `lines | parseInt`)
        // as well as a single value. Output is explicit (stage values are not
        // auto-pushed). A user-defined parseInt in scope takes precedence.
        if (std.mem.eql(u8, identifier.name, "parseInt") and
            self.lookup(identifier.name, .{ .shallow = false }) == null)
        {
            try self.compileParseMapStage(source, "parse_int", .parse_int, ast.TypeExpr.global(.integer));
            return .fromValue(.void);
        }

        // parseFloat builtin (`fn String parseFloat() Float`): like parseInt but
        // produces a Float per input value.
        if (std.mem.eql(u8, identifier.name, "parseFloat") and
            self.lookup(identifier.name, .{ .shallow = false }) == null)
        {
            try self.compileParseMapStage(source, "parse_float", .parse_float, ast.TypeExpr.global(.float));
            return .fromValue(.void);
        }

        // lines builtin (`fn String lines() String`): reads the whole byte
        // stdin and frames it into per-line values — each non-empty line is
        // enqueued as a separate value onto the (typed) stdout pipe, so a
        // downstream `for (&0)` / mapping stage processes one line at a time.
        if (std.mem.eql(u8, identifier.name, "lines") and
            self.lookup(identifier.name, .{ .shallow = false }) == null)
        {
            try self.addInstruction(.init(.from(source), .collect_stdin));
            try self.addInstruction(.init(.from(source), .{ .emit_lines = self.threadStdout() }));
            return .fromValue(.void);
        }

        const source_binding = self.lookup(identifier.name, .{ .shallow = false }) orelse {
            // A type identifier used where a value is expected serializes to the
            // type's name (`echo "${Int}"` → "Int", a bound `|T|` → its type).
            if (try self.typeIdentifierString(identifier.name)) |type_name| {
                return .fromValue(type_name);
            }
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
            // A type application *with arguments* in value position
            // (`${Box(Int)}`) serializes to its type-application string. A bare
            // type name (zero args) is left to `compileIdentifier`, which
            // resolves a captured type to its bound type rather than its name.
            if (call.arguments.len > 0 and self.isTypeName(name) and
                self.lookup(name, .{ .shallow = false }) == null)
            {
                if (try self.compileTypeApplicationString(call)) |type_string| {
                    return .fromValue(type_string);
                }
            }
            if (std.mem.eql(u8, name, "@src")) {
                return self.compileIdentifier(source, call.callee.identifier);
            }
            if (std.mem.eql(u8, name, "cd") and self.lookup(name, .{ .shallow = false }) == null) {
                return self.compileBuiltinCd(source, call.arguments);
            }
            // `run "path" args…` — execute an executable at a runtime-computed
            // path (a variable, interpolation, or a spaced path a bare command
            // token can't express). The first argument is the executable; the
            // rest are its arguments.
            if (std.mem.eql(u8, name, "run") and
                call.arguments.len >= 1 and
                self.lookup(name, .{ .shallow = false }) == null)
            {
                return self.compileExecutableCall(source, .void, call.arguments[0], call.arguments[1..], call.redirects);
            }
            // `setenv name value` — set a dynamically-named environment variable
            // (backs std.env.set). Shadowable by a local binding.
            if (std.mem.eql(u8, name, "setenv") and
                call.arguments.len == 2 and
                self.lookup(name, .{ .shallow = false }) == null)
            {
                return self.compileBuiltinSetEnv(source, call.arguments);
            }
        }

        // String builtins with arguments (`s.contains "x"`, `s.slice 1 3`, …) —
        // a `.member` callee naming a string builtin with a matching arity. Wins
        // over UFCS for these reserved names — *unless* the receiver is a struct
        // (a module value or user struct) that actually declares a member of that
        // name, e.g. `std.path.join` must call the module's `join`, not the
        // built-in `[]String.join`.
        if (call.callee.* == .binary and call.callee.binary.op == .member and call.callee.binary.right.* == .identifier and call.redirects.len == 0) {
            const member_name = call.callee.binary.right.identifier.name;
            if (!self.memberIsStructField(call.callee.binary.left, member_name)) {
                if (stringBuiltin(member_name)) |sb| {
                    if (sb.arity == @as(u8, @intCast(call.arguments.len)) and sb.arity > 0) {
                        const object = try self.compileExpression(call.callee.binary.left);
                        return self.compileStrOp(source, object, sb, call.arguments);
                    }
                }
                if (floatBuiltin(member_name)) |fb| {
                    if (fb.arity == @as(u8, @intCast(call.arguments.len)) and fb.arity > 0) {
                        const object = try self.compileExpression(call.callee.binary.left);
                        return self.compileFloatOp(source, object, fb, call.arguments);
                    }
                }
            }
            // `arr.push value` — append to an array, yielding a new array.
            if (std.mem.eql(u8, call.callee.binary.right.identifier.name, "push") and call.arguments.len == 1) {
                const object = try self.compileExpression(call.callee.binary.left);
                const array_type_expr = object.typeExpr() orelse array_type(&push_fallback_element);
                const array_ref = try self.newRef(source, "push_array");
                try self.set(source, array_ref, stableResultSource(object));
                const value = try self.compileExpression(call.arguments[0]);
                const value_ref = try self.newRef(source, "push_value");
                try self.set(source, value_ref, stableResultSource(value));
                const result_ref = try self.newRef(source, "push_result");
                try self.addInstruction(.init(.from(source), .{ .array_push = .{
                    .array = .from(array_ref.dereference()),
                    .value = .from(value_ref.dereference()),
                    .result = result_ref.dereference(),
                } }));
                // Refine an unknown element type (`var xs = .{ }` starts as
                // `[]Void`) from the pushed value, so a later `xs[i].field` /
                // `xs[i][j]` can resolve the element's layout.
                var result_array_type = array_type_expr;
                if (array_type_expr == .array and array_type_expr.array.element.* == .void) {
                    if (value.typeExpr() orelse self.argTypeExpr(call.arguments[0])) |vt| {
                        const element = try self.allocator.create(ast.TypeExpr);
                        element.* = vt;
                        result_array_type = array_type(element);
                    }
                }
                return .fromLocation(result_ref.dereference().typed(result_array_type));
            }
        }

        // UFCS method call: `recv.method args…` ≡ `method(recv, args…)` when
        // `method` is a function in scope. (A field wouldn't be callable with
        // args, so the method wins in call position.)
        if (call.callee.* == .binary and call.callee.binary.op == .member and call.callee.binary.right.* == .identifier) {
            if (try self.tryUfcsRewrite(source, call.callee.binary.left, call.callee.binary.right.identifier.name, call.arguments, call.redirects)) |r| return r;
        }

        const callee = try self.compileExpression(call.callee);

        // Indirect call: a function-valued location (a `fn(...)`-typed parameter)
        // called with arguments. The target function is only known at runtime.
        if (callee.source == .location and call.arguments.len > 0 and call.redirects.len == 0) {
            if (callee.typeExpr()) |t| {
                if (t == .function) {
                    const rt: ?ast.TypeExpr = if (t.function.return_type) |r| r.* else null;
                    return self.compileIndirectCall(source, callee.source.location, rt, call.arguments);
                }
            }
        }

        return switch (callee.source) {
            .value => |v| switch (v) {
                .executable => self.compileExecutableCall(source, v, null, call.arguments, call.redirects),
                // A zero-arg reference to a function that declares parameters is a
                // function *value* (`const f = dbl`), not a call — yield the fn_ref
                // so it can be called later (`f x`). A nullary function is called.
                .fn_ref => if (call.arguments.len == 0 and call.redirects.len == 0 and
                    self.instruction_sets.items[v.fn_ref.fn_addr.instr_set].param_count > 0)
                    .from(v)
                else
                    self.compileFunctionCall(source, v, call.arguments, call.redirects, null),
                .slice, .stream, .addr, .void, .null, .integer, .float, .strct, .exit_code, .pipe, .thread, .closeable, .err => .from(v),
                .zig_string => Error.UnsupportedValueType,
            },
            .location => |loc| .from(loc),
        };
    }

    const RedirectStreams = struct {
        stdout: ir.Location,
        stderr: ir.Location,
        // When a `path` redirect points a stream at a file, the redirect pipe
        // needs a drain thread to move its data pipe→file. These record the
        // pipe locations so the caller can spawn that drain (see
        // `compileFileRedirectDrains`). Null when the stream isn't file-redirected.
        stdout_file_pipe: ?ir.Location = null,
        stderr_file_pipe: ?ir.Location = null,
    };

    /// Compiles a list of redirects into the stdout/stderr stream locations a
    /// command should run with (defaulting to the thread's own streams). A
    /// `path` redirect opens a pipe-to-file; an `fd` redirect points the stream
    /// at another descriptor. Shared by block and function call redirection.
    ///
    /// A file-redirect pipe is created with `keep_open=true` so its drain won't
    /// close the file before the writer connects and finishes. The caller must
    /// spawn a drain and clear that flag via `compileFileRedirectDrains`.
    fn compileRedirectStreams(
        self: *IRCompiler,
        source: anytype,
        redirects: []const ast.Redirection,
    ) Error!RedirectStreams {
        var streams = RedirectStreams{ .stdout = self.threadStdout(), .stderr = self.threadStderr() };
        for (redirects) |redirect| {
            switch (redirect.target) {
                .path => |path_target| {
                    const redirect_target = try self.compileExpression(path_target.value);
                    const redirect_pipe_ref = try self.newRef(source, "stdout_redirect_pipe");
                    try self.pipe(source, redirect_pipe_ref);
                    try self.pipeOpt(source, redirect_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(true)));
                    try self.pipeFile(
                        source,
                        redirect_pipe_ref.dereference(),
                        stableResultSource(redirect_target),
                        redirect.mode,
                    );
                    switch (redirect.stream) {
                        .stdout => {
                            streams.stdout = redirect_pipe_ref.dereference();
                            streams.stdout_file_pipe = redirect_pipe_ref.dereference();
                        },
                        .stderr => {
                            streams.stderr = redirect_pipe_ref.dereference();
                            streams.stderr_file_pipe = redirect_pipe_ref.dereference();
                        },
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
                        .stdout => streams.stdout = target_loc,
                        .stderr => streams.stderr = target_loc,
                        else => {},
                    }
                },
            }
        }
        return streams;
    }

    /// Returns true when `streams` has at least one file-redirect pipe that
    /// needs a drain (i.e. the redirect targets a file, not another fd).
    fn hasFileRedirect(streams: RedirectStreams) bool {
        return streams.stdout_file_pipe != null or streams.stderr_file_pipe != null;
    }

    /// Drains a function/block's file-redirect pipes to their files. Unlike a
    /// command (which *is* the process, so its own exec closure drives the
    /// drain), a function or block only writes to the redirect pipe — nothing
    /// reads the pipe's source, so the inner command's stdout never reaches EOF
    /// and it never completes. This spawns a concurrent drain per file pipe
    /// (`stdoutStreamSet` driving pipe→file forwarding) so the writer's output
    /// flows out as it is produced, then — once the writer thread finishes —
    /// clears each pipe's `keep_open` flag so the drain flushes and closes the
    /// file, and waits the drains.
    ///
    /// `writer_handle` is the (stable) thread handle of the function/block
    /// thread. The drains are forked *before* waiting it so they run
    /// concurrently with the writer.
    fn compileFileRedirectDrains(
        self: *IRCompiler,
        source: anytype,
        writer_handle: ir.Location,
        streams: RedirectStreams,
    ) Error!void {
        var stdout_drain: ?ir.Location = null;
        var stderr_drain: ?ir.Location = null;

        if (streams.stdout_file_pipe) |file_pipe| {
            const drain = try self.fork(
                source,
                self.stdoutStreamSet(),
                self.threadStdin(),
                file_pipe,
                self.threadStderr(),
                .noll,
                .inherit,
            );
            const drain_ref = try self.newRef(source, "stdout_redirect_drain");
            try self.set(source, drain_ref, .from(drain));
            stdout_drain = drain_ref.dereference().typed(thread_type);
        }

        if (streams.stderr_file_pipe) |file_pipe| {
            const drain = try self.fork(
                source,
                self.stdoutStreamSet(),
                self.threadStdin(),
                file_pipe,
                self.threadStderr(),
                .noll,
                .inherit,
            );
            const drain_ref = try self.newRef(source, "stderr_redirect_drain");
            try self.set(source, drain_ref, .from(drain));
            stderr_drain = drain_ref.dereference().typed(thread_type);
        }

        // Wait for the writer (the function/block thread) to finish. The drains
        // run concurrently, so the writer's inner commands can complete.
        try self.wait(source, writer_handle);

        // The writer is done and has connected (and EOF'd) its stdout. Clear the
        // keep_open flags so each drain closes its file once the source drains,
        // instead of spinning forever.
        if (streams.stdout_file_pipe) |file_pipe| {
            try self.pipeOpt(source, file_pipe, .keep_open, .fromValue(.fromBoolean(false)));
        }
        if (streams.stderr_file_pipe) |file_pipe| {
            try self.pipeOpt(source, file_pipe, .keep_open, .fromValue(.fromBoolean(false)));
        }

        if (stdout_drain) |drain| try self.wait(source, drain);
        if (stderr_drain) |drain| try self.wait(source, drain);
    }

    fn compileBlockCallWithRedirects(
        self: *IRCompiler,
        source: *ast.Expression,
        block: ast.Block,
        redirects: []const ast.Redirection,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const block_instr_set = try self.addInstructionSet();

        const streams = try self.compileRedirectStreams(source, redirects);

        const spawned = try self.spawnClosure(
            source,
            .initAbs(block_instr_set, 0),
            self.threadStdin(),
            streams.stdout,
            streams.stderr,
        );

        const prev_instr_set = self.current_instruction_set;
        self.current_instruction_set = block_instr_set;
        try self.scopes.push(self.allocator, .closure);

        _ = try self.compileBlock(source, block);
        try self.exitWith(source, .fromValue(.fromBoolean(true)));

        try self.setClosureIdentifiers();
        self.current_instruction_set = prev_instr_set;
        // Emit the closure's initialization block (populates captures and jumps
        // back to the fork). Without this the create-closure jump lands in an
        // empty set and the block thread is never actually spawned.
        try self.compileClosureInitialization(source, spawned.closure);
        self.scopes.pop();

        if (hasFileRedirect(streams)) {
            // The block writes to a redirect pipe; drive its drain to the file
            // and wait for both the block and the drain. The redirect consumes
            // the block's output, so the call yields nothing (void).
            const thread_ref = try self.newRef(source, "block_thread");
            try self.set(source, thread_ref, .from(spawned.thread_handle));
            try self.compileFileRedirectDrains(
                source,
                thread_ref.dereference().typed(thread_type),
                streams,
            );
            return .fromValue(.void);
        }

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

    /// `setenv name value` builtin: set the environment variable named by the
    /// runtime string `name` to `value` in the current subshell context (the
    /// dynamic-name counterpart of `$NAME = value`). Backs `std.env.set`.
    fn compileBuiltinSetEnv(
        self: *IRCompiler,
        source: *ast.Expression,
        arguments: []const *ast.Expression,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const name = try self.compileExpression(arguments[0]);
        const name_ref = try self.newRef(source, "setenv_name");
        try self.set(source, name_ref, stableResultSource(name));
        const value = try self.compileExpression(arguments[1]);
        const value_ref = try self.newRef(source, "setenv_value");
        try self.set(source, value_ref, stableResultSource(value));

        try self.addInstruction(.init(.from(source), .{ .set_env = .{
            .name = "",
            .name_source = .from(name_ref.dereference()),
            .value = .from(value_ref.dereference()),
        } }));
        return .fromValue(.void);
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
        // When non-null, the executable is a runtime string produced by this
        // expression (`run "path" …`) rather than the static `executable` value;
        // it is compiled inside the exec closure alongside the arguments.
        executable_expr: ?*ast.Expression,
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
                    const redirect_target = try self.compileExpression(path_target.value);
                    const redirect_pipe_ref = try self.newRef(source, if (stream_fd == 1) "stdout_redirect_pipe" else "stderr_redirect_pipe");
                    try self.pipe(source, redirect_pipe_ref);
                    // Keep the redirect pipe open until its source (the command's
                    // stdout) connects and closes. Without this, a drain that runs
                    // before the command connects — e.g. when interpolated arguments
                    // delay the exec — sees no source, closes the file immediately,
                    // and the command's output then has nowhere to go (deadlock).
                    try self.pipeOpt(source, redirect_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(true)));
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

        // Compile every argument into its own stable ref first, then push them
        // all. Building an interpolated (multi-segment) string argument leaves
        // its own ref on the stack; interleaving those refs with the pushed
        // argument values would make `exec` pop the wrong slots — so an
        // `echo "${x}" "${y}"` argument list must be pushed contiguously, after
        // every argument is built.
        const arg_value_refs = try self.allocator.alloc(ir.Location, arguments.len);
        defer self.allocator.free(arg_value_refs);
        var it = std.mem.reverseIterator(arguments);
        var arg_i: usize = 0;
        while (it.next()) |arg_expr| : (arg_i += 1) {
            var arg = try self.compileExpression(arg_expr);
            if (arg.isType(execution_result_struct_type)) {
                const arg_ref = try self.newRef(source, "exec_result_arg");
                try self.set(source, arg_ref, arg.source);
                try self.set(source, .initRegister(.r2), .from(arg_ref.dereference()));
                arg = .fromLocation(.initAdd(
                    .{ .register = .r2 },
                    executionResultFieldOffset(.merged),
                    .{ .dereference = true },
                ));
            }
            const value_ref = try self.newRef(source, "exec_arg");
            try self.set(source, value_ref, arg.source);
            arg_value_refs[arg_i] = value_ref.dereference();
        }
        for (arg_value_refs) |value_ref| {
            try self.push(source, .from(value_ref));
        }
        if (executable_expr) |exe_expr| {
            const exe = try self.compileExpression(exe_expr);
            try self.push(source, exe.source);
        } else {
            try self.push(source, .from(executable));
        }
        try self.push(source, .fromValue(.{ .integer = @as(i64, @intCast(arguments.len)) }));

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

        // The command has finished (and connected its stdout). Clear the redirect
        // pipe's keep_open flag set at creation, so its drain closes the file once
        // the (now-EOF) source drains, instead of spinning forever.
        if (has_stdout_file_redirect) {
            try self.pipeOpt(source, self.threadStdout(), .keep_open, .fromValue(.fromBoolean(false)));
        }

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

    /// UFCS: compile `receiver.method(args…)` as `method(receiver, args…)` when
    /// `method` names a function in scope. Returns null when there is no such
    /// function (so the caller falls back to field access / its normal path).
    /// The receiver AST node is reused as arg 0, so it is evaluated exactly once.
    const StringBuiltin = struct {
        op: ir.Instruction.StrOp.Op,
        arity: u8,
        result: ast.TypeExpr,
        /// Only applies to string receivers (e.g. `len`, which also names the
        /// array length). String-only names (`upper`, `contains`, …) apply to
        /// any receiver, materialized to bytes.
        string_only_receiver: bool,
    };

    /// Maps a method name to its string-builtin descriptor (UFCS over Zig string
    /// ops), or null. Result types: `Int` for len/indexOf, `Bool` for the
    /// predicates, `String` for the transforms.
    fn stringBuiltin(name: []const u8) ?StringBuiltin {
        const int_t = ast.TypeExpr.global(.integer);
        const bool_t = ast.TypeExpr.global(.boolean);
        const eql = std.mem.eql;
        if (eql(u8, name, "len")) return .{ .op = .len, .arity = 0, .result = int_t, .string_only_receiver = true };
        if (eql(u8, name, "upper")) return .{ .op = .upper, .arity = 0, .result = string_type, .string_only_receiver = false };
        if (eql(u8, name, "lower")) return .{ .op = .lower, .arity = 0, .result = string_type, .string_only_receiver = false };
        if (eql(u8, name, "trim")) return .{ .op = .trim, .arity = 0, .result = string_type, .string_only_receiver = false };
        if (eql(u8, name, "trimStart")) return .{ .op = .trim_start, .arity = 0, .result = string_type, .string_only_receiver = false };
        if (eql(u8, name, "trimEnd")) return .{ .op = .trim_end, .arity = 0, .result = string_type, .string_only_receiver = false };
        if (eql(u8, name, "contains")) return .{ .op = .contains, .arity = 1, .result = bool_t, .string_only_receiver = false };
        if (eql(u8, name, "startsWith")) return .{ .op = .starts_with, .arity = 1, .result = bool_t, .string_only_receiver = false };
        if (eql(u8, name, "endsWith")) return .{ .op = .ends_with, .arity = 1, .result = bool_t, .string_only_receiver = false };
        if (eql(u8, name, "indexOf")) return .{ .op = .index_of, .arity = 1, .result = int_t, .string_only_receiver = false };
        if (eql(u8, name, "slice")) return .{ .op = .slice, .arity = 2, .result = string_type, .string_only_receiver = false };
        if (eql(u8, name, "repeat")) return .{ .op = .repeat, .arity = 1, .result = string_type, .string_only_receiver = false };
        if (eql(u8, name, "replace")) return .{ .op = .replace, .arity = 2, .result = string_type, .string_only_receiver = false };
        if (eql(u8, name, "bytes")) return .{ .op = .bytes, .arity = 0, .result = int_array_type, .string_only_receiver = true };
        if (eql(u8, name, "split")) return .{ .op = .split, .arity = 1, .result = array_type(&string_type), .string_only_receiver = false };
        if (eql(u8, name, "join")) return .{ .op = .join, .arity = 1, .result = string_type, .string_only_receiver = false };
        return null;
    }

    /// True when a compile-time type denotes the string type (`[]Byte`).
    /// Normalizes the `String`-named type identifier to the structural string
    /// type (`[]Byte`), recursing through arrays and optionals, so a `String`
    /// parameter or `[]String` element is recognized by every string-handling
    /// site (which key on `.array` with a byte element). Non-`String` identifiers
    /// — including user struct names — are left untouched.
    fn lookupTypeCapture(self: *IRCompiler, name: []const u8) ?ast.TypeExpr {
        return self.type_captures.get(name);
    }

    /// Registers each `|T|` capture appearing in a function parameter's type as
    /// a permissive type variable, so a reference to `T` in the body (including
    /// `${T}` serialization) resolves to the variable name. A concrete per-call
    /// type would require monomorphization (not yet implemented).
    fn registerParamTypeVars(self: *IRCompiler, t: ast.TypeExpr) void {
        switch (t) {
            .type_capture => |capture| {
                if (!self.type_captures.contains(capture.name)) {
                    self.type_captures.put(self.allocator, capture.name, .{ .type_var = .{ .name = capture.name, .span = capture.span } }) catch {};
                }
            },
            .array => |a| self.registerParamTypeVars(a.element.*),
            .optional => |o| self.registerParamTypeVars(o.child.*),
            .promise => |p| self.registerParamTypeVars(p.child.*),
            .type_application => |app| for (app.args) |arg| self.registerParamTypeVars(arg.*),
            else => {},
        }
    }

    /// Collects the `|T|` capture names appearing in a type into `out`.
    fn collectCapturesInType(t: ast.TypeExpr, out: *std.ArrayList([]const u8), allocator: Allocator) Allocator.Error!void {
        switch (t) {
            .type_capture => |c| try out.append(allocator, c.name),
            .array => |a| try collectCapturesInType(a.element.*, out, allocator),
            .optional => |o| try collectCapturesInType(o.child.*, out, allocator),
            .promise => |p| try collectCapturesInType(p.child.*, out, allocator),
            .type_application => |app| for (app.args) |arg| try collectCapturesInType(arg.*, out, allocator),
            else => {},
        }
    }

    /// Whether a tracked type is still unknown — absent, `Void`, or an array
    /// whose element type hasn't been determined (`var xs = .{ }` → `[]Void`).
    /// Such a type may be refined when a concretely-typed value is assigned.
    fn typeIsUnknown(t: ?ast.TypeExpr) bool {
        const ty = t orelse return true;
        return switch (ty) {
            .void => true,
            .array => |a| typeIsUnknown(a.element.*),
            else => false,
        };
    }

    /// The compile-time static type of a call argument, when knowable.
    fn argTypeExpr(self: *IRCompiler, arg: *ast.Expression) ?ast.TypeExpr {
        return switch (arg.*) {
            .literal => |lit| switch (lit) {
                .integer => ast.TypeExpr.global(.integer),
                .float => ast.TypeExpr.global(.float),
                .bool => ast.TypeExpr.global(.boolean),
                .string => string_type,
                .null => null,
            },
            .identifier, .call => self.resolveStaticType(arg),
            else => null,
        };
    }

    /// Monomorphization: for a direct call to a top-level, non-recursive generic
    /// function whose parameters carry `|T|` captures, compile (or reuse) a
    /// specialization with the type variables bound to the call's concrete
    /// argument types, and return its instruction set. Returns null (use the
    /// generic function) when the callee isn't a specializable candidate or the
    /// type arguments can't be determined.
    fn maybeSpecialize(
        self: *IRCompiler,
        instr_set: usize,
        arguments: []const *ast.Expression,
    ) Error!?usize {
        if (self.specialization_depth >= specialization_max_depth) return null;
        const fn_source = self.fn_decl_sources.get(instr_set) orelse return null;
        const fn_decl = self.comptime_fn_decls.get(instr_set).?.*;
        if (fn_decl.params != ._non_variadic) return null;
        const params = fn_decl.params._non_variadic;
        if (params.len != arguments.len) return null;
        // Specialization recompiles the body in the current scope, so a function
        // that captures closure variables is not a safe candidate.
        if (self.instruction_sets.items[instr_set].closure_captures.len != 0) return null;

        var capture_names = std.ArrayList([]const u8).empty;
        defer capture_names.deinit(self.allocator);
        for (params) |param| {
            if (param.type_annotation) |ann| try collectCapturesInType(ann.*, &capture_names, self.allocator);
        }
        if (capture_names.items.len == 0) return null;

        // Save any prior bindings for these names so the caller's context is
        // restored after the specialization is compiled.
        var saved = std.ArrayList(?ast.TypeExpr).empty;
        defer saved.deinit(self.allocator);
        for (capture_names.items) |name| try saved.append(self.allocator, self.type_captures.get(name));
        defer for (capture_names.items, saved.items) |name, prior| {
            if (prior) |p| self.type_captures.put(self.allocator, name, p) catch {} else _ = self.type_captures.remove(name);
        };
        for (capture_names.items) |name| _ = self.type_captures.remove(name);

        // Bind each capture to its argument's concrete type.
        for (params, arguments) |param, arg| {
            const ann = param.type_annotation orelse continue;
            if (!hasTypeCapture(ann.*)) continue;
            const arg_type = self.argTypeExpr(arg) orelse return null;
            self.bindTypeCaptures(ann.*, arg_type);
        }
        // Every capture must have bound to a concrete (non-variable) type.
        for (capture_names.items) |name| {
            const bound = self.type_captures.get(name) orelse return null;
            if (bound == .type_var) return null;
        }

        // Build a cache key from the bound type arguments.
        var key_writer = std.Io.Writer.Allocating.init(self.allocator);
        defer key_writer.deinit();
        try key_writer.writer.print("{}", .{instr_set});
        for (capture_names.items) |name| {
            try key_writer.writer.print("|{s}=", .{name});
            try self.writeTypeName(&key_writer.writer, self.type_captures.get(name).?);
        }
        const key = key_writer.written();

        if (self.specializations.get(key)) |cached| return cached;

        // Compile the specialization with the type variables bound. Cache it
        // under a stable key first (compileFnDecl registers the generic→spec
        // mapping via `specializing_generic` so a self-recursive call inside the
        // body targets this same specialization).
        const owned_key = try self.allocator.dupe(u8, key);
        const prev_specializing = self.specializing;
        const prev_generic = self.specializing_generic;
        self.specializing = true;
        self.specializing_generic = instr_set;
        self.specialization_depth += 1;
        const result = self.compileFnDecl(fn_source, fn_decl) catch |err| {
            self.specializing = prev_specializing;
            self.specializing_generic = prev_generic;
            self.specialization_depth -= 1;
            return err;
        };
        self.specializing = prev_specializing;
        self.specializing_generic = prev_generic;
        self.specialization_depth -= 1;
        const spec_instr_set = result.source.value.fn_ref.fn_addr.instr_set;
        try self.specializations.put(self.allocator, owned_key, spec_instr_set);
        return spec_instr_set;
    }

    /// A best-effort static type for a runtime value, used to seed a `|T|`
    /// capture when the initializer's Result carries no type (e.g. a literal).
    fn valueTypeExpr(value: ir.Value) ?ast.TypeExpr {
        return switch (value) {
            .integer => ast.TypeExpr.global(.integer),
            .float => ast.TypeExpr.global(.float),
            .exit_code => ast.TypeExpr.global(.boolean),
            .slice => string_type,
            else => null,
        };
    }

    /// Unifies a binding's annotation `pattern` against the initializer's
    /// concrete `subject` type, recording each `|T|` capture's matched type.
    /// Recurses through the built-in generic constructors so `[]|T|` binds `T`
    /// to the element type and `?|T|` to the child.
    /// Substitutes each identifier naming one of `params` with the corresponding
    /// `args` entry — a generic application's body substitution. Recurses through
    /// the composite type shapes. Mirrors the type-checker's version.
    fn substituteTypeParams(
        self: *IRCompiler,
        t: ast.TypeExpr,
        params: []const ast.Identifier,
        args: []const *const ast.TypeExpr,
    ) Allocator.Error!ast.TypeExpr {
        switch (t) {
            .identifier => |named| {
                if (named.path.segments.len == 1) {
                    const name = named.path.segments[0].name;
                    for (params, 0..) |param, i| {
                        if (i < args.len and std.mem.eql(u8, param.name, name)) return args[i].*;
                    }
                }
                return t;
            },
            .array => |a| {
                const elem = try self.allocator.create(ast.TypeExpr);
                elem.* = try self.substituteTypeParams(a.element.*, params, args);
                return .{ .array = .{ .element = elem, .span = a.span } };
            },
            .optional => |o| {
                const child = try self.allocator.create(ast.TypeExpr);
                child.* = try self.substituteTypeParams(o.child.*, params, args);
                return .{ .optional = .{ .child = child, .span = o.span } };
            },
            .promise => |p| {
                const child = try self.allocator.create(ast.TypeExpr);
                child.* = try self.substituteTypeParams(p.child.*, params, args);
                return .{ .promise = .{ .child = child, .span = p.span } };
            },
            .type_application => |app| {
                const new_args = try self.allocator.alloc(*const ast.TypeExpr, app.args.len);
                for (app.args, new_args) |arg, *dst| {
                    const na = try self.allocator.create(ast.TypeExpr);
                    na.* = try self.substituteTypeParams(arg.*, params, args);
                    dst.* = na;
                }
                return .{ .type_application = .{ .name = app.name, .args = new_args, .span = app.span } };
            },
            .struct_type => |st| {
                const new_fields = try self.allocator.alloc(ast.TypeExpr.StructField, st.fields.len);
                for (st.fields, new_fields) |field, *dst| {
                    dst.* = field;
                    const ft = try self.allocator.create(ast.TypeExpr);
                    ft.* = try self.substituteTypeParams(field.type_expr.*, params, args);
                    dst.type_expr = ft;
                }
                var new_st = st;
                new_st.fields = new_fields;
                return .{ .struct_type = new_st };
            },
            else => return t,
        }
    }

    /// Resolves a generic application (`Box(Int)`) to the constructor's struct
    /// body with the arguments substituted, or null when the constructor isn't a
    /// registered generic struct.
    fn resolveTypeApplication(self: *IRCompiler, app: ast.TypeExpr.TypeApplication) ?ast.TypeExpr {
        const body_st = self.user_struct_types.get(app.name.name) orelse return null;
        const params = self.generic_ctor_params.get(app.name.name) orelse &[_]ast.Identifier{};
        return self.substituteTypeParams(.{ .struct_type = body_st }, params, app.args) catch null;
    }

    fn bindTypeCaptures(self: *IRCompiler, pattern: ast.TypeExpr, subject: ast.TypeExpr) void {
        switch (pattern) {
            .type_capture => |capture| {
                self.type_captures.put(self.allocator, capture.name, subject) catch {};
            },
            .array => |a| if (subject == .array) self.bindTypeCaptures(a.element.*, subject.array.element.*),
            .optional => |o| if (subject == .optional) self.bindTypeCaptures(o.child.*, subject.optional.child.*),
            .promise => |p| if (subject == .promise) self.bindTypeCaptures(p.child.*, subject.promise.child.*),
            // `Box(|T|)` — substitute into the body (keeping the capture), then
            // match structurally against the subject struct.
            .type_application => |app| {
                if (self.resolveTypeApplication(app)) |resolved| self.bindTypeCaptures(resolved, subject);
            },
            .struct_type => |st| {
                var subj = subject;
                while (subj == .alias) subj = subj.alias.type_expr.*;
                if (subj == .struct_type) {
                    for (st.fields) |field| {
                        for (subj.struct_type.fields) |sfield| {
                            if (std.mem.eql(u8, field.name.name, sfield.name.name)) {
                                self.bindTypeCaptures(field.type_expr.*, sfield.type_expr.*);
                                break;
                            }
                        }
                    }
                }
            },
            else => {},
        }
    }

    /// Writes a type's serialized form: a name for named/primitive types
    /// (`Int`, `String`, `Box(Int)`), the structural form for an anonymous
    /// struct (`struct { value: Int }`), and the composed form for `[]T` / `?T`.
    fn writeTypeName(self: *IRCompiler, w: *std.Io.Writer, t: ast.TypeExpr) std.Io.Writer.Error!void {
        switch (t) {
            .integer => try w.writeAll("Int"),
            .float => try w.writeAll("Float"),
            .boolean => try w.writeAll("Bool"),
            .void => try w.writeAll("Void"),
            .null => try w.writeAll("Null"),
            .byte => try w.writeAll("Byte"),
            .thread => try w.writeAll("Thread"),
            .execution => try w.writeAll("Execution"),
            .array => |a| {
                if (a.element.* == .byte) {
                    try w.writeAll("String");
                } else {
                    try w.writeAll("[]");
                    try self.writeTypeName(w, a.element.*);
                }
            },
            .optional => |o| {
                try w.writeByte('?');
                try self.writeTypeName(w, o.child.*);
            },
            .identifier => |id| for (id.path.segments, 0..) |seg, i| {
                if (i > 0) try w.writeByte('.');
                try w.writeAll(seg.name);
            },
            .alias => |al| try self.writeTypeName(w, al.type_expr.*),
            .type_var => |tv| try w.writeAll(tv.name),
            .type_application => |app| {
                try w.writeAll(app.name.name);
                try w.writeByte('(');
                for (app.args, 0..) |arg, i| {
                    if (i > 0) try w.writeAll(", ");
                    try self.writeTypeName(w, arg.*);
                }
                try w.writeByte(')');
            },
            .struct_type => |st| {
                try w.writeAll("struct { ");
                for (st.fields, 0..) |field, i| {
                    if (i > 0) try w.writeAll(", ");
                    try w.writeAll(field.name.name);
                    try w.writeAll(": ");
                    try self.writeTypeName(w, field.type_expr.*);
                }
                try w.writeAll(" }");
            },
            else => try w.writeAll("?"),
        }
    }

    /// Whether `name` denotes a compile-time type: a primitive keyword, a
    /// declared struct or generic constructor, an error set, or a bound capture.
    fn isTypeName(self: *IRCompiler, name: []const u8) bool {
        const primitives = [_][]const u8{ "Int", "String", "Bool", "Float", "Void", "Byte" };
        for (primitives) |p| {
            if (std.mem.eql(u8, name, p)) return true;
        }
        return self.user_struct_types.contains(name) or self.error_sets.contains(name) or self.type_captures.contains(name);
    }

    /// Writes a type argument (from a `Name(args…)` application in value
    /// position) as its type name — a bare type identifier, a bound capture, or
    /// a nested application (`Box(String)`).
    fn writeArgTypeName(self: *IRCompiler, w: *std.Io.Writer, arg: *ast.Expression) std.Io.Writer.Error!void {
        switch (arg.*) {
            .identifier => |id| {
                if (self.type_captures.get(id.name)) |t| try self.writeTypeName(w, t) else try w.writeAll(id.name);
            },
            .call => |call| if (call.callee.* == .identifier) {
                try w.writeAll(call.callee.identifier.name);
                if (call.arguments.len > 0) {
                    try w.writeByte('(');
                    for (call.arguments, 0..) |a, i| {
                        if (i > 0) try w.writeAll(", ");
                        try self.writeArgTypeName(w, a);
                    }
                    try w.writeByte(')');
                }
            },
            else => {},
        }
    }

    /// Serializes a `Name(args…)` application in value position (`${Box(Int)}`)
    /// to its type-application string, or null when the callee isn't a type.
    fn compileTypeApplicationString(self: *IRCompiler, call: ast.CallExpr) Error!?ir.Value {
        if (call.callee.* != .identifier) return null;
        const name = call.callee.identifier.name;
        if (!self.isTypeName(name)) return null;

        var alloc_writer = std.Io.Writer.Allocating.init(self.allocator);
        defer alloc_writer.deinit();
        const w = &alloc_writer.writer;
        try w.writeAll(name);
        if (call.arguments.len > 0) {
            try w.writeByte('(');
            for (call.arguments, 0..) |arg, i| {
                if (i > 0) try w.writeAll(", ");
                try self.writeArgTypeName(w, arg);
            }
            try w.writeByte(')');
        }
        return try self.addSlice(1, alloc_writer.written());
    }

    /// If `name` refers to a compile-time type — a primitive keyword, a declared
    /// struct or generic constructor, an error set, or a bound `|T|` capture —
    /// returns its serialized name as a string value (for using a type
    /// identifier where a string is expected). Otherwise null.
    fn typeIdentifierString(self: *IRCompiler, name: []const u8) Error!?ir.Value {
        const primitives = [_][]const u8{ "Int", "String", "Bool", "Float", "Void", "Byte" };
        for (primitives) |p| {
            if (std.mem.eql(u8, name, p)) return try self.addSlice(1, p);
        }
        // A named struct / generic constructor / error set serializes as its name.
        if (self.user_struct_types.contains(name) or self.error_sets.contains(name)) {
            return try self.addSlice(1, name);
        }
        // A `|T|` capture bound to a concrete type serializes as that type.
        if (self.type_captures.get(name)) |t| {
            var alloc_writer = std.Io.Writer.Allocating.init(self.allocator);
            defer alloc_writer.deinit();
            try self.writeTypeName(&alloc_writer.writer, t);
            return try self.addSlice(1, alloc_writer.written());
        }
        return null;
    }

    /// Whether a type expression contains a `|T|` capture anywhere.
    fn hasTypeCapture(t: ast.TypeExpr) bool {
        return switch (t) {
            .type_capture => true,
            .array => |a| hasTypeCapture(a.element.*),
            .optional => |o| hasTypeCapture(o.child.*),
            .promise => |p| hasTypeCapture(p.child.*),
            .type_application => |app| for (app.args) |arg| {
                if (hasTypeCapture(arg.*)) break true;
            } else false,
            else => false,
        };
    }

    fn normalizeStringTypes(self: *IRCompiler, t: ast.TypeExpr) ast.TypeExpr {
        switch (t) {
            .identifier => |id| {
                const segs = id.path.segments;
                if (segs.len == 1) {
                    if (self.lookupTypeCapture(segs[0].name)) |bound| return self.normalizeStringTypes(bound);
                    if (std.mem.eql(u8, segs[segs.len - 1].name, "String")) return string_type;
                }
                return t;
            },
            .array => |a| {
                const elem = self.allocator.create(ast.TypeExpr) catch return t;
                elem.* = self.normalizeStringTypes(a.element.*);
                return .{ .array = .{ .element = elem, .span = a.span } };
            },
            .optional => |o| {
                const child = self.allocator.create(ast.TypeExpr) catch return t;
                child.* = self.normalizeStringTypes(o.child.*);
                return .{ .optional = .{ .child = child, .span = o.span } };
            },
            // A `|T|` capture is permissive here (like a type variable); a
            // concrete binding position resolves it against its initializer
            // via `bindTypeCaptures` before this is consulted.
            .type_capture => |capture| {
                if (self.lookupTypeCapture(capture.name)) |bound| return self.normalizeStringTypes(bound);
                return .{ .type_var = .{ .name = capture.name, .span = capture.span } };
            },
            // A generic application `Box(Int)` resolves to the constructor's
            // struct body with the arguments substituted (`struct { value: Int }`),
            // so member access, capture matching, and `${T}` see concrete fields.
            .type_application => |app| {
                if (self.resolveTypeApplication(app)) |resolved| return self.normalizeStringTypes(resolved);
                if (self.user_struct_types.get(app.name.name)) |st| return .{ .struct_type = st };
                return t;
            },
            else => return t,
        }
    }

    fn typeIsString(_: *IRCompiler, type_expr: ast.TypeExpr) bool {
        var t = type_expr;
        while (t == .alias) t = t.alias.type_expr.*;
        if (t == .array and t.array.element.* == .byte) return true;
        // A `String`-named element (e.g. the element type of a `[]String`
        // parameter) arrives as the unresolved identifier rather than the
        // structural `[]Byte`.
        if (t == .identifier) {
            const segs = t.identifier.path.segments;
            return segs.len == 1 and std.mem.eql(u8, segs[segs.len - 1].name, "String");
        }
        return false;
    }

    /// Compiles a string builtin: stabilizes the receiver (operand) and its args,
    /// then emits a `str_op`. The receiver is already compiled (`object`); `args`
    /// are the (0–2) argument expressions.
    fn compileStrOp(
        self: *IRCompiler,
        source: *ast.Expression,
        object: Result,
        sb: StringBuiltin,
        args: []const *ast.Expression,
    ) Error!Result {
        const operand_ref = try self.newRef(source, "str_operand");
        try self.set(source, operand_ref, stableResultSource(object));

        var arg_sources = [_]ir.ValueSource{ .fromValue(.void), .fromValue(.void) };
        for (args, 0..) |arg, i| {
            const arg_result = try self.compileExpression(arg);
            const arg_ref = try self.newRef(source, "str_arg");
            try self.set(source, arg_ref, stableResultSource(arg_result));
            arg_sources[i] = .from(arg_ref.dereference());
        }

        const result_ref = try self.newRef(source, "str_result");
        try self.addInstruction(.init(.from(source), .{ .str_op = .{
            .op = sb.op,
            .operand = .from(operand_ref.dereference()),
            .arg0 = arg_sources[0],
            .arg1 = arg_sources[1],
            .result = result_ref.dereference(),
        } }));
        return .fromLocation(result_ref.dereference().typed(sb.result));
    }

    const FloatBuiltin = struct {
        op: ir.Instruction.FloatOp.Op,
        arity: u8,
    };

    /// Maps a method name to its Float-math builtin descriptor (UFCS over the
    /// operand: `x.sqrt`, `x.floor`, `x.pow y`), or null. All yield a Float.
    fn floatBuiltin(name: []const u8) ?FloatBuiltin {
        const eql = std.mem.eql;
        if (eql(u8, name, "sqrt")) return .{ .op = .sqrt, .arity = 0 };
        if (eql(u8, name, "floor")) return .{ .op = .floor, .arity = 0 };
        if (eql(u8, name, "ceil")) return .{ .op = .ceil, .arity = 0 };
        if (eql(u8, name, "round")) return .{ .op = .round, .arity = 0 };
        if (eql(u8, name, "trunc")) return .{ .op = .trunc, .arity = 0 };
        if (eql(u8, name, "powF")) return .{ .op = .pow, .arity = 1 };
        return null;
    }

    /// Compiles a Float-math builtin: stabilizes the operand and its optional
    /// argument, then emits a `float_op` yielding a Float.
    fn compileFloatOp(
        self: *IRCompiler,
        source: *ast.Expression,
        object: Result,
        fb: FloatBuiltin,
        args: []const *ast.Expression,
    ) Error!Result {
        const operand_ref = try self.newRef(source, "float_operand");
        try self.set(source, operand_ref, stableResultSource(object));

        var arg0: ir.ValueSource = .fromValue(.void);
        if (args.len > 0) {
            const arg_result = try self.compileExpression(args[0]);
            const arg_ref = try self.newRef(source, "float_arg");
            try self.set(source, arg_ref, stableResultSource(arg_result));
            arg0 = .from(arg_ref.dereference());
        }

        const result_ref = try self.newRef(source, "float_result");
        try self.addInstruction(.init(.from(source), .{ .float_op = .{
            .op = fb.op,
            .operand = .from(operand_ref.dereference()),
            .arg0 = arg0,
            .result = result_ref.dereference(),
        } }));
        return .fromLocation(result_ref.dereference().typed(.global(.float)));
    }

    fn tryUfcsRewrite(
        self: *IRCompiler,
        source: *ast.Expression,
        receiver: *ast.Expression,
        method_name: []const u8,
        args: []const *ast.Expression,
        redirects: []const ast.Redirection,
    ) Error!?Result {
        const binding = self.lookup(method_name, .{ .shallow = false }) orelse return null;
        if (!binding.result.isFunctionRef()) return null;

        const full = try self.allocator.alloc(*ast.Expression, args.len + 1);
        full[0] = receiver;
        for (args, 0..) |a, i| full[i + 1] = a;

        return try self.compileFunctionCall(source, binding.result.source.value, full, redirects, null);
    }

    /// Indirect call `f x …`: the callee is a *function value* known only at
    /// runtime (a function-typed parameter). Forks the fn_ref read from the
    /// callee location, passing the arguments in the closure, and captures the
    /// yielded value. Closure size is the argument count — valid for a
    /// capture-free (top-level) function, which is what a HOF receives.
    fn compileIndirectCall(
        self: *IRCompiler,
        source: *ast.Expression,
        callee_loc: ir.Location,
        return_type: ?ast.TypeExpr,
        args: []const *ast.Expression,
    ) Error!Result {
        const fn_ref_ref = try self.newRef(source, "indirect_fn");
        try self.set(source, fn_ref_ref, .from(callee_loc));

        const pipe_ref = try self.newRef(source, "indirect_pipe");
        try self.pipe(source, pipe_ref.dereference());
        try self.pipeOpt(source, pipe_ref.dereference(), .typed, .fromValue(.fromBoolean(true)));

        const arg_refs = try self.allocator.alloc(ir.Location, args.len);
        defer self.allocator.free(arg_refs);
        for (args, arg_refs) |arg, *ar| {
            const arg_value = try self.compileExpression(arg);
            const r = try self.newRef(source, "indirect_arg");
            try self.set(source, r, stableResultSource(arg_value));
            ar.* = r.dereference();
        }

        try self.alloc(source, args.len);
        const closure_ref = try self.newRef(source, "indirect_closure");
        try self.set(source, closure_ref, .fromLocation(.initRegister(.r)));
        for (arg_refs, 0..) |ar, i| {
            try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
            try self.set(source, .initAdd(.{ .register = .r }, i, .{ .dereference = true }), .from(ar));
        }

        try self.addInstruction(.init(.from(source), .{ .fork = .{
            .dest = ir.InstructionAddr.initAbs(0, 0),
            .dest_from = fn_ref_ref.dereference(),
            .stdin = self.threadStdin(),
            .stdout = pipe_ref.dereference(),
            .stderr = self.threadStderr(),
            .closure = closure_ref.dereference(),
            .subshell = .inherit,
        } }));
        const thread_ref = try self.newRef(source, "indirect_thread");
        try self.set(source, thread_ref, .fromLocation(.initRegister(.r)));
        try self.wait(source, thread_ref.dereference().typed(thread_type));

        try self.addInstruction(.init(.from(source), .{ .pipe_dequeue = pipe_ref.dereference() }));
        return .fromLocation(ir.Location.initRegister(.r).typed(return_type orelse .global(.void)));
    }

    fn compileFunctionCall(
        self: *IRCompiler,
        source: *ast.Expression,
        fn_ref_value: ir.Value,
        arguments: []const *ast.Expression,
        redirects: []const ast.Redirection,
        // When set, the forked function runs with this stdout instead of the
        // caller's (used by the in-process typed value capture).
        stdout_override: ?ir.Location,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const fn_ref = fn_ref_value.fn_ref;
        var fn_addr = fn_ref.fn_addr;

        // A self-recursive call inside a specialization body: its callee resolves
        // to the generic set, so redirect it to the specialization being compiled
        // (it then recurses at the same concrete type).
        for (self.active_specializations.items) |as| {
            if (fn_addr.instr_set == as.generic) {
                fn_addr = ir.InstructionAddr.initAbs(as.spec, 0);
                break;
            }
        }

        const is_self_recursive = self.current_instruction_set == fn_addr.instr_set;

        // Monomorphization: for a non-recursive direct call, redirect to a
        // per-type specialization when the callee's `|T|` captures resolve to
        // concrete argument types (so `${T}` folds to the concrete type name).
        if (!is_self_recursive and stdout_override == null) {
            if (try self.maybeSpecialize(fn_addr.instr_set, arguments)) |spec_instr_set| {
                fn_addr = ir.InstructionAddr.initAbs(spec_instr_set, 0);
            }
        }

        const self_closure_depth = if (is_self_recursive) blk: {
            const frame = try self.scopes.getFrame(0);
            break :blk if (frame.scope_type == .closure) @as(usize, 0) else try self.nearestClosureDepth();
        } else 0;

        // Manual closure compilation
        // TODO: Add closure variables as well (we need to extend the function reference value to be able to understand what closure variables are needed)
        const closure_captures = self.instruction_sets.items[fn_addr.instr_set].closure_captures;
        const closure_size = if (is_self_recursive)
            (try self.scopes.getFrame(self_closure_depth)).closure_bindings.items.len
        else
            self.instruction_sets.items[fn_addr.instr_set].closure_slot_count;
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
            // Track whether compiling this argument pushed an owned temporary.
            // `consume` pops any stack-location result, but a bare binding
            // reference (e.g. a struct-valued `p` passed as an arg) is a
            // *borrowed* stack slot, not a temp we own — popping it corrupts the
            // stack. Only pop when the arg compilation actually grew the frame.
            const stack_before_arg = self.currentFrame().rel_stack_counter;
            const arg_result = try self.compileResultSaveR(source, try self.compileExpression(arg));
            try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
            try self.set(
                source,
                .initAdd(.{ .register = .r }, i, .{ .dereference = true }),
                arg_result.source,
            );
            if (self.currentFrame().rel_stack_counter > stack_before_arg) {
                try self.consume(source, arg_result);
            }
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

        // Honor any output redirects (e.g. `myFn > "file"`): the function runs
        // with its stdout/stderr pointed at the redirect targets instead of
        // inheriting the caller's. With no redirects this is the same as
        // `forkInherit`.
        const streams = try self.compileRedirectStreams(source, redirects);
        const handle = try self.fork(
            source,
            fn_addr,
            self.threadStdin(),
            stdout_override orelse streams.stdout,
            streams.stderr,
            closure_ref.dereference(),
            .inherit,
        );

        const pub_exports = self.instruction_sets.items[fn_addr.instr_set].pub_exports;
        const n_pub = pub_exports.len;

        if (n_pub > 0) {
            // Save thread handle and wait for function body to finish so pub exports are populated
            const thread_ref = try self.newRef(source, "fn_thread");
            try self.set(source, thread_ref, .from(handle));
            try self.wait(source, thread_ref.dereference().typed(thread_type));

            // Build merged result struct: 5 standard fields + n_pub pub fields
            try self.alloc(source, 5 + n_pub);
            const result_ref = try self.newRef(source, "fn_result");
            try self.set(source, result_ref, .fromLocation(.initRegister(.r)));

            try self.set(source, .initRegister(.r2), .from(result_ref.dereference()));
            // stdout/stderr/merged: null — function uses inherited I/O, no capture
            try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.stdout), .{ .dereference = true }), .fromValue(.null));
            try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.stderr), .{ .dereference = true }), .fromValue(.null));
            try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.merged), .{ .dereference = true }), .fromValue(.null));
            try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.closeable), .{ .dereference = true }), .from(thread_ref.dereference()));
            try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.completion_is_thread), .{ .dereference = true }), .fromValue(.fromBoolean(true)));

            for (pub_exports, 0..) |pub_export, i| {
                try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
                try self.set(source, .initRegister(.r2), .from(result_ref.dereference()));
                try self.set(
                    source,
                    .initAdd(.{ .register = .r2 }, 5 + i, .{ .dereference = true }),
                    .fromLocation(.initAdd(.{ .register = .r }, pub_export.slot, .{ .dereference = true })),
                );
            }

            // Build merged struct type for member-access typing
            const all_fields = try self.allocator.alloc(ast.TypeExpr.StructField, 5 + n_pub);
            @memcpy(all_fields[0..5], execution_result_struct_type.struct_type.fields);
            for (pub_exports, 0..) |pub_export, i| {
                const field_type_ptr = try self.allocator.create(ast.TypeExpr);
                if (pub_export.fn_ref_value) |fn_ref_val| {
                    field_type_ptr.* = .{ .fn_ref_type = try self.fnRefTypeFor(fn_ref_val, pub_export.type_expr) };
                } else {
                    field_type_ptr.* = pub_export.type_expr orelse .global(.integer);
                }
                all_fields[5 + i] = .{
                    .name = ast.Identifier.global(pub_export.name),
                    .type_expr = field_type_ptr,
                    .span = .global,
                };
            }
            const merged_type = ast.TypeExpr{ .struct_type = .{
                .span = .global,
                .decls = &.{},
                .fields = all_fields,
                .by_reference_fields = true,
            } };

            return .fromLocation(result_ref.dereference().typed(merged_type));
        }

        // A file-redirected function call (`myFn > "file"`) with no pub exports:
        // drive the redirect pipe's drain to the file and wait for both the
        // function and the drain. The output is consumed by the redirect, so
        // the call yields nothing (void). Capture (`stdout_override`) never
        // combines with a file redirect.
        if (stdout_override == null and hasFileRedirect(streams)) {
            // Capture the fn thread handle (in %r) into a fresh ref on top of the
            // stack, then drive the redirect drain. Do NOT pop the closure here:
            // the redirect pipe ref sits above `closure_ref` on the stack, so a
            // pop would corrupt the pipe location the drain still needs. A few
            // slots leak for the rest of this frame, which is torn down after.
            const handle_ref = try self.newRef(source, "fn_thread");
            try self.set(source, handle_ref, .from(handle));
            try self.compileFileRedirectDrains(
                source,
                handle_ref.dereference().typed(thread_type),
                streams,
            );
            return .fromValue(.void);
        }

        // No pub exports: original behavior — return thread handle
        try self.set(source, .initRegister(.r2), .from(handle));
        try self.consume(source, try .from(closure_ref));
        try self.set(source, .initRegister(.r), .fromLocation(.initRegister(.r2)));

        return .fromLocation(
            ir.Location.initRegister(.r).typed(handle.options.type_expr),
        );
    }

    /// A saved binding type, restored after a narrowed `if (x is T)` then-branch.
    const NarrowGuard = struct {
        binding: *Scope.Binding,
        saved_result: Result,
    };

    /// The canonical compiler type an `is T` test narrows its subject to in the
    /// then-branch (`String` → `[]Byte`, primitive identifiers → the primitive,
    /// a user struct name → itself). Null when the type isn't a narrowable kind.
    fn resolveTestedType(self: *IRCompiler, type_expr: *const ast.TypeExpr) ?ast.TypeExpr {
        return switch (type_expr.*) {
            .identifier => |named| {
                const n = named.path.segments[named.path.segments.len - 1].name;
                if (std.mem.eql(u8, n, "String")) return string_type;
                if (std.mem.eql(u8, n, "Int")) return ast.TypeExpr.global(.integer);
                if (std.mem.eql(u8, n, "Float")) return ast.TypeExpr.global(.float);
                if (std.mem.eql(u8, n, "Bool")) return ast.TypeExpr.global(.boolean);
                if (self.user_struct_types.contains(n)) return type_expr.*;
                return null;
            },
            .integer, .float, .boolean => type_expr.*,
            .array => |a| if (a.element.* == .byte) string_type else type_expr.*,
            else => null,
        };
    }

    /// When `condition` is `x is T` on a plain identifier, narrows `x`'s binding
    /// to `T` for the duration of the then-branch (so `x.upper` / `x.bytes`
    /// resolve inside `if (x is String)`), returning a guard to restore it.
    fn narrowConditionSubject(self: *IRCompiler, condition: *ast.Expression) ?NarrowGuard {
        if (condition.* != .is_expr) return null;
        const is_expr = condition.is_expr;
        const name = subjectIdentifierName(is_expr.subject) orelse return null;
        const narrowed = self.resolveTestedType(is_expr.type_expr) orelse return null;
        const binding = self.lookup(name, .{ .shallow = false }) orelse return null;
        const guard = NarrowGuard{ .binding = binding, .saved_result = binding.result };
        binding.result = binding.result.typed(narrowed);
        return guard;
    }

    /// The bound name a narrowing subject refers to. A bare identifier in value
    /// position parses as a nullary call, so unwrap that too.
    fn subjectIdentifierName(expr: *ast.Expression) ?[]const u8 {
        return switch (expr.*) {
            .identifier => |id| id.name,
            .call => |call| if (call.arguments.len == 0 and call.callee.* == .identifier)
                call.callee.identifier.name
            else
                null,
            else => null,
        };
    }

    fn restoreNarrow(guard: ?NarrowGuard) void {
        if (guard) |g| g.binding.result = g.saved_result;
    }

    fn compileIf(
        self: *IRCompiler,
        source: *ast.Expression,
        if_expr: ast.IfExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (if_expr.capture == null) {
            if (try self.evalComptimeExpression(if_expr.condition)) |condition| {
                if (self.comptimeConditionTruth(if_expr.condition, condition)) |truth| {
                    if (truth) {
                        return self.compileExpression(if_expr.then_expr);
                    }
                    if (if_expr.else_branch) |else_branch| return switch (else_branch) {
                        .expr => |expr_| self.compileExpression(expr_),
                        .if_expr => |if_expr_| self.compileIf(source, if_expr_.*),
                        .condition => condition,
                    };
                    return .fromValue(.void);
                }
            }
        }

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
        const branch_stack_base = self.currentFrame().rel_stack_counter;
        const after_addr = try self.newLabel("if_after", .unknown);
        const else_addr = try self.newLabel("if_else", .unknown);

        try self.jmp(source, condition, false, else_addr);
        const narrow = self.narrowConditionSubject(if_expr.condition);
        const then = try self.compileIfBranchExpression(source, if_expr.then_expr, if_condition.capture_binding);
        restoreNarrow(narrow);
        try self.set(source, result, stableResultSource(then));
        try self.popToStackBase(source, branch_stack_base);
        try self.jmp(source, null, false, after_addr);
        try self.setLabel(else_addr.local_addr.label, .abs);
        var result_type = if (else_branch == .condition) mergedResultType(condition, then) else null;
        // A branch that yields a (discardable) thread handle — e.g. a nested Void
        // call — must keep the merged result typed as a thread so a Void-function
        // body waits on it (see compileFnDecl) instead of racing the caller.
        var branch_is_thread = then.isType(thread_type);
        switch (else_branch) {
            .expr => |expr_| {
                const else_ = try self.compileExpression(expr_);
                try self.set(source, result, stableResultSource(else_));
                result_type = mergedResultType(then, else_);
                if (else_.isType(thread_type)) branch_is_thread = true;
            },
            .if_expr => |if_expr_| {
                const else_ = try self.compileIf(source, if_expr_.*);
                try self.set(source, result, stableResultSource(else_));
                result_type = mergedResultType(then, else_);
                if (else_.isType(thread_type)) branch_is_thread = true;
            },
            .condition => {},
        }
        try self.popToStackBase(source, branch_stack_base);
        try self.setLabel(after_addr.local_addr.label, .abs);
        try self.set(source, .initRegister(.r2), .from(result.dereference()));

        return .fromLocation(ir.Location.initRegister(.r2).typed(if (branch_is_thread) thread_type else result_type));
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
        const branch_stack_base = self.currentFrame().rel_stack_counter;
        try self.jmp(source, condition, false, after_addr);
        const narrow = self.narrowConditionSubject(if_expr.condition);
        const then_result = try self.compileIfBranchExpression(source, if_expr.then_expr, if_condition.capture_binding);
        restoreNarrow(narrow);
        if (isWaitable(then_result)) |loc| {
            try self.wait(source, loc);
        }
        // Emit real `pop`s for the runtime slots the branch body pushed via
        // `.ref` (a bare `rel_stack_counter = base` reset leaks them, drifting
        // the value stack — a later deref then hits a leaked slot). The pops sit
        // before `after_addr`, so the false path (which jumped straight here) is
        // balanced too. Same discipline as the catch handler / match case body.
        try self.popToStackBase(source, branch_stack_base);
        try self.setLabel(after_addr.local_addr.label, .abs);
        return .fromValue(.void);
    }

    /// Pops runtime stack slots until the counter returns to `base`, emitting a
    /// real `pop` per slot rather than bare-resetting the counter (which would
    /// leak the slots).
    fn popToStackBase(self: *IRCompiler, source: anytype, base: usize) Error!void {
        while (self.currentFrame().rel_stack_counter > base) {
            _ = try self.pop(source);
        }
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
                // Stash into a stable ref first: the condition may be a volatile
                // register (e.g. an in-process typed capture of an optional-
                // returning function), which would be clobbered before the
                // capture binding reads it.
                const cond_ref = try self.newRef(source, "if_optional_cond");
                try self.set(source, cond_ref, stableResultSource(condition));

                const is_present_ref = try self.newRef(source, "if_optional_present");
                try self.cmp(
                    source,
                    .not_equal,
                    .from(cond_ref.dereference()),
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

                        break :blk .{
                            .pattern = capture_clause.bindings[0],
                            .value = .fromLocation(cond_ref.dereference().typed(optional.child.*)),
                        };
                    } else null,
                };
            },
            // `if (errorUnion)` is true when the value is ok (not an error);
            // an `|value|` capture binds the ok payload.
            .error_union, .error_set, .err => {
                const cond_ref = try self.newRef(source, "if_error_cond");
                try self.set(source, cond_ref, stableResultSource(condition));

                const is_err_ref = try self.newRef(source, "if_is_err");
                try self.addInstruction(.init(.from(source), .{ .is_err = .{
                    .operand = cond_ref.dereference(),
                    .result = is_err_ref,
                } }));
                const is_ok_ref = try self.newRef(source, "if_is_ok");
                try self.addInstruction(.init(.from(source), .{ .neg = .{
                    .operand = is_err_ref.dereference(),
                    .result = is_ok_ref,
                } }));

                return .{
                    .condition = try .from(is_ok_ref.dereference()),
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

                        var capture_value: ir.ValueSource = .fromLocation(cond_ref.dereference());
                        if (condition_type_ == .error_union) {
                            capture_value.location = capture_value.location.typed(condition_type_.error_union.payload.*);
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

    fn mergeResultTypes(
        self: *IRCompiler,
        current: ?ast.TypeExpr,
        next: Result,
    ) ?ast.TypeExpr {
        _ = self;
        const next_type = next.typeExpr() orelse return null;
        const current_type = current orelse return next_type;
        if (std.meta.eql(current_type, next_type)) return current_type;
        return null;
    }

    fn allocExpression(
        self: *IRCompiler,
        expr: ast.Expression,
    ) Error!*ast.Expression {
        const ptr = try self.allocator.create(ast.Expression);
        ptr.* = expr;
        return ptr;
    }

    fn allocPathExpression(
        self: *IRCompiler,
        path: ast.Path,
    ) Error!*ast.Expression {
        var expr = try self.allocExpression(.{ .identifier = path.segments[0] });
        for (path.segments[1..]) |segment| {
            expr = try self.allocExpression(.{ .member = .{
                .object = expr,
                .member = segment,
                .span = expr.span().endAt(segment.span),
            } });
        }
        return expr;
    }

    fn allocMatcherCallExpression(
        self: *IRCompiler,
        callee: *ast.Expression,
        subject_identifier: ast.Identifier,
        span: ast.Span,
    ) Error!*ast.Expression {
        const subject_expr = try self.allocExpression(.{ .identifier = subject_identifier });
        const args = try self.allocator.alloc(*ast.Expression, 1);
        args[0] = subject_expr;
        return self.allocExpression(.{ .call = .{
            .callee = callee,
            .arguments = args,
            .redirects = &.{},
            .span = span,
        } });
    }

    /// Compiles a match case body, binding the optional error-payload capture
    /// (`Set.Variant => |payload| ...`) in a fresh scope.
    fn compileMatchCaseBody(
        self: *IRCompiler,
        source: *ast.Expression,
        case: ast.MatchCase,
        subject_ref: ir.Location,
        is_type_pattern: bool,
    ) Error!Result {
        const capture = case.capture orelse return self.compileBlock(source, case.body);

        // The capture binding pushes transient slots; carry the body result in
        // a register and restore the stack counter so they don't accumulate
        // (mirrors `compileIfBranchExpression`/`compileIfElse`).
        const stack_base = self.currentFrame().rel_stack_counter;

        try self.scopes.push(self.allocator, .lexical);
        defer self.scopes.pop();

        // A sum member-type capture binds the (narrowed) subject value itself;
        // an error-variant capture binds the extracted payload.
        const bound_ref = try self.newRef(source, "match_capture");
        if (is_type_pattern) {
            try self.set(source, bound_ref, .from(subject_ref.dereference()));
        } else {
            try self.addInstruction(.init(.from(source), .{ .err_payload = .{
                .operand = subject_ref.dereference(),
                .result = bound_ref,
            } }));
        }

        switch (capture.bindings[0].*) {
            .discard => {},
            .identifier => |identifier| try self.compileIdentifierBinding(
                source,
                identifier,
                .from(bound_ref.dereference()),
                null,
                false,
                .normal,
            ),
            else => {
                try self.reportSourceError(
                    source,
                    Error.UnsupportedBindingPattern,
                    .@"error",
                    "match capture binding pattern not yet supported",
                    .{},
                );
                return .fromValue(.void);
            },
        }

        const body_result = try self.compileBlock(source, case.body);
        try self.set(source, .initRegister(.r2), stableResultSource(body_result));
        // Pop the payload/binding slots (the result is safe in r2) so the
        // runtime stack matches the counter — a bare counter reset would leave
        // the slots on the runtime stack and mis-address later code.
        while (self.currentFrame().rel_stack_counter > stack_base) {
            _ = try self.pop(source);
        }
        return .fromLocation(ir.Location.initRegister(.r2).typed(body_result.typeExpr()));
    }

    fn compileMatchPredicate(
        self: *IRCompiler,
        source: *ast.Expression,
        pattern: ast.MatchPattern,
        subject_identifier: ast.Identifier,
    ) Error!Result {
        const callee = switch (pattern) {
            .binding => |binding| try self.allocExpression(.{ .identifier = binding }),
            .path => |path| try self.allocPathExpression(path),
            else => {
                try self.reportSourceError(
                    source,
                    Error.UnsupportedExpression,
                    .@"error",
                    "match currently supports only literal, predicate function, and _ patterns",
                    .{},
                );
                return .fromValue(.void);
            },
        };

        const call_expr = try self.allocMatcherCallExpression(callee, subject_identifier, pattern.span());
        return self.compileExpression(call_expr);
    }

    fn compileMatch(
        self: *IRCompiler,
        source: *ast.Expression,
        match_expr: ast.MatchExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (match_expr.cases.len == 0) {
            try self.reportSourceError(
                source,
                Error.UnsupportedExpression,
                .@"error",
                "match requires at least one case",
                .{},
            );
            return .fromValue(.void);
        }

        if (try self.evalComptimeExpression(match_expr.subject)) |subject| {
            for (match_expr.cases) |case| {
                if (case.capture != null) break;
                switch (case.pattern) {
                    .wildcard => return self.compileBlock(source, case.body),
                    .literal => |literal| {
                        const pattern = (try self.evalComptimeLiteral(literal)) orelse continue;
                        if (self.comptimeValueEql(subject, pattern)) {
                            return self.compileBlock(source, case.body);
                        }
                    },
                    else => break,
                }
            }
        }

        try self.scopes.push(self.allocator, .lexical);
        defer self.scopes.pop();

        const subject_ref = try self.newRef(source, "match_subject");
        const subject_result = try self.compileStableExpressionIntoRef(source, match_expr.subject, subject_ref);
        const subject_identifier: ast.Identifier = .{ .name = "__match_subject", .span = match_expr.subject.span() };
        try self.compileIdentifierBinding(
            source,
            subject_identifier,
            .from(subject_ref.dereference().typed(subject_result.typeExpr())),
            null,
            true,
            .normal,
        );

        const result_ref = try self.newRef(source, "match_result");
        const after_addr = try self.newLabel("match_after", .unknown);
        var result_type: ?ast.TypeExpr = null;
        var has_wildcard = false;

        for (match_expr.cases, 0..) |case, i| {
            const next_case_addr = try self.newLabel(
                try std.fmt.allocPrint(self.allocator, "match_case_next_{}", .{i}),
                .unknown,
            );

            // `Set.Variant` where `Set` is a known error set matches an error value.
            const is_error_variant = switch (case.pattern) {
                .path => |path| path.segments.len >= 2 and self.error_sets.get(path.segments[0].name) != null,
                else => false,
            };
            // `Int`/`String`/… — a sum member-type pattern (a type-tag test).
            const is_type_pattern = switch (case.pattern) {
                .binding => |binding| typeTagForName(binding.name) != null,
                else => false,
            };

            switch (case.pattern) {
                .wildcard => {
                    has_wildcard = true;
                },
                .literal => |literal| {
                    const pattern = try self.compileLiteral(source, literal);
                    // Use a register for the transient test result so the case
                    // loop pushes no stack slots (which would drift the counter).
                    try self.cmp(
                        source,
                        .equal,
                        .from(subject_ref.dereference()),
                        pattern.source,
                        .initRegister(.r2),
                    );
                    try self.jmp(source, .fromLocation(.initRegister(.r2)), false, next_case_addr);
                },
                .path => |path| {
                    if (is_error_variant) {
                        try self.addInstruction(.init(.from(source), .{ .match_err = .{
                            .operand = subject_ref.dereference(),
                            .set = path.segments[0].name,
                            .variant = path.segments[path.segments.len - 1].name,
                            .result = .initRegister(.r2),
                        } }));
                        try self.jmp(source, .fromLocation(.initRegister(.r2)), false, next_case_addr);
                    } else {
                        const predicate = try self.compileMatchPredicate(source, case.pattern, subject_identifier);
                        try self.jmp(source, predicate, false, next_case_addr);
                    }
                },
                .binding => |binding| {
                    // A member-type name (`Int`, `String`, …) → a runtime type
                    // tag test (sum type-match). Otherwise a predicate function.
                    if (typeTagForName(binding.name)) |tag| {
                        try self.addInstruction(.init(.from(source), .{ .is_type = .{
                            .operand = subject_ref.dereference(),
                            .tag = tag,
                            .result = .initRegister(.r2),
                        } }));
                        try self.jmp(source, .fromLocation(.initRegister(.r2)), false, next_case_addr);
                    } else {
                        const predicate = try self.compileMatchPredicate(source, case.pattern, subject_identifier);
                        try self.jmp(source, predicate, false, next_case_addr);
                    }
                },
                else => unreachable,
            }

            // A capture (`|value|`) binds an error variant's payload, or — for a
            // sum member-type pattern — the narrowed subject value itself.
            if (case.capture) |capture| {
                if ((!is_error_variant and !is_type_pattern) or capture.bindings.len != 1) {
                    try self.reportSourceError(
                        source,
                        Error.UnsupportedExpression,
                        .@"error",
                        "match captures are only supported for error variants or sum member types with a single binding",
                        .{},
                    );
                    return .fromValue(.void);
                }
            }

            const case_result = try self.compileMatchCaseBody(source, case, subject_ref, is_type_pattern);
            try self.set(source, result_ref, stableResultSource(case_result));
            result_type = self.mergeResultTypes(result_type, case_result);
            try self.jmp(source, null, false, after_addr);
            try self.setLabel(next_case_addr.local_addr.label, .abs);
        }

        if (!has_wildcard) {
            try self.exit_(source, .fromValue(.fromBoolean(false)));
        }

        try self.setLabel(after_addr.local_addr.label, .abs);
        try self.set(source, .initRegister(.r2), .from(result_ref.dereference()));
        return .fromLocation(ir.Location.initRegister(.r2).typed(result_type));
    }

    fn refLocation(self: *IRCompiler, ref_def: RefDef) ir.Location {
        return .fromRef(
            ref_def.name,
            self.currentFrame().rel_stack_counter - ref_def.rel_stack_addr,
        );
    }

    /// The kind of transport used (or planned) between two adjacent pipeline stages.
    const PipeBoundaryKind = enum {
        /// Both stages carry a matching non-void, non-execution typed value.
        /// Currently still compiled with byte pipes; will use direct value
        /// passing once &0 (stdin) is supported.
        exact_typed,
        /// At least one stage is an external executable whose return type is
        /// ExecutionResult (mapped to String at boundaries). Byte pipes required.
        byte_stream,
        /// One side is Void; no data is transported.
        void_boundary,
    };

    /// Whether `stage` is the unshadowed builtin pipeline stage named `name`
    /// (e.g. `parseInt`, `lines`), which has no scope binding.
    fn isBuiltinStage(self: *IRCompiler, stage: *ast.Expression, name: []const u8) bool {
        const callee = switch (stage.*) {
            .call => |call| call.callee,
            else => stage,
        };
        return callee.* == .identifier and
            std.mem.eql(u8, callee.identifier.name, name) and
            self.lookup(callee.identifier.name, .{ .shallow = false }) == null;
    }

    /// Determine how to connect the given stage to the pipeline at runtime.
    /// Returns the kind of transport that would ideally be used.
    fn classifyStageOutputKind(self: *IRCompiler, stage: *ast.Expression) PipeBoundaryKind {
        const callee = switch (stage.*) {
            .call => |call| call.callee,
            else => stage,
        };

        // parseInt/parseFloat produce a scalar; lines produces a framed stream
        // of String values. All use typed (queue) transport.
        if (self.isBuiltinStage(stage, "parseInt") or
            self.isBuiltinStage(stage, "parseFloat") or
            self.isBuiltinStage(stage, "lines"))
        {
            return .exact_typed;
        }

        // A block stage's output kind comes from what it yields (see
        // `inferBlockStdoutType`): a scalar yield enables typed transport.
        if (callee.* == .block) {
            const inferred = self.inferBlockStdoutType(callee.block) orelse return .byte_stream;
            return switch (inferred) {
                .void => .void_boundary,
                else => .exact_typed,
            };
        }

        const binding = switch (callee.*) {
            .identifier => |id| self.lookup(id.name, .{ .shallow = false }),
            else => null,
        } orelse return .byte_stream;

        const fn_type = switch (binding.type_expr orelse return .byte_stream) {
            .function => |f| f,
            else => return .byte_stream,
        };

        const return_type = fn_type.return_type orelse return .byte_stream;
        return switch (return_type.*) {
            .void => .void_boundary,
            .execution => .byte_stream,
            else => .exact_typed,
        };
    }

    fn classifyStageInputKind(self: *IRCompiler, stage: *ast.Expression) PipeBoundaryKind {
        const callee = switch (stage.*) {
            .call => |call| call.callee,
            else => stage,
        };
        // parseInt/parseFloat map over their input stream value-by-value, so
        // they accept a typed (framed) stream from `lines`. (An executable/byte
        // upstream still forces the byte path via the upstream's output kind,
        // where they read the whole blob as a single value.)
        if (self.isBuiltinStage(stage, "parseInt") or self.isBuiltinStage(stage, "parseFloat")) return .exact_typed;
        // A block stage declares no stdin type — it adapts to the upstream
        // stage (its `&0` is typed by inference). Treat it as permissive so the
        // upstream's output kind decides the boundary; an executable upstream
        // still forces the byte path via its own output kind.
        if (callee.* == .block) return .exact_typed;
        const binding = switch (callee.*) {
            .identifier => |id| self.lookup(id.name, .{ .shallow = false }),
            else => null,
        } orelse return .byte_stream;

        const fn_type = switch (binding.type_expr orelse return .byte_stream) {
            .function => |f| f,
            else => return .byte_stream,
        };

        const stdin_type = fn_type.stdin_type orelse return .byte_stream;
        return switch (stdin_type.*) {
            .void => .void_boundary,
            .execution => .byte_stream,
            else => .exact_typed,
        };
    }

    fn classifyBoundary(
        self: *IRCompiler,
        upstream: *ast.Expression,
        downstream: *ast.Expression,
    ) PipeBoundaryKind {
        const out_kind = self.classifyStageOutputKind(upstream);
        const in_kind = self.classifyStageInputKind(downstream);

        if (out_kind == .byte_stream or in_kind == .byte_stream) return .byte_stream;
        if (out_kind == .void_boundary or in_kind == .void_boundary) return .void_boundary;
        return .exact_typed;
    }

    /// Resolves the concrete stdout type a pipeline stage produces, so a
    /// downstream stage that does not declare its own stdin type (a block or a
    /// bare expression) can have its `&0` typed by inference. Returns null
    /// when the type is unknown (e.g. an external executable), in which case
    /// `&0` falls back to String.
    fn stageStdoutType(self: *IRCompiler, stage: *ast.Expression) ?ast.TypeExpr {
        const callee = switch (stage.*) {
            .call => |call| call.callee,
            else => stage,
        };

        if (self.isBuiltinStage(stage, "parseInt")) return .global(.integer);
        if (self.isBuiltinStage(stage, "parseFloat")) return .global(.float);
        // `lines` frames its byte input into per-line String values.
        if (self.isBuiltinStage(stage, "lines")) return string_type;

        // A block/bare-expression stage has no signature; infer its stdout type
        // from what it `yield`s to stdout (&1), so a `{ yield 1; ... }` producer
        // is recognized as an `Int` stage and gets in-process typed transport.
        if (callee.* == .block) {
            return self.inferBlockStdoutType(callee.block);
        }

        const binding = switch (callee.*) {
            .identifier => |id| self.lookup(id.name, .{ .shallow = false }),
            else => null,
        } orelse return null;

        const fn_type = switch (binding.type_expr orelse return null) {
            .function => |f| f,
            else => return null,
        };

        const return_type = fn_type.return_type orelse return null;
        return switch (return_type.*) {
            // Executables surface ExecutionResult; at a pipe boundary that is bytes.
            .execution => string_type,
            else => return_type.*,
        };
    }

    /// When `expr` is a pipeline in which *any* stage can produce an error (e.g.
    /// `… | parseInt → ParseError`), returns the pipeline's value-capture type
    /// `E!T`: that error set with the final stage's ok output as the payload. An
    /// upstream error short-circuits through the downstream stages (arithmetic on
    /// an `.err` propagates it) and surfaces as the result, so the whole pipeline
    /// is captured via typed transport — letting `catch`/`try` see the real error
    /// rather than its flattened text. Returns null when no stage can error (the
    /// byte capture path applies). Mid-pipeline stages keep their `Int`/`Float`
    /// transport (`stageStdoutType`); this only governs the *final* capture.
    fn pipelineCaptureErrorUnionType(self: *IRCompiler, expr: *ast.Expression) Error!?ast.TypeExpr {
        if (expr.* != .pipeline) return null;
        const stages = expr.pipeline.stages;
        if (stages.len == 0) return null;

        // Find an error-producing stage anywhere in the pipeline. Track its
        // error set and natural payload (used as the fallback ok type below).
        const ErrInfo = struct { set: *const ast.TypeExpr, payload: ast.TypeExpr };
        const err_info: ErrInfo = blk: {
            for (stages) |stage| {
                if (self.isBuiltinStage(stage, "parseInt")) break :blk .{ .set = &ast.TypeExpr.parseErrorType, .payload = .global(.integer) };
                if (self.isBuiltinStage(stage, "parseFloat")) break :blk .{ .set = &ast.TypeExpr.parseErrorType, .payload = .global(.float) };
                const callee = switch (stage.*) {
                    .call => |call| call.callee,
                    else => stage,
                };
                const binding = switch (callee.*) {
                    .identifier => |id| self.lookup(id.name, .{ .shallow = false }),
                    else => null,
                } orelse continue;
                const fn_type = switch (binding.type_expr orelse continue) {
                    .function => |f| f,
                    else => continue,
                };
                const return_type = fn_type.return_type orelse continue;
                if (return_type.* == .error_union) break :blk .{ .set = return_type.error_union.err_set, .payload = return_type.error_union.payload.* };
            }
            return null;
        };

        // Payload = the final stage's ok output type (what flows when no error).
        // A block final stage infers its output from `&0`, so make the upstream
        // stage's stdout type visible as the stdin type while inferring it; fall
        // back to the error producer's payload when inference can't resolve it
        // (the typed capture must still fire so `catch`/`try` see the error).
        const last = stages[stages.len - 1];
        const upstream_stdout: ?ast.TypeExpr = if (stages.len >= 2)
            self.stageStdoutType(stages[stages.len - 2])
        else
            null;
        if (upstream_stdout) |u| try self.stdin_type_stack.append(self.allocator, u);
        const payload_opt = self.stageStdoutType(last);
        if (upstream_stdout != null) _ = self.stdin_type_stack.pop();
        const payload = payload_opt orelse err_info.payload;
        const payload_ptr = try self.allocator.create(ast.TypeExpr);
        payload_ptr.* = payload;
        return ast.TypeExpr{ .error_union = .{
            .err_set = err_info.set,
            .payload = payload_ptr,
            .span = .global,
        } };
    }

    /// Whether capturing `expr` as a value yields an error union — an
    /// error-union-returning function call or a pipeline with an error stage.
    /// Such an `||`/`&&` LHS must be captured via `compileExpressionWithCapture`
    /// (the typed capture) so its error is observed, not via the exit-code path.
    fn lhsCapturesError(self: *IRCompiler, expr: *ast.Expression) Error!bool {
        if ((try self.pipelineCaptureErrorUnionType(expr)) != null) return true;
        if (expr.* != .call) return false;
        const call = expr.call;
        if (call.background or call.redirects.len != 0 or call.callee.* != .identifier) return false;
        const binding = self.lookup(call.callee.identifier.name, .{ .shallow = false }) orelse return false;
        if (!binding.result.isFunctionRef()) return false;
        const fn_type = binding.type_expr orelse return false;
        if (fn_type != .function) return false;
        const return_type = fn_type.function.return_type orelse return false;
        return return_type.* == .error_union;
    }

    /// A loop capture in scope during block-stdout inference, mapping the
    /// capture name to the element type its source iterates (e.g. `i` -> `Int`
    /// for `for (0..5) |i|`). Needed because a producer block often yields a
    /// capture (`{ for (0..5) |i| { yield i } }`) that is not yet a compile-time
    /// binding when the boundary is classified.
    const InferCapture = struct { name: []const u8, type_expr: ast.TypeExpr };

    /// Infers a block stage's stdout type from the first value it `yield`s to
    /// stdout (`&1`), recursing into nested `for`/`while`/`if`/`match`/block
    /// bodies (a yield inside a loop still determines the block's output type).
    /// Returns null when no stdout `yield` is found or its type cannot be
    /// determined, in which case the caller falls back to the byte path.
    fn inferBlockStdoutType(self: *IRCompiler, block: ast.Block) ?ast.TypeExpr {
        return self.findStdoutYieldType(block.statements, &.{});
    }

    fn findStdoutYieldType(
        self: *IRCompiler,
        statements: []const *ast.Statement,
        captures: []const InferCapture,
    ) ?ast.TypeExpr {
        for (statements) |statement| {
            const found = switch (statement.*) {
                .yield_stmt => |y| if (y.fd == 1) self.inferYieldValueType(y.value, captures) else null,
                .expression => |e| self.findStdoutYieldTypeInExpr(e.expression, captures),
                .while_stmt => |w| self.findStdoutYieldType(w.body.statements, captures),
                else => null,
            };
            if (found) |t| return t;
        }
        return null;
    }

    fn findStdoutYieldTypeInExpr(
        self: *IRCompiler,
        expr: *ast.Expression,
        captures: []const InferCapture,
    ) ?ast.TypeExpr {
        return switch (expr.*) {
            .block => |b| self.findStdoutYieldType(b.statements, captures),
            .for_expr => |f| self.findForBodyYieldType(f, captures),
            .if_expr => |i| self.findStdoutYieldTypeInIf(i, captures),
            .match_expr => |m| blk: {
                for (m.cases) |case| {
                    if (self.findStdoutYieldType(case.body.statements, captures)) |t| break :blk t;
                }
                break :blk null;
            },
            else => null,
        };
    }

    fn findStdoutYieldTypeInIf(
        self: *IRCompiler,
        if_expr: ast.IfExpr,
        captures: []const InferCapture,
    ) ?ast.TypeExpr {
        if (self.findStdoutYieldTypeInExpr(if_expr.then_expr, captures)) |t| return t;
        const else_branch = if_expr.else_branch orelse return null;
        return switch (else_branch) {
            .expr => |e| self.findStdoutYieldTypeInExpr(e, captures),
            .if_expr => |ei| self.findStdoutYieldTypeInIf(ei.*, captures),
            .condition => null,
        };
    }

    /// Recurses into a `for` body with the loop's captures added to scope, so a
    /// `yield <capture>` (or arithmetic on one) resolves to the source's element
    /// type. Only ranges (always `Int`) are resolved for now; other sources
    /// leave the capture unknown (inference falls back to the byte path).
    fn findForBodyYieldType(
        self: *IRCompiler,
        for_expr: ast.ForExpr,
        captures: []const InferCapture,
    ) ?ast.TypeExpr {
        var buf: [8]InferCapture = undefined;
        var n: usize = 0;
        for (captures) |c| {
            if (n >= buf.len) break;
            buf[n] = c;
            n += 1;
        }
        const count = @min(for_expr.sources.len, for_expr.capture.bindings.len);
        for (for_expr.sources[0..count], for_expr.capture.bindings[0..count]) |src, pat| {
            const elem_type: ast.TypeExpr = switch (src.*) {
                .range => ast.TypeExpr.global(.integer),
                else => continue,
            };
            const name = switch (pat.*) {
                .identifier => |id| id.name,
                else => continue,
            };
            if (n >= buf.len) break;
            buf[n] = .{ .name = name, .type_expr = elem_type };
            n += 1;
        }
        return self.findStdoutYieldTypeInExpr(for_expr.body, buf[0..n]);
    }

    /// Best-effort, pre-compilation type inference for a `yield`ed expression.
    /// Handles the cases that matter for transport classification (literals,
    /// arithmetic on them, `&0`, loop captures, and simple bindings); returns
    /// null otherwise.
    fn inferYieldValueType(self: *IRCompiler, expr: *ast.Expression, captures: []const InferCapture) ?ast.TypeExpr {
        return switch (expr.*) {
            .literal => |literal| switch (literal) {
                .integer => ast.TypeExpr.global(.integer),
                .float => ast.TypeExpr.global(.float),
                .string => string_type,
                else => null,
            },
            // Arithmetic preserves the operand's numeric type.
            .binary => |binary| switch (binary.op) {
                .add, .subtract, .multiply, .divide, .remainder => self.inferYieldValueType(binary.left, captures),
                else => null,
            },
            // `&0` carries the stage's inferred stdin type (pushed by the
            // pipeline compiler before the stage is classified/compiled).
            .fd => |fd_expr| if (fd_expr.fd == 0 and self.stdin_type_stack.items.len > 0)
                self.stdin_type_stack.items[self.stdin_type_stack.items.len - 1]
            else
                null,
            .identifier => |id| self.inferNamedValueType(id.name, captures),
            // A bare value reference parses as a zero-arg call (`yield i` →
            // call `i()`). Resolve it like an identifier; a real call (with
            // args) is left unknown.
            .call => |call| if (call.arguments.len == 0 and call.callee.* == .identifier)
                self.inferNamedValueType(call.callee.identifier.name, captures)
            else
                null,
            else => null,
        };
    }

    fn inferNamedValueType(self: *IRCompiler, name: []const u8, captures: []const InferCapture) ?ast.TypeExpr {
        for (captures) |c| {
            if (std.mem.eql(u8, c.name, name)) return c.type_expr;
        }
        const binding = self.lookup(name, .{ .shallow = false }) orelse return null;
        return binding.type_expr;
    }

    /// Whether the boundary between two stages should use in-process typed
    /// transport: an exact boundary carrying a by-value scalar (`Int`/`Float`),
    /// or a `lines` stage's framed per-line String stream. String/byte
    /// boundaries and any boundary touching an executable keep the byte path.
    fn boundaryUsesTypedTransport(
        self: *IRCompiler,
        upstream: *ast.Expression,
        downstream: *ast.Expression,
    ) bool {
        if (self.classifyBoundary(upstream, downstream) != .exact_typed) return false;
        // `lines` enqueues each line as a distinct value; the framed stream
        // needs the typed (queue) path even though it carries String values.
        if (self.isBuiltinStage(upstream, "lines")) return true;
        const out_type = self.stageStdoutType(upstream) orelse return false;
        return typeExprIsNamed(out_type, "Int") or typeExprIsNamed(out_type, "Float");
    }

    /// Pipeline↔param coercion: a stage that is a zero-arg reference to a
    /// single-parameter function receives the upstream value in that parameter.
    /// Collects stdin into a value, binds it to the parameter, and calls the
    /// function (whose output flows to the stage's stdout). Returns null when the
    /// stage isn't such a reference (fall back to normal stage compilation).
    fn tryCompilePipelineParamStage(self: *IRCompiler, source: *ast.Expression, stage_expr: *ast.Expression) Error!?Result {
        const callee: *ast.Expression = switch (stage_expr.*) {
            .call => |call| if (call.arguments.len == 0 and call.redirects.len == 0) call.callee else return null,
            .identifier => stage_expr,
            else => return null,
        };
        if (callee.* != .identifier) return null;
        const binding = self.lookup(callee.identifier.name, .{ .shallow = false }) orelse return null;
        if (!binding.result.isFunctionRef()) return null;
        const fn_ref = binding.result.source.value;
        if (self.instruction_sets.items[fn_ref.fn_ref.fn_addr.instr_set].param_count != 1) return null;

        // The single parameter's type. `collect_stdin` (== `&0`) dequeues the
        // upstream value typed when the boundary is typed (Int/Float/…) and as a
        // String byte blob otherwise, matching the parameter type either way.
        const param_type: ast.TypeExpr = blk: {
            if (binding.type_expr) |t| if (t == .function) switch (t.function.params) {
                ._non_variadic => |ps| if (ps.len == 1) {
                    if (ps[0]) |pt| break :blk pt.*;
                },
                ._variadic => {},
            };
            break :blk string_type;
        };

        // Collect the upstream value and bind it to the single parameter.
        try self.addInstruction(.init(.from(source), .collect_stdin));
        const input_ref = try self.newRef(source, "pipe_param_input");
        try self.set(source, input_ref, .fromLocation(.initRegister(.r)));

        const input_name = "\x00pipe_param_input";
        try self.scopes.declare(
            self.allocator,
            input_name,
            try .from(input_ref.dereference().typed(param_type)),
            param_type,
            false,
            .normal,
        );
        const arg_expr = try self.allocator.create(ast.Expression);
        arg_expr.* = .{ .identifier = .{ .name = input_name, .span = source.span() } };

        return try self.compileFunctionCall(source, fn_ref, &.{arg_expr}, &.{}, null);
    }

    fn compilePipeline(
        self: *IRCompiler,
        source: *ast.Expression,
        pipeline: ast.Pipeline,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const refs = try self.allocator.alloc(ir.Location, pipeline.stages.len - 1);
        defer self.allocator.free(refs);

        for (refs, 0..) |*ref, i| {
            ref.* = try self.newRef(source, "pipe");
            try self.pipe(source, ref.dereference());
            try self.pipeOpt(
                source,
                ref.dereference(),
                .keep_open,
                .fromValue(.fromBoolean(true)),
            );

            // In-process typed transport: when an exact boundary carries a
            // by-value scalar (Int/Float), mark the inter-stage pipe `typed` so
            // the upstream `yield` stores the value directly and the downstream
            // `&0` reads it back, skipping the text serialize/re-parse round-trip.
            if (self.boundaryUsesTypedTransport(pipeline.stages[i], pipeline.stages[i + 1])) {
                try self.pipeOpt(
                    source,
                    ref.dereference(),
                    .typed,
                    .fromValue(.fromBoolean(true)),
                );
            }
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

        for (pipeline.stages[0 .. pipeline.stages.len - 1], pipeline.stages[1..], 0..) |upstream, downstream, i| {
            const kind = self.classifyBoundary(upstream, downstream);
            try self.comment("boundary {}: {s}", .{ i, @tagName(kind) });
        }

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

        for (stage_sets, stage_closures, pipeline.stages[0 .. pipeline.stages.len - 1], pipeline.stages[1..], 0..) |*stage_set, *stage_closure, stage_expr, next_expr, i| {
            self.current_instruction_set = stage_set.*;
            try self.scopes.push(self.allocator, .closure);
            const stdout_stream_thread_ref = try self.newRef(source, "pipeline_stdout_stream_thread");
            try self.set(
                source,
                stdout_stream_thread_ref,
                .from(try self.forkInherit(source, self.stdoutStreamSet(), .noll)),
            );
            _ = next_expr;

            // Infer the stage's &0 type from the upstream stage's stdout, so
            // a block/bare-expression stage that does not declare a stdin type
            // (e.g. `... | parseInt | { yield &0 * &0 }`) reads a typed value.
            // The first stage has no upstream stage to infer from.
            const inferred_stdin: ?ast.TypeExpr = if (i > 0)
                self.stageStdoutType(pipeline.stages[i - 1])
            else
                null;
            const pushed_stdin = i > 0;
            if (pushed_stdin) try self.stdin_type_stack.append(self.allocator, inferred_stdin);
            // Only a receiver stage (not the producer at i==0) can bind stdin to
            // a parameter.
            const result = if (i > 0)
                (try self.tryCompilePipelineParamStage(source, stage_expr)) orelse try self.compileExpression(stage_expr)
            else
                try self.compileExpression(stage_expr);
            if (pushed_stdin) _ = self.stdin_type_stack.pop();

            // A stage's value is no longer auto-pushed to stdout; output is
            // explicit via `yield` (or subprocess writes). Just wait for a
            // waitable stage to finish, then signal completion downstream.
            if (isWaitable(result)) |loc| {
                try self.comment("wait from {s}", .{@src().fn_name});
                try self.wait(source, loc);
            }
            try self.pipeOpt(
                source,
                self.threadStdout(),
                .keep_open,
                .fromValue(.fromBoolean(false)),
            );

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
        const last_idx = pipeline.stages.len - 1;
        const last_pushed_stdin = last_idx > 0;
        if (last_pushed_stdin) try self.stdin_type_stack.append(
            self.allocator,
            self.stageStdoutType(pipeline.stages[last_idx - 1]),
        );
        const result = (try self.tryCompilePipelineParamStage(source, pipeline.stages[last_idx])) orelse
            try self.compileExpression(pipeline.stages[last_idx]);
        if (last_pushed_stdin) _ = self.stdin_type_stack.pop();
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

    fn compileImportExpr(
        self: *IRCompiler,
        source: *ast.Expression,
        import_expr: ast.ImportExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        // 1. Resolve module path
        const module_path = resolveModulePath(
            self.io,
            self.allocator,
            import_expr.importer,
            import_expr.module_name,
        ) catch {
            try self.reportSourceError(source, Error.UnsupportedExpression, .@"error", "cannot resolve module path \"{s}\"", .{import_expr.module_name});
            return .fromValue(.void);
        };
        defer self.allocator.free(module_path);

        // Cycle detection: error if this module is already being compiled up the call stack
        if (self.loading_set.contains(module_path)) {
            try self.reportSourceError(source, Error.UnsupportedExpression, .@"error", "circular import detected for \"{s}\"", .{import_expr.module_name});
            return .fromValue(.void);
        }

        // 2. Get module AST
        const module_ast = self.document_store.getAst(module_path) catch {
            try self.reportSourceError(source, Error.UnsupportedExpression, .@"error", "module \"{s}\" not found", .{import_expr.module_name});
            return .fromValue(.void);
        } orelse {
            try self.reportSourceError(source, Error.UnsupportedExpression, .@"error", "module \"{s}\" not found", .{import_expr.module_name});
            return .fromValue(.void);
        };

        // 3. Compile module statements as a new instruction set (compile-time cache)
        const module_instr_set: usize = if (self.compiled_modules.get(module_path)) |cached| cached else blk: {
            const instr_set = try self.addInstructionSet();
            const orig_instr_set = self.current_instruction_set;
            self.current_instruction_set = instr_set;

            // Mark this module as in-flight so nested imports of the same path error
            try self.loading_set.put(self.allocator, module_path, {});
            try self.scopes.push(self.allocator, .closure);

            // Register the module's type constructors / structs / error sets so a
            // module like `std.map` can construct and apply its own `Map(K, V)`.
            try self.registerTypeDecls(module_ast.statements);

            for (module_ast.statements) |stmt| {
                _ = try self.compileStatement(stmt);
            }

            // Pub-export epilogue: write each pub binding to a closure slot
            const module_frame = try self.scopes.getFrame(0);
            const pub_slot_base = module_frame.closure_bindings.items.len;
            var pub_exports_list = std.ArrayListUnmanaged(InstructionSet.PubExport).empty;
            {
                var frame_iter = module_frame.bindings.iterator();
                while (frame_iter.next()) |entry| {
                    if (!entry.value_ptr.is_pub) continue;
                    const slot = pub_slot_base + pub_exports_list.items.len;
                    try self.set(
                        source,
                        ir.Location.initAdd(.closure, slot, .{}),
                        entry.value_ptr.result.source,
                    );
                    try pub_exports_list.append(self.allocator, .{
                        .name = entry.key_ptr.*,
                        .slot = slot,
                        .type_expr = entry.value_ptr.type_expr,
                        .fn_ref_value = if (entry.value_ptr.result.source == .value and
                            entry.value_ptr.result.source.value == .fn_ref)
                            entry.value_ptr.result.source.value
                        else
                            null,
                    });
                }
            }
            try self.exitWith(source, .fromValue(.fromBoolean(true)));
            try self.setClosureIdentifiers();
            const n_pub_inner = pub_exports_list.items.len;
            self.currentInstrSet().closure_slot_count += n_pub_inner;
            self.currentInstrSet().pub_exports = try pub_exports_list.toOwnedSlice(self.allocator);

            self.current_instruction_set = orig_instr_set;
            self.scopes.pop();
            _ = self.loading_set.remove(module_path);

            const path_owned = try self.allocator.dupe(u8, module_path);
            try self.compiled_modules.put(self.allocator, path_owned, instr_set);
            break :blk instr_set;
        };

        const module_fn_addr = ir.InstructionAddr.initAbs(module_instr_set, 0);
        const pub_exports = self.instruction_sets.items[module_instr_set].pub_exports;
        const n_pub = pub_exports.len;

        // Allocate path string for runtime cache instructions (lifetime = instruction set lifetime)
        const module_path_ir = try self.allocator.dupe(u8, module_path);

        // Pre-declare ALL refs before the cache check so the stack layout is consistent on
        // both cache-hit and cache-miss paths (jumped-over `ref` instructions would corrupt it).
        const result_ref = try self.newRef(source, "import_result");
        const stdout_pipe_ref = try self.newRef(source, "import_stdout_pipe");
        const stderr_pipe_ref = try self.newRef(source, "import_stderr_pipe");
        const merged_pipe_ref = try self.newRef(source, "import_merged_pipe");
        const stdout_stream_ref = try self.newRef(source, "import_stdout_stream");
        const stderr_stream_ref = try self.newRef(source, "import_stderr_stream");
        const closure_ref = try self.newRef(source, "import_closure");
        const thread_ref = try self.newRef(source, "import_thread");

        // Runtime cache check — jump past execution block on hit
        const cache_hit_label = try self.newLabel("import_cache_hit", .unknown);
        const cache_end_label = try self.newLabel("import_cache_end", .unknown);
        try self.addInstruction(.init(.from(source), .{ .get_module_cache = module_path_ir }));
        const cache_hit_cond = Result.fromLocation(ir.Location.initRegister(.r2).typed(.global(.boolean)));
        try self.jmp(source, cache_hit_cond, true, cache_hit_label);

        // 4. Set up I/O capture pipes (same pattern as compileExpressionWithCapture)
        try self.pipe(source, stdout_pipe_ref);
        try self.pipeOpt(source, stdout_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(true)));
        try self.pipeOpt(source, stdout_pipe_ref.dereference(), .close_destination, .fromValue(.fromBoolean(false)));
        try self.pipeOpt(source, stdout_pipe_ref.dereference(), .disconnect_destination, .fromValue(.fromBoolean(false)));
        try self.pipe(source, stderr_pipe_ref);
        try self.pipeOpt(source, stderr_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(true)));
        try self.pipeOpt(source, stderr_pipe_ref.dereference(), .close_destination, .fromValue(.fromBoolean(false)));
        try self.pipeOpt(source, stderr_pipe_ref.dereference(), .disconnect_destination, .fromValue(.fromBoolean(false)));
        try self.pipe(source, merged_pipe_ref);
        try self.pipeFwd(source, stdout_pipe_ref.dereference(), merged_pipe_ref.dereference());
        try self.pipeFwd(source, stderr_pipe_ref.dereference(), merged_pipe_ref.dereference());
        try self.set(source, stdout_stream_ref, .from(
            try self.fork(source, self.stdoutStreamSet(), self.threadStdin(), stdout_pipe_ref.dereference(), self.threadStderr(), .noll, .inherit),
        ));
        try self.set(source, stderr_stream_ref, .from(
            try self.fork(source, self.stderrStreamSet(), self.threadStdin(), self.threadStdout(), stderr_pipe_ref.dereference(), .noll, .inherit),
        ));

        // 5. Allocate module closure and fork
        const closure_size = self.instruction_sets.items[module_instr_set].closure_slot_count;
        try self.alloc(source, @max(closure_size, 1));
        try self.set(source, closure_ref, .fromLocation(.initRegister(.r)));
        try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
        const thread_location = try self.fork(
            source,
            module_fn_addr,
            self.threadStdin(),
            stdout_pipe_ref.dereference(),
            stderr_pipe_ref.dereference(),
            closure_ref.dereference(),
            .inherit,
        );
        try self.set(source, thread_ref, .from(thread_location));

        // 6. Wait for module to complete
        try self.wait(source, thread_ref.dereference().typed(thread_type));

        // 7. Close pipes and wait for stream drain threads
        try self.pipeOpt(source, stdout_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(false)));
        try self.pipeOpt(source, stderr_pipe_ref.dereference(), .keep_open, .fromValue(.fromBoolean(false)));
        try self.wait(source, stdout_stream_ref.dereference());
        try self.wait(source, stderr_stream_ref.dereference());

        // 8. Build merged result struct: 5 standard execution result fields + n_pub pub fields
        try self.alloc(source, 5 + n_pub);
        try self.set(source, result_ref, .fromLocation(.initRegister(.r)));

        try self.set(source, .initRegister(.r2), .from(result_ref.dereference()));
        try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.stdout), .{ .dereference = true }), .from(stdout_pipe_ref.dereference()));
        try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.stderr), .{ .dereference = true }), .from(stderr_pipe_ref.dereference()));
        try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.merged), .{ .dereference = true }), .from(merged_pipe_ref.dereference()));
        try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.closeable), .{ .dereference = true }), .from(thread_ref.dereference()));
        try self.set(source, .initAdd(.{ .register = .r2 }, executionResultFieldOffset(.completion_is_thread), .{ .dereference = true }), .fromValue(.fromBoolean(true)));

        for (pub_exports, 0..) |pub_export, i| {
            try self.set(source, .initRegister(.r), .from(closure_ref.dereference()));
            try self.set(source, .initRegister(.r2), .from(result_ref.dereference()));
            try self.set(
                source,
                .initAdd(.{ .register = .r2 }, 5 + i, .{ .dereference = true }),
                .fromLocation(.initAdd(.{ .register = .r }, pub_export.slot, .{ .dereference = true })),
            );
        }

        // Store result addr in %r and cache it, then jump past cache-hit block
        try self.set(source, .initRegister(.r), .fromLocation(result_ref.dereference()));
        try self.addInstruction(.init(.from(source), .{ .set_module_cache = module_path_ir }));
        try self.jmp(source, null, false, cache_end_label);

        // Cache-hit path: %r already holds the cached heap addr; save to result_ref
        try self.setLabel(cache_hit_label.local_addr.label, .abs);
        try self.set(source, result_ref, .fromLocation(.initRegister(.r)));

        try self.setLabel(cache_end_label.local_addr.label, .abs);

        // 9. Build the merged struct type for member access
        const all_fields = try self.allocator.alloc(ast.TypeExpr.StructField, 5 + n_pub);
        @memcpy(all_fields[0..5], execution_result_struct_type.struct_type.fields);
        for (pub_exports, 0..) |pub_export, i| {
            const field_type_ptr = try self.allocator.create(ast.TypeExpr);
            if (pub_export.fn_ref_value) |fn_ref_val| {
                field_type_ptr.* = .{ .fn_ref_type = try self.fnRefTypeFor(fn_ref_val, pub_export.type_expr) };
            } else {
                field_type_ptr.* = pub_export.type_expr orelse .global(.integer);
            }
            all_fields[5 + i] = .{
                .name = ast.Identifier.global(pub_export.name),
                .type_expr = field_type_ptr,
                .span = .global,
            };
        }
        const merged_type = ast.TypeExpr{ .struct_type = .{
            .span = .global,
            .decls = &.{},
            .fields = all_fields,
            .by_reference_fields = true,
        } };

        return .fromLocation(result_ref.dereference().typed(merged_type));
    }

    /// Builds the `fn_ref_type` for a module pub-fn field, carrying the function's
    /// declared return type and parameter count (recovered from its `.function`
    /// binding type) so a call through the member can value-capture the result
    /// typed as its return type, exactly like a direct call.
    fn fnRefTypeFor(
        self: *IRCompiler,
        fn_ref_val: ir.Value,
        binding_type: ?ast.TypeExpr,
    ) Error!ast.TypeExpr.FnRefType {
        var return_type: ?*const ast.TypeExpr = null;
        var param_count: ?usize = null;
        if (binding_type) |bt| if (bt == .function) {
            return_type = bt.function.return_type;
            param_count = switch (bt.function.params) {
                ._non_variadic => |ps| ps.len,
                ._variadic => null,
            };
        };
        _ = self;
        return .{
            .instr_set = fn_ref_val.fn_ref.fn_addr.instr_set,
            .span = .global,
            .return_type = return_type,
            .param_count = param_count,
        };
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
        // Record the AST so `comptime` calls can interpret this function's body
        // and monomorphization can recompile it. Skip while specializing (the
        // generic entry is authoritative). `source.fn_decl` is arena-stable.
        if (source.* == .fn_decl and !self.specializing) {
            try self.comptime_fn_decls.put(self.allocator, instr_set, &source.fn_decl);
            try self.fn_decl_sources.put(self.allocator, instr_set, source);
        }
        // While specializing, register this new set as the specialization of the
        // generic being compiled, so a self-recursive call inside the body (whose
        // callee resolves to the generic set) recurses into this specialization.
        var pushed_active_specialization = false;
        if (self.specializing_generic) |generic| {
            try self.active_specializations.append(self.allocator, .{ .generic = generic, .spec = instr_set });
            self.specializing_generic = null;
            pushed_active_specialization = true;
        }
        defer if (pushed_active_specialization) {
            _ = self.active_specializations.pop();
        };
        const orig_instr_set = self.current_instruction_set;
        self.current_instruction_set = instr_set;
        try self.scopes.push(self.allocator, .closure);

        if (fn_decl.name) |name| {
            // Give the (self-visible) function binding a `.function` type carrying
            // its return type, so a recursive call in the body can be value-captured
            // by type (`const rest = cd (n - 1)` keeps `rest` typed as the return,
            // usable in arithmetic — not byte-flattened to a string).
            const param_types = try self.allocator.alloc(?*const ast.TypeExpr, fn_decl.params._non_variadic.len);
            for (fn_decl.params._non_variadic, param_types) |param, *pt| {
                pt.* = param.type_annotation;
            }
            const fn_type = ast.TypeExpr{ .function = .{
                .params = .{ ._non_variadic = param_types },
                .stdin_type = fn_decl.stdin_type,
                .return_type = fn_decl.return_type,
                .span = fn_decl.span,
            } };
            try self.scopes.declare(
                self.allocator,
                name.name,
                try .from(fn_ref),
                fn_type,
                false,
                .normal,
            );
        }

        // Track the enclosing function's declared stdin type so compileIdentifier
        // can give &0 the correct type (needed for T→?T / T→E!T coercions).
        // A stack handles nested function declarations.
        try self.stdin_type_stack.append(self.allocator, if (fn_decl.stdin_type) |st| st.* else null);
        defer _ = self.stdin_type_stack.pop();

        self.instruction_sets.items[instr_set].param_count = fn_decl.params._non_variadic.len;
        for (fn_decl.params._non_variadic) |param| {
            if (param.type_annotation) |type_annotation| self.registerParamTypeVars(type_annotation.*);
            switch (param.pattern.*) {
                .discard => {},
                .identifier => |identifier| {
                    _ = try self.declareClosureValue(
                        .mutable(identifier, .normal),
                        0,
                        false,
                        if (param.type_annotation) |type_annotation| self.normalizeStringTypes(type_annotation.*) else null,
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

        // Seed the closure slot count with the parameters so a recursive call
        // compiled *inside the body* (e.g. a value-captured `cd (n - 1)`, whose
        // fork is emitted from a nested capture wrapper where `is_self_recursive`
        // is false) allocates a closure large enough for the arguments. The final
        // count (params + captured closure variables) is set after the body.
        self.currentInstrSet().closure_slot_count = fn_decl.params._non_variadic.len;

        // TODO: closure bindings, how do we manage them (non-parameters)?
        // TODO: figure out how to be able to call async functions multiple times and have the result not be overwritten in a ref
        //
        // Compile the function body directly in the closure scope (no extra lexical push/pop)
        // so that top-level `pub` bindings land in the closure frame and are visible to the
        // pub-export epilogue below.  For non-block bodies fall back to compileExpression.
        const result = if (fn_decl.body.* == .block) blk: {
            const block = fn_decl.body.block;
            var r: Result = .fromValue(.void);
            for (block.statements[0..block.statements.len -| 1]) |stmt| {
                _ = try self.compileStatement(stmt);
            }
            if (block.statements.len > 0) {
                const last_stmt = block.statements[block.statements.len - 1];
                r = switch (last_stmt.*) {
                    .expression => |expr| try self.compileExpression(expr.expression),
                    else => try self.compileStatement(last_stmt),
                };
            }
            // Mirror compileBlock: stabilize ref-location results to a register
            switch (r.source) {
                .location => |loc| if (loc.abs == .ref) {
                    try self.set(source, .initRegister(.r2), stableResultSource(r));
                    r = .fromLocation(ir.Location.initRegister(.r2).typed(r.typeExpr()));
                },
                else => {},
            }
            // A Void function discards its body's value. If that value is a thread
            // handle — a nested Void call, e.g. the last statement of an if-branch
            // (compiled as an expression, so it skips the statement-level wait) —
            // wait for it, so the callee's side effects and any nested `exit`
            // finish before this function returns instead of racing the caller.
            const is_void_fn = if (fn_decl.return_type) |rt| rt.* == .void else true;
            if (is_void_fn and r.isType(thread_type)) {
                const stable = if (r.source.isRegister(.r))
                    try self.compileResultSaveR(source, r)
                else
                    r;
                if (isWaitable(stable)) |loc| try self.wait(source, loc);
                r = .fromValue(.void);
            }
            break :blk r;
        } else try self.compileExpression(fn_decl.body);

        // Pub-export epilogue: write each pub binding's value to a closure slot so the
        // caller can read it after the fork completes.
        const current_frame = try self.scopes.getFrame(0);
        const pub_slot_base = current_frame.closure_bindings.items.len;
        var pub_exports_list = std.ArrayListUnmanaged(InstructionSet.PubExport).empty;
        var frame_iter = current_frame.bindings.iterator();
        while (frame_iter.next()) |entry| {
            if (!entry.value_ptr.is_pub) continue;
            const slot = pub_slot_base + pub_exports_list.items.len;
            try self.set(
                source,
                ir.Location.initAdd(.closure, slot, .{}),
                entry.value_ptr.result.source,
            );
            try pub_exports_list.append(self.allocator, .{
                .name = entry.key_ptr.*,
                .slot = slot,
                .type_expr = entry.value_ptr.type_expr,
                .fn_ref_value = if (entry.value_ptr.result.source == .value and
                    entry.value_ptr.result.source.value == .fn_ref)
                    entry.value_ptr.result.source.value
                else
                    null,
            });
        }

        // TODO: figure out how to make this async
        if (isWaitable(result)) |loc| {
            try self.wait(source, loc);
        }
        // A Void-stdout function produces no piped output, so discard the body's
        // value (which may be a non-void expression such as an assignment) and
        // exit with success. Without this, a numeric body value would now be
        // serialized to the stdout pipe by exit_with.
        const is_void_stdout = if (fn_decl.return_type) |rt|
            rt.* == .void or typeExprIsNamed(rt.*, "Void")
        else
            false;
        if (is_void_stdout or result.source.isValueTag(.void)) {
            try self.exitWith(source, .fromValue(.fromBoolean(true)));
        } else {
            try self.exitWith(source, result);
        }

        try self.setClosureIdentifiers();
        self.currentInstrSet().closure_slot_count += pub_exports_list.items.len;
        self.currentInstrSet().pub_exports = try pub_exports_list.toOwnedSlice(self.allocator);
        self.current_instruction_set = orig_instr_set;
        self.scopes.pop();
        // A specialization is called directly by instruction set; it must not
        // re-declare the (already-declared) function name in the enclosing scope.
        if (fn_decl.name) |name| {
            if (!self.specializing) {
                const fn_type = ast.TypeExpr{ .function = .{
                    .params = .nonVariadic(&.{}),
                    .stdin_type = fn_decl.stdin_type,
                    .return_type = fn_decl.return_type,
                    .span = fn_decl.span,
                } };
                try self.scopes.declare(
                    self.allocator,
                    name.name,
                    try .from(fn_ref),
                    fn_type,
                    false,
                    .normal,
                );
                if (fn_decl.is_pub) {
                    if (self.lookup(name.name, .{ .shallow = true })) |binding| {
                        binding.is_pub = true;
                    }
                }
            }
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
            // `-x` is `0 - x` — works for Int and Float via the arithmetic path.
            .negate => {
                const operand = try self.compileArithmeticOperand(source, unary.operand);
                const zero = ir.ValueSource.fromValue(.{ .integer = 0 });
                if (evaluateArithmetic(.sub, zero, operand.source)) |folded| {
                    return .from(folded);
                }
                const ref = try self.newRef(source, "neg_result");
                try self.ath(source, .subtract, zero, operand.source, ref);
                return .from(ref.dereference());
            },
        }
    }

    /// `>` is overloaded: an output redirect when the left operand is a command,
    /// the greater-than comparison when it is a value. The parser leaves it as a
    /// `.binary{.greater}`; this resolves the meaning from the left operand,
    /// which only the compiler can classify (a bare `n` and a bare external
    /// command both parse as zero-arg calls — the scope tells them apart).
    ///
    /// A command is a call to an external executable (an unresolved identifier)
    /// or a Runic function, a block, a subshell, or a call that already carries
    /// redirects. A call to a value binding, or any non-call value, is a
    /// comparison — so `echo "x" > "f"` and `myFn > "f"` redirect, while
    /// `n > 2` and `count > limit` compare. To compare a function's return
    /// value, bind it first: `const r = myFn; if (r > 2) ...`.
    fn greaterLeftIsCommand(self: *IRCompiler, left: *ast.Expression) bool {
        return switch (left.*) {
            .block, .subshell => true,
            .call => |call| call.redirects.len > 0 or self.calleeIsCommand(call.callee),
            else => false,
        };
    }

    fn calleeIsCommand(self: *IRCompiler, callee: *ast.Expression) bool {
        return switch (callee.*) {
            // Not in scope: an external executable. In scope: a command only if
            // it is a function reference; a value binding is compared.
            .identifier => |id| blk: {
                const binding = self.lookup(id.name, .{ .shallow = false }) orelse break :blk true;
                break :blk binding.result.isFunctionRef();
            },
            .block, .subshell => true,
            else => false,
        };
    }

    /// Builds the redirected call for a command-left `>` (e.g. `echo "x" > "f"`),
    /// folding a truncate-stdout redirect onto the command. Returns null when the
    /// left is not a command, so `>` stays the greater-than comparison.
    fn greaterRedirectCall(self: *IRCompiler, binary: ast.BinaryExpr) Error!?*ast.Expression {
        if (binary.op != .greater or !self.greaterLeftIsCommand(binary.left)) return null;

        const target = coerceRedirectTargetExpr(binary.right);
        const redirect = ast.Redirection{
            .stream = .stdout,
            .mode = .truncate,
            .target = .{ .path = .{ .value = target, .span = target.span() } },
            .span = binary.left.span().endAt(binary.right.span()),
        };

        const new_expr = try self.allocator.create(ast.Expression);
        switch (binary.left.*) {
            .call => |call| new_expr.* = .{ .call = .{
                .callee = call.callee,
                .arguments = call.arguments,
                .redirects = try std.mem.concat(self.allocator, ast.Redirection, &.{ call.redirects, &.{redirect} }),
                .background = call.background,
                .span = call.span.endAt(binary.right.span()),
            } },
            else => new_expr.* = .{ .call = .{
                .callee = binary.left,
                .arguments = &.{},
                .redirects = try self.allocator.dupe(ast.Redirection, &.{redirect}),
                .background = false,
                .span = binary.left.span().endAt(binary.right.span()),
            } },
        }
        return new_expr;
    }

    /// Unwraps a bare zero-arg call (`> file`) to its identifier callee so the
    /// redirect target is the path expression, mirroring the parser.
    fn coerceRedirectTargetExpr(expr: *ast.Expression) *ast.Expression {
        return switch (expr.*) {
            .call => |call| if (call.arguments.len == 0 and call.redirects.len == 0 and call.callee.* == .identifier)
                call.callee
            else
                expr,
            else => expr,
        };
    }

    /// Compiles an operand of an arithmetic expression as a value. An operand
    /// that produces output (a function call, UFCS method, pipeline, …) must be
    /// *captured* so it yields its value rather than a fork/thread handle;
    /// the captured value is stabilized into a fresh ref so a second capturing
    /// operand can't clobber it (both would otherwise land in `%r`). A plain
    /// binding/literal operand takes the cheap path.
    fn compileArithmeticOperand(self: *IRCompiler, source: anytype, expr: *ast.Expression) Error!Result {
        if (self.analyzeExpressionEffects(expr).needs_stdio_capture) {
            const captured = try self.compileExpressionWithCapture(source, expr);
            const type_expr = captured.typeExpr();
            const ref = try self.newRef(source, "arith_operand");
            try self.set(source, ref, stableResultSource(captured));
            return .from(ref.dereference().typed(type_expr));
        }
        return (try self.compileExpression(expr)).dereference();
    }

    fn compileBinary(
        self: *IRCompiler,
        source: anytype,
        binary: ast.BinaryExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        switch (binary.op) {
            .add, .subtract, .multiply, .divide, .remainder => {
                const left = try self.compileArithmeticOperand(source, binary.left);
                const right = try self.compileArithmeticOperand(source, binary.right);

                if (evaluateArithmetic(.from(binary.op), left.source, right.source)) |comptime_result| {
                    return .from(comptime_result);
                }

                const ref = try self.newRef(source, "ath_result");

                try self.ath(source, binary.op, left.source, right.source, ref);

                return .from(ref.dereference());
            },
            .logical_and, .logical_or, .sequence => {
                // `errorUnion || fallback` discards the error; `errorUnion && next`
                // is a monadic guard. Handle the non-capture cases here; everything
                // else keeps the existing exit-code logical lowering.
                // Route to the value paths when neither operand needs capture, or
                // when the LHS is an error producer (a function call / pipeline
                // whose error must be observed via the typed capture).
                const lhs_captures_error = binary.op != .sequence and try self.lhsCapturesError(binary.left);
                if (binary.op != .sequence and
                    (lhs_captures_error or
                        (!self.analyzeExpressionEffects(binary.left).needs_stdio_capture and
                            !self.analyzeExpressionEffects(binary.right).needs_stdio_capture)))
                {
                    return switch (binary.op) {
                        .logical_or => self.compileLogicalOrValue(source, binary),
                        else => self.compileLogicalAndValue(source, binary),
                    };
                }
                return self.compileLogicalBinary(source, binary, .value);
            },
            .@"orelse" => {
                return self.compileOrelseBinary(source, binary);
            },
            .greater, .greater_equal, .less, .less_equal, .equal, .not_equal => {
                // A command-left `>` is an output redirect, not a comparison.
                if (try self.greaterRedirectCall(binary)) |call_expr| {
                    return self.compileExpression(call_expr);
                }

                const left = try self.compileExpression(binary.left);
                const right = try self.compileExpression(binary.right);

                if (evaluateCompare(.from(binary.op), left.source, right.source)) |comptime_result| {
                    return .from(comptime_result);
                }

                const ref = try self.newRef(source, "cmp_result");

                try self.cmp(source, binary.op, left.source, right.source, ref);

                return .from(ref.dereference());
            },
            .fd_source_truncate_redirect, .fd_source_append_redirect, .append_redirect, .redirect_fd => return Error.UnsupportedBinaryOperation,
            .assign => {
                if (binary.left.* == .env_var) {
                    const env_var = binary.left.env_var;
                    var exec_result_ref_opt: ?ir.Location = null;
                    var right = try self.compileExpressionWithCapture(source, binary.right);
                    if (right.isType(execution_result_struct_type)) {
                        const exec_result_ref = try self.newRef(source, "env_var_exec_result");
                        try self.set(source, exec_result_ref, stableResultSource(right));
                        exec_result_ref_opt = exec_result_ref;
                        try self.set(source, .initRegister(.r2), .from(exec_result_ref.dereference()));
                        right = .fromLocation(ir.Location.initAdd(
                            .{ .register = .r2 },
                            executionResultFieldOffset(.merged),
                            .{
                                .dereference = true,
                                .type_expr = string_type,
                            },
                        ));
                    }
                    try self.setEnv(source, env_var.identifier.name, right.source);
                    const result_ref = try self.newRef(source, "env_var_assign");
                    try self.set(source, result_ref, right.source);
                    if (exec_result_ref_opt) |exec_result_ref| {
                        try self.set(source, .initRegister(.r2), .from(exec_result_ref.dereference()));
                        try self.pipeOpt(
                            source,
                            .initAdd(
                                .{ .register = .r2 },
                                executionResultFieldOffset(.stdout),
                                .{ .dereference = true },
                            ),
                            .keep_open,
                            .fromValue(.fromBoolean(false)),
                        );
                        try self.pipeOpt(
                            source,
                            .initAdd(
                                .{ .register = .r2 },
                                executionResultFieldOffset(.stderr),
                                .{ .dereference = true },
                            ),
                            .keep_open,
                            .fromValue(.fromBoolean(false)),
                        );
                        try self.pipeOpt(
                            source,
                            .initAdd(
                                .{ .register = .r2 },
                                executionResultFieldOffset(.merged),
                                .{ .dereference = true },
                            ),
                            .keep_open,
                            .fromValue(.fromBoolean(false)),
                        );
                    }
                    return .from(result_ref.dereference().typed(optional_string_type));
                }

                // Struct field assignment `p.x = v`: compile the value into a
                // stable ref first (so evaluating it can't clobber the base
                // register), then compile the target as an lvalue (raw slot) and
                // write immediately — nothing runs between that could reset %r2.
                if (binary.left.* == .binary and binary.left.binary.op == .member) {
                    const value = try self.compileExpression(binary.right);
                    const value_ref = try self.newRef(source, "field_assign_value");
                    try self.set(source, value_ref, stableResultSource(value));
                    const slot = try self.compileMemberBinary(source, binary.left.binary, .lvalue);
                    try self.set(source, slot.source.location, .from(value_ref.dereference()));
                    return .from(value_ref.dereference().typed(value.typeExpr()));
                }

                const left = try self.compileExpression(binary.left);
                if (left.source == .location and binary.right.* == .binary) {
                    const right_binary = binary.right.binary;
                    switch (right_binary.op) {
                        .add, .subtract, .multiply, .divide, .remainder => {
                            if (expressionsStructurallyEqual(binary.left, right_binary.left)) {
                                const right_operand = (try self.compileExpression(right_binary.right)).dereference();
                                try self.ath(
                                    source,
                                    right_binary.op,
                                    left.source,
                                    right_operand.source,
                                    left.source.location,
                                );
                                return .from(left.source.location);
                            }
                        },
                        else => {},
                    }
                }
                // Capture a function call's typed return by value (a struct,
                // optional, …) on reassignment, like a `const` binding — without
                // this, `m = mapSet m …` would store the callee's thread handle.
                const right = try self.compileExpressionWithCapture(source, binary.right);
                try self.set(source, left.source.location, right.source);

                // Refine a mutable variable's tracked type when the new value is
                // concretely typed but the variable's type was still unknown
                // (`var out = .{ }` is `[]Void`; `out = out.push Box{…}` makes it
                // `[]Box`), so a later `out[i].field` resolves the element layout.
                // Update both the `result` (read via compileIdentifier) and
                // `type_expr` (read via resolveStaticType, e.g. when the variable
                // becomes a struct-literal field value).
                if (binary.left.* == .identifier) {
                    if (self.lookup(binary.left.identifier.name, .{ .shallow = false })) |binding| {
                        if (binding.is_mutable and
                            (typeIsUnknown(binding.result.typeExpr()) or typeIsUnknown(binding.type_expr)))
                        {
                            if (right.typeExpr()) |rt| {
                                binding.result = binding.result.typed(rt);
                                binding.type_expr = rt;
                            }
                        }
                    }
                }

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
                // The index may be a function call whose result is delivered as a
                // thread/pipe to await (e.g. `arr[hash key]`); capture it so the
                // arithmetic below adds a materialized integer, not a handle.
                const right = try self.compileArithmeticOperand(source, binary.right);
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
            .member => return self.compileMemberBinary(source, binary, .read),
        }

        try self.reportSourceError(source, Error.UnsupportedBinaryOperation, .@"error", "binary operator \"{t}\" not yet supported", .{binary.op});
        return .fromValue(.void);
    }

    fn expressionsStructurallyEqual(a: *ast.Expression, b: *ast.Expression) bool {
        const a_name = bareIdentifierName(a) orelse return false;
        const b_name = bareIdentifierName(b) orelse return false;
        return std.mem.eql(u8, a_name, b_name);
    }

    fn bareIdentifierName(expr: *ast.Expression) ?[]const u8 {
        return switch (expr.*) {
            .identifier => |identifier| identifier.name,
            .call => |call| if (call.arguments.len == 0 and call.redirects.len == 0 and !call.background and call.callee.* == .identifier)
                call.callee.identifier.name
            else
                null,
            else => null,
        };
    }

    fn compileArray(
        self: *IRCompiler,
        source: *ast.Expression,
        array: ast.ArrayLiteral,
    ) Error!Result {
        try self.alloc(source, array.elements.len + 1);
        try self.set(source, .initAbs(.{ .register = .r }, .{ .dereference = true }), .fromValue(.{ .integer = @as(i64, @intCast(array.elements.len)) }));
        const array_ref = try self.newRef(source, "array");
        try self.set(source, array_ref, .fromLocation(.initRegister(.r)));
        var element_type: ?ast.TypeExpr = null;
        for (array.elements, 1..) |element, i| {
            const result = try self.compileExpressionWithCapture(source, element);
            // Literals carry no type on their Result; fall back to the element
            // expression's static type so `.{ 1, 2, 3 }` infers `[]Int`.
            element_type = result.typeExpr() orelse self.argTypeExpr(element);
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
        range_limit_ref: ?ir.Location = null,
        zero_based_range: bool = false,
    };

    fn compileForSource(
        self: *IRCompiler,
        source: *ast.Expression,
        for_source: *ast.Expression,
        index: usize,
    ) Error!ForSource {
        switch (for_source.*) {
            .range => |range| {
                const zero_based_range = isLiteralZero(range.start);
                const start_ref: ?ir.Location = if (zero_based_range) null else blk: {
                    const start_result = try self.compileExpression(range.start);
                    const ref = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}_range_start", .{index}));
                    try self.set(source, ref, start_result.source);
                    break :blk ref;
                };

                var len_ref: ?ir.Location = null;
                var range_limit_ref: ?ir.Location = null;
                if (range.end) |end_expr| {
                    const end_result = try self.compileExpression(end_expr);
                    if (zero_based_range) {
                        const range_len_ref = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}_range_len", .{index}));
                        try self.set(source, range_len_ref, end_result.source);
                        len_ref = range_len_ref;
                        range_limit_ref = range_len_ref;
                    } else {
                        const range_limit_ref_ = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}_range_limit", .{index}));
                        try self.set(source, range_limit_ref_, end_result.source);
                        const range_len_ref = try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_source_{}_range_len", .{index}));
                        try self.ath(
                            source,
                            .subtract,
                            .from(range_limit_ref_.dereference()),
                            .from(start_ref.?.dereference()),
                            range_len_ref,
                        );
                        len_ref = range_len_ref;
                        range_limit_ref = range_limit_ref_;
                    }
                    if (range.inclusive_end) {
                        try self.ath(source, .add, .from(range_limit_ref.?.dereference()), .fromValue(.{ .integer = 1 }), range_limit_ref.?);
                        try self.ath(source, .add, .from(len_ref.?.dereference()), .fromValue(.{ .integer = 1 }), len_ref.?);
                    }
                }

                return .{
                    .kind = .range,
                    .start_ref = start_ref,
                    .len_ref = len_ref,
                    .range_limit_ref = range_limit_ref,
                    .value_type = ast.TypeExpr.global(.integer),
                    .zero_based_range = zero_based_range,
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
                    .range_limit_ref = null,
                    .value_type = self.normalizeStringTypes(source_result.typeExpr().?.array.element.*),
                };
            },
        }
    }

    fn isLiteralZero(expr: *ast.Expression) bool {
        if (expr.* != .literal or expr.literal != .integer) return false;
        return std.mem.eql(u8, expr.literal.integer.text, "0");
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
                var left = try self.compileExpression(binary.left);
                if (left.source.isRegister(.r) and left.isType(thread_type)) {
                    left = try self.compileResultSaveR(source, left);
                }

                // `errorUnion || fallback` discards the error: run the fallback
                // only when left holds an error. A plain exit-code jmp here would
                // misread the error value as an exit code and crash the runner.
                if (binary.op == .logical_or and resultIsErrorLike(left)) {
                    const subject_ref = try self.newRef(source, "logical_or_subject");
                    try self.set(source, subject_ref, stableResultSource(left));
                    const is_err_ref = try self.newRef(source, "logical_or_is_err");
                    try self.addInstruction(.init(.from(source), .{ .is_err = .{
                        .operand = subject_ref.dereference(),
                        .result = is_err_ref,
                    } }));
                    const after_addr = try self.newLabel("logical_or_after", .unknown);
                    // Not an error: nothing to do (statement result is discarded).
                    try self.jmp(source, try .from(is_err_ref.dereference()), false, after_addr);
                    _ = try self.compileLogicalRightStatement(source, binary.right);
                    try self.setLabel(after_addr.local_addr.label, .abs);
                    return .fromValue(.void);
                }

                // `errorUnion && next` monadic guard. A bare `a && b` is rejected
                // by the type checker (UnhandledError); this guards the compile
                // path so an error-like left never hits the exit-code jmp below.
                if (binary.op == .logical_and and resultIsErrorLike(left)) {
                    const subject_ref = try self.newRef(source, "logical_and_subject");
                    try self.set(source, subject_ref, stableResultSource(left));
                    const is_err_ref = try self.newRef(source, "logical_and_is_err");
                    try self.addInstruction(.init(.from(source), .{ .is_err = .{
                        .operand = subject_ref.dereference(),
                        .result = is_err_ref,
                    } }));
                    const after_addr = try self.newLabel("logical_and_after", .unknown);
                    // Error: short-circuit (nothing to do at statement level).
                    try self.jmp(source, try .from(is_err_ref.dereference()), true, after_addr);
                    _ = try self.compileLogicalRightStatement(source, binary.right);
                    try self.setLabel(after_addr.local_addr.label, .abs);
                    return .fromValue(.void);
                }

                try self.finalizeStatementResult(source, left);

                // sequence (;) always runs right — no conditional jump needed
                if (binary.op == .sequence) {
                    _ = try self.compileLogicalRightStatement(source, binary.right);
                    return .fromValue(.void);
                }

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
                    // Compile sub-expressions directly — we are already inside a capture fork
                    // (set up by compileExpressionWithCapture → compileWithContext) where the
                    // current thread's stdout/stderr ARE the outer capture pipes. Creating
                    // nested captures here would intercept the output into inner pipes and
                    // leave the outer pipes empty.
                    const result_ref = try self.newRef(source, "logical_result");
                    const left = try self.compileExpression(binary.left);

                    // `errorUnion || fallback` discards the error. We are already
                    // inside a capture fork, so compile the fallback directly
                    // (no nested capture) rather than via the exit-code jmp, which
                    // would misread the error value as an exit code and crash.
                    if (binary.op == .logical_or and resultIsErrorLike(left)) {
                        try self.set(source, result_ref, stableResultSource(left));
                        const is_err_ref = try self.newRef(source, "logical_or_is_err");
                        try self.addInstruction(.init(.from(source), .{ .is_err = .{
                            .operand = result_ref.dereference(),
                            .result = is_err_ref,
                        } }));
                        const after_addr = try self.newLabel("logical_or_after", .unknown);
                        try self.jmp(source, try .from(is_err_ref.dereference()), false, after_addr);
                        const right = try self.compileExpression(binary.right);
                        try self.finalizeStatementResult(source, right);
                        try self.set(source, result_ref, stableResultSource(right));
                        try self.setLabel(after_addr.local_addr.label, .abs);
                        const result_type: ?ast.TypeExpr = if (left.typeExpr()) |left_type| switch (left_type) {
                            .error_union => |error_union| error_union.payload.*,
                            else => right.typeExpr(),
                        } else right.typeExpr();
                        return .from(result_ref.dereference().typed(result_type));
                    }

                    // `errorUnion && next` monadic guard (already inside a capture
                    // fork, so compile the rhs directly): `next` when left is ok,
                    // the left's error otherwise. Result is `E!(typeof next)`.
                    if (binary.op == .logical_and and resultIsErrorLike(left)) {
                        try self.set(source, result_ref, stableResultSource(left));
                        const is_err_ref = try self.newRef(source, "logical_and_is_err");
                        try self.addInstruction(.init(.from(source), .{ .is_err = .{
                            .operand = result_ref.dereference(),
                            .result = is_err_ref,
                        } }));
                        const after_addr = try self.newLabel("logical_and_after", .unknown);
                        // Error: short-circuit, keep the error already in result_ref.
                        try self.jmp(source, try .from(is_err_ref.dereference()), true, after_addr);
                        const right = try self.compileExpression(binary.right);
                        try self.finalizeStatementResult(source, right);
                        try self.set(source, result_ref, stableResultSource(right));
                        try self.setLabel(after_addr.local_addr.label, .abs);
                        const result_type: ?ast.TypeExpr = blk: {
                            const lt = left.typeExpr() orelse break :blk null;
                            if (lt != .error_union) break :blk lt;
                            const rt = right.typeExpr() orelse break :blk lt;
                            const payload_ptr = try self.allocator.create(ast.TypeExpr);
                            payload_ptr.* = rt;
                            break :blk ast.TypeExpr{ .error_union = .{
                                .err_set = lt.error_union.err_set,
                                .payload = payload_ptr,
                                .span = lt.error_union.span,
                            } };
                        };
                        return .from(result_ref.dereference().typed(result_type));
                    }

                    // Wait for left so we can check its exit code (for &&/||)
                    try self.finalizeStatementResult(source, left);
                    try self.set(source, result_ref, stableResultSource(left));

                    // sequence (;): always run right
                    if (binary.op == .sequence) {
                        const right = try self.compileExpression(binary.right);
                        try self.finalizeStatementResult(source, right);
                        try self.set(source, result_ref, stableResultSource(right));
                        return .from(result_ref.dereference().typed(mergedResultType(left, right)));
                    }

                    if (evaluateLogical(.from(binary.op), left.source)) |comptime_result| {
                        return switch (comptime_result) {
                            .left => left,
                            .right => blk: {
                                const right = try self.compileExpression(binary.right);
                                try self.finalizeStatementResult(source, right);
                                try self.set(source, result_ref, stableResultSource(right));
                                // The result is exactly the right operand (the
                                // comptime-known left was discarded), so it carries
                                // the right's type — not a merge with the left,
                                // which would be null when they differ (e.g.
                                // `false || echo "x"`: bool vs execution) and lose
                                // the capture.
                                break :blk .from(result_ref.dereference().typed(right.typeExpr()));
                            },
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

                    const right = try self.compileExpression(binary.right);
                    try self.finalizeStatementResult(source, right);
                    try self.set(source, result_ref, stableResultSource(right));
                    try self.setLabel(after_addr.local_addr.label, .abs);

                    return .from(result_ref.dereference().typed(mergedResultType(left, right)));
                }

                // No stdio capture needed — compile normally
                if (binary.op == .sequence) {
                    _ = try self.compileExpression(binary.left);
                    return try self.compileExpression(binary.right);
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
                .logical_and, .logical_or, .sequence => return self.compileLogicalBinary(source, binary, .statement),
                else => {},
            },
            else => {},
        }
        const result = try self.compileExpression(expr);
        try self.finalizeStatementResult(source, result);
        return .fromValue(.void);
    }

    /// Non-capture `a || b`. If `a` is an error union, this is error-discard:
    /// the error union's ok value, or `b` when `a` is an error. Otherwise it is
    /// the ordinary exit-code logical-or.
    fn compileLogicalOrValue(
        self: *IRCompiler,
        source: anytype,
        binary: ast.BinaryExpr,
    ) Error!Result {
        // An error-producing LHS (function call / pipeline) is captured via the
        // typed capture so its error value is observed (mirrors `catch`).
        const left = if (try self.lhsCapturesError(binary.left))
            try self.compileExpressionWithCapture(source, binary.left)
        else
            try self.compileExpression(binary.left);

        const left_type_opt = left.typeExpr();
        if (resultIsErrorLike(left)) {
            const result_ref = try self.newRef(source, "logical_or_result");
            try self.set(source, result_ref, stableResultSource(left));

            const is_err_ref = try self.newRef(source, "logical_or_is_err");
            try self.addInstruction(.init(.from(source), .{ .is_err = .{
                .operand = result_ref.dereference(),
                .result = is_err_ref,
            } }));

            const after_addr = try self.newLabel("logical_or_after", .unknown);
            // Not an error: keep the ok value already in result_ref.
            try self.jmp(source, try .from(is_err_ref.dereference()), false, after_addr);
            // Error: discard it and use the fallback.
            const right = try self.compileStableExpressionIntoRef(source, binary.right, result_ref);
            try self.setLabel(after_addr.local_addr.label, .abs);

            const result_type: ?ast.TypeExpr = if (left_type_opt) |left_type| switch (left_type) {
                .error_union => |error_union| error_union.payload.*,
                else => right.typeExpr(),
            } else right.typeExpr();
            return .from(result_ref.dereference().typed(result_type));
        }

        // Ordinary exit-code logical-or (mirrors the non-capture value path).
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
    }

    /// Non-capture `a && b`. If `a` is an error union, this is a monadic guard:
    /// `b` when `a` is ok, otherwise `a`'s error (short-circuit). The result is
    /// an error union (`E!(typeof b)`) so it must itself be handled. Otherwise
    /// it is the ordinary exit-code logical-and.
    fn compileLogicalAndValue(
        self: *IRCompiler,
        source: anytype,
        binary: ast.BinaryExpr,
    ) Error!Result {
        const left = if (try self.lhsCapturesError(binary.left))
            try self.compileExpressionWithCapture(source, binary.left)
        else
            try self.compileExpression(binary.left);

        const left_type_opt = left.typeExpr();
        if (resultIsErrorLike(left)) {
            const result_ref = try self.newRef(source, "logical_and_result");
            try self.set(source, result_ref, stableResultSource(left));

            const is_err_ref = try self.newRef(source, "logical_and_is_err");
            try self.addInstruction(.init(.from(source), .{ .is_err = .{
                .operand = result_ref.dereference(),
                .result = is_err_ref,
            } }));

            const after_addr = try self.newLabel("logical_and_after", .unknown);
            // Error: short-circuit, keep the error already in result_ref.
            try self.jmp(source, try .from(is_err_ref.dereference()), true, after_addr);
            // Ok: evaluate the right-hand side as the result.
            const right = try self.compileStableExpressionIntoRef(source, binary.right, result_ref);
            try self.setLabel(after_addr.local_addr.label, .abs);

            // Result is `E!(typeof right)`: the left's error set with the
            // right's value as the payload, so `catch`/`try` see an error union.
            const result_type: ?ast.TypeExpr = blk: {
                const lt = left_type_opt orelse break :blk null;
                if (lt != .error_union) break :blk lt;
                const rt = right.typeExpr() orelse break :blk lt;
                const payload_ptr = try self.allocator.create(ast.TypeExpr);
                payload_ptr.* = rt;
                break :blk ast.TypeExpr{ .error_union = .{
                    .err_set = lt.error_union.err_set,
                    .payload = payload_ptr,
                    .span = lt.error_union.span,
                } };
            };
            return .from(result_ref.dereference().typed(result_type));
        }

        // Ordinary exit-code logical-and (mirrors the non-capture value path).
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
    }

    fn compileOrelseBinary(
        self: *IRCompiler,
        source: anytype,
        binary: ast.BinaryExpr,
    ) Error!Result {
        if (try self.evalComptimeExpression(binary.left)) |left_comptime| {
            if (left_comptime.source.isValueTag(.null)) {
                return self.compileExpression(binary.right);
            }
            return self.compileExpression(binary.left);
        }

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
        capture_ref: ?ir.Location,
    ) Error!void {
        switch (for_source.kind) {
            .array => {
                const capture_ref_ = capture_ref orelse return Error.ScopeNotFound;
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
                try self.set(source, capture_ref_, .fromLocation(.initAbs(.{ .register = .r2 }, .{ .dereference = true })));
            },
            .range => {
                if (!for_source.zero_based_range) {
                    const capture_ref_ = capture_ref orelse return Error.ScopeNotFound;
                    try self.ath(
                        source,
                        .add,
                        .from(for_source.start_ref.?.dereference()),
                        .from(counter_ref.dereference()),
                        capture_ref_,
                    );
                }
            },
        }

        try self.compileIdentifierBinding(
            source,
            binding,
            if (for_source.kind == .range and for_source.zero_based_range)
                .from(counter_ref.dereference().typed(for_source.value_type))
            else
                .from(capture_ref.?.dereference().typed(for_source.value_type)),
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
        var needs_owned_iterations_ref = false;

        for (for_sources, 0..) |for_source, i| {
            const len_ref = for_source.len_ref orelse continue;
            if (iterations_ref == null) {
                iterations_ref = len_ref;
                continue;
            }

            if (!needs_owned_iterations_ref) {
                const owned_iterations_ref = try self.newRef(source, "for_iterations");
                try self.set(source, owned_iterations_ref, .from(iterations_ref.?.dereference()));
                iterations_ref = owned_iterations_ref;
                needs_owned_iterations_ref = true;
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

    /// Lowers `comptime <operand>`: fold the operand to a constant value with
    /// call-folding enabled, or fail to compile. (The success path is normally
    /// taken earlier by the automatic folder in `compileExpression`; this
    /// handles — and reports — the case where the operand isn't reducible.)
    fn compileComptimeExpr(
        self: *IRCompiler,
        source: *ast.Expression,
        comptime_expr: ast.ComptimeExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        const folded = blk: {
            self.comptime_forcing += 1;
            defer self.comptime_forcing -= 1;
            break :blk try self.evalComptimeExpression(comptime_expr.operand);
        };

        if (folded) |result| {
            if (!(result.source == .value and result.source.value == .zig_string)) {
                return result;
            }
        }

        try self.reportSourceError(
            source,
            Error.UnsupportedExpression,
            .@"error",
            "`comptime` expression could not be evaluated at compile time",
            .{},
        );
        return .fromValue(.void);
    }

    fn compileForLoop(
        self: *IRCompiler,
        source: *ast.Expression,
        for_expr: ast.ForExpr,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        // A single `&0` source is a live stream, not a finite sequence: iterate
        // by reading one value per turn and stop at EOF. This is the consumer
        // half of multi-value typed pipes (`producer | for (&0) |v| { ... }`).
        if (for_expr.sources.len == 1 and for_expr.sources[0].* == .fd) {
            return self.compileFdForLoop(source, for_expr);
        }

        const for_sources = try self.allocator.alloc(ForSource, for_expr.sources.len);
        defer self.allocator.free(for_sources);
        for (for_expr.sources, 0..) |for_source_expr, i| {
            for_sources[i] = try self.compileForSource(source, for_source_expr, i);
        }

        if (for_sources.len == 1 and for_sources[0].kind == .range and for_sources[0].range_limit_ref != null) {
            return self.compileSingleRangeForLoop(source, for_expr, for_sources[0]);
        }

        const capture_refs = try self.allocator.alloc(?ir.Location, for_expr.capture.bindings.len);
        defer self.allocator.free(capture_refs);
        for (for_expr.capture.bindings, for_sources, 0..) |capture, for_source, i| {
            capture_refs[i] = switch (capture.*) {
                .identifier => if (for_source.kind == .range and for_source.zero_based_range)
                    null
                else
                    try self.newRef(source, try std.fmt.allocPrint(self.allocator, "for_capture_{}", .{i})),
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
        try self.set(source, counter_ref, .fromValue(.{ .integer = 0 }));
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
                    capture_refs[i],
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
        const fallback_for_body_result = try self.compileExpressionAsStatement(source, for_expr.body);

        if (isWaitable(fallback_for_body_result)) |loc| {
            try self.wait(source, loc);
        }

        // Pop any refs that were allocated during the body but not cleaned up.
        // Without this, the runtime stack grows each iteration and ref lookups
        // based on compile-time rel_stack_addr become incorrect on iteration 2+.
        const fallback_body_extra_refs = self.currentFrame().rel_stack_counter - stack_before_body;
        for (0..fallback_body_extra_refs) |_| {
            _ = try self.pop(source);
        }

        // 5. Pop bindings scope

        self.scopes.pop();

        try self.ath(
            source,
            .add,
            .from(counter_ref.dereference()),
            .fromValue(.{ .integer = 1 }),
            counter_ref,
        );
        try self.jmp(source, null, true, for_label);

        try self.setLabel(after_label.local_addr.label, .abs);

        // TODO: return something like the block compilation is doing
        return .fromValue(.void);
    }

    /// Lowers `while (condition) { body }`: re-evaluate the condition at the top
    /// of each iteration, exit when it is falsy, otherwise run the body and jump
    /// back. The condition is compiled *inside* the loop so it re-runs every
    /// pass; its truthiness is stashed in a stable ref allocated outside the loop
    /// so the transient refs it pushes can be popped back to a fixed base before
    /// the exit branch — keeping the compile-time and runtime stacks aligned on
    /// both the continue and exit paths.
    fn compileWhileLoop(
        self: *IRCompiler,
        source: *ast.Statement,
        while_stmt: ast.WhileStmt,
    ) Error!Result {
        try self.comment("{f} -> {s}", .{ self.formatInlineSpan(source.span()), @src().fn_name });

        if (while_stmt.capture) |capture| {
            if (capture.bindings.len != 1) {
                try self.reportSourceError(
                    source,
                    Error.UnsupportedBindingPattern,
                    .@"error",
                    "while capture clauses currently require exactly one binding",
                    .{},
                );
                return .fromValue(.void);
            }
        }

        const after_label = try self.newLabel("while_after", .unknown);
        const cond_ref = try self.newRef(source, "while_cond");
        // For an optional-capture loop the exit test is "the optional is present",
        // computed into its own stable ref (allocated outside the loop so the
        // transient-ref pop below leaves it intact).
        const present_ref: ?ir.Location = if (while_stmt.capture != null)
            try self.newRef(source, "while_present")
        else
            null;
        const while_label = try self.newLabel("while", .abs);
        const loop_stack_base = self.currentFrame().rel_stack_counter;

        // Evaluate the condition and stash its result, then drop any transient
        // refs it pushed so the stack is back at a fixed base before we branch.
        const condition = try self.compileTransientExpression(source, while_stmt.condition);
        try self.set(source, cond_ref, stableResultSource(condition));

        // The value the exit branch tests, plus (for a capture) the unwrapped
        // binding to declare in the body scope.
        var loop_cond: Result = undefined;
        var capture_binding: ?IfCaptureBinding = null;
        if (while_stmt.capture) |capture| {
            const condition_type = blk: {
                if (condition.typeExpr()) |type_expr| break :blk type_expr;
                if (while_stmt.condition.* == .identifier) {
                    if (self.lookup(while_stmt.condition.identifier.name, .{ .shallow = false })) |binding| {
                        if (binding.result.typeExpr()) |type_expr| break :blk type_expr;
                    }
                }
                break :blk null;
            };
            const child: ast.TypeExpr = switch (condition_type orelse ast.TypeExpr.global(.void)) {
                .optional => |optional| optional.child.*,
                else => {
                    try self.reportSourceError(
                        source,
                        Error.UnsupportedExpression,
                        .@"error",
                        "a `while (…) |v|` capture requires an optional condition",
                        .{},
                    );
                    return .fromValue(.void);
                },
            };
            // present = (cond != null); loop while present, bind the unwrapped value.
            try self.cmp(source, .not_equal, .from(cond_ref.dereference()), .fromValue(.null), present_ref.?);
            loop_cond = try .from(present_ref.?.dereference());
            capture_binding = .{
                .pattern = capture.bindings[0],
                .value = .fromLocation(cond_ref.dereference().typed(child)),
            };
        } else {
            loop_cond = try .from(cond_ref.dereference());
        }

        try self.popToStackBase(source, loop_stack_base);
        try self.jmp(source, loop_cond, false, after_label);

        // Body: a fresh lexical scope per iteration (loop-local bindings), with
        // its runtime slots popped at the end so they don't accumulate.
        try self.scopes.push(self.allocator, .lexical);
        const stack_before_body = self.currentFrame().rel_stack_counter;

        if (capture_binding) |binding| switch (binding.pattern.*) {
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
                self.scopes.pop();
                try self.reportSourceError(
                    source,
                    Error.UnsupportedBindingPattern,
                    .@"error",
                    "while capture binding pattern not yet supported",
                    .{},
                );
                return .fromValue(.void);
            },
        };

        var body_result: Result = .fromValue(.void);
        for (while_stmt.body.statements) |body_stmt| {
            body_result = try self.compileStatement(body_stmt);
        }
        if (isWaitable(body_result)) |loc| {
            try self.wait(source, loc);
        }

        try self.popToStackBase(source, stack_before_body);
        self.scopes.pop();

        try self.jmp(source, null, true, while_label);
        try self.setLabel(after_label.local_addr.label, .abs);

        return .fromValue(.void);
    }

    /// Lowers `for (&0) |v| body` — a loop over the live stdin stream. Each
    /// iteration reads the next value with `collect_stdin` (the evaluator blocks
    /// the green thread until a value arrives or the producer closes). EOF is
    /// reported as `.null`, which ends the loop. Unlike the counted for-loop,
    /// there is no precomputed iteration count.
    fn compileFdForLoop(
        self: *IRCompiler,
        source: *ast.Expression,
        for_expr: ast.ForExpr,
    ) Error!Result {
        const fd_expr = for_expr.sources[0].fd;
        if (fd_expr.fd != 0) {
            try self.reportSourceError(source, Error.UnsupportedExpression, .@"error", "&{d} is a write-only stream; iterate over &0", .{fd_expr.fd});
            return .fromValue(.void);
        }

        const capture = for_expr.capture.bindings[0];

        // The element type matches the enclosing function's declared stdin type
        // (the stream carries one such value per turn). An `Int` stdin parses
        // the collected bytes; everything else stays a `String`.
        const declared_type: ?ast.TypeExpr = if (self.stdin_type_stack.items.len > 0)
            self.stdin_type_stack.items[self.stdin_type_stack.items.len - 1]
        else
            null;
        const reads_int = if (declared_type) |dt| typeExprIsNamed(dt, "Int") else false;
        const value_type: ?ast.TypeExpr = if (reads_int)
            ast.TypeExpr.global(.integer)
        else
            declared_type orelse string_type;

        // Refs allocated once and overwritten each iteration so the runtime
        // stack does not grow per turn.
        const value_ref = try self.newRef(source, "for_fd_value");
        const is_eof_ref = try self.newRef(source, "for_fd_is_eof");

        const after_label = try self.newLabel("for_fd_after", .unknown);
        const for_label = try self.newLabel("for_fd", .abs);

        // Read the next value off the stream into value_ref.
        try self.addInstruction(.init(.from(source), .collect_stdin));
        if (reads_int) try self.addInstruction(.init(.from(source), .parse_int));
        try self.set(source, value_ref, .fromLocation(.initRegister(.r)));

        // EOF (`.null`) ends the loop.
        try self.cmp(source, .equal, .from(value_ref.dereference()), .fromValue(.null), is_eof_ref);
        try self.jmp(source, try .from(is_eof_ref.dereference()), true, after_label);

        try self.scopes.push(self.allocator, .lexical);

        switch (capture.*) {
            .discard => {},
            .identifier => |identifier| try self.compileIdentifierBinding(
                source,
                identifier,
                .from(value_ref.dereference().typed(value_type)),
                null,
                false,
                .normal,
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

        const stack_before_body = self.currentFrame().rel_stack_counter;
        const body_result = try self.compileExpressionAsStatement(source, for_expr.body);
        if (isWaitable(body_result)) |loc| try self.wait(source, loc);

        const body_extra_refs = self.currentFrame().rel_stack_counter - stack_before_body;
        for (0..body_extra_refs) |_| {
            _ = try self.pop(source);
        }

        self.scopes.pop();

        try self.jmp(source, null, true, for_label);
        try self.setLabel(after_label.local_addr.label, .abs);

        return .fromValue(.void);
    }

    fn compileSingleRangeForLoop(
        self: *IRCompiler,
        source: *ast.Expression,
        for_expr: ast.ForExpr,
        for_source: ForSource,
    ) Error!Result {
        const iter_ref = try self.newRef(source, "for_counter");
        if (for_source.zero_based_range) {
            try self.set(source, iter_ref, .fromValue(.{ .integer = 0 }));
        } else {
            try self.set(source, iter_ref, .from(for_source.start_ref.?.dereference()));
        }

        const frame_before_body = self.currentFrame().rel_stack_counter;
        const body_set = try self.addInstructionSet();
        const prev_set = self.current_instruction_set;
        self.current_instruction_set = body_set;
        defer self.current_instruction_set = prev_set;

        try self.scopes.push(self.allocator, .lexical);
        defer self.scopes.pop();

        switch (for_expr.capture.bindings[0].*) {
            .discard => {},
            .identifier => |identifier| try self.compileIdentifierBinding(
                source,
                identifier,
                .from(iter_ref.dereference().typed(for_source.value_type)),
                null,
                false,
                .normal,
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

        const counted_loop_stack_before_body = self.currentFrame().rel_stack_counter;
        const for_body_result = try self.compileExpressionAsStatement(source, for_expr.body);
        const body_extra_refs = self.currentFrame().rel_stack_counter - counted_loop_stack_before_body;
        for (0..body_extra_refs) |_| {
            _ = try self.pop(source);
        }

        const can_use_counted_loop = isWaitable(for_body_result) == null and self.instructionSetIsCountedLoopSafe(body_set);
        self.currentFrame().rel_stack_counter = frame_before_body;

        self.current_instruction_set = prev_set;
        if (can_use_counted_loop) {
            try self.addInstruction(.init(.from(source), .{ .counted_loop = .{
                .counter = iter_ref,
                .limit = .from(for_source.range_limit_ref.?.dereference()),
                .body_instr_set = body_set,
            } }));
            return .fromValue(.void);
        }

        const after_label = try self.newLabel("for_after", .unknown);
        const for_label = try self.newLabel("for", .abs);

        try self.cmp(source, .less, .from(iter_ref.dereference()), .from(for_source.range_limit_ref.?.dereference()), .initRegister(.r2));
        try self.jmp(source, .fromLocation(.initRegister(.r2)), false, after_label);

        try self.scopes.push(self.allocator, .lexical);

        switch (for_expr.capture.bindings[0].*) {
            .discard => {},
            .identifier => |identifier| try self.compileIdentifierBinding(
                source,
                identifier,
                .from(iter_ref.dereference().typed(for_source.value_type)),
                null,
                false,
                .normal,
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

        const stack_before_body = self.currentFrame().rel_stack_counter;
        const fallback_for_body_result = try self.compileExpressionAsStatement(source, for_expr.body);

        if (isWaitable(fallback_for_body_result)) |loc| {
            try self.wait(source, loc);
        }

        const fallback_body_extra_refs = self.currentFrame().rel_stack_counter - stack_before_body;
        for (0..fallback_body_extra_refs) |_| {
            _ = try self.pop(source);
        }

        self.scopes.pop();

        try self.ath(
            source,
            .add,
            .from(iter_ref.dereference()),
            .fromValue(.{ .integer = 1 }),
            iter_ref,
        );
        try self.jmp(source, null, true, for_label);

        try self.setLabel(after_label.local_addr.label, .abs);
        return .fromValue(.void);
    }

    fn instructionSetIsCountedLoopSafe(self: *IRCompiler, instr_set: usize) bool {
        for (self.instruction_sets.items[instr_set].instructions.items) |instr| {
            switch (instr.type) {
                .comment, .set, .ath, .cmp, .neg, .is_err, .make_err, .match_err, .err_payload, .get_env, .set_env, .simple_exec => {},
                else => return false,
            }
        }
        return true;
    }

    fn analyzeExpressionEffects(
        self: *IRCompiler,
        expr: *ast.Expression,
    ) ExprEffects {
        return switch (expr.*) {
            .call => |call| .{ .needs_stdio_capture = callNeedsStdioCapture(self, call) },
            .pipeline => .{ .needs_stdio_capture = true }, // definitely stdio-heavy
            .block => .{ .needs_stdio_capture = true }, // may emit output
            .if_expr => |if_expr| self.analyzeIfExpressionEffects(if_expr),
            .match_expr => |match_expr| brk: {
                var result = self.analyzeExpressionEffects(match_expr.subject);
                if (match_expr.cases.len > 0) result.needs_stdio_capture = true;
                break :brk result;
            },
            .binary => |binary| brk: {
                // A UFCS method access `recv.method` is a function call in value
                // position, so its output must be captured like any call.
                if (binary.op == .member and binary.right.* == .identifier) {
                    if (self.lookup(binary.right.identifier.name, .{ .shallow = false })) |b| {
                        if (b.result.isFunctionRef()) break :brk .{ .needs_stdio_capture = true };
                    }
                    // A nullary module-member function (`m.cwd`) is auto-called in
                    // a value context, so its output must be captured like any call.
                    if (self.memberFieldType(binary.left, binary.right.identifier.name)) |ft| {
                        if (ft == .fn_ref_type and self.instruction_sets.items[ft.fn_ref_type.instr_set].param_count == 0) {
                            break :brk .{ .needs_stdio_capture = true };
                        }
                    }
                }
                var result = self.analyzeExpressionEffects(binary.left);
                result.merge(self.analyzeExpressionEffects(binary.right));
                break :brk result;
            },
            .unary => |unary| self.analyzeExpressionEffects(unary.operand),
            .array => .{ .needs_stdio_capture = false },
            else => .{},
        };
    }

    /// The declared parameter count of a fn_ref value source, or null if the
    /// source isn't a compile-time fn_ref.
    fn fnRefParamCount(self: *IRCompiler, source: ir.ValueSource) ?usize {
        if (source != .value or source.value != .fn_ref) return null;
        return self.instruction_sets.items[source.value.fn_ref.fn_addr.instr_set].param_count;
    }

    fn callNeedsStdioCapture(self: *IRCompiler, call: ast.CallExpr) bool {
        if (call.arguments.len != 0 or call.redirects.len != 0 or call.background) {
            return true;
        }

        return switch (call.callee.*) {
            .identifier => |identifier| blk: {
                if (std.mem.eql(u8, identifier.name, "@src")) break :blk false;
                if (std.mem.eql(u8, identifier.name, "cd") and self.lookup(identifier.name, .{ .shallow = false }) == null) {
                    break :blk false;
                }
                // A bare identifier parses as a zero-arg call. An unknown name is
                // an external command whose output must be captured. A known
                // *function* call also produces output to capture in a value
                // context; a known *variable* read must NOT be captured.
                const binding = self.lookup(identifier.name, .{ .shallow = false }) orelse break :blk true;
                if (!binding.result.isFunctionRef()) break :blk false;
                // A zero-arg reference to a function that declares parameters is
                // a function *value* (`const f = dbl`), not a call — no output.
                if (self.fnRefParamCount(binding.result.source)) |pc| if (pc > 0) break :blk false;
                break :blk true;
            },
            else => true,
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

        var stderr = std.Io.File.stderr().writer(self.io, &.{});
        const writer = &stderr.interface;

        try writer.print("[{s}{*}{s}]\n", .{ prefix_color, self, end_color });
        // try writer.print("{s}:\n", .{self.path});
        try writer.print(fmt ++ "\n", args);
    }

    pub fn logWithoutPrefix(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        if (@hasField(@This(), "logging_enabled")) {
            if (!self.logging_enabled) return;
        }

        var stderr = std.Io.File.stderr().writer(self.io, &.{});
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
