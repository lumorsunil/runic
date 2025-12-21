const std = @import("std");
const token = @import("token.zig");
const semantic = @import("../semantic/root.zig");
const resolveModulePath = @import("document_store.zig").resolveModulePath;

pub const Span = token.Span;
pub const Spanned = token.Spanned;

/// Identifiers back every binding name as well as module and member
/// references. The slice is backed by the original source text.
pub const Identifier = struct {
    name: []const u8,
    span: Span,

    pub fn fromToken(tok: token.Token) Identifier {
        return .{
            .name = tok.lexeme,
            .span = tok.span,
        };
    }

    pub fn isTypeIdentifier(self: Identifier) bool {
        return if (self.name.len == 0) false else std.ascii.isUpper(self.name[0]);
    }

    pub fn resolveType(
        self: *@This(),
        _: std.mem.Allocator,
        scope: *semantic.Scope,
    ) ?*const TypeExpr {
        const binding = scope.lookup(self.name) orelse return &TypeExpr.executableType;
        return binding.type_expr;
    }
};

/// Path represents qualified identifiers such as `error.FileError.NotFound`.
pub const Path = struct {
    segments: []const Identifier,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

/// Blocks appear in functions, control flow, and expression contexts. The
/// statements are stored as pointers so the arena allocator can own them in
/// one place.
pub const Block = struct {
    statements: []const *Statement,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

/// Patterns are shared between `const` bindings, destructuring assignment, loop
/// captures, and catch clauses.
pub const BindingPattern = union(enum) {
    /// Example: `const identifier = expression`
    identifier: Identifier,
    /// Example: `const _ = expression`
    discard: Span,
    /// Example: `const x, const y = position`
    tuple: Tuple,
    record: Record,

    pub const Tuple = struct {
        elements: []const *BindingPattern,
        span: Span,
        has_rest: bool,
    };

    pub const Record = struct {
        fields: []const RecordField,
        span: Span,
        has_rest: bool,
    };

    pub const RecordField = struct {
        label: Identifier,
        binding: ?*BindingPattern,
        span: Span,
    };

    pub fn span(self: BindingPattern) Span {
        return switch (self) {
            .identifier => |id| id.span,
            .discard => |loc| loc,
            .tuple => |tuple| tuple.span,
            .record => |record| record.span,
        };
    }
};

/// CaptureClause powers `if (opt) |value| { ... }`, loop captures, `await`
/// success clauses, and similar syntax.
pub const CaptureClause = struct {
    bindings: []const *BindingPattern,
    span: Span,
};

/// CaptureBlock bundles an optional capture clause with the block that owns the
/// bindings. Await expressions and catch handlers reuse this shape.
pub const CaptureBlock = struct {
    capture: ?CaptureClause,
    body: Block,
    span: Span,
};

/// Type expressions span primitive names, optional/promise modifiers, error
/// unions, and inline record/tuple types.
pub const TypeExpr = union(enum) {
    void: PrimitiveType,
    identifier: NamedType,
    alias: AliasedType,
    optional: PrefixType,
    promise: PrefixType,
    error_union: ErrorUnion,
    error_set: ErrorSet,
    err: ErrorType,
    array: ArrayType,
    struct_type: StructType,
    module: ModuleType,
    tuple: TupleType,
    function: FunctionType,
    integer: PrimitiveType,
    float: PrimitiveType,
    boolean: PrimitiveType,
    byte: PrimitiveType,
    execution: PrimitiveType,
    failed: FailedType,
    // lazy: LazyType,

    pub const NamedType = struct {
        path: Path,
        // parameters: []const *const TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            std.debug.assert(self.path.segments.len > 0);
            try writer.writeAll(self.path.segments[0].name);
            for (self.path.segments[1..]) |segment| {
                try writer.print(".{s}", .{segment.name});
            }
        }
    };

    pub const AliasedType = struct {
        name: []const u8,
        span: Span,
        type_expr: *const TypeExpr,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll(self.name);
        }
    };

    pub const PrefixType = struct {
        child: *const TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("{f}", .{self.child});
        }
    };

    pub const ErrorUnion = struct {
        err_set: *const TypeExpr,
        payload: *const TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("{f}!{f}", .{ self.err_set, self.payload });
        }
    };

    pub const ErrorSet = struct {
        error_types: []*const TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("error{");
            for (self.error_types, 0..) |err, i| {
                try err.format(writer);
                if (i < self.error_types.len - 1) {
                    try writer.writeByte(',');
                }
            }
            try writer.writeByte('}');
        }
    };

    pub const ErrorType = struct {
        error_payload: *const TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("Error({f})", .{self.error_payload});
        }
    };

    pub const ArrayType = struct {
        element: *const TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("[]{f}", .{self.element});
        }
    };

    pub const StructType = struct {
        fields: []const StructField,
        decls: []const StructDecl,
        span: Span,

        pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("<struct>");
        }
    };

    pub const ModuleType = struct {
        path: []const u8,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("<module:{s}>", .{self.path});
        }
    };

    pub const StructField = struct {
        name: Identifier,
        type_expr: *const TypeExpr,
        span: Span,

        pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("<struct_field>");
        }
    };

    pub const StructDecl = struct {
        name: Identifier,
        type_expr: ?*const TypeExpr,
        decl_source: union(enum) {
            fn_decl: *const FunctionDecl,
            binding_decl: *const BindingDecl,
        },
        span: Span,

        pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("<struct_decl>");
        }
    };

    pub const TupleType = struct {
        elements: []const *const TypeExpr,
        span: Span,

        pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("<tuple>");
        }
    };

    pub const FunctionType = struct {
        params: Parameters,
        stdin_type: ?*const TypeExpr,
        return_type: ?*const TypeExpr,
        span: Span,

        pub const Parameters = union(enum) {
            _non_variadic: []const ?*const TypeExpr,
            _variadic: ?*const TypeExpr,

            pub fn nonVariadic(_non_variadic: []const ?*const TypeExpr) @This() {
                return .{ ._non_variadic = _non_variadic };
            }

            pub fn variadic(_variadic: ?*const TypeExpr) @This() {
                return .{ ._variadic = _variadic };
            }
        };

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("fn (");
            switch (self.params) {
                ._non_variadic => |params| {
                    for (params, 0..) |param, i| {
                        try if (param) |p| p.format(writer) else writer.writeAll("<not_found>");

                        if (i < params.len - 1) {
                            try writer.writeAll(", ");
                        }
                    }
                },
                ._variadic => |params| try writer.print("...[]{?f}", .{params}),
            }
            try writer.writeAll(") ");
            try if (self.return_type) |r| r.format(writer) else writer.writeAll("<not_found>");
        }
    };

    pub const PrimitiveType = struct {
        span: Span,
    };

    pub const LazyType = struct {
        type_expr: ?*const TypeExpr = null,
        dependencies: std.ArrayList(*LazyType) = .empty,
        dependees: std.ArrayList(*LazyType) = .empty,
        materializer: Materializer,

        const Materializer = union(enum) {
            same: struct {
                pub fn materialize(
                    _: @This(),
                    _: std.mem.Allocator,
                    type_expr: *const TypeExpr,
                ) std.mem.Allocator!*const TypeExpr {
                    return type_expr;
                }
            },

            pub fn materialize(
                self: Materializer,
                allocator: std.mem.Allocator,
                type_expr: *const TypeExpr,
            ) std.mem.Allocator!*const TypeExpr {
                return switch (self) {
                    inline else => |m| m.materialize(allocator, type_expr),
                };
            }
        };

        pub fn materializeType(
            self: *LazyType,
            allocator: std.mem.Allocator,
            type_expr: *const TypeExpr,
        ) std.mem.Allocator.Error!void {
            self.type_expr = try self.materializer.materialize(allocator, type_expr);
            for (self.dependees.items) |dependee| {
                try dependee.materializeType(type_expr);
            }
        }

        pub fn resolveType(
            self: *@This(),
            _: std.mem.Allocator,
            _: *semantic.Scope,
        ) semantic.Scope.Error!?*const TypeExpr {
            return self.type_expr;
        }
    };

    pub const FailedType = struct {
        span: Span,

        pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("<failed to resolve>");
        }
    };

    pub fn allocFailed(allocator: std.mem.Allocator, span_: Span) !*const TypeExpr {
        const type_expr = try allocator.create(TypeExpr);
        type_expr.* = .{ .failed = .{ .span = span_ } };
        return type_expr;
    }

    pub fn span(self: TypeExpr) Span {
        return switch (self) {
            inline else => |s| s.span,
        };
    }

    pub fn format(self: *const TypeExpr, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.*) {
            .void => {
                try writer.writeAll("Void");
            },
            .optional => |optional| {
                try writer.writeByte('?');
                try optional.format(writer);
            },
            .promise => |promise| {
                try writer.writeByte('^');
                try promise.format(writer);
            },
            .integer => {
                try writer.writeAll("Int");
            },
            .float => {
                try writer.writeAll("Float");
            },
            .boolean => {
                try writer.writeAll("Bool");
            },
            .byte => {
                try writer.writeAll("Byte");
            },
            .execution => {
                try writer.writeAll("Execution");
            },
            inline else => |s| try s.format(writer),
        }
    }

    const globalByteType = TypeExpr{
        .byte = .{ .span = .global },
    };
    const globalStringType = TypeExpr{
        .array = .{ .element = &globalByteType, .span = .global },
    };
    const executableParameterType = globalStringType;
    const executableReturnType = TypeExpr{
        .execution = .{ .span = .global },
    };
    pub const executableType = TypeExpr{
        .function = .{
            .params = .variadic(&executableParameterType),
            // TODO: implement
            .stdin_type = null,
            .return_type = &executableReturnType,
            .span = .global,
        },
    };
};

/// StringLiteral supports interpolation segments so command arguments and
/// expressions can embed `${expr}` inline.
pub const StringLiteral = struct {
    segments: []const Segment,
    span: Span,

    pub const Segment = union(enum) {
        text: Spanned([]const u8),
        interpolation: *Expression,

        pub fn span(self: Segment) Span {
            return switch (self) {
                .text => |s| s.span,
                .interpolation => |s| s.span(),
            };
        }
    };
};

pub const Literal = union(enum) {
    integer: IntegerLiteral,
    float: FloatLiteral,
    bool: BoolLiteral,
    string: StringLiteral,
    null: NullLiteral,

    pub fn span(self: Literal) Span {
        return switch (self) {
            inline else => |s| s.span,
        };
    }

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        const type_expr = try allocator.create(TypeExpr);

        type_expr.* = switch (self.*) {
            .integer => .{ .integer = .{ .span = self.span() } },
            .float => .{ .float = .{ .span = self.span() } },
            .bool => .{ .boolean = .{ .span = self.span() } },
            .string => brk: {
                const element = try allocator.create(TypeExpr);
                element.* = .{ .byte = .{ .span = self.span() } };
                break :brk .{
                    .array = .{ .element = element, .span = self.span() },
                };
            },
            else => {
                allocator.destroy(type_expr);
                return null;
            },
        };

        return type_expr;
    }
};

pub const IntegerLiteral = struct {
    text: []const u8,
    span: Span,

    pub fn fromToken(tok: token.Token) IntegerLiteral {
        return .{
            .text = tok.lexeme,
            .span = tok.span,
        };
    }

    pub fn resolveType(
        self: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return .{ .integer = .{ .span = self.span } };
    }
};

pub const FloatLiteral = struct {
    text: []const u8,
    span: Span,
};

pub const BoolLiteral = struct {
    value: bool,
    span: Span,
};

pub const NullLiteral = struct {
    span: Span,
};

/// Expressions cover values, pipelines, and every control-flow construct used
/// within other expressions. Nested expressions are stored as pointers.
pub const Expression = union(enum) {
    identifier: Identifier,
    path: Path,
    literal: Literal,
    array: ArrayLiteral,
    map: MapLiteral,
    range: RangeLiteral,
    pipeline: Pipeline,
    pipeline_deprecated: Pipeline_deprecated,
    call: CallExpr,
    member: MemberExpr,
    index: IndexExpr,
    unary: UnaryExpr,
    binary: BinaryExpr,
    block: Block,
    fn_decl: FunctionDecl,
    if_expr: IfExpr,
    for_expr: ForExpr,
    match_expr: MatchExpr,
    try_expr: TryExpr,
    catch_expr: CatchExpr,
    import_expr: ImportExpr,
    assignment: Assignment,
    executable: ExecutableExpr,
    builtin: BuiltinExpr,

    pub fn span(self: Expression) Span {
        return switch (self) {
            .literal => |lit| lit.span(),
            inline else => |expr| expr.span,
        };
    }

    pub fn from(expr: anytype) Expression {
        inline for (std.meta.fields(Expression)) |field| {
            if (field.type == @TypeOf(expr)) {
                return @unionInit(Expression, field.name, expr);
            }
        }

        @compileError("Invalid expression pass to Expression.from: " ++ @typeName(@TypeOf(expr)));
    }

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return switch (self.*) {
            inline else => |*expr| expr.resolveType(allocator, scope),
        };
    }
};

pub const ArrayLiteral = struct {
    elements: []const *Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const MapLiteral = struct {
    entries: []const Entry,
    span: Span,

    pub const Entry = struct {
        key: *Expression,
        value: *Expression,
        span: Span,
    };

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const RangeLiteral = struct {
    start: *Expression,
    end: ?*Expression,
    inclusive_end: bool,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const CallExpr = struct {
    callee: *Expression,
    arguments: []const *Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        // 1. turn expression into function (or already function)
        // 2. return return type of function
        return null;
    }
};

pub const MemberExpr = struct {
    object: *Expression,
    member: Identifier,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const IndexExpr = struct {
    target: *Expression,
    index: *Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    operand: *Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const UnaryOp = enum {
    positive,
    negative,
    logical_not,
};

pub const BinaryExpr = struct {
    op: BinaryOp,
    left: *Expression,
    right: *Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const BinaryOp = enum {
    add,
    subtract,
    multiply,
    divide,
    remainder,
    greater,
    greater_equal,
    less,
    less_equal,
    not_equal,
    equal,
    logical_and,
    logical_or,
    /// (Stream(T) | fn Stream(T) (...Args) Stream(E!U)) : Stream(E!U)
    /// (Stream(E1!T) | fn Stream(T) (...Args) Stream(E2!U)) : Stream(E1||E2!U)
    /// (Stream(E1||E2!U) || do_something_else) : Stream(U)
    /// (Stream(String) | parseFloat | Stream(Float) + Stream(Float))
    pipe,
    apply,
    member,
    assign,

    pub fn precedence(self: BinaryOp) usize {
        return switch (self) {
            .add => 20,
            .subtract => 20,
            .multiply => 30,
            .divide => 30,
            .remainder => 30,
            .greater => 15,
            .greater_equal => 15,
            .less => 15,
            .less_equal => 15,
            .not_equal => 15,
            .equal => 15,
            .logical_and => 10,
            .logical_or => 5,
            .pipe => 50,
            .apply => 70,
            .member => 90,
            .assign => 0,
        };
    }

    pub fn fromToken(tok: token.Token) ?BinaryOp {
        return switch (tok.tag) {
            .plus => .add,
            .minus => .subtract,
            .star => .multiply,
            .slash => .divide,
            .percent => .remainder,
            .greater => .greater,
            .greater_equal => .greater_equal,
            .less => .less,
            .less_equal => .less_equal,
            .bang_equal => .not_equal,
            .kw_and => .logical_and,
            .kw_or => .logical_or,
            .equal_equal => .equal,
            .pipe => .pipe,
            .dot => .member,
            .assign => .assign,
            else => null,
        };
    }

    pub fn capturesStdin(self: @This()) bool {
        return switch (self) {
            .pipe => true,
            else => false,
        };
    }
};

pub const FunctionLiteral = struct {
    params: []const Parameter,
    return_type: ?*const TypeExpr,
    body: *Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const IfExpr = struct {
    condition: *Expression,
    capture: ?CaptureClause,
    then_expr: *Expression,
    else_branch: ?ElseBranch,
    span: Span,

    pub const ElseBranch = union(enum) {
        expr: *Expression,
        if_expr: *IfExpr,

        pub fn span(self: ElseBranch) Span {
            return switch (self) {
                .expr => |expr| expr.span(),
                .if_expr => |if_expr| if_expr.span,
            };
        }
    };

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const MatchExpr = struct {
    subject: *Expression,
    cases: []const MatchCase,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const MatchCase = struct {
    pattern: MatchPattern,
    capture: ?CaptureClause,
    body: Block,
    span: Span,
};

pub const MatchPattern = union(enum) {
    wildcard: Span,
    literal: Literal,
    path: Path,
    binding: Identifier,
    destructure: *BindingPattern,

    pub fn span(self: MatchPattern) Span {
        return switch (self) {
            .wildcard => |wildcard_span| wildcard_span,
            .literal => |lit| lit.span(),
            .path => |path| path.span,
            .binding => |binding| binding.span,
            .destructure => |pattern| pattern.span(),
        };
    }
};

pub const TryExpr = struct {
    subject: *Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const CatchExpr = struct {
    subject: *Expression,
    handler: CatchClause,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const CatchClause = struct {
    binding: ?*BindingPattern,
    body: Block,
    span: Span,
};

pub const ImportExpr = struct {
    importer: []const u8,
    module_name: []const u8,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        const module_path = resolveModulePath(
            allocator,
            self.importer,
            self.module_name,
        ) catch return null;

        const type_expr = try allocator.create(TypeExpr);
        type_expr.* = .{ .module = .{ .path = module_path, .span = self.span } };
        return type_expr;
    }
};

pub const Pipeline = struct {
    stages: []*Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

/// Pipelines model chained command and expression stages. Each stage carries a
/// `StageRole` so the runtime can distinguish external commands from pure
/// expressions while preserving location metadata for diagnostics.
pub const Pipeline_deprecated = struct {
    stages: []const PipelineStage,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        if (self.stages.len == 1) {
            const stage = self.stages[0];

            return switch (stage.payload) {
                .expression => |expr| return expr.resolveType(allocator, scope),
                .command => |command| switch (command.name) {
                    .word => |word| {
                        const binding = scope.lookup(
                            word.text,
                        ) orelse return TypeExpr.allocFailed(allocator, self.span);
                        return binding.type_expr;
                    },
                    else => TypeExpr.allocFailed(allocator, self.span),
                },
            };
        }

        return TypeExpr.allocFailed(allocator, self.span);
    }
};

pub const PipelineStage = struct {
    role: StageRole,
    payload: StagePayload,
    span: Span,
};

pub const StageRole = enum {
    command,
    expression,
};

pub const StagePayload = union(enum) {
    command: CommandInvocation,
    expression: *Expression,
};

pub const CommandInvocation = struct {
    name: CommandPart,
    args: []const CommandPart,
    env_assignments: []const EnvAssignment,
    redirects: []const Redirection,
    capture: ?CaptureDirective,
    background: bool,
    span: Span,
};

pub const CommandPart = union(enum) {
    word: CommandWord,
    string: StringLiteral,
    expr: *Expression,

    pub fn span(self: CommandPart) Span {
        return switch (self) {
            .expr => |expr| expr.span(),
            inline else => |s| s.span,
        };
    }
};

pub const CommandWord = struct {
    text: []const u8,
    span: Span,

    pub fn fromToken(tok: token.Token) CommandWord {
        return .{
            .text = tok.lexeme,
            .span = tok.span,
        };
    }
};

pub const EnvAssignment = struct {
    name: Identifier,
    value: *Expression,
    span: Span,
};

pub const Redirection = struct {
    stream: StreamRef,
    mode: RedirectionMode,
    target: RedirectionTarget,
    span: Span,
};

pub const StreamRef = union(enum) {
    stdin,
    stdout,
    stderr,
    descriptor: u8,
};

pub const RedirectionMode = enum {
    truncate,
    append,
};

pub const RedirectionTarget = union(enum) {
    variable: Identifier,
    expression: *Expression,
};

pub const CaptureDirective = struct {
    entries: []const CaptureEntry,
    span: Span,
};

pub const CaptureEntry = struct {
    stream: CaptureStream,
    mode: CaptureMode,
    span: Span,
};

pub const CaptureStream = enum {
    stdin,
    stdout,
    stderr,
};

pub const CaptureMode = enum {
    inherit,
    buffer,
    stream,
};

pub const BashBlock = struct {
    body: []const u8,
    span: Span,
};

/// Script is the root of every parsed Runic file.
pub const Script = Block;

/// Statements represent top-level items as well as imperative expressions.
pub const Statement = union(enum) {
    type_binding_decl: TypeBindingDecl,
    binding_decl: BindingDecl,
    expression: ExpressionStmt,

    error_decl: ErrorDecl,
    return_stmt: ReturnStmt,
    while_stmt: WhileStmt,
    bash_block: BashBlock,

    pub fn span(self: Statement) Span {
        return switch (self) {
            .type_binding_decl => |decl| decl.span,
            .binding_decl => |decl| decl.span,
            .error_decl => |err| err.span,
            .return_stmt => |ret| ret.span,
            // .for_stmt => |loop_stmt| loop_stmt.span,
            .while_stmt => |loop_stmt| loop_stmt.span,
            .bash_block => |bash_block| bash_block.span,
            .expression => |expr_stmt| expr_stmt.span,
        };
    }

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return switch (self) {
            inline else => |s| s.resolveType(allocator, scope),
        };
    }
};

pub const TypeBindingDecl = struct {
    identifier: Identifier,
    type_expr: *const TypeExpr,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const BindingDecl = struct {
    is_mutable: bool,
    pattern: *BindingPattern,
    annotation: ?*const TypeExpr,
    initializer: *Expression,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return self.annotation orelse self.initializer.resolveType(allocator, scope);
    }
};

pub const Parameter = struct {
    pattern: *BindingPattern,
    type_annotation: ?*const TypeExpr,
    default_value: ?*Expression,
    is_mutable: bool,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        if (self.type_annotation) |type_annot| return type_annot;
        if (self.default_value) |def_val| return def_val.resolveType(
            allocator,
            scope,
        );
        return semantic.Scope.Error.TypeNotFound;
    }
};

pub const FunctionDecl = struct {
    name: ?Identifier,
    params: Parameters,
    stdin_type: ?*const TypeExpr,
    return_type: ?*const TypeExpr,
    body: *Expression,
    span: Span,

    pub const Parameters = union(enum) {
        _non_variadic: []*Parameter,
        _variadic: *Parameter,

        pub const none = nonVariadic(&.{});

        pub fn nonVariadic(_non_variadic: []*Parameter) @This() {
            return .{ ._non_variadic = _non_variadic };
        }

        pub fn variadic(_variadic: *Parameter) @This() {
            return .{ ._variadic = _variadic };
        }
    };

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        const params_types: TypeExpr.FunctionType.Parameters = switch (self.params) {
            ._non_variadic => |params| brk: {
                const params_types = try allocator.alloc(?*const TypeExpr, params.len);

                for (params_types, params) |*param_type, param| {
                    param_type.* = try param.resolveType(allocator, scope);
                }

                break :brk .nonVariadic(params_types);
            },
            ._variadic => |params| .variadic(try params.resolveType(allocator, scope)),
        };

        const fn_type = try allocator.create(TypeExpr);

        fn_type.* = .{ .function = .{
            .params = params_types,
            .stdin_type = self.stdin_type,
            .return_type = self.return_type,
            .span = self.span,
        } };

        return fn_type;
    }
};

pub const ExecutableExpr = struct {
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const BuiltinExpr = struct {
    tag: Tag,
    span: Span,

    pub const Tag = enum { inspect };

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const ErrorDecl = struct {
    name: Identifier,
    definition: ErrorBody,
    span: Span,
};

pub const ErrorBody = union(enum) {
    enumeration: EnumBody,
    union_type: UnionBody,
};

pub const EnumBody = struct {
    variants: []const EnumVariant,
    span: Span,
};

pub const EnumVariant = struct {
    name: Identifier,
    span: Span,
};

pub const UnionBody = struct {
    variants: []const UnionVariant,
    span: Span,
};

pub const UnionVariant = struct {
    name: Identifier,
    payload: ?*const TypeExpr,
    span: Span,
};

pub const ModulePath = struct {
    literal: StringLiteral,
    span: Span,
};

pub const ReturnStmt = struct {
    value: ?*Expression,
    span: Span,
};

pub const ForExpr = struct {
    sources: []const *Expression,
    capture: CaptureClause,
    body: *Expression,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return null;
    }
};

pub const WhileStmt = struct {
    condition: *Expression,
    capture: ?CaptureClause,
    body: Block,
    span: Span,
};

pub const Assignment = struct {
    identifier: Identifier,
    expr: *Expression,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return self.expr.resolveType(allocator, scope);
    }
};

pub const ExpressionStmt = struct {
    expression: *Expression,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*const TypeExpr {
        return self.expression.resolveType(allocator, scope);
    }
};
