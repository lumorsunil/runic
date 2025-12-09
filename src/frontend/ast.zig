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

    pub fn resolveType(
        self: *@This(),
        _: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
        const binding = try scope.lookup(self.name) orelse return null;
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    // identifier: NamedType,
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
    // lazy: LazyType,

    pub const NamedType = struct {
        path: Path,
        parameters: []const *TypeExpr,
        span: Span,
    };

    pub const PrefixType = struct {
        child: *TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("{f}", .{self.child});
        }
    };

    pub const ErrorUnion = struct {
        err_set: *TypeExpr,
        payload: *TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("{f}!{f}", .{ self.err_set, self.payload });
        }
    };

    pub const ErrorSet = struct {
        error_types: []*TypeExpr,
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
        error_payload: *TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.print("Error({f})", .{self.error_payload});
        }
    };

    pub const ArrayType = struct {
        element: *TypeExpr,
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
        type_expr: *TypeExpr,
        span: Span,

        pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("<struct_field>");
        }
    };

    pub const StructDecl = struct {
        name: Identifier,
        type_expr: ?*TypeExpr,
        decl_source: union(enum) {
            fn_decl: *FunctionDecl,
            binding_decl: *BindingDecl,
        },
        span: Span,

        pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("<struct_decl>");
        }
    };

    pub const TupleType = struct {
        elements: []const *TypeExpr,
        span: Span,

        pub fn format(_: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("<tuple>");
        }
    };

    pub const FunctionType = struct {
        params: []const ?*TypeExpr,
        return_type: ?*TypeExpr,
        span: Span,

        pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("fn (");
            for (self.params, 0..) |param, i| {
                try if (param) |p| p.format(writer) else writer.writeAll("<not_found>");

                if (i < self.params.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            try writer.writeAll(") ");
            try if (self.return_type) |r| r.format(writer) else writer.writeAll("<not_found>");
        }
    };

    pub const PrimitiveType = struct {
        span: Span,
    };

    pub const LazyType = struct {
        type_expr: ?*TypeExpr = null,
        dependencies: std.ArrayList(*LazyType) = .empty,
        dependees: std.ArrayList(*LazyType) = .empty,
        materializer: Materializer,

        const Materializer = union(enum) {
            same: struct {
                pub fn materialize(
                    _: @This(),
                    _: std.mem.Allocator,
                    type_expr: *TypeExpr,
                ) std.mem.Allocator!*TypeExpr {
                    return type_expr;
                }
            },

            pub fn materialize(
                self: Materializer,
                allocator: std.mem.Allocator,
                type_expr: *TypeExpr,
            ) std.mem.Allocator!*TypeExpr {
                return switch (self) {
                    inline else => |m| m.materialize(allocator, type_expr),
                };
            }
        };

        pub fn materializeType(
            self: *LazyType,
            allocator: std.mem.Allocator,
            type_expr: *TypeExpr,
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
        ) semantic.Scope.Error!?*TypeExpr {
            return self.type_expr;
        }
    };

    pub fn span(self: TypeExpr) Span {
        return switch (self) {
            inline else => |s| s.span,
        };
    }

    pub fn format(self: *TypeExpr, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.*) {
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
            inline else => |s| try s.format(writer),
        }
    }
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    call: CallExpr,
    member: MemberExpr,
    index: IndexExpr,
    unary: UnaryExpr,
    binary: BinaryExpr,
    block: Block,
    fn_literal: FunctionLiteral,
    if_expr: IfExpr,
    for_expr: ForExpr,
    match_expr: MatchExpr,
    try_expr: TryExpr,
    catch_expr: CatchExpr,
    import_expr: ImportExpr,
    assignment: Assignment,

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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
        return null;
    }
};

pub const CallExpr = struct {
    callee: *Expression,
    arguments: []const CallArgument,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
        return null;
    }
};

pub const CallArgument = struct {
    label: ?Identifier = null,
    value: *Expression,
    span: Span,
};

pub const MemberExpr = struct {
    object: *Expression,
    member: Identifier,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
            else => null,
        };
    }
};

pub const FunctionLiteral = struct {
    params: []const Parameter,
    return_type: ?*TypeExpr,
    body: FunctionBody,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
        return null;
    }
};

pub const FunctionBody = union(enum) {
    block: Block,
    expression: *Expression,
};

pub const IfExpr = struct {
    condition: *Expression,
    capture: ?CaptureClause,
    then_block: Block,
    else_branch: ?ElseBranch,
    span: Span,

    pub const ElseBranch = union(enum) {
        block: Block,
        if_expr: *IfExpr,

        pub fn span(self: ElseBranch) Span {
            return switch (self) {
                inline else => |s| s.span,
            };
        }
    };

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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

/// Pipelines model chained command and expression stages. Each stage carries a
/// `StageRole` so the runtime can distinguish external commands from pure
/// expressions while preserving location metadata for diagnostics.
pub const Pipeline = struct {
    stages: []const PipelineStage,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
        return null;
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
    binding_decl: BindingDecl,
    fn_decl: FunctionDecl,
    expression: ExpressionStmt,

    error_decl: ErrorDecl,
    return_stmt: ReturnStmt,
    while_stmt: WhileStmt,
    bash_block: BashBlock,

    pub fn span(self: Statement) Span {
        return switch (self) {
            .binding_decl => |decl| decl.span,
            .fn_decl => |fn_decl| fn_decl.span,
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
    ) semantic.Scope.Error!?*TypeExpr {
        return switch (self) {
            inline else => |s| s.resolveType(allocator, scope),
        };
    }
};

pub const BindingDecl = struct {
    is_mutable: bool,
    pattern: *BindingPattern,
    annotation: ?*TypeExpr,
    initializer: *Expression,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
        return self.annotation orelse self.initializer.resolveType(allocator, scope);
    }
};

pub const Parameter = struct {
    pattern: *BindingPattern,
    type_annotation: ?*TypeExpr,
    default_value: ?*Expression,
    is_mutable: bool,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
        if (self.type_annotation) |type_annot| return type_annot;
        if (self.default_value) |def_val| return def_val.resolveType(
            allocator,
            scope,
        );
        return semantic.Scope.Error.TypeNotFound;
    }
};

pub const FunctionDecl = struct {
    name: Identifier,
    is_async: bool,
    params: []Parameter,
    return_type: ?*TypeExpr,
    body: FunctionBody,
    span: Span,

    pub fn resolveType(
        self: *@This(),
        allocator: std.mem.Allocator,
        scope: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
        const params_types = try allocator.alloc(?*TypeExpr, self.params.len);

        for (params_types, self.params) |*param_type, *param| {
            param_type.* = try param.resolveType(allocator, scope);
        }

        const fn_type = try allocator.create(TypeExpr);

        fn_type.* = .{ .function = .{
            .params = params_types,
            .return_type = self.return_type,
            .span = self.span,
        } };

        return fn_type;
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
    payload: ?*TypeExpr,
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
    body: Block,
    span: Span,

    pub fn resolveType(
        _: *@This(),
        _: std.mem.Allocator,
        _: *semantic.Scope,
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
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
    ) semantic.Scope.Error!?*TypeExpr {
        return self.expression.resolveType(allocator, scope);
    }
};
