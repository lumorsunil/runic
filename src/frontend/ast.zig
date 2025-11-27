const std = @import("std");
const token = @import("token.zig");

pub const Span = token.Span;
pub const Spanned = token.Spanned;

/// Identifiers back every binding name as well as module and member
/// references. The slice is backed by the original source text.
pub const Identifier = struct {
    name: []const u8,
    span: Span,
};

/// Path represents qualified identifiers such as `error.FileError.NotFound`.
pub const Path = struct {
    segments: []const Identifier,
    span: Span,
};

/// Blocks appear in functions, control flow, and expression contexts. The
/// statements are stored as pointers so the arena allocator can own them in
/// one place.
pub const Block = struct {
    statements: []const *Statement,
    span: Span,
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
    identifier: NamedType,
    optional: PrefixType,
    promise: PrefixType,
    error_union: ErrorUnion,
    array: ArrayType,
    struct_type: StructType,
    tuple: TupleType,
    function: FunctionType,

    pub const NamedType = struct {
        path: Path,
        parameters: []const *TypeExpr,
        span: Span,
    };

    pub const PrefixType = struct {
        child: *TypeExpr,
        span: Span,
    };

    pub const ErrorUnion = struct {
        err_set: *TypeExpr,
        payload: *TypeExpr,
        span: Span,
    };

    pub const ArrayType = struct {
        element: *TypeExpr,
        span: Span,
    };

    pub const StructType = struct {
        fields: []const StructField,
        decls: []const StructDecl,
        span: Span,
    };

    pub const StructField = struct {
        name: Identifier,
        type_expr: *TypeExpr,
        span: Span,
    };

    pub const StructDecl = struct {
        name: Identifier,
        type_expr: ?*TypeExpr,
        decl_source: union(enum) {
            fn_decl: *FunctionDecl,
            binding_decl: *BindingDecl,
        },
        span: Span,
    };

    pub const TupleType = struct {
        elements: []const *TypeExpr,
        span: Span,
    };

    pub const FunctionType = struct {
        params: []const *TypeExpr,
        return_type: *TypeExpr,
        span: Span,
    };

    pub fn span(self: TypeExpr) Span {
        return switch (self) {
            .identifier => |named| named.span,
            .optional => |opt| opt.span,
            .promise => |promise| promise.span,
            .error_union => |err| err.span,
            .array => |array| array.span,
            .struct_type => |record| record.span,
            .tuple => |tuple| tuple.span,
            .function => |func| func.span,
        };
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
            .integer => |int| int.span,
            .float => |flt| flt.span,
            .bool => |b| b.span,
            .string => |str| str.span,
            .null => |n| n.span,
        };
    }
};

pub const IntegerLiteral = struct {
    text: []const u8,
    span: Span,
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
    async_expr: AsyncExpr,
    await_expr: AwaitExpr,
    if_expr: IfExpr,
    match_expr: MatchExpr,
    try_expr: TryExpr,
    catch_expr: CatchExpr,
    import_expr: ImportExpr,

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
};

pub const ArrayLiteral = struct {
    elements: []const *Expression,
    span: Span,
};

pub const MapLiteral = struct {
    entries: []const Entry,
    span: Span,

    pub const Entry = struct {
        key: *Expression,
        value: *Expression,
        span: Span,
    };
};

pub const RangeLiteral = struct {
    start: *Expression,
    end: ?*Expression,
    inclusive_end: bool,
    span: Span,
};

pub const CallExpr = struct {
    callee: *Expression,
    arguments: []const CallArgument,
    span: Span,
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
};

pub const IndexExpr = struct {
    target: *Expression,
    index: *Expression,
    span: Span,
};

pub const UnaryExpr = struct {
    op: UnaryOp,
    operand: *Expression,
    span: Span,
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
};

pub const BinaryOp = enum {
    add,
    subtract,
    multiply,
    divide,
    remainder,
    logical_and,
    logical_or,
    equal,
    not_equal,
    greater,
    greater_equal,
    less,
    less_equal,
};

pub const FunctionLiteral = struct {
    params: []const Parameter,
    return_type: ?*TypeExpr,
    body: FunctionBody,
    span: Span,
};

pub const FunctionBody = union(enum) {
    block: Block,
    expression: *Expression,
};

pub const AsyncExpr = struct {
    body: Block,
    span: Span,
};

pub const AwaitExpr = struct {
    subject: *Expression,
    success: ?CaptureBlock,
    failure: ?CatchClause,
    span: Span,
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
    };
};

pub const MatchExpr = struct {
    subject: *Expression,
    cases: []const MatchCase,
    span: Span,
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
};

pub const CatchExpr = struct {
    subject: *Expression,
    handler: CatchClause,
    span: Span,
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
};

/// Pipelines model chained command and expression stages. Each stage carries a
/// `StageRole` so the runtime can distinguish external commands from pure
/// expressions while preserving location metadata for diagnostics.
pub const Pipeline = struct {
    stages: []const PipelineStage,
    span: Span,
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
pub const Script = struct {
    statements: []const *Statement,
    span: Span,
};

/// Statements represent top-level items as well as imperative expressions.
pub const Statement = union(enum) {
    binding_decl: BindingDecl,
    fn_decl: FunctionDecl,
    error_decl: ErrorDecl,
    return_stmt: ReturnStmt,
    for_stmt: ForStmt,
    while_stmt: WhileStmt,
    bash_block: BashBlock,
    expression: ExpressionStmt,

    pub fn span(self: Statement) Span {
        return switch (self) {
            .binding_decl => |decl| decl.span,
            .fn_decl => |fn_decl| fn_decl.span,
            .error_decl => |err| err.span,
            .return_stmt => |ret| ret.span,
            .for_stmt => |loop_stmt| loop_stmt.span,
            .while_stmt => |loop_stmt| loop_stmt.span,
            .bash_block => |bash_block| bash_block.span,
            .expression => |expr_stmt| expr_stmt.span,
        };
    }
};

pub const BindingDecl = struct {
    is_mutable: bool,
    pattern: *BindingPattern,
    annotation: ?*TypeExpr,
    initializer: *Expression,
    span: Span,
};

pub const Parameter = struct {
    name: Identifier,
    type_annotation: ?*TypeExpr,
    default_value: ?*Expression,
    is_mutable: bool,
    span: Span,
};

pub const FunctionDecl = struct {
    name: Identifier,
    is_async: bool,
    params: []const Parameter,
    return_type: ?*TypeExpr,
    body: FunctionBody,
    span: Span,
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

pub const ForStmt = struct {
    sources: []const *Expression,
    capture: CaptureClause,
    body: Block,
    span: Span,
};

pub const WhileStmt = struct {
    condition: *Expression,
    capture: ?CaptureClause,
    body: Block,
    span: Span,
};

pub const ExpressionStmt = struct {
    expression: *Expression,
    span: Span,
};
