const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("../frontend/ast.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;

const endian = @import("builtin").cpu.arch.endian();

pub const TypeAddr = union(enum) {
    value: std.meta.Tag(Value),
    /// Element size
    slice: usize,
    struct_type: usize,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .struct_type => |struct_type| try writer.writeInt(
                usize,
                struct_type,
                endian,
            ),
            .slice => |s| try writer.print("{t}({})", .{ self, s }),
            .value => |t| try writer.print("{t}", .{t}),
        }
    }
};

pub const InstructionAddr = union(enum) {
    rel: isize,
    abs: usize,
    label: LabelKey,

    pub const LabelKey = struct {
        counter: usize,
        name: []const u8,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("{s}_{}", .{ self.name, self.counter });
        }
    };

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .rel => |a| try w.print("<+{x}>", .{a}),
            .abs => |a| try w.print("<{x}>", .{a}),
            .label => |a| try w.print(":{f}", .{a}),
        }
    }
};

pub const Value = union(enum) {
    void,
    uinteger: usize,
    slice: Slice,
    strct: Struct,
    exit_code: ExitCode,
    addr: usize,
    stream: []const Value,

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        switch (self.*) {
            .void, .location, .uinteger, .slice, .exit_code, .addr => {},
            .strct => |strct| strct.deinit(allocator),
            .stream => |stream| allocator.free(stream),
        }
    }

    pub const Slice = struct {
        addr: usize,
        element_size: usize,
        len: usize,

        pub fn empty(element_size: usize) @This() {
            return Slice{
                .addr = 0,
                .element_size = element_size,
                .len = 0,
            };
        }

        pub fn size() usize {
            return @sizeOf(usize) + @sizeOf(usize);
        }

        pub fn serialize(self: @This(), w: *std.Io.Writer) !void {
            try w.writeInt(usize, self.addr, endian);
            try w.writeInt(usize, self.len, endian);
        }

        pub fn deserialize(
            element_size: usize,
            r: *std.Io.Reader,
        ) std.Io.Reader.Error!@This() {
            const addr = try r.takeInt(usize, endian);
            const len = try r.takeInt(usize, endian);

            return .{
                .addr = addr,
                .element_size = element_size,
                .len = len,
            };
        }

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("[{x}..+{}]u8", .{
                self.addr,
                self.len,
            });
        }
    };

    pub const Struct = struct {
        type: usize,
        fields: []const Value,

        pub fn deinit(self: @This(), allocator: Allocator) void {
            for (self.fields) |*field| {
                field.deinit(allocator);
            }
            allocator.free(self.fields);
        }

        pub fn size(self: Struct) usize {
            return self.type.size();
        }

        pub fn serialize(self: Struct, w: *std.Io.Writer) std.Io.Writer.Error!void {
            for (self.fields) |field| {
                try field.serialize(w);
            }
        }

        pub fn deserialize(
            context: DeserializeStructContext,
            struct_type: usize,
            r: *std.Io.Reader,
        ) !@This() {
            const type_ = context.get(struct_type);
            const fields = try context.allocFields(struct_type);

            for (fields, type_.fields.values()) |*field, field_type_addr| {
                field.* = try switch (field_type_addr) {
                    .struct_type => |staddr| Struct.deserialize(context, staddr, r),
                    .value => |t| Value.deserialize(t, r),
                };
            }

            return .{
                .type = type_,
                .fields = fields,
            };
        }

        pub const DeserializeStructContext = struct {
            allocator: Allocator,
            struct_types: []const Type,

            pub fn get(self: @This(), struct_type: usize) Type {
                return self.struct_types[struct_type];
            }

            pub fn allocFields(self: @This(), struct_type: usize) ![]Value {
                const type_ = self.get(struct_type);
                return self.allocator.alloc(Value, type_.fields.count());
            }
        };

        pub const Type = struct {
            name: []const u8,
            decls: std.StringArrayHashMapUnmanaged(Decl) = .empty,
            fields: std.StringArrayHashMapUnmanaged(TypeAddr) = .empty,

            pub fn size(self: @This()) usize {
                return @reduce(.Add, self.fields.values());
            }

            pub const Decl = union(enum) {
                function: Function,

                pub const Function = struct {
                    signature: Signature,
                    addr: usize,

                    pub const Signature = struct {
                        params: []const Param,

                        pub const Param = struct { name: []const u8 };
                    };
                };
            };
        };
    };

    pub const refSize: usize = @sizeOf(Location);

    pub fn fromAddr(addr: usize) @This() {
        return .{ .addr = addr };
    }

    pub fn toBytes(self: @This()) []const u8 {
        return std.mem.toBytes(self);
    }

    pub fn fromBytes(bytes: []const u8) @This() {
        return std.mem.bytesToValue(@This(), bytes);
    }

    pub const ToStreamError = Allocator.Error || error{UnsupportedStreamCast};

    pub fn toStream(self: @This(), allocator: Allocator) ToStreamError!@This() {
        return switch (self) {
            .stream => self,
            .slice => .{ .stream = try allocator.dupe(Value, &.{self}) },
            else => ToStreamError.UnsupportedStreamCast,
        };
    }

    pub const DeserializeError = std.Io.Reader.Error || error{UnsupportedDeserialize};

    pub fn deserialize(
        tag: std.meta.Tag(@This()),
        r: *std.Io.Reader,
    ) DeserializeError!@This() {
        return switch (tag) {
            .void => .void,
            .uinteger => .{ .uinteger = try r.takeInt(usize, endian) },
            .addr => .{ .addr = try r.takeInt(usize, endian) },
            .stream => .{ .stream = std.mem.bytesAsValue(
                []Value,
                try r.takeArray(@sizeOf([]Value)),
            ).* },
            .slice, .strct => DeserializeError.UnsupportedDeserialize,
            .exit_code => .{ .exit_code = try ExitCode.deserialize(r) },
        };
    }

    pub fn serialize(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .void => {},
            .stream => |stream| try w.writeAll(&std.mem.toBytes(stream)),
            inline .uinteger => |t| try w.writeInt(@TypeOf(t), t, endian),
            inline else => |t| {
                if (std.meta.hasMethod(@TypeOf(t), "serialize")) {
                    return t.serialize(w);
                } else {
                    return w.writeAll(&std.mem.toBytes(self));
                }
            },
        }
    }

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .void => try w.writeAll("void"),
            .addr => |addr| try w.print("{x}", .{addr}),
            .stream => try w.writeAll("<stream>"),
            inline .slice, .exit_code => |s| try w.print("{f}", .{s}),
            inline else => |t| try w.print("{}", .{t}),
        }
    }
};

pub const Ref = struct {
    addr: usize,
    name: []const u8 = "<unknown>",

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("@@{s}({x})", .{ self.name, self.addr });
    }
};

pub const ScopeLocation = []const u8;

pub const Location = union(enum) {
    ref: Ref,
    scope: ScopeLocation,
    stack: usize,
    instruction: InstructionAddr,
    data: struct {
        page: usize,
        addr: usize,

        pub const zero = @This(){ .page = 0, .addr = 0 };

        pub fn init(page: usize, addr: usize) @This() {
            return .{ .page = page, .addr = addr };
        }

        pub fn fromAddr(addr: usize) @This() {
            const page = @divFloor(addr, IRReadOnly.page_size);
            return .init(page, @mod(addr, IRReadOnly.page_size));
        }

        pub fn get(self: @This(), len: usize, data: []const []const u8) []const u8 {
            const end = self.addr + len;
            return data[self.page][self.addr..end];
        }
    },

    pub const Error = error{UnsupportedAddrType};

    pub fn toAddr(self: Location) Error!usize {
        return switch (self) {
            .data => |data| data.page * IRReadOnly.page_size + data.addr,
            .ref => |ref| ref.addr,
            .scope => Error.UnsupportedAddrType,
            .stack => |stack| IRContext.stack_start + stack,
            .instruction => Error.UnsupportedAddrType,
        };
    }

    pub fn size() usize {
        return @sizeOf(Location);
    }

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .ref => |r| try w.print("{f}", .{r}),
            .scope => |s| try w.print("@{s}", .{s}),
            .data => |d| try w.print("#{}:{}", .{ d.page, d.addr }),
            .stack => |s| try w.print("S{}", .{s}),
            .instruction => |i| try w.print("I{f}", .{i}),
        }
    }
};

pub const IRReadOnly = struct {
    data: []const []const u8,
    instructions: []const Instruction,

    pub const page_size = 1024 * 4;

    pub fn dataSize(self: IRReadOnly) usize {
        if (self.data.len == 0) return 0;
        return (self.data.len - 1) * page_size + self.data[self.data.len - 1].len;
    }
};

pub const Labels = struct {
    map: std.ArrayHashMapUnmanaged(
        InstructionAddr.LabelKey,
        ?usize,
        HashContext,
        true,
    ) = .empty,

    const HashContext = struct {
        pub fn hash(_: @This(), key: InstructionAddr.LabelKey) u32 {
            var hasher = std.hash.Wyhash.init(0);
            std.hash.autoHash(&hasher, key.counter);
            return @truncate(hasher.final());
        }

        pub fn eql(
            _: @This(),
            a: InstructionAddr.LabelKey,
            b: InstructionAddr.LabelKey,
            _: usize,
        ) bool {
            return a.counter == b.counter;
        }
    };

    pub fn init() @This() {
        return .{};
    }

    pub fn new(
        self: *Labels,
        allocator: Allocator,
        name: []const u8,
        addr: ?usize,
    ) Allocator.Error!InstructionAddr.LabelKey {
        const label: InstructionAddr.LabelKey = .{ .counter = self.map.count(), .name = name };
        try self.set(allocator, label, addr);
        return label;
    }

    pub fn set(
        self: *Labels,
        allocator: Allocator,
        label: InstructionAddr.LabelKey,
        addr: ?usize,
    ) Allocator.Error!void {
        try self.map.put(allocator, label, addr);
    }

    pub fn get(
        self: *Labels,
        label: InstructionAddr.LabelKey,
    ) ?usize {
        return self.map.get(label).?;
    }

    pub fn sort(
        self: *Labels,
    ) void {
        return self.map.sort(self);
    }

    pub fn lessThan(ctx: *@This(), a_index: usize, b_index: usize) bool {
        return ctx.map.values()[a_index].? < ctx.map.values()[b_index].?;
    }
};

pub const IRContext = struct {
    read_only: IRReadOnly,
    instruction_counter: usize = 0,
    stack: std.ArrayList(Value) = .empty,
    labels: Labels,
    refs: std.AutoArrayHashMapUnmanaged(usize, Value) = .empty,
    struct_types: []const Value.Struct.Type,

    pub const stack_start: usize = std.math.maxInt(usize) - 1024 * 1024 * 10;

    pub fn mapAddr(self: *IRContext, addr: usize) Location {
        const data_end = self.read_only.dataSize();

        if (addr < data_end) {
            return .{ .data = .fromAddr(addr) };
        } else if (addr >= stack_start) {
            return .{ .stack = stack_start - addr };
        } else {
            return .{ .ref = .{ .addr = addr } };
        }
    }
};

pub const Label = struct {
    key: InstructionAddr.LabelKey,
    addr: usize,

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("{f}:", .{self.key});
    }
};

pub const Instruction = struct {
    source: ?Source,
    type: Type,

    pub const Source = union(enum) {
        expr: *ast.Expression,
        stmt: *ast.Statement,

        pub fn from(a: anytype) ?@This() {
            const T = @TypeOf(a);
            if (T == Source or T == ?Source) {
                return a;
            } else if (T == *ast.Expression) {
                return .fromExpr(a);
            } else if (T == *ast.Statement) {
                return .fromStmt(a);
            }

            @compileError("expected *Expression or *Statement, actual: " ++ @typeName(T));
        }

        pub fn fromExpr(expr: *ast.Expression) @This() {
            return .{ .expr = expr };
        }

        pub fn fromStmt(stmt: *ast.Statement) @This() {
            return .{ .stmt = stmt };
        }

        pub fn span(self: @This()) ast.Span {
            return switch (self) {
                inline else => |s| s.span(),
            };
        }

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            const span_ = self.span();
            var cwd_buffer: [512]u8 = undefined;
            const cwd = std.process.getCwd(&cwd_buffer) catch "";
            var file_name: []const u8 = span_.start.file;
            if (std.mem.startsWith(u8, span_.start.file, cwd)) {
                file_name = file_name[cwd.len..];
            }
            try writer.print("{s}:{}:{}", .{
                file_name,
                span_.start.line,
                span_.start.column,
            });
        }
    };

    pub const Type = union(enum) {
        /// pushes a new scope
        push_scope: Push,
        /// pops a scope
        pop_scope,
        /// push a Value to the stack
        push: Value,
        /// pop a Value from the stack
        pop,
        /// performs arithmetic operation with a and b and stores the result into result
        ath: Ath,
        /// compares a with b and stores the result into result
        cmp: Cmp,
        /// performs logical operator with a and b and stores the result into result
        log: Log,
        /// declares a new ref
        ref: Ref,
        /// sets a Location to a Value from a Location
        set: Set,
        /// sets the instruction counter if condition is true
        jmp: Jump,
        /// spawns a process using argv, env map and cwd from scope
        call,
        /// exits the process
        exit: ExitCode,

        pub fn push_(value: Value) @This() {
            return .{ .push = value };
        }

        pub fn exit_(exit_code: ExitCode) @This() {
            return .{ .exit = exit_code };
        }

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            switch (self) {
                inline .push, .exit, .jmp, .set, .ref => |t| try w.print("{t} {f}", .{ self, t }),
                else => try w.print("{t}", .{self}),
            }
        }
    };

    pub fn init(source: ?Source, type_: Type) @This() {
        return .{ .source = source, .type = type_ };
    }

    pub fn span(self: @This()) ast.Span {
        return self.source.span();
    }

    pub const Push = union(enum) {
        bindings,
        blocking,
        processes,
        allocating,
    };

    pub fn BinaryOperation(comptime OpType: type) type {
        return struct {
            op: OpType,
            a: Location,
            b: Location,
            result: Location,
        };
    }

    pub const FromToLocation = struct {
        from: Location,
        to: Location,
    };

    pub const Set = struct {
        location: Location,
        value: Value,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("{f}={f}", .{ self.location, self.value });
        }
    };
    pub const AthOp = enum { add, sub, mul, div, mod };
    pub const Ath = BinaryOperation(AthOp);
    pub const CmpOp = enum { gt, gte, lt, lte, eq, ne };
    pub const Cmp = BinaryOperation(CmpOp);
    pub const LogOp = enum { nd, r, n };
    pub const Log = BinaryOperation(LogOp);

    pub const Jump = struct {
        cond: ?Value,
        jump_if: bool = false,
        dest: InstructionAddr,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            if (self.cond) |cond| {
                try w.print("({s}{f}) {f}", .{
                    if (self.jump_if) "" else "!",
                    cond,
                    self.dest,
                });
            } else {
                try w.print("(true) {f}", .{self.dest});
            }
        }
    };

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("{f} ({?f})", .{
            self.type,
            if (self.source) |source| source else null,
        });
    }
};
