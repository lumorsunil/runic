const std = @import("std");
const ast = @import("../frontend/ast.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;

pub const Register = enum { a, b, c, d };

pub const Value = union(enum) {
    void,
    location: Location,
    boolean: bool,
    uinteger: usize,
    array: Array,
    slice: Slice,
    executable: []const u8,
    exit_code: ExitCode,
    addr: Addr,

    pub const Array = struct {
        element_size: usize,
        capacity: usize,
    };

    pub const Slice = struct {
        location: Location,
        len: usize,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("[{f}:{}]u8", .{
                self.location,
                self.len,
            });
        }
    };

    pub const Addr = union(enum) {
        rel: isize,
        abs: usize,
        label: LabelKey,

        pub const LabelKey = struct { usize, []const u8 };

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            switch (self) {
                .rel => |a| try w.print("<+{x}>", .{ self, a }),
                .abs => |a| try w.print("<{x}>", .{ self, a }),
                .label => |a| try w.print(":{s}_{}", .{ self, a.@"1", a.@"0" }),
            }
        }
    };

    pub const refSize: usize = @sizeOf(Location);

    pub fn array_(element_size: usize, capacity: usize) @This() {
        return .{ .array = .{ .element_size = element_size, .capacity = capacity } };
    }

    pub fn fromLocation(location: Location) @This() {
        return .{ .location = location };
    }

    pub fn toBytes(self: @This()) []const u8 {
        return std.mem.toBytes(self);
    }

    pub fn fromBytes(bytes: []const u8) @This() {
        return std.mem.bytesToValue(@This(), bytes);
    }

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .void => try w.writeAll("void"),
            .executable => |s| try w.print("{s}", .{s}),
            inline .location, .slice, .exit_code => |s| try w.print("{f}", .{s}),
            inline else => |t| try w.print("{}", .{t}),
        }
    }
};

pub const Ref = usize;

pub const ScopeLocation = []const u8;

pub const Location = union(enum) {
    register: Register,
    ref: Ref,
    scope: ScopeLocation,
    data: struct {
        page: usize,
        addr: usize,

        pub fn init(page: usize, addr: usize) @This() {
            return .{ .page = page, .addr = addr };
        }

        pub fn get(self: @This(), len: usize, data: []const []const u8) []const u8 {
            const end = self.addr + len;
            return data[self.page][self.addr..end];
        }
    },

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .register => |r| try w.print("%{t}", .{r}),
            .ref => |r| try w.print("@@{}", .{r}),
            .scope => |s| try w.print("@{s}", .{s}),
            .data => |d| try w.print("#{}:{}", .{ d.page, d.addr }),
        }
    }
};

pub const IRReadOnly = struct {
    data: []const []const u8,
    instructions: []const Instruction,
};

pub const IRContext = struct {
    read_only: IRReadOnly,
    registers: std.enums.EnumFieldStruct(Register, Value, .void),
    instruction_counter: usize = 0,
    stack: std.ArrayList(Value) = .empty,
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
                .push => |t| try w.print("{t} {f}", .{ self, t }),
                .exit => |t| try w.print("{t} {f}", .{ self, t }),
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
        dest: Value.Addr,

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
        try w.print("{f}", .{self.type});
    }
};
