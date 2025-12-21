const std = @import("std");
const ast = @import("../frontend/ast.zig");
const Value = @import("../interpreter/value.zig").Value;

pub const Register = enum { a, b, c, d };

pub const ScopeLocation = []const u8;

pub const Location = union(enum) {
    register: Register,
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
};

pub const IRReadOnly = struct {
    data: []const []const u8,
    instructions: []const Instruction,
};

pub const IRContext = struct {
    read_only: IRReadOnly,
    registers: std.enums.EnumFieldStruct(Register, Value, null),
};

pub const Instruction = struct {
    source: Source,
    type: Type,

    pub const Source = union(enum) {
        expr: *ast.Expression,
        stmt: *ast.Statement,

        pub fn span(self: @This()) ast.Span {
            return switch (self) {
                inline else => |s| s.span(),
            };
        }
    };

    pub const Type = union(enum) {
        /// pushes a new scope
        push: Push,
        /// pops a scope
        pop,
        /// performs arithmetic operation with a and b and stores the result into result
        ath: Ath,
        /// compares a with b and stores the result into result
        cmp: Cmp,
        /// performs logical operator with a and b and stores the result into result
        log: Log,
        /// sets a Location to a Value from a Location
        set: Set,
        /// spawns a process using argv, env map and cwd from scope
        call,
    };

    pub fn init(source: Source, type_: Type) @This() {
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

    pub const Set = FromToLocation;
    pub const AthOp = enum { add, sub, mul, div, mod };
    pub const Ath = BinaryOperation(AthOp);
    pub const CmpOp = enum { gt, gte, lt, lte, eq, ne };
    pub const Cmp = BinaryOperation(CmpOp);
    pub const LogOp = enum { nd, r, n };
    pub const Log = BinaryOperation(LogOp);
};
