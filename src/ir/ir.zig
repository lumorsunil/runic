const std = @import("std");
const ast = @import("../frontend/ast.zig");
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;

pub const Register = enum { a, b, c, d };

pub const Value = union(enum) {
    void,
    location: Location,
    boolean: bool,

    pub fn fromLocation(location: Location) @This() {
        return .{ .location = location };
    }

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .void => try w.writeAll("void"),
            .location => |location| try w.print("{f}", .{location}),
            inline else => |t| try w.print("{}", .{t}),
        }
    }
};

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

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        switch (self) {
            .register => |r| try w.print("%{t}", .{r}),
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
        /// spawns a process using argv, env map and cwd from scope
        call,
        /// exits the process
        exit: ExitCode,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            switch (self) {
                .push => |t| try w.print("{t} {f}", .{ self, t }),
                else => try w.print("{t}", .{self}),
            }
        }
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

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("{f}", .{self.type});
    }
};
