const std = @import("std");
const ast = @import("../frontend/ast.zig");
const Value = @import("value.zig").Value;
const Ref = @import("ref.zig").Ref;
const ExitCode = @import("../runtime/command_runner.zig").ExitCode;
const Location = @import("location.zig").Location;
const InstructionAddr = @import("instruction-addr.zig").InstructionAddr;

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

    pub fn span(self: @This()) ?ast.Span {
        const source = self.source orelse return null;
        return source.span();
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
