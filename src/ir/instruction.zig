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

    pub const Type = union(enum) {
        /// forward program stdin, stdout and stderr
        fwd_stdio,
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
        /// declares a new ref (basically a labeled push)
        ref: []const u8,
        /// sets a Location to a Value from a Location
        set: Set,
        /// sets the instruction counter if condition is true
        jmp: Jump,
        /// creates a pipe and stores it's handle in the given location
        pipe: Pipe,
        /// changes a pipe option
        pipe_opt: PipeOption,
        /// forwards a pipe to another pipe
        pipe_fwd: Forward,
        /// executes an instruction atomically (all instructions executed at once)
        atomic: usize,
        /// spawns a process using argv, env map and cwd from scope
        exec: Exec,
        /// spawns a new thread at the given instruction addr
        fork: Fork,
        /// waits for a thread or process to be closed
        wait: Wait,
        /// streams a pipe until it is closed, blocking
        stream: Location,
        /// allocates n number of values on the heap, stores address in %r
        alloc: usize,
        /// exits the process
        exit: ExitCode,

        pub fn push_(value: Value) @This() {
            return .{ .push = value };
        }

        pub fn exit_(exit_code: ExitCode) @This() {
            return .{ .exit = exit_code };
        }

        pub fn fork_(
            dest: InstructionAddr,
            stdin: Location,
            stdout: Location,
            stderr: Location,
        ) @This() {
            return .{ .fork = .{
                .dest = dest,
                .stdin = stdin,
                .stdout = stdout,
                .stderr = stderr,
            } };
        }

        pub fn wait_(waitee: Location) @This() {
            return .{ .wait = .{ .waitee = waitee } };
        }

        pub fn stream_(streamee: Location) @This() {
            return .{ .stream = streamee };
        }

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            switch (self) {
                inline .push, .exit, .jmp, .fork, .set, .pipe_fwd, .wait, .stream, .pipe, .pipe_opt, .ath, .log, .cmp => |t| try w.print("{t} {f}", .{ self, t }),
                inline .ref => |t| try w.print("{t} {s}", .{ self, t }),
                inline .alloc => |t| try w.print("{t} {}", .{ self, t }),
                else => try w.print("{t}", .{self}),
            }
        }
    };

    pub const Source = union(enum) {
        expr: *ast.Expression,
        stmt: *ast.Statement,

        pub fn from(a: anytype) ?@This() {
            const T = @TypeOf(a);
            if (T == Source or T == ?Source or T == @TypeOf(null)) {
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

    pub fn init(source: ?Source, type_: Type) @This() {
        return .{ .source = source, .type = type_ };
    }

    pub fn span(self: @This()) ?ast.Span {
        const source = self.source orelse return null;
        return source.span();
    }

    pub fn BinaryOperation(comptime OpType: type) type {
        return struct {
            op: OpType,
            a: Value,
            b: Value,
            result: Location,

            pub fn format(
                self: @This(),
                writer: *std.Io.Writer,
            ) std.Io.Writer.Error!void {
                try writer.print("{f} = {f} {f} {f}", .{ self.result, self.a, self.op, self.b });
            }
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
    pub const AthOp = enum {
        add,
        sub,
        mul,
        div,
        mod,

        pub fn from(binary_op: ast.BinaryOp) @This() {
            return switch (binary_op) {
                .add => .add,
                .subtract => .sub,
                .multiply => .mul,
                .divide => .div,
                .remainder => .mod,
                else => unreachable,
            };
        }

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.writeAll(switch (self) {
                .add => "+",
                .sub => "-",
                .mul => "*",
                .div => "/",
                .mod => "%",
            });
        }
    };
    pub const Ath = BinaryOperation(AthOp);
    pub const CmpOp = enum {
        gt,
        gte,
        lt,
        lte,
        eq,
        ne,

        pub fn from(binary_op: ast.BinaryOp) @This() {
            return switch (binary_op) {
                .greater => .gt,
                .greater_equal => .gte,
                .less => .lt,
                .less_equal => .lte,
                .equal => .eq,
                .not_equal => .ne,
                else => unreachable,
            };
        }

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.writeAll(switch (self) {
                .gt => ">",
                .gte => ">=",
                .lt => "<",
                .lte => "<=",
                .eq => "==",
                .ne => "!=",
            });
        }
    };
    pub const Cmp = BinaryOperation(CmpOp);
    pub const LogOp = enum {
        nd,
        r,

        pub fn from(binary_op: ast.BinaryOp) @This() {
            return switch (binary_op) {
                .logical_and => .nd,
                .logical_or => .r,
                else => unreachable,
            };
        }

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.writeAll(switch (self) {
                .nd => "&&",
                .r => "||",
            });
        }
    };
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

    pub const Pipe = struct {
        result: Location,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("{f}", .{self.result});
        }
    };

    pub const PipeOption = struct {
        handle: Location,
        option: OptionType,
        value: Value,

        pub const OptionType = enum {
            keep_open,
            close_destination,
            disconnect_destination,
            close_source,
            disconnect_source,
        };

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("{f} {t} {f}", .{ self.handle, self.option, self.value });
        }
    };

    pub const Exec = struct {
        sync: bool,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            if (self.sync) try w.writeAll("sync");
            if (!self.sync) try w.writeAll("async");
        }
    };

    pub const Fork = struct {
        dest: InstructionAddr,
        stdin: Location,
        stdout: Location,
        stderr: Location,

        pub fn format(self: @This(), w: *std.Io.Writer) !void {
            try w.print("{f} {f} {f} {f}", .{ self.dest, self.stdin, self.stdout, self.stderr });
        }
    };

    pub const Forward = struct {
        source: Location,
        destination: Location,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("{f} {f}", .{ self.source, self.destination });
        }
    };

    pub const Wait = struct {
        waitee: Location,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("{f}", .{self.waitee});
        }
    };

    pub fn format(self: @This(), w: *std.Io.Writer) !void {
        try w.print("{f} ({?f})", .{
            self.type,
            if (self.source) |source| source else null,
        });
    }
};
