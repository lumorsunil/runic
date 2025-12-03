const ExpressionEnum = enum {
    int,
    float,
};

const Expression = union(ExpressionEnum) {
    int: struct {
        value: i32,

        pub fn evaluate(self: @This()) std.json.Value {
            return self.value;
        }
    },
    float: struct {
        value: f32,

        pub fn evaluate(self: @This()) std.json.Value {
            return self.value;
        }
    },

    pub fn evaluate(self: Expression) std.json.Value {
        switch (self) {
            inline else => |v| v.evaluate(),
        }
    }
};

fn add(left: Expression, right: Expression) Expression {
    if (left == .int and right == .int) {
        return .{ .int = left.int + right.int };
    }

    if (left == .float and right == .float) {
        return .{ .float = left.float + right.float };
    }
}

const Int = struct {
    value: i32,
};

const Float = struct {
    value: f32,
};

fn Add(comptime Left: type, comptime Right: type) type {
    if (Left == Int and Right == Int) return i32;
    if (Left == Float and Right == Float) return f32;

    @compileError("Cannot add a " ++ @typeName(Left) ++ " with a " ++ @typeName(Right));
}

pub fn add_(left: anytype, right: anytype) Add(@TypeOf(left), @TypeOf(right)) {
    return left.value + right.value;
}

pub fn stringify(value: anytype) []const u8 {
    return value.stringify();
}

test "add_" {
    const testing = @import("std").testing;
    const int1 = Int{ .value = 3 };
    const int2 = Int{ .value = 5 };

    const result = add_(int1, int2);

    try testing.expectEqual(8, result);
}

// Interfaces with VTables

const std = @import("std");
const ExpressionInterface = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        evaluate: *const fn (*anyopaque) std.json.Value,
    };

    pub fn evaluate(self: ExpressionInterface) std.json.Value {
        return self.vtable.evaluate(self.ptr);
    }
};

const NumberExpression = struct {
    value: i32,

    fn evaluate(ptr: *anyopaque) std.json.Value {
        const ctx: *NumberExpression = @ptrCast(@alignCast(ptr));
        return .{ .integer = ctx.value };
    }

    pub fn init(value: i32) NumberExpression {
        return .{
            .value = value,
        };
    }

    pub fn expression(self: *NumberExpression) ExpressionInterface {
        return .{
            .ptr = self,
            .vtable = &.{
                .evaluate = evaluate,
            },
        };
    }
};

pub fn evaluateExpr(expr: ExpressionInterface) std.json.Value {
    return expr.evaluate();
}

test "interface" {
    const testing = @import("std").testing;

    var number = NumberExpression.init(3);

    const result = evaluateExpr(number.expression());

    try testing.expectEqual(3, result.integer);
}
