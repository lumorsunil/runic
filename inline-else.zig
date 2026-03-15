const std = @import("std");

const Car = struct {
    model: []const u8,
    wheels: usize,
    doors: usize,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.writeAll("Car:\n");
        inline for (std.meta.fields(Car)) |field| {
            const value = @field(self, field.name);

            try writer.print("{s}: ", .{field.name});

            if (comptime std.mem.eql(u8, field.name, "model")) {
                try writer.print("{s}", .{value});
            } else {
                try writer.print("{}", .{value});
            }

            try writer.writeByte('\n');
        }
    }
};

const Animal = union(enum) {
    cat: []const u8,
    dog: []const u8,
    snake: []const u8,
    elephant: usize,
};

pub fn main() !void {
    const animal = Animal{ .cat = "Oscar" };
    const animal2 = Animal{ .dog = "Rosa" };
    const animal3 = Animal{ .snake = "Jonas" };
    const animal4 = Animal{ .elephant = 5 };

    const car = Car{
        .model = "MyModel",
        .doors = 4,
        .wheels = 5,
    };

    std.log.debug("{f}", .{car});

    printAnimal(animal);
    printAnimal(animal2);
    printAnimal(animal3);
    printAnimal(animal4);
}

fn printAnimal(animal: Animal) void {
    switch (animal) {
        .elephant => |age| std.log.debug("animal is a {t} with age: {}", .{ animal, age }),
        inline else => |name, t| std.log.debug("animal is a {t} named: {s}", .{ t, name }),
    }

    // switch (animal) {
    //     .cat => |name, t| std.log.debug("animal is a {t} named: {s}", .{t, name}),
    //     .dog => |name, t| std.log.debug("animal is a {t} named: {s}", .{t, name}),
    // }
}
