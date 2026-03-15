const std = @import("std");
const Allocator = std.mem.Allocator;

const GameObject = struct {
    arena: std.heap.ArenaAllocator,
    health: usize,
    max_health: usize,
    targets: std.ArrayList(GameObject) = .empty,

    pub fn init(allocator: Allocator, max_health: usize) GameObject {
        return .{
            .arena = .init(allocator),
            .health = max_health,
            .max_health = max_health,
        };
    }

    pub fn deinit(self: *GameObject) void {
        self.arena.deinit();
    }

    pub fn addTarget(self: *GameObject, target: GameObject) !void {
        try self.targets.append(self.arena.allocator(), target);
    }
};

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const game_objects = try allocator.alloc(GameObject, 100);

    defer {
        for (game_objects) |*game_object| game_object.deinit();
        allocator.free(game_objects);
    }

    for (game_objects) |*game_object| {
        game_object.* = .init(allocator, 3);
    }
}
