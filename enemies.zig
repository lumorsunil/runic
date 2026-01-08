const std = @import("std");

const Game = struct {
    enemy_id_counter: usize = 0,
    enemies: std.AutoArrayHashMap(usize, Enemy) = .init(allocator),

    fn spawnEnemy(self: *Game, enemy: Enemy) !void {
        try self.enemies.put(self.enemy_id_counter, enemy);
        enemy.id = self.enemy_id_counter;
        self.enemy_id_counter += 1;
    }

    fn killEnemy(self: *Game, enemy: Enemy) void {
        _ = self.enemies.swapRemove(enemy.id);
    }
};
