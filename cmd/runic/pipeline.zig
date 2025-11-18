const std = @import("std");
const runic = @import("runic");

const CommandRunner = runic.command_runner.CommandRunner;
const StageStatus = runic.command_runner.StageStatus;
const CommandDisplay = runic.command_runner.CommandDisplay;

pub const TokenList = struct {
    allocator: std.mem.Allocator,
    items: [][]const u8 = &[_][]const u8{},

    pub fn init(allocator: std.mem.Allocator) TokenList {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *TokenList) void {
        if (self.items.len == 0) return;
        for (self.items) |item| self.allocator.free(item);
        self.allocator.free(self.items);
    }

    pub fn populate(self: *TokenList, command: []const u8) !void {
        var tokens = std.ArrayList([]const u8).empty;
        defer tokens.deinit(self.allocator);

        var current = std.ArrayList(u8).empty;
        defer current.deinit(self.allocator);

        var in_single = false;
        var in_double = false;
        var escape = false;

        for (command) |byte| {
            if (escape) {
                try current.append(self.allocator, byte);
                escape = false;
                continue;
            }
            if (!in_single and byte == '\\') {
                escape = true;
                continue;
            }
            if (!in_double and byte == '\'') {
                in_single = !in_single;
                continue;
            }
            if (!in_single and byte == '"') {
                in_double = !in_double;
                continue;
            }
            if (!in_single and !in_double and (byte == ' ' or byte == '\t')) {
                try flushToken(self.allocator, &tokens, &current);
                continue;
            }
            if (!in_single and !in_double and byte == '|') {
                try flushToken(self.allocator, &tokens, &current);
                try tokens.append(self.allocator, try self.allocator.dupe(u8, "|"));
                continue;
            }
            try current.append(self.allocator, byte);
        }

        if (in_single or in_double) return error.UnterminatedQuote;
        if (escape) try current.append(self.allocator, '\\');

        try flushToken(self.allocator, &tokens, &current);
        self.items = try tokens.toOwnedSlice(self.allocator);
    }
};

pub const Pipeline = struct {
    allocator: std.mem.Allocator,
    specs: []CommandRunner.CommandSpec = &[_]CommandRunner.CommandSpec{},

    pub fn init(allocator: std.mem.Allocator) Pipeline {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Pipeline) void {
        if (self.specs.len == 0) return;
        for (self.specs) |spec| {
            self.allocator.free(spec.argv);
        }
        self.allocator.free(self.specs);
    }

    pub fn build(self: *Pipeline, tokens: [][]const u8) !void {
        var specs = std.ArrayList(CommandRunner.CommandSpec).empty;
        defer specs.deinit(self.allocator);

        var args = std.ArrayList([]const u8).empty;
        defer args.deinit(self.allocator);

        for (tokens) |token| {
            if (token.len == 1 and token[0] == '|') {
                try self.flushStage(&specs, &args);
                continue;
            }
            try args.append(self.allocator, token);
        }

        try self.flushStage(&specs, &args);
        self.specs = try specs.toOwnedSlice(self.allocator);
    }

    fn flushStage(
        self: *Pipeline,
        specs: *std.ArrayList(CommandRunner.CommandSpec),
        args: *std.ArrayList([]const u8),
    ) !void {
        if (args.items.len == 0) return error.MissingCommand;
        const argv = try self.allocator.alloc([]const u8, args.items.len);
        for (args.items, 0..) |token, idx| {
            argv[idx] = token;
        }
        try specs.append(self.allocator, .{ .argv = argv });
        args.clearRetainingCapacity();
    }
};

pub fn describeStage(
    allocator: std.mem.Allocator,
    argv: []const []const u8,
    status: StageStatus,
) ![]u8 {
    var buffer = std.ArrayList(u8).empty;
    defer buffer.deinit(allocator);

    try buffer.print(allocator, "{any}", .{CommandDisplay{ .argv = argv }});

    if (status.signal) |sig| {
        try buffer.print(allocator, " (signal {d})", .{sig});
    } else if (status.exit_code) |code| {
        if (!status.ok or code != 0) {
            try buffer.print(allocator, " (exit code {d})", .{code});
        }
    } else if (!status.ok) {
        try buffer.print(allocator, " (failed)", .{});
    }

    return try buffer.toOwnedSlice(allocator);
}

fn flushToken(
    allocator: std.mem.Allocator,
    tokens: *std.ArrayList([]const u8),
    current: *std.ArrayList(u8),
) !void {
    if (current.items.len == 0) return;
    const owned = try allocator.dupe(u8, current.items);
    try tokens.append(allocator, owned);
    current.clearRetainingCapacity();
}

test "tokenize pipe with quotes" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var tokens = TokenList.init(gpa.allocator());
    defer tokens.deinit();
    try tokens.populate("echo \"hello world\" | upper");

    try std.testing.expectEqual(@as(usize, 4), tokens.items.len);
    try std.testing.expectEqualStrings("echo", tokens.items[0]);
    try std.testing.expectEqualStrings("hello world", tokens.items[1]);
    try std.testing.expectEqualStrings("|", tokens.items[2]);
    try std.testing.expectEqualStrings("upper", tokens.items[3]);
}

test "build pipeline from tokens" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var tokens = TokenList.init(gpa.allocator());
    defer tokens.deinit();
    try tokens.populate("ls | wc -l");

    var pipeline_builder = Pipeline.init(gpa.allocator());
    defer pipeline_builder.deinit();
    try pipeline_builder.build(tokens.items);

    try std.testing.expectEqual(@as(usize, 2), pipeline_builder.specs.len);
    try std.testing.expectEqual(@as(usize, 1), pipeline_builder.specs[0].argv.len);
    try std.testing.expectEqualStrings("ls", pipeline_builder.specs[0].argv[0]);
    try std.testing.expectEqual(@as(usize, 2), pipeline_builder.specs[1].argv.len);
    try std.testing.expectEqualStrings("wc", pipeline_builder.specs[1].argv[0]);
    try std.testing.expectEqualStrings("-l", pipeline_builder.specs[1].argv[1]);
}
