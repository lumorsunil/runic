const std = @import("std");

pub const StackTrace = struct {
    allocator: std.mem.Allocator,
    frames: std.ArrayList(Frame),

    pub fn init(allocator: std.mem.Allocator) StackTrace {
        return .{ .allocator = allocator, .frames = .empty };
    }

    pub fn deinit(self: *StackTrace) void {
        for (self.frames.items) |frame| {
            self.freeFrame(frame);
        }
        self.frames.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn len(self: StackTrace) usize {
        return self.frames.items.len;
    }

    pub fn push(self: *StackTrace, desc: FrameDesc) !void {
        const label_copy = try self.allocator.dupe(u8, desc.label);
        errdefer self.allocator.free(label_copy);

        var detail_copy: ?[]u8 = null;
        if (desc.detail) |detail| {
            detail_copy = try self.allocator.dupe(u8, detail);
            errdefer self.allocator.free(detail_copy.?);
        }

        var location_copy: ?OwnedLocation = null;
        if (desc.location) |loc| {
            const file_copy = try self.allocator.dupe(u8, loc.file);
            errdefer self.allocator.free(file_copy);
            location_copy = .{
                .file = file_copy,
                .line = loc.line,
                .column = loc.column,
            };
        }

        try self.frames.append(self.allocator, .{
            .label = label_copy,
            .detail = detail_copy,
            .location = location_copy,
        });
    }

    pub fn pop(self: *StackTrace) void {
        if (self.frames.items.len == 0) return;
        const frame = self.frames.pop();
        self.freeFrame(frame);
    }

    pub fn render(self: StackTrace, writer: *std.Io.Writer) !void {
        const total = self.frames.items.len;
        if (total == 0) {
            try writer.print("Runtime stack trace (empty)\n", .{});
            try writer.flush();
            return;
        }

        try writer.print("Runtime stack trace ({d} frames):\n", .{total});
        var idx: usize = total;
        var display: usize = 1;
        while (idx > 0) : (idx -= 1) {
            const frame = self.frames.items[idx - 1];
            if (frame.location) |loc| {
                try writer.print(
                    "  {d}. {s} at {s}:{d}:{d}\n",
                    .{ display, frame.label, loc.file, loc.line, loc.column },
                );
            } else {
                try writer.print("  {d}. {s}\n", .{ display, frame.label });
            }

            if (frame.detail) |detail| {
                try writer.print("       {s}\n", .{detail});
            }

            display += 1;
        }
        try writer.flush();
    }

    fn freeFrame(self: *StackTrace, frame: Frame) void {
        self.allocator.free(frame.label);
        if (frame.detail) |detail| {
            self.allocator.free(detail);
        }
        if (frame.location) |loc| {
            self.allocator.free(loc.file);
        }
    }

    const Frame = struct {
        label: []u8,
        detail: ?[]u8,
        location: ?OwnedLocation,
    };
};

pub const FrameDesc = struct {
    label: []const u8,
    location: ?FrameLocation = null,
    detail: ?[]const u8 = null,
};

pub const FrameLocation = struct {
    file: []const u8,
    line: usize,
    column: usize,
};

const OwnedLocation = struct {
    file: []u8,
    line: usize,
    column: usize,
};

test "stack trace renders newest frame first" {
    var trace = StackTrace.init(std.testing.allocator);
    defer trace.deinit();

    try trace.push(.{ .label = "pipeline", .detail = "echo hi | upper" });
    try trace.push(.{
        .label = "stage 2",
        .detail = "upper (exit code 1)",
        .location = .{ .file = "script.rn", .line = 5, .column = 3 },
    });

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();
    var writer = buffer.writer();
    var writer_adapter = writer.adaptToNewApi(&.{});
    try trace.render(&writer_adapter.new_interface);

    const expected =
        \\Runtime stack trace (2 frames):
        \\  1. stage 2 at script.rn:5:3
        \\       upper (exit code 1)
        \\  2. pipeline
        \\       echo hi | upper
        \\
    ;
    try std.testing.expectEqualStrings(expected, buffer.items);
}

test "stack trace pop removes the newest frame" {
    var trace = StackTrace.init(std.testing.allocator);
    defer trace.deinit();
    try trace.push(.{ .label = "pipeline" });
    try trace.push(.{ .label = "stage 1" });
    try std.testing.expectEqual(@as(usize, 2), trace.len());
    trace.pop();
    try std.testing.expectEqual(@as(usize, 1), trace.len());
    trace.pop();
    try std.testing.expectEqual(@as(usize, 0), trace.len());
}
