const std = @import("std");

const ascii = std.ascii;
const AnyWriter = std.io.AnyWriter;

/// Logical tracing categories that can be toggled independently via
/// command-line `--trace <topic>` switches.
pub const Topic = enum {
    pipeline,
    async,
    process,

    pub fn label(self: Topic) []const u8 {
        return switch (self) {
            .pipeline => "pipeline",
            .async => "async",
            .process => "process",
        };
    }
};

pub const TopicSet = struct {
    pipeline: bool = false,
    async: bool = false,
    process: bool = false,

    pub fn enable(self: *TopicSet, topic: Topic) void {
        switch (topic) {
            .pipeline => self.pipeline = true,
            .async => self.async = true,
            .process => self.process = true,
        }
    }

    pub fn enableNames(self: *TopicSet, names: [][]const u8) void {
        for (names) |name| {
            if (parseTopic(name)) |topic| self.enable(topic);
        }
    }

    pub fn enabled(self: TopicSet, topic: Topic) bool {
        return switch (topic) {
            .pipeline => self.pipeline,
            .async => self.async,
            .process => self.process,
        };
    }

    pub fn any(self: TopicSet) bool {
        return self.pipeline or self.async or self.process;
    }
};

/// Lightweight tracer invoked by the runtime to emit structured events when
/// debugging pipeline execution, asynchronous schedulers, or process handles.
/// The tracer is intentionally tiny: it tracks which topics are enabled and
/// writes pre-formatted log lines to the provided sink under a mutex so callers
/// can safely emit events from background threads.
pub const Tracer = struct {
    sink: ?AnyWriter = null,
    topics: TopicSet = .{},
    mutex: std.Thread.Mutex = .{},

    pub fn init(names: [][]const u8, sink: ?AnyWriter) Tracer {
        var tracer = Tracer{ .sink = sink };
        tracer.topics.enableNames(names);
        return tracer;
    }

    pub fn setSink(self: *Tracer, sink: AnyWriter) void {
        self.sink = sink;
    }

    pub fn clearSink(self: *Tracer) void {
        self.sink = null;
    }

    pub fn enabled(self: Tracer, topic: Topic) bool {
        return self.topics.enabled(topic);
    }

    pub fn anyTopicEnabled(self: Tracer) bool {
        return self.topics.any();
    }

    pub fn log(self: *Tracer, topic: Topic, comptime fmt: []const u8, args: anytype) !void {
        if (!self.enabled(topic)) return;
        const sink = self.sink orelse return;
        self.mutex.lock();
        defer self.mutex.unlock();
        try sink.print("[trace {s}] ", .{topic.label()});
        try sink.print(fmt, args);
        try sink.print("\n", .{});
    }
};

pub fn parseTopic(name: []const u8) ?Topic {
    if (ascii.eqlIgnoreCase(name, "pipeline")) return .pipeline;
    if (ascii.eqlIgnoreCase(name, "async")) return .async;
    if (ascii.eqlIgnoreCase(name, "process")) return .process;
    return null;
}

test "topic parsing is case insensitive" {
    try std.testing.expectEqual(@as(?Topic, .pipeline), parseTopic("PIPELINE"));
    try std.testing.expectEqual(@as(?Topic, .async), parseTopic("Async"));
    try std.testing.expectEqual(@as(?Topic, .process), parseTopic("process"));
    try std.testing.expectEqual(@as(?Topic, null), parseTopic("unknown"));
}

test "tracer logs only enabled topics" {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();
    var writer = buffer.writer(std.testing.allocator);
    const sink = writer.any();

    var tracer = Tracer.init(&.{ "PIPELINE", "process" }, sink);
    try tracer.log(.pipeline, "start {d} stages", .{2});
    try tracer.log(.async, "should skip", .{});
    try tracer.log(.process, "pid={d}", .{42});

    const contents = buffer.items;
    try std.testing.expect(std.mem.indexOf(u8, contents, "pipeline") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "pid=42") != null);
    try std.testing.expect(std.mem.indexOf(u8, contents, "async") == null);
}
