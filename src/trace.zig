const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const Span = runic.ast.Span;
const Severity = runic.diagnostics.Severity;
const rainbow = runic.rainbow;

pub const BasicTrace = struct {
    message: []const u8,
    trace: Trace,

    const vtable = Trace.VTable{
        .format = format,
    };

    pub fn init(
        message: []const u8,
        tags: []const []const u8,
        severity: Severity,
        span: ?Span,
    ) @This() {
        return .{
            .message = message,
            .trace = .{
                .vtable = &vtable,
                .timestamp = std.time.timestamp(),
                .severity = severity,
                .tags = tags,
                .span = span,
            },
        };
    }

    pub fn format(
        self: *Trace,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        const parent: *@This() = @fieldParentPtr("trace", self);
        try writer.print("{s}", .{parent.message});
    }
};

pub const Trace = struct {
    vtable: *const VTable,
    timestamp: i64,
    span: ?Span,
    severity: Severity,
    tags: []const []const u8,

    pub const VTable = struct {
        format: *const fn (*Trace, writer: *std.Io.Writer) std.Io.Writer.Error!void,
    };

    pub fn format(self: *@This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("[{D}] {t}: ", .{ self.timestamp, self.severity });
        return self.vtable.format(self, writer);
    }

    pub const ColorizedTrace = struct {
        trace: *Trace,

        pub fn format(
            self: @This(),
            writer: *std.Io.Writer,
        ) std.Io.Writer.Error!void {
            try writer.print("{s}{f}{s}", .{
                rainbow.beginColor(self.trace.severity.color()),
                self.trace,
                rainbow.endColor(),
            });
        }
    };

    pub fn colorize(self: *@This()) ColorizedTrace {
        return .{ .trace = self };
    }
};

const TraceIterator = struct {
    ctx: *Tracer,
    index: ?usize = 0,
    filter: TraceFilter,

    pub fn next(self: *@This()) ?*Trace {
        var index = self.index orelse return null;
        while (index < self.ctx.full_log.items.len) : (index += 1) {
            if (self.isIncluded(index)) break;
        } else {
            self.index = null;
            return null;
        }
        self.index = index;
    }

    fn isIncluded(self: *@This(), index: usize) bool {
        const trace = self.ctx.full_log.items[index];
        return self.filter.isIncluded(trace);
    }
};

pub const TraceFilter = union(enum) {
    all,
    tags_and: []const []const u8,
    tags_or: []const []const u8,

    pub fn tagsAnd(tags: []const []const u8) @This() {
        return .{ .tags_and = tags };
    }

    pub fn tagsOr(tags: []const []const u8) @This() {
        return .{ .tags_or = tags };
    }

    pub fn isIncluded(self: @This(), trace: *Trace) bool {
        return switch (self) {
            .any => true,
            .tags_and => |tags_and| for (tags_and) |tag| {
                for (trace.tags) |trace_tag| {
                    if (std.mem.eql(u8, trace_tag, tag)) {
                        break;
                    }
                } else return false;
            } else return true,
            .tags_or => |tags_or| for (tags_or) |tag| {
                for (trace.tags) |trace_tag| {
                    if (std.mem.eql(u8, trace_tag, tag)) {
                        return true;
                    }
                }
            } else return true,
        };
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try switch (self) {
            .all => writer.writeAll("any"),
            inline .tags_and, .tags_or => |tags| {
                switch (self) {
                    .tags_and => try writer.writeAll("and"),
                    .tags_or => try writer.writeAll("or"),
                    else => unreachable,
                }
                try writer.writeAll(" [");
                for (tags, 0..) |tag, i| {
                    try writer.writeAll(tag);

                    if (i == tags.len - 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll("]");
            },
        };
    }
};

pub const Tracer = struct {
    start_time: i64,
    full_log: std.ArrayList(*Trace) = .empty,
    arena: std.heap.ArenaAllocator,
    config: Config,
    stdout_writer: std.fs.File.Writer,

    pub const Config = struct {
        echo_to_stdout: bool = true,
    };

    pub fn init(allocator: Allocator, config: Config) @This() {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const stdout_buffer = arena.allocator().alloc(u8, 1024) catch @panic("out of memory");

        return .{
            .start_time = std.time.timestamp(),
            .arena = arena,
            .config = config,
            .stdout_writer = std.fs.File.stdout().writer(stdout_buffer),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.arena.deinit();
    }

    fn initTraceBasic(
        self: *@This(),
        severity: Severity,
        tags: []const []const u8,
        span: ?Span,
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error!*Trace {
        const basic_trace = try self.arena.allocator().create(BasicTrace);
        const message = try std.fmt.allocPrint(self.arena.allocator(), fmt, args);
        basic_trace.* = .init(message, tags, severity, span);
        return &basic_trace.trace;
    }

    pub fn trace(
        self: *@This(),
        severity: Severity,
        tags: []const []const u8,
        span: ?Span,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        const new_trace = self.initTraceBasic(severity, tags, span, fmt, args) catch return;
        self.traceCustom(new_trace);
    }

    pub fn traceCustom(
        self: *@This(),
        trace_: *Trace,
    ) void {
        trace_.timestamp -= self.start_time;
        self.full_log.append(self.arena.allocator(), trace_) catch return;
        if (self.config.echo_to_stdout) {
            self.stdout_writer.interface.print("{f}\n", .{trace_.colorize()}) catch return;
            self.stdout_writer.interface.flush() catch return;
        }
    }

    pub fn getTraces(self: *@This(), filter: TraceFilter) TraceIterator {
        return .{ .ctx = self, .filter = filter };
    }
};
