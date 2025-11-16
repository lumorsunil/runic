const std = @import("std");
const command_runner = @import("command_runner.zig");
const tracing = @import("tracing.zig");

const CommandRunner = command_runner.CommandRunner;
const ProcessHandle = command_runner.ProcessHandle;
const Tracer = tracing.Tracer;

pub const PromiseError = error{
    AlreadyAwaited,
};

/// Scheduler coordinates asynchronous background commands and returns
/// promise-shaped handles that scripts can `await`.
pub const Scheduler = struct {
    allocator: std.mem.Allocator,
    tasks: std.ArrayList(*BackgroundTask),
    tracer: ?*Tracer = null,
    next_task_id: usize = 1,

    pub fn init(allocator: std.mem.Allocator) Scheduler {
        return Scheduler.initWithTracer(allocator, null);
    }

    pub fn initWithTracer(allocator: std.mem.Allocator, tracer: ?*Tracer) Scheduler {
        return .{
            .allocator = allocator,
            .tasks = std.ArrayList(*BackgroundTask).init(allocator),
            .tracer = tracer,
            .next_task_id = 1,
        };
    }

    pub fn deinit(self: *Scheduler) void {
        var idx: usize = 0;
        while (idx < self.tasks.items.len) : (idx += 1) {
            const task = self.tasks.items[idx];
            task.discard() catch {};
            task.destroy(self.allocator);
        }
        self.tasks.deinit();
        self.* = undefined;
    }

    /// Spawns the provided pipeline in the background and returns a promise
    /// that resolves to the process handle once the execution finishes.
    pub fn spawnBackground(self: *Scheduler, specs: []const CommandRunner.CommandSpec) !ProcessPromise {
        const task_id = self.next_task_id;
        self.next_task_id += 1;

        var task = try BackgroundTask.create(self.allocator, specs, task_id, self.tracer);
        errdefer task.destroy(self.allocator);

        try self.tasks.append(task);
        errdefer {
            _ = self.tasks.pop();
        }

        try task.start();
        self.traceAsync("spawned task #{d} (stages={d})", .{ task_id, specs.len });

        return ProcessPromise{ .scheduler = self, .task = task };
    }

    fn awaitTask(self: *Scheduler, task: *BackgroundTask) !ProcessHandle {
        const handle = try task.waitForResult(self.allocator);
        self.traceAsync("task #{d} resolved", .{task.id});
        self.traceHandle("async task resolved", &handle);
        self.removeTask(task);
        task.destroy(self.allocator);
        return handle;
    }

    fn removeTask(self: *Scheduler, target: *BackgroundTask) void {
        const idx = self.findTaskIndex(target) orelse return;
        _ = self.tasks.swapRemove(idx);
    }

    fn findTaskIndex(self: *Scheduler, target: *BackgroundTask) ?usize {
        for (self.tasks.items, 0..) |task, idx| {
            if (task == target) return idx;
        }
        return null;
    }

    fn traceAsync(self: *Scheduler, comptime fmt: []const u8, args: anytype) void {
        const tracer = self.tracer orelse return;
        tracer.log(.async, fmt, args) catch {};
    }

    fn traceHandle(self: *Scheduler, label: []const u8, handle: *const ProcessHandle) void {
        const tracer = self.tracer orelse return;
        handle.traceSummary(tracer, label) catch {};
    }
};

/// Promise exposing the eventual `ProcessHandle` produced by a background
/// command execution.
pub const ProcessPromise = struct {
    scheduler: *Scheduler,
    task: ?*BackgroundTask,

    pub const State = enum {
        pending,
        ready,
        consumed,
    };

    pub fn state(self: ProcessPromise) State {
        if (self.task) |task| {
            if (task.isReady()) return .ready;
            return .pending;
        }
        return .consumed;
    }

    pub fn isReady(self: ProcessPromise) bool {
        return self.state() == .ready;
    }

    /// Blocks until the background command finishes and returns the cloned
    /// process handle. Promises are single-use; awaiting twice returns an
    /// `AlreadyAwaited` error.
    pub fn wait(self: *ProcessPromise) (PromiseError || CommandRunner.Error)!ProcessHandle {
        const task = self.task orelse return PromiseError.AlreadyAwaited;
        self.task = null;
        self.scheduler.traceAsync("awaiting task #{d}", .{task.id});
        return self.scheduler.awaitTask(task);
    }
};

const BackgroundTask = struct {
    scheduler_allocator: std.mem.Allocator,
    specs: []CommandRunner.CommandSpec,
    state: State = .pending,
    mutex: std.Thread.Mutex = .{},
    cond: std.Thread.Condition = .{},
    thread: ?std.Thread = null,
    result: ?ProcessHandle = null,
    failure: ?CommandRunner.Error = null,
    gpa: std.heap.GeneralPurposeAllocator(.{}) = .{},
    id: usize,
    tracer: ?*Tracer,

    const State = enum {
        pending,
        completed,
    };

    fn create(
        allocator: std.mem.Allocator,
        specs: []const CommandRunner.CommandSpec,
        id: usize,
        tracer: ?*Tracer,
    ) !*BackgroundTask {
        const task = try allocator.create(BackgroundTask);
        task.* = .{
            .scheduler_allocator = allocator,
            .specs = try copySpecs(allocator, specs),
            .state = .pending,
            .thread = null,
            .result = null,
            .failure = null,
            .id = id,
            .tracer = tracer,
        };
        return task;
    }

    fn destroy(self: *BackgroundTask, allocator: std.mem.Allocator) void {
        self.cleanupSpecs();
        allocator.destroy(self);
    }

    fn start(self: *BackgroundTask) !void {
        self.thread = try std.Thread.spawn(.{}, BackgroundTask.run, .{self});
    }

    fn run(self: *BackgroundTask) void {
        const runner = CommandRunner.initWithTracer(self.gpa.allocator(), self.tracer);
        var result = runner.runPipeline(self.specs) catch |err| {
            self.finishError(err);
            return;
        };
        self.finishSuccess(&result);
    }

    fn finishSuccess(self: *BackgroundTask, handle: *ProcessHandle) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        std.debug.assert(self.state == .pending);
        self.result = handle.*;
        self.state = .completed;
        self.cond.broadcast();
        if (self.tracer) |tracer| {
            tracer.log(.async, "task #{d} completed (ok={})", .{ self.id, handle.status.ok }) catch {};
            handle.traceSummary(tracer, "async background task") catch {};
        }
    }

    fn finishError(self: *BackgroundTask, err: CommandRunner.Error) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        std.debug.assert(self.state == .pending);
        self.failure = err;
        self.state = .completed;
        self.cond.broadcast();
        if (self.tracer) |tracer| {
            tracer.log(.async, "task #{d} failed: {s}", .{ self.id, @errorName(err) }) catch {};
        }
    }

    fn isReady(self: *BackgroundTask) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.state == .completed;
    }

    fn waitForResult(self: *BackgroundTask, result_allocator: std.mem.Allocator) !ProcessHandle {
        self.blockUntilFinished();

        if (self.failure) |err| {
            _ = self.joinThread();
            const leak_check = self.gpa.deinit();
            switch (leak_check) {
                .ok => {},
                .leak => std.debug.panic("background task leaked allocations", .{}),
            }
            self.cleanupSpecs();
            return err;
        }

        var handle = self.result orelse unreachable;
        const cloned = try handle.clone(result_allocator);
        handle.deinit();
        self.result = null;

        _ = self.joinThread();

        const leak_check = self.gpa.deinit();
        switch (leak_check) {
            .ok => {},
            .leak => std.debug.panic("background task leaked allocations", .{}),
        }
        self.cleanupSpecs();
        return cloned;
    }

    fn discard(self: *BackgroundTask) !void {
        self.blockUntilFinished();

        if (self.result) |*handle| {
            handle.deinit();
            self.result = null;
        }

        _ = self.joinThread();

        const leak_check = self.gpa.deinit();
        switch (leak_check) {
            .ok => {},
            .leak => std.debug.panic("background task leaked allocations", .{}),
        }
        self.cleanupSpecs();
    }

    fn blockUntilFinished(self: *BackgroundTask) void {
        self.mutex.lock();
        while (self.state == .pending) {
            self.cond.wait(&self.mutex);
        }
        self.mutex.unlock();
    }

    fn joinThread(self: *BackgroundTask) bool {
        if (self.thread) |thread| {
            thread.join();
            self.thread = null;
            return true;
        }
        return false;
    }

    fn cleanupSpecs(self: *BackgroundTask) void {
        if (self.specs.len == 0) return;
        for (self.specs) |spec| {
            if (spec.argv.len > 0) self.scheduler_allocator.free(spec.argv);
        }
        self.scheduler_allocator.free(self.specs);
        self.specs = &.{};
    }
};

fn copySpecs(allocator: std.mem.Allocator, specs: []const CommandRunner.CommandSpec) ![]CommandRunner.CommandSpec {
    if (specs.len == 0) return &.{};
    const copy = try allocator.alloc(CommandRunner.CommandSpec, specs.len);
    errdefer destroySpecCopies(allocator, copy, specs.len);

    var initialized: usize = 0;
    errdefer destroySpecCopies(allocator, copy, initialized);

    while (initialized < specs.len) : (initialized += 1) {
        const spec = specs[initialized];
        copy[initialized] = .{
            .argv = try copyArgv(allocator, spec.argv),
            .cwd = spec.cwd,
            .env_map = spec.env_map,
            .max_output_bytes = spec.max_output_bytes,
        };
    }

    return copy;
}

fn copyArgv(allocator: std.mem.Allocator, argv: []const []const u8) ![]const []const u8 {
    if (argv.len == 0) return &.{};
    const buf = try allocator.alloc([]const u8, argv.len);
    std.mem.copy([]const u8, buf, argv);
    return buf;
}

fn destroySpecCopies(allocator: std.mem.Allocator, specs: []CommandRunner.CommandSpec, count: usize) void {
    var idx: usize = 0;
    while (idx < count) : (idx += 1) {
        const spec = specs[idx];
        if (spec.argv.len > 0) allocator.free(spec.argv);
    }
    if (specs.len > 0) allocator.free(specs);
}

test "scheduler resolves background process promises" {
    var scheduler = Scheduler.init(std.testing.allocator);
    defer scheduler.deinit();

    var promise = try scheduler.spawnBackground(&[_]CommandRunner.CommandSpec{
        .{ .argv = &.{ "sh", "-c", "sleep 0.05; printf 'ready'" } },
    });

    try std.testing.expect(promise.state() != .consumed);
    try std.testing.expect(!promise.isReady());

    var handle = try promise.wait();
    defer handle.deinit();

    try expectStringEqual("ready", handle.stdoutBytes());
    try std.testing.expect(handle.status.ok);
}

test "scheduler surfaces background spawn errors" {
    var scheduler = Scheduler.init(std.testing.allocator);
    defer scheduler.deinit();

    var promise = try scheduler.spawnBackground(&[_]CommandRunner.CommandSpec{
        .{ .argv = &.{} },
    });

    try std.testing.expectError(CommandRunner.Error.EmptyCommand, promise.wait());
}

test "scheduler drain on deinit waits on outstanding promises" {
    var scheduler = Scheduler.init(std.testing.allocator);

    _ = try scheduler.spawnBackground(&[_]CommandRunner.CommandSpec{
        .{ .argv = &.{ "sh", "-c", "sleep 0.02" } },
    });
    _ = try scheduler.spawnBackground(&[_]CommandRunner.CommandSpec{
        .{ .argv = &.{ "sh", "-c", "sleep 0.01" } },
    });

    scheduler.deinit();
}

test "scheduler traces async lifecycle" {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();
    var writer = buffer.writer(std.testing.allocator);
    var writer_adapter = writer.adaptToNewApi(&.{});
    var tracer = Tracer.init(&.{ "async", "process", "pipeline" }, &writer_adapter.new_interface);

    var scheduler = Scheduler.initWithTracer(std.testing.allocator, &tracer);
    defer scheduler.deinit();

    var promise = try scheduler.spawnBackground(&[_]CommandRunner.CommandSpec{
        .{ .argv = &.{ "sh", "-c", "printf 'async-ready'" } },
    });

    var handle = try promise.wait();
    defer handle.deinit();

    const log = buffer.items;
    try std.testing.expect(std.mem.indexOf(u8, log, "spawned task") != null);
    try std.testing.expect(std.mem.indexOf(u8, log, "awaiting task") != null);
    try std.testing.expect(std.mem.indexOf(u8, log, "async task resolved") != null);
    try std.testing.expect(std.mem.indexOf(u8, log, "pipeline complete") != null);
}

test "scheduler propagates non-zero async exits" {
    var scheduler = Scheduler.init(std.testing.allocator);
    defer scheduler.deinit();

    var promise = try scheduler.spawnBackground(&[_]CommandRunner.CommandSpec{
        .{ .argv = &.{ "sh", "-c", "exit 9" } },
    });

    var handle = try promise.wait();
    defer handle.deinit();

    try std.testing.expect(!handle.status.ok);
    try std.testing.expectEqual(@as(?u8, 9), handle.status.exit_code);
}

test "process promises reject duplicate wait calls" {
    var scheduler = Scheduler.init(std.testing.allocator);
    defer scheduler.deinit();

    var promise = try scheduler.spawnBackground(&[_]CommandRunner.CommandSpec{
        .{ .argv = &.{ "sh", "-c", "printf 'ready'" } },
    });

    var handle = try promise.wait();
    defer handle.deinit();

    try std.testing.expectError(PromiseError.AlreadyAwaited, promise.wait());
}

fn expectStringEqual(expected: []const u8, actual: []const u8) !void {
    try std.testing.expectEqualStrings(expected, actual);
}
