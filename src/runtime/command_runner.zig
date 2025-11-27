const std = @import("std");
const tracing = @import("tracing.zig");

const Tracer = tracing.Tracer;
const TraceTopic = tracing.Topic;

/// CommandRunner encapsulates the lower-level mechanics for spawning external
/// programs and collecting their outputs. Higher-level runtime components feed
/// commands through this struct so the rest of the interpreter can reason
/// about process handles instead of juggling std.process.Child directly.
pub const CommandRunner = struct {
    allocator: std.mem.Allocator,
    tracer: ?*Tracer = null,

    pub const Error = error{EmptyCommand} ||
        std.process.Child.RunError ||
        std.process.Child.SpawnError ||
        std.process.Child.WaitError ||
        std.fs.File.WriteError ||
        std.mem.Allocator.Error;

    pub const CommandSpec = struct {
        argv: []const []const u8,
        cwd: ?[]const u8 = null,
        env_map: ?*const std.process.EnvMap = null,
        max_output_bytes: usize = default_max_capture_bytes,
    };

    pub fn init(allocator: std.mem.Allocator) CommandRunner {
        return CommandRunner.initWithTracer(allocator, null);
    }

    pub fn initWithTracer(allocator: std.mem.Allocator, tracer: ?*Tracer) CommandRunner {
        return .{
            .allocator = allocator,
            .tracer = tracer,
        };
    }

    /// Runs `spec` synchronously, capturing stdout/stderr, and returns a
    /// structured process handle that describes the execution.
    pub fn runSync(self: CommandRunner, spec: CommandSpec) Error!ProcessHandle {
        const handle = try self.runPipeline(&[_]CommandSpec{spec});
        return handle;
    }

    /// Executes the sequence of command specs as a Runic pipeline. The stdout
    /// from stage `n` becomes the stdin for stage `n + 1`, and every stage's
    /// outputs plus exit metadata are preserved for later inspection.
    pub fn runPipeline(self: CommandRunner, specs: []const CommandSpec) Error!ProcessHandle {
        if (specs.len == 0) {
            return error.EmptyCommand;
        }

        var stage_statuses = try self.allocator.alloc(StageStatus, specs.len);
        errdefer self.allocator.free(stage_statuses);

        var stage_captures = try self.allocator.alloc(StageCapture, specs.len);
        var initialized_captures: usize = 0;
        errdefer destroyStageCaptures(self.allocator, stage_captures, initialized_captures);

        var started_at_ns: i128 = 0;
        var finished_at_ns: i128 = 0;
        var primary_pid: std.process.Child.Id = undefined;
        var previous_stdout: ?[]const u8 = null;

        self.tracePipelineStart(specs);

        for (specs, 0..) |spec, index| {
            const stage = try self.runStage(.{
                .spec = spec,
                .index = index,
                .stdin_data = previous_stdout,
            });

            if (index == 0) {
                primary_pid = stage.pid;
                started_at_ns = stage.started_at_ns;
            }
            finished_at_ns = stage.finished_at_ns;

            stage_statuses[index] = stage.status;
            stage_captures[index] = .{
                .stdout = stage.stdout_owned,
                .stderr = stage.stderr_owned,
            };
            initialized_captures = index + 1;
            previous_stdout = stage_captures[index].stdout;

            self.traceStageResult(&stage);
        }

        var handle = ProcessHandle{
            .allocator = self.allocator,
            .pid = primary_pid,
            .started_at_ns = started_at_ns,
            .finished_at_ns = finished_at_ns,
            .status = ProcessStatus.fromStages(stage_statuses),
            .stage_statuses = stage_statuses,
            .stage_captures = stage_captures,
        };

        self.traceProcessHandle(&handle);
        return handle;
    }

    fn runStage(self: CommandRunner, args: StageRunArgs) Error!StageExecution {
        if (args.spec.argv.len == 0) {
            return error.EmptyCommand;
        }

        var child = std.process.Child.init(args.spec.argv, self.allocator);
        child.stdin_behavior = if (args.stdin_data != null) .Pipe else .Ignore;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;
        child.cwd = args.spec.cwd;
        child.env_map = args.spec.env_map;

        var stdout_buf = std.ArrayList(u8).empty;
        defer stdout_buf.deinit(self.allocator);
        var stderr_buf = std.ArrayList(u8).empty;
        defer stderr_buf.deinit(self.allocator);

        const started_at = std.time.nanoTimestamp();
        try child.spawn();
        errdefer {
            _ = child.kill() catch {};
        }

        if (args.stdin_data) |input| {
            var stdin_file = child.stdin orelse unreachable;
            defer {
                stdin_file.close();
                child.stdin = null;
            }
            try stdin_file.writeAll(input);
        }

        const pid_snapshot = child.id;
        try child.collectOutput(self.allocator, &stdout_buf, &stderr_buf, args.spec.max_output_bytes);
        const term = try child.wait();
        const finished_at = std.time.nanoTimestamp();

        const stdout_owned = try stdout_buf.toOwnedSlice(self.allocator);
        const stderr_owned = try stderr_buf.toOwnedSlice(self.allocator);

        return StageExecution{
            .pid = pid_snapshot,
            .started_at_ns = started_at,
            .finished_at_ns = finished_at,
            .status = StageStatus.fromTerm(args.index, term),
            .stdout_owned = stdout_owned,
            .stderr_owned = stderr_owned,
        };
    }

    fn tracePipelineStart(self: CommandRunner, specs: []const CommandSpec) void {
        const tracer = self.tracer orelse return;
        tracer.log(.pipeline, "starting pipeline (stages={})", .{specs.len}) catch {};
        for (specs, 0..) |spec, idx| {
            tracer.log(.pipeline, "stage {}: {any}", .{
                idx + 1,
                CommandDisplay{ .argv = spec.argv },
            }) catch {};
        }
    }

    fn traceStageResult(self: CommandRunner, stage: *const StageExecution) void {
        const tracer = self.tracer orelse return;
        const stdout_len = stage.stdout_owned.len;
        const stderr_len = stage.stderr_owned.len;
        const duration = stage.finished_at_ns - stage.started_at_ns;

        if (stage.status.signal) |sig| {
            tracer.log(
                .pipeline,
                "stage {} pid={} signal={} stdout={}B stderr={}B duration_ns={}",
                .{ stage.status.index + 1, stage.pid, sig, stdout_len, stderr_len, duration },
            ) catch {};
            return;
        }

        if (stage.status.exit_code) |code| {
            tracer.log(
                .pipeline,
                "stage {} pid={} exit={} ok={} stdout={}B stderr={}B duration_ns={}",
                .{ stage.status.index + 1, stage.pid, code, stage.status.ok, stdout_len, stderr_len, duration },
            ) catch {};
            return;
        }

        tracer.log(
            .pipeline,
            "stage {} pid={} ok={} stdout={}B stderr={}B duration_ns={}",
            .{ stage.status.index + 1, stage.pid, stage.status.ok, stdout_len, stderr_len, duration },
        ) catch {};
    }

    fn traceProcessHandle(self: CommandRunner, handle: *const ProcessHandle) void {
        const tracer = self.tracer orelse return;
        handle.traceSummary(tracer, "pipeline complete") catch {};
    }
};

pub const ProcessHandle = struct {
    allocator: std.mem.Allocator,
    pid: std.process.Child.Id,
    started_at_ns: i128,
    finished_at_ns: i128,
    status: ProcessStatus,
    stage_statuses: []StageStatus,
    stage_captures: []StageCapture,

    pub fn deinit(self: *ProcessHandle) void {
        destroyStageCaptures(self.allocator, self.stage_captures, self.stage_captures.len);
        self.allocator.free(self.stage_statuses);
        self.* = undefined;
    }

    pub fn stdoutBytes(self: ProcessHandle) []const u8 {
        if (self.stage_captures.len == 0) return &[_]u8{};
        return self.stage_captures[self.stage_captures.len - 1].stdout;
    }

    pub fn stderrBytes(self: ProcessHandle) []const u8 {
        if (self.stage_captures.len == 0) return &[_]u8{};
        return self.stage_captures[self.stage_captures.len - 1].stderr;
    }

    pub fn durationNs(self: ProcessHandle) i128 {
        return self.finished_at_ns - self.started_at_ns;
    }

    pub fn stageStatuses(self: ProcessHandle) []const StageStatus {
        return self.stage_statuses;
    }

    pub fn stageCaptures(self: ProcessHandle) []const StageCapture {
        return self.stage_captures;
    }

    /// Emits a multi-line summary of the process handle (overall status followed
    /// by per-stage outcomes) through the provided tracer so callers can inspect
    /// pipelines without manually destructuring the handle.
    pub fn traceSummary(self: ProcessHandle, tracer: *Tracer, label: []const u8) !void {
        try tracer.log(
            .process,
            "{s}: pid={} stages={} ok={} duration_ns={}",
            .{ label, self.pid, self.stage_statuses.len, self.status.ok, self.durationNs() },
        );
        if (self.status.exit_code) |code| {
            try tracer.log(.process, "{s}: exit code {}", .{ label, code });
        } else {
            try tracer.log(.process, "{s}: exit code unknown", .{label});
        }
        if (self.status.signal) |sig| {
            try tracer.log(.process, "{s}: signal {}", .{ label, sig });
        }
        if (self.status.failed_stage) |idx| {
            try tracer.log(.process, "{s}: failed stage {}", .{ label, idx + 1 });
        }
        const statuses = self.stageStatuses();
        for (statuses) |status| {
            if (status.signal) |sig| {
                try tracer.log(.process, "{s}: stage {} signal {}", .{ label, status.index + 1, sig });
                continue;
            }
            if (status.exit_code) |code| {
                try tracer.log(
                    .process,
                    "{s}: stage {} exit {} (ok={})",
                    .{ label, status.index + 1, code, status.ok },
                );
                continue;
            }
            try tracer.log(.process, "{s}: stage {} ok={}", .{ label, status.index + 1, status.ok });
        }
    }

    /// Produces a snapshot of the process handle for destructuring or stream
    /// redirection sugar. Callers choose which streams to clone so commands
    /// like `1>var` can avoid copying stderr unnecessarily.
    pub fn snapshot(self: ProcessHandle, allocator: std.mem.Allocator, options: SnapshotOptions) !ProcessSnapshot {
        const stdout_copy = try copyOwnedSlice(allocator, self.stdoutBytes(), options.include_stdout);
        errdefer if (stdout_copy) |buf| allocator.free(buf);

        const stderr_copy = try copyOwnedSlice(allocator, self.stderrBytes(), options.include_stderr);
        errdefer if (stderr_copy) |buf| allocator.free(buf);

        return ProcessSnapshot{
            .allocator = allocator,
            .stdout = stdout_copy,
            .stderr = stderr_copy,
            .status = self.status,
            .pid = self.pid,
            .started_at_ns = self.started_at_ns,
            .finished_at_ns = self.finished_at_ns,
        };
    }

    /// Convenience wrapper that clones both stdout and stderr into an
    /// independent snapshot for destructuring assignments.
    pub fn destructure(self: ProcessHandle, allocator: std.mem.Allocator) !ProcessSnapshot {
        return self.snapshot(allocator, .{ .include_stdout = true, .include_stderr = true });
    }

    /// Captures a single stream (stdout or stderr) into a new snapshot so `1>`
    /// and `2>` sugar can bind the requested stream without throwing away the
    /// original handle.
    pub fn redirectStream(self: ProcessHandle, allocator: std.mem.Allocator, stream: SnapshotStream) !ProcessSnapshot {
        return self.snapshot(allocator, .{
            .include_stdout = stream == .stdout,
            .include_stderr = stream == .stderr,
        });
    }

    /// Creates a deep copy of the process handle so background executions can
    /// shuttle results between different allocators. The cloned handle owns its
    /// own capture buffers and must be deinitialized independently.
    pub fn clone(self: ProcessHandle, allocator: std.mem.Allocator) !ProcessHandle {
        const statuses_copy = try allocator.alloc(StageStatus, self.stage_statuses.len);
        errdefer allocator.free(statuses_copy);
        @memcpy(statuses_copy, self.stage_statuses);

        const captures_copy = try allocator.alloc(StageCapture, self.stage_captures.len);
        var initialized: usize = 0;
        errdefer destroyStageCaptures(allocator, captures_copy, initialized);

        while (initialized < self.stage_captures.len) : (initialized += 1) {
            const capture = self.stage_captures[initialized];
            const stdout_copy = cloneBytes(allocator, capture.stdout) catch |err| {
                destroyStageCaptures(allocator, captures_copy, initialized);
                return err;
            };
            const stderr_copy = cloneBytes(allocator, capture.stderr) catch |err| {
                allocator.free(stdout_copy);
                destroyStageCaptures(allocator, captures_copy, initialized);
                return err;
            };
            captures_copy[initialized] = .{
                .stdout = stdout_copy,
                .stderr = stderr_copy,
            };
        }

        return .{
            .allocator = allocator,
            .pid = self.pid,
            .started_at_ns = self.started_at_ns,
            .finished_at_ns = self.finished_at_ns,
            .status = self.status,
            .stage_statuses = statuses_copy,
            .stage_captures = captures_copy,
        };
    }
};

pub const SnapshotOptions = struct {
    include_stdout: bool = false,
    include_stderr: bool = false,
};

pub const SnapshotStream = enum {
    stdout,
    stderr,
};

/// ProcessSnapshot owns cloned stdout/stderr buffers so the original process
/// handle can be dropped while scripts keep working with redirected bindings.
pub const ProcessSnapshot = struct {
    allocator: std.mem.Allocator,
    stdout: ?[]u8,
    stderr: ?[]u8,
    status: ProcessStatus,
    pid: std.process.Child.Id,
    started_at_ns: i128,
    finished_at_ns: i128,

    pub fn deinit(self: *ProcessSnapshot) void {
        if (self.stdout) |buf| self.allocator.free(buf);
        if (self.stderr) |buf| self.allocator.free(buf);
        self.* = undefined;
    }

    pub fn stdoutBytes(self: ProcessSnapshot) []const u8 {
        return if (self.stdout) |buf| buf else &[_]u8{};
    }

    pub fn stderrBytes(self: ProcessSnapshot) []const u8 {
        return if (self.stderr) |buf| buf else &[_]u8{};
    }

    pub fn durationNs(self: ProcessSnapshot) i128 {
        return self.finished_at_ns - self.started_at_ns;
    }
};

pub const ProcessStatus = struct {
    ok: bool,
    exit_code: ?u8,
    signal: ?u32,
    failed_stage: ?usize,

    pub fn fromStages(stages: []const StageStatus) ProcessStatus {
        const last = stages[stages.len - 1];
        var failure_index: ?usize = null;
        var failure_exit: ?u8 = null;
        var failure_signal: ?u32 = null;

        for (stages) |stage| {
            if (!stage.ok) {
                failure_index = stage.index;
                failure_exit = stage.exit_code;
                failure_signal = stage.signal;
                break;
            }
        }

        return .{
            .ok = failure_index == null,
            .exit_code = failure_exit orelse last.exit_code,
            .signal = failure_signal orelse last.signal,
            .failed_stage = failure_index,
        };
    }
};

pub const StageStatus = struct {
    index: usize,
    term: std.process.Child.Term,
    exit_code: ?u8,
    signal: ?u32,
    ok: bool,

    pub fn fromTerm(index: usize, term: std.process.Child.Term) StageStatus {
        var exit_code: ?u8 = null;
        var signal: ?u32 = null;
        var ok = false;

        switch (term) {
            .Exited => |code| {
                exit_code = code;
                ok = code == 0;
            },
            .Signal => |sig| signal = sig,
            .Stopped => |sig| signal = sig,
            .Unknown => {},
        }

        return .{
            .index = index,
            .term = term,
            .exit_code = exit_code,
            .signal = signal,
            .ok = ok,
        };
    }
};

const default_max_capture_bytes = 1024 * 1024;

const StageExecution = struct {
    pid: std.process.Child.Id,
    started_at_ns: i128,
    finished_at_ns: i128,
    status: StageStatus,
    stdout_owned: []u8,
    stderr_owned: []u8,
};

const StageRunArgs = struct {
    spec: CommandRunner.CommandSpec,
    index: usize,
    stdin_data: ?[]const u8 = null,
};

pub const StageCapture = struct {
    stdout: []u8,
    stderr: []u8,

    pub fn stdoutBytes(self: StageCapture) []const u8 {
        return self.stdout;
    }

    pub fn stderrBytes(self: StageCapture) []const u8 {
        return self.stderr;
    }
};

pub const CommandDisplay = struct {
    argv: []const []const u8,

    pub fn format(
        self: CommandDisplay,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        for (self.argv, 0..) |arg, idx| {
            if (idx > 0) try writer.writeByte(' ');
            if (needsQuotes(arg)) {
                try writer.writeByte('"');
                try writer.writeAll(arg);
                try writer.writeByte('"');
            } else {
                try writer.writeAll(arg);
            }
        }
    }
};

fn destroyStageCaptures(allocator: std.mem.Allocator, captures: []StageCapture, count: usize) void {
    var idx: usize = 0;
    while (idx < count) : (idx += 1) {
        const capture = captures[idx];
        if (capture.stdout.len > 0) allocator.free(capture.stdout);
        if (capture.stderr.len > 0) allocator.free(capture.stderr);
    }
    allocator.free(captures);
}

fn cloneBytes(allocator: std.mem.Allocator, bytes: []const u8) ![]u8 {
    if (bytes.len == 0) {
        return allocator.alloc(u8, 0);
    }
    const copy = try allocator.alloc(u8, bytes.len);
    std.mem.copyForwards(u8, copy, bytes);
    return copy;
}

fn needsQuotes(arg: []const u8) bool {
    if (arg.len == 0) return true;
    for (arg) |byte| {
        if (std.ascii.isWhitespace(byte)) return true;
    }
    return false;
}

fn copyOwnedSlice(allocator: std.mem.Allocator, bytes: []const u8, include: bool) !?[]u8 {
    if (!include) return null;
    const copy = try allocator.alloc(u8, bytes.len);
    std.mem.copyForwards(u8, copy, bytes);
    return copy;
}

fn expectStringEqual(expected: []const u8, actual: []const u8) !void {
    try std.testing.expectEqualStrings(expected, actual);
}

test "command runner captures stdout and stderr" {
    var runner = CommandRunner.init(std.testing.allocator);
    var handle = try runner.runSync(.{
        .argv = &.{ "sh", "-c", "printf 'out'; printf 'err' 1>&2" },
    });
    defer handle.deinit();

    try expectStringEqual("out", handle.stdoutBytes());
    try expectStringEqual("err", handle.stderrBytes());
    try std.testing.expect(handle.status.ok);
    try std.testing.expect(handle.pid != 0);
    try std.testing.expect(handle.durationNs() >= 0);
    try std.testing.expectEqual(@as(usize, 1), handle.stageStatuses().len);
    try std.testing.expectEqual(@as(?u8, 0), handle.status.exit_code);
    const captures = handle.stageCaptures();
    try std.testing.expectEqual(@as(usize, 1), captures.len);
    try expectStringEqual("out", captures[0].stdoutBytes());
    try expectStringEqual("err", captures[0].stderrBytes());
}

test "command runner rejects empty pipelines and blank commands" {
    var runner = CommandRunner.init(std.testing.allocator);

    try std.testing.expectError(
        CommandRunner.Error.EmptyCommand,
        runner.runPipeline(&[_]CommandRunner.CommandSpec{}),
    );

    try std.testing.expectError(
        CommandRunner.Error.EmptyCommand,
        runner.runSync(.{ .argv = &.{} }),
    );
}

test "non-zero exit surfaces failed stage metadata" {
    var runner = CommandRunner.init(std.testing.allocator);
    var handle = try runner.runSync(.{
        .argv = &.{ "sh", "-c", "echo failed 1>&2; exit 7" },
    });
    defer handle.deinit();

    try std.testing.expect(!handle.status.ok);
    try std.testing.expectEqual(@as(?u8, 7), handle.status.exit_code);
    try std.testing.expectEqual(@as(?usize, 0), handle.status.failed_stage);
    try expectStringEqual("failed\n", handle.stderrBytes());
}

test "pipeline preserves per-stage outputs and statuses" {
    var runner = CommandRunner.init(std.testing.allocator);
    const specs = [_]CommandRunner.CommandSpec{
        .{ .argv = &.{ "sh", "-c", "printf 'alpha'" } },
        .{ .argv = &.{ "sh", "-c", "tr 'a-z' 'A-Z'" } },
    };
    var handle = try runner.runPipeline(&specs);
    defer handle.deinit();

    try expectStringEqual("ALPHA", handle.stdoutBytes());
    try std.testing.expect(handle.status.ok);
    try std.testing.expectEqual(@as(usize, specs.len), handle.stageStatuses().len);

    const captures = handle.stageCaptures();
    try std.testing.expectEqual(@as(usize, specs.len), captures.len);
    try expectStringEqual("alpha", captures[0].stdoutBytes());
    try expectStringEqual("ALPHA", captures[1].stdoutBytes());
}

test "pipeline tracks earliest failing stage" {
    var runner = CommandRunner.init(std.testing.allocator);
    const specs = [_]CommandRunner.CommandSpec{
        .{ .argv = &.{ "sh", "-c", "printf 'ok'; exit 0" } },
        .{ .argv = &.{ "sh", "-c", "echo boom 1>&2; exit 7" } },
        .{ .argv = &.{ "sh", "-c", "printf 'should still run'; exit 3" } },
    };
    var handle = try runner.runPipeline(&specs);
    defer handle.deinit();

    try std.testing.expect(!handle.status.ok);
    try std.testing.expectEqual(@as(?usize, 1), handle.status.failed_stage);
    try std.testing.expectEqual(@as(?u8, 7), handle.status.exit_code);

    const captures = handle.stageCaptures();
    try std.testing.expectEqual(@as(usize, specs.len), captures.len);
    try expectStringEqual("boom\n", captures[1].stderrBytes());
    // Later stages still execute so ensure final capture reflects last stage output.
    try expectStringEqual("should still run", captures[2].stdoutBytes());
}

test "process snapshot clones streams for destructuring" {
    var runner = CommandRunner.init(std.testing.allocator);
    var handle = try runner.runSync(.{
        .argv = &.{ "sh", "-c", "printf 'out'; printf 'err' 1>&2" },
    });
    defer handle.deinit();

    var snapshot = try handle.destructure(std.testing.allocator);
    defer snapshot.deinit();

    try expectStringEqual("out", snapshot.stdoutBytes());
    try expectStringEqual("err", snapshot.stderrBytes());
    try std.testing.expectEqual(handle.status.exit_code, snapshot.status.exit_code);

    try std.testing.expect(snapshot.stdout != null);
    try std.testing.expect(snapshot.stderr != null);

    const final_capture = handle.stage_captures[handle.stage_captures.len - 1];
    try std.testing.expect(snapshot.stdout.?.ptr != final_capture.stdout.ptr);
    try std.testing.expect(snapshot.stderr.?.ptr != final_capture.stderr.ptr);
}

test "process snapshot supports single stream redirects" {
    var runner = CommandRunner.init(std.testing.allocator);
    var handle = try runner.runSync(.{
        .argv = &.{ "sh", "-c", "printf 'alpha'; printf 'beta' 1>&2" },
    });
    defer handle.deinit();

    var stdout_capture = try handle.redirectStream(std.testing.allocator, .stdout);
    defer stdout_capture.deinit();
    try expectStringEqual("alpha", stdout_capture.stdoutBytes());
    try std.testing.expectEqual(@as(usize, 0), stdout_capture.stderrBytes().len);
    try std.testing.expect(stdout_capture.stderr == null);

    var stderr_capture = try handle.redirectStream(std.testing.allocator, .stderr);
    defer stderr_capture.deinit();
    try expectStringEqual("beta", stderr_capture.stderrBytes());
    try std.testing.expectEqual(@as(usize, 0), stderr_capture.stdoutBytes().len);
    try std.testing.expect(stderr_capture.stdout == null);
}

test "process handle clone preserves outputs and metadata" {
    var runner = CommandRunner.init(std.testing.allocator);
    var handle = try runner.runSync(.{
        .argv = &.{ "sh", "-c", "printf 'alpha'; printf 'beta' 1>&2; exit 2" },
    });
    defer handle.deinit();

    var cloned = try handle.clone(std.testing.allocator);
    defer cloned.deinit();

    try expectStringEqual(handle.stdoutBytes(), cloned.stdoutBytes());
    try expectStringEqual(handle.stderrBytes(), cloned.stderrBytes());
    try std.testing.expectEqual(handle.status.ok, cloned.status.ok);
    try std.testing.expectEqual(handle.status.exit_code, cloned.status.exit_code);
    try std.testing.expect(handle.stage_captures.ptr != cloned.stage_captures.ptr);
    try std.testing.expect(handle.stage_statuses.ptr != cloned.stage_statuses.ptr);

    const original_capture = handle.stage_captures[0];
    const cloned_capture = cloned.stage_captures[0];
    try std.testing.expect(original_capture.stdout.ptr != cloned_capture.stdout.ptr);
    try std.testing.expect(original_capture.stderr.ptr != cloned_capture.stderr.ptr);
}

test "command runner emits tracing logs" {
    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();
    var writer = buffer.writer(std.testing.allocator);
    var writer_adapter = writer.adaptToNewApi(&.{});
    var tracer = Tracer.init(&.{ "pipeline", "process" }, &writer_adapter.new_interface);

    var runner = CommandRunner.initWithTracer(std.testing.allocator, &tracer);
    var handle = try runner.runSync(.{ .argv = &.{ "sh", "-c", "printf 'trace me'" } });
    defer handle.deinit();

    const log = buffer.items;
    try std.testing.expect(std.mem.indexOf(u8, log, "starting pipeline") != null);
    try std.testing.expect(std.mem.indexOf(u8, log, "pipeline complete") != null);
}
