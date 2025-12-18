const std = @import("std");
// const tracing = @import("tracing.zig");
// const ast = @import("../frontend/ast.zig");
// const interpreter = @import("../interpreter/root.zig");
// const mem = @import("../mem/root.zig");
// const rainbow = @import("../rainbow.zig");
// const Stream = @import("../stream.zig").Stream;
// const StreamError = @import("../stream.zig").StreamError;
//
// const Tracer = tracing.Tracer;
// const TraceTopic = tracing.Topic;
//
// const logging_name = "EXECUTOR";
// const prefix_color = rainbow.beginColor(.orange);
// const end_color = rainbow.endColor();
//
// /// CommandRunner encapsulates the lower-level mechanics for spawning external
// /// programs and collecting their outputs. Higher-level runtime components feed
// /// commands through this struct so the rest of the interpreter can reason
// /// about process handles instead of juggling std.process.Child directly.
// pub const CommandRunner = struct {
//     allocator: std.mem.Allocator,
//     tracer: ?*Tracer = null,
//     logging_enabled: bool = false,
//
//     pub const Error = error{ EmptyCommand, PrimitiveValueInPipelineNotSupported } ||
//         mem.RCError ||
//         std.process.Child.RunError ||
//         std.process.Child.SpawnError ||
//         std.process.Child.WaitError ||
//         std.fs.File.WriteError ||
//         std.Io.Reader.Error ||
//         std.mem.Allocator.Error;
//
//     pub const CommandType = union(enum) {
//         function: interpreter.Value.FunctionRef,
//         value: *const interpreter.Value,
//         executable,
//     };
//
//     pub const CommandSpec = struct {
//         command_type: CommandType,
//         argv: []interpreter.Value.String,
//         cwd: ?[]const u8 = null,
//         env_map: ?*const std.process.EnvMap = null,
//         max_output_bytes: usize = default_max_capture_bytes,
//     };
//
//     pub fn init(allocator: std.mem.Allocator) CommandRunner {
//         return CommandRunner.initWithTracer(allocator, null);
//     }
//
//     pub fn initWithTracer(
//         allocator: std.mem.Allocator,
//         tracer: ?*Tracer,
//     ) CommandRunner {
//         const logging_enabled_s = std.process.getEnvVarOwned(allocator, "RUNIC_LOG_" ++ logging_name) catch null;
//         defer if (logging_enabled_s) |le| allocator.free(le);
//         const logging_enabled = if (logging_enabled_s) |le| std.mem.eql(u8, le, "1") else false;
//
//         return .{
//             .allocator = allocator,
//             .tracer = tracer,
//             .logging_enabled = logging_enabled,
//         };
//     }
//
//     pub fn log(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
//         if (@hasField(@This(), "logging_enabled")) {
//             if (!self.logging_enabled) return;
//         }
//
//         var stderr = std.fs.File.stderr().writer(&.{});
//         const writer = &stderr.interface;
//
//         try writer.print("[{s}{*}{s}] ", .{ prefix_color, self, end_color });
//         try writer.print(fmt ++ "\n", args);
//     }
//
//     /// Runs `spec` synchronously, capturing stdout/stderr, and returns a
//     /// structured process handle that describes the execution.
//     pub fn runSync(self: CommandRunner, spec: CommandSpec) Error!ProcessHandle {
//         const handle = try self.runPipeline(&[_]CommandSpec{spec});
//         return handle;
//     }
//
//     /// Executes the sequence of command specs as a Runic pipeline. The stdout
//     /// from stage `n` becomes the stdin for stage `n + 1`, and every stage's
//     /// outputs plus exit metadata are preserved for later inspection.
//     pub fn runPipeline(
//         self: *CommandRunner,
//         evaluator: *interpreter.Evaluator,
//         scopes: *interpreter.ScopeStack,
//         specs: []const CommandSpec,
//     ) Error!ProcessHandle {
//         errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
//
//         try self.log("{s}", .{@src().fn_name});
//
//         if (specs.len == 0) {
//             return error.EmptyCommand;
//         }
//
//         var stage_statuses = try self.allocator.alloc(StageStatus, specs.len);
//         errdefer self.allocator.free(stage_statuses);
//
//         var stage_captures = try self.allocator.alloc(StageCapture, specs.len);
//         var initialized_captures: usize = 0;
//         errdefer destroyStageCaptures(self.allocator, stage_captures, initialized_captures);
//
//         var started_at_ns: i128 = 0;
//         var finished_at_ns: i128 = 0;
//         var primary_pid: ?std.process.Child.Id = undefined;
//         var previous_stdout: ?[]const u8 = null;
//
//         self.tracePipelineStart(specs);
//
//         for (specs, 0..) |spec, index| {
//             // TODO: Fix streaming, each stage will be spawned in parallel, and each forward should happen in a streaming manner, the last stage should be using the current implementation
//             var stage_run_args: StageRunArgs = .{
//                 .spec = spec,
//                 .index = index,
//                 .stdin_data = previous_stdout,
//             };
//             const stage = try self.runStage(evaluator, scopes, &stage_run_args, specs);
//
//             if (!stage.status.ok) {
//                 // TODO: error handling
//             }
//
//             if (index == 0) {
//                 primary_pid = stage.pid;
//                 started_at_ns = stage.started_at_ns;
//             }
//             finished_at_ns = stage.finished_at_ns;
//
//             stage_statuses[index] = stage.status;
//             stage_captures[index] = .{
//                 .stdout = stage.stdout_owned,
//                 .stderr = stage.stderr_owned,
//             };
//             initialized_captures = index + 1;
//             previous_stdout = stage_captures[index].stdout;
//
//             self.traceStageResult(&stage);
//         }
//
//         var handle = ProcessHandle{
//             .allocator = self.allocator,
//             .handle_type = if (primary_pid) |ppid| .{ .process = ppid } else .function,
//             .started_at_ns = started_at_ns,
//             .finished_at_ns = finished_at_ns,
//             .status = ProcessStatus.fromStages(stage_statuses),
//             .stage_statuses = stage_statuses,
//             .stage_captures = stage_captures,
//         };
//
//         self.traceProcessHandle(&handle);
//         return handle;
//     }
//
//     fn runStage(
//         self: *CommandRunner,
//         evaluator: *interpreter.Evaluator,
//         scopes: *interpreter.ScopeStack,
//         args: *StageRunArgs,
//         specs: []const CommandSpec,
//     ) Error!StageExecution {
//         errdefer |err| self.log(@src().fn_name ++ ": error {}", .{err}) catch {};
//
//         const stdout_pipe = scopes.getStdoutPipe();
//
//         try self.log("{s}: {{stdout.is_streaming: {}}}", .{ @src().fn_name, stdout_pipe.is_streaming });
//
//         if (args.spec.argv.len == 0) {
//             return error.EmptyCommand;
//         }
//
//         return switch (args.spec.command_type) {
//             .executable => self.runStageExecutable(scopes, args, specs),
//             .function => |fn_ref| self.runStageFunction(evaluator, scopes, fn_ref, args),
//             .value => |value| self.runValueAsStream(value, args),
//         };
//     }
//
//     fn runStageFunction(
//         _: CommandRunner,
//         evaluator: *interpreter.Evaluator,
//         scopes: *interpreter.ScopeStack,
//         fn_ref: interpreter.Value.FunctionRef,
//         // args: *StageRunArgs,
//     ) Error!StageExecution {
//         // var scopes = interpreter.ScopeStack.init(self.allocator);
//         // defer scopes.deinit();
//
//         // executable
//         //   exitCode
//         //   stdout
//         //   stderr
//         //
//         // function
//         //   exitCode
//         //   stdout
//         //   stderr
//         //   return value
//         //
//         // fn parseInt(@stdin: String, radix: Int) !Int {
//         //   ...
//         // }
//         //
//         // fn add5(@stdin: Int) !Int {
//         //   echo "asdf"
//         //   @echo "asdf"
//         //   return @stdin + 5
//         // }
//         //
//         // const int = echo "a5" | parseInt 16
//         // const hello = echo "hello"
//         //
//         // echo "${hello}"
//         // echo "${int}"
//         //
//         // const helloResult = hello
//         // # helloResult = ProcessHandle{
//         //     .exitCode = .{ .error = 4 } | .success,
//         //     .stdout = "",
//         //     .stderr = "",
//         // }
//
//         const started_at = std.time.nanoTimestamp();
//
//         const Result = union(enum) {
//             success: interpreter.Evaluator.RunFunctionResult,
//             err: anyerror,
//         };
//
//         // TODO: fix stdin wiring when implementing that for functions
//         const result: Result = if (evaluator.runFunction(scopes, fn_ref)) |value| .{
//             .success = value,
//         } else |err| .{
//             .err = err,
//         };
//
//         const finished_at = std.time.nanoTimestamp();
//
//         const exitCode: ExitCode = switch (result) {
//             .success => .success,
//             .err => |err| .{ .err = err },
//         };
//
//         const stdout: []u8 = switch (result) {
//             .success => |f| f.stdout,
//             .err => &.{},
//         };
//         const stderr: []u8 = switch (result) {
//             .success => |f| f.stderr,
//             .err => &.{},
//         };
//
//         return .{
//             .pid = null,
//             .started_at_ns = started_at,
//             .finished_at_ns = finished_at,
//             .status = undefined,//StageStatus.fromTerm(args.index, .{ .function = exitCode }),
//             // TODO: fix when implementing typed stdout for functions
//             .stdout_owned = stdout,
//             .stderr_owned = stderr,
//         };
//     }
//
//     fn runValueAsStream(
//         self: CommandRunner,
//         value: *const interpreter.Value,
//         args: *StageRunArgs,
//     ) Error!StageExecution {
//         const started_at = std.time.nanoTimestamp();
//
//         const Result = union(enum) {
//             success: interpreter.Evaluator.RunFunctionResult,
//             err: anyerror,
//         };
//
//         // TODO: fix stdin wiring when implementing that for functions
//         const value_stream = try Stream([]const u8).fromValue(self.allocator, value.*);
//         defer value_stream.deinit();
//
//         var alloc_writer = std.Io.Writer.Allocating.init(self.allocator);
//         defer alloc_writer.deinit();
//         var stream_error: ?StreamError = null;
//
//         while (true) switch (value_stream.next() catch |err| {
//             stream_error = err;
//             break;
//         } orelse continue) {
//             .completed => break,
//             .next => |string| try alloc_writer.writer.writeAll(string),
//         };
//
//         const finished_at = std.time.nanoTimestamp();
//
//         const result: Result = if (stream_error) |err| .{
//             .err = err,
//         } else .{
//             .success = .{
//                 .stdout = try alloc_writer.toOwnedSlice(),
//                 .stderr = &.{},
//                 .started_at_ns = started_at,
//                 .finished_at_ns = finished_at,
//             },
//         };
//
//         // const exitCode: ExitCode = switch (result) {
//         //     .success => .success,
//         //     .err => |err| .{ .err = err },
//         // };
//
//         const stdout: []u8 = switch (result) {
//             .success => |f| f.stdout,
//             .err => &.{},
//         };
//         const stderr: []u8 = switch (result) {
//             .success => |f| f.stderr,
//             .err => &.{},
//         };
//
//         return .{
//             .pid = null,
//             .started_at_ns = started_at,
//             .finished_at_ns = finished_at,
//             .status = undefined,//StageStatus.fromTerm(args.index, .{ .function = exitCode }),
//             // TODO: fix when implementing typed stdout for functions
//             .stdout_owned = stdout,
//             .stderr_owned = stderr,
//         };
//     }
//
//     fn dupeArgs(
//         self: CommandRunner,
//         args: []const interpreter.Value.String,
//     ) Error![]const []const u8 {
//         const argv: [][]const u8 = try self.allocator.alloc([]const u8, args.len);
//         for (args, 0..) |arg, i| argv[i] = try arg.get();
//         return argv;
//     }
//
//     const PipelineExecution = struct {
//         children: []std.process.Child,
//
//         pub fn init() PipelineExecution {}
//     };
//
//     fn runStageExecutable(
//         self: *CommandRunner,
//         scopes: *interpreter.ScopeStack,
//         args: *StageRunArgs,
//         specs: []const CommandSpec,
//     ) Error!StageExecution {
//         errdefer self.log(@src().fn_name ++ ": error", .{}) catch {};
//
//         try self.log("{s}: \"{s}\"", .{ @src().fn_name, try args.spec.argv[0].get() });
//
//         const argv = try self.dupeArgs(args.spec.argv);
//         defer self.allocator.free(argv);
//         var child = std.process.Child.init(argv, self.allocator);
//         child.stdin_behavior = if (args.stdin_data != null) .Pipe else .Ignore;
//         child.stdout_behavior = .Pipe;
//         child.stderr_behavior = .Pipe;
//         child.cwd = args.spec.cwd;
//         child.env_map = args.spec.env_map;
//
//         var stdout_buf_writer = std.Io.Writer.Allocating.init(self.allocator);
//         defer stdout_buf_writer.deinit();
//         var stderr_buf_writer = std.Io.Writer.Allocating.init(self.allocator);
//         defer stderr_buf_writer.deinit();
//
//         const started_at = std.time.nanoTimestamp();
//         child.spawn() catch |err| switch (err) {
//             error.FileNotFound => {
//                 std.log.err("command `{s}` not found", .{argv[0]});
//                 return err;
//             },
//             else => {
//                 std.log.err("error when trying to run command `{s}`: {}", .{ argv[0], err });
//                 return err;
//             },
//         };
//         errdefer {
//             _ = child.kill() catch {};
//         }
//
//         const pid_snapshot = child.id;
//
//         // TODO: Implement streaming input
//         if (args.stdin_data) |input| {
//             var stdin_file = child.stdin orelse unreachable;
//             defer {
//                 stdin_file.close();
//                 child.stdin = null;
//             }
//             try stdin_file.writeAll(input);
//         }
//
//         var forward_stdout = scopes.getStdoutPipe();
//         var forward_stderr = scopes.getStderrPipe();
//
//         forward_stdout.is_streaming = if (specs.len > 1) .non_streaming else forward_stdout.is_streaming;
//         forward_stderr.is_streaming = if (specs.len > 1) .non_streaming else forward_stderr.is_streaming;
//
//         var stdout_fwd_buffer: [512]u8 = undefined;
//         var stdout_file_reader = child.stdout.?.readerStreaming(&stdout_fwd_buffer);
//         const stdout_reader = &stdout_file_reader.interface;
//
//         var stderr_fwd_buffer: [512]u8 = undefined;
//         var stderr_file_reader = child.stderr.?.readerStreaming(&stderr_fwd_buffer);
//         const stderr_reader = &stderr_file_reader.interface;
//
//         var stdout_closed = false;
//         var stderr_closed = false;
//
//         const stdout_writer = if (forward_stdout.is_streaming == .streaming) forward_stdout.writer else &stdout_buf_writer.writer;
//         const stderr_writer = if (forward_stderr.is_streaming == .streaming) forward_stderr.writer else &stderr_buf_writer.writer;
//
//         try self.log("forward stdout: {}", .{forward_stdout.is_streaming});
//         try self.log("forward stderr: {}", .{forward_stderr.is_streaming});
//
//         while (true) {
//             if (child.term != null) break;
//             stdout_closed = try stream(stdout_reader, stdout_writer);
//             stderr_closed = try stream(stderr_reader, stderr_writer);
//
//             if (stdout_closed and stderr_closed) {
//                 break;
//             }
//         }
//
//         // const term = child.wait() catch |err| switch (err) {
//         //     error.FileNotFound => {
//         //         std.log.err("command `{s}` not found", .{argv[0]});
//         //         return err;
//         //     },
//         //     else => {
//         //         std.log.err("error when trying to run command `{s}`: {}", .{ argv[0], err });
//         //         return err;
//         //     },
//         // };
//         const finished_at = std.time.nanoTimestamp();
//
//         const stdout_owned = switch (forward_stdout.is_streaming) {
//             .streaming => try self.allocator.dupe(u8, ""),
//             .non_streaming => brk: {
//                 try forward_stdout.writer.writeAll(stdout_buf_writer.written());
//                 break :brk try stdout_buf_writer.toOwnedSlice();
//             },
//         };
//         const stderr_owned = switch (forward_stderr.is_streaming) {
//             .streaming => try self.allocator.dupe(u8, ""),
//             .non_streaming => brk: {
//                 try forward_stderr.writer.writeAll(stderr_buf_writer.written());
//                 break :brk try stderr_buf_writer.toOwnedSlice();
//             },
//         };
//
//         return StageExecution{
//             .pid = pid_snapshot,
//             .started_at_ns = started_at,
//             .finished_at_ns = finished_at,
//             .status = undefined,//StageStatus.fromTerm(args.index, .{ .process = term }),
//             .stdout_owned = stdout_owned,
//             .stderr_owned = stderr_owned,
//         };
//     }
//
//     /// Returns true if stream was closed
//     fn stream(reader: *std.Io.Reader, writer: *std.Io.Writer) Error!bool {
//         const bytes_streamed = reader.stream(
//             writer,
//             .unlimited,
//         ) catch |err| switch (err) {
//             std.Io.Reader.StreamError.EndOfStream => return true,
//             else => return err,
//         };
//         if (bytes_streamed > 0) {
//             try writer.flush();
//         }
//
//         return false;
//     }
//
//     fn tracePipelineStart(self: CommandRunner, specs: []const CommandSpec) void {
//         const tracer = self.tracer orelse return;
//         tracer.log(.pipeline, "starting pipeline (stages={})", .{specs.len}) catch {};
//         for (specs, 0..) |spec, idx| {
//             tracer.log(.pipeline, "stage {}: {any}", .{
//                 idx + 1,
//                 CommandDisplay{ .argv = spec.argv },
//             }) catch {};
//         }
//     }
//
//     fn traceStageResult(self: CommandRunner, stage: *const StageExecution) void {
//         const tracer = self.tracer orelse return;
//         const stdout_len = stage.stdout_owned.len;
//         const stderr_len = stage.stderr_owned.len;
//         const duration = stage.finished_at_ns - stage.started_at_ns;
//
//         if (stage.status.signal) |sig| {
//             tracer.log(
//                 .pipeline,
//                 "stage {} pid={?} signal={} stdout={}B stderr={}B duration_ns={}",
//                 .{ stage.status.index + 1, stage.pid, sig, stdout_len, stderr_len, duration },
//             ) catch {};
//             return;
//         }
//
//         if (stage.status.exit_code) |code| {
//             tracer.log(
//                 .pipeline,
//                 "stage {} pid={?} exit={f} ok={} stdout={}B stderr={}B duration_ns={}",
//                 .{ stage.status.index + 1, stage.pid, code, stage.status.ok, stdout_len, stderr_len, duration },
//             ) catch {};
//             return;
//         }
//
//         tracer.log(
//             .pipeline,
//             "stage {} pid={?} ok={} stdout={}B stderr={}B duration_ns={}",
//             .{ stage.status.index + 1, stage.pid, stage.status.ok, stdout_len, stderr_len, duration },
//         ) catch {};
//     }
//
//     fn traceProcessHandle(self: CommandRunner, handle: *const ProcessHandle) void {
//         const tracer = self.tracer orelse return;
//         handle.traceSummary(tracer, "pipeline complete") catch {};
//     }
// };
//
// pub const ProcessHandle = struct {
//     handle_type: union(enum) {
//         process: std.process.Child.Id,
//         function,
//     },
//     allocator: std.mem.Allocator,
//     started_at_ns: i128,
//     finished_at_ns: i128,
//     status: ProcessStatus,
//     stage_statuses: []StageStatus,
//     stage_captures: []StageCapture,
//
//     pub fn deinit(self: *ProcessHandle) void {
//         destroyStageCaptures(self.allocator, self.stage_captures, self.stage_captures.len);
//         self.allocator.free(self.stage_statuses);
//         self.* = undefined;
//     }
//
//     pub fn getPid(self: ProcessHandle) ?std.process.Child.Id {
//         return if (self.handle_type == .process) self.handle_type.process else null;
//     }
//
//     pub fn stdoutBytes(self: ProcessHandle) []const u8 {
//         if (self.stage_captures.len == 0) return &[_]u8{};
//         return self.stage_captures[self.stage_captures.len - 1].stdout;
//     }
//
//     pub fn stderrBytes(self: ProcessHandle) []const u8 {
//         if (self.stage_captures.len == 0) return &[_]u8{};
//         return self.stage_captures[self.stage_captures.len - 1].stderr;
//     }
//
//     pub fn durationNs(self: ProcessHandle) i128 {
//         return self.finished_at_ns - self.started_at_ns;
//     }
//
//     pub fn stageStatuses(self: ProcessHandle) []const StageStatus {
//         return self.stage_statuses;
//     }
//
//     pub fn stageCaptures(self: ProcessHandle) []const StageCapture {
//         return self.stage_captures;
//     }
//
//     /// Emits a multi-line summary of the process handle (overall status followed
//     /// by per-stage outcomes) through the provided tracer so callers can inspect
//     /// pipelines without manually destructuring the handle.
//     pub fn traceSummary(self: ProcessHandle, tracer: *Tracer, label: []const u8) !void {
//         try tracer.log(
//             .process,
//             "{s}: pid={?} stages={} ok={} duration_ns={}",
//             .{ label, self.getPid(), self.stage_statuses.len, self.status.ok, self.durationNs() },
//         );
//         if (self.status.exit_code) |code| {
//             try tracer.log(.process, "{s}: exit code {f}", .{ label, code });
//         } else {
//             try tracer.log(.process, "{s}: exit code unknown", .{label});
//         }
//         if (self.status.signal) |sig| {
//             try tracer.log(.process, "{s}: signal {}", .{ label, sig });
//         }
//         if (self.status.failed_stage) |idx| {
//             try tracer.log(.process, "{s}: failed stage {}", .{ label, idx + 1 });
//         }
//         const statuses = self.stageStatuses();
//         for (statuses) |status| {
//             if (status.signal) |sig| {
//                 try tracer.log(.process, "{s}: stage {} signal {}", .{ label, status.index + 1, sig });
//                 continue;
//             }
//             if (status.exit_code) |code| {
//                 try tracer.log(
//                     .process,
//                     "{s}: stage {} exit {f} (ok={})",
//                     .{ label, status.index + 1, code, status.ok },
//                 );
//                 continue;
//             }
//             try tracer.log(.process, "{s}: stage {} ok={}", .{ label, status.index + 1, status.ok });
//         }
//     }
//
//     /// Produces a snapshot of the process handle for destructuring or stream
//     /// redirection sugar. Callers choose which streams to clone so commands
//     /// like `1>var` can avoid copying stderr unnecessarily.
//     pub fn snapshot(self: ProcessHandle, allocator: std.mem.Allocator, options: SnapshotOptions) !ProcessSnapshot {
//         const stdout_copy = try copyOwnedSlice(allocator, self.stdoutBytes(), options.include_stdout);
//         errdefer if (stdout_copy) |buf| allocator.free(buf);
//
//         const stderr_copy = try copyOwnedSlice(allocator, self.stderrBytes(), options.include_stderr);
//         errdefer if (stderr_copy) |buf| allocator.free(buf);
//
//         return ProcessSnapshot{
//             .allocator = allocator,
//             .stdout = stdout_copy,
//             .stderr = stderr_copy,
//             .status = self.status,
//             .pid = self.pid,
//             .started_at_ns = self.started_at_ns,
//             .finished_at_ns = self.finished_at_ns,
//         };
//     }
//
//     /// Convenience wrapper that clones both stdout and stderr into an
//     /// independent snapshot for destructuring assignments.
//     pub fn destructure(self: ProcessHandle, allocator: std.mem.Allocator) !ProcessSnapshot {
//         return self.snapshot(allocator, .{ .include_stdout = true, .include_stderr = true });
//     }
//
//     /// Captures a single stream (stdout or stderr) into a new snapshot so `1>`
//     /// and `2>` sugar can bind the requested stream without throwing away the
//     /// original handle.
//     pub fn redirectStream(self: ProcessHandle, allocator: std.mem.Allocator, stream: SnapshotStream) !ProcessSnapshot {
//         return self.snapshot(allocator, .{
//             .include_stdout = stream == .stdout,
//             .include_stderr = stream == .stderr,
//         });
//     }
//
//     /// Creates a deep copy of the process handle so background executions can
//     /// shuttle results between different allocators. The cloned handle owns its
//     /// own capture buffers and must be deinitialized independently.
//     pub fn clone(self: ProcessHandle, allocator: std.mem.Allocator) !ProcessHandle {
//         const statuses_copy = try allocator.alloc(StageStatus, self.stage_statuses.len);
//         errdefer allocator.free(statuses_copy);
//         @memcpy(statuses_copy, self.stage_statuses);
//
//         const captures_copy = try allocator.alloc(StageCapture, self.stage_captures.len);
//         var initialized: usize = 0;
//         errdefer destroyStageCaptures(allocator, captures_copy, initialized);
//
//         while (initialized < self.stage_captures.len) : (initialized += 1) {
//             const capture = self.stage_captures[initialized];
//             const stdout_copy = cloneBytes(allocator, capture.stdout) catch |err| {
//                 destroyStageCaptures(allocator, captures_copy, initialized);
//                 return err;
//             };
//             const stderr_copy = cloneBytes(allocator, capture.stderr) catch |err| {
//                 allocator.free(stdout_copy);
//                 destroyStageCaptures(allocator, captures_copy, initialized);
//                 return err;
//             };
//             captures_copy[initialized] = .{
//                 .stdout = stdout_copy,
//                 .stderr = stderr_copy,
//             };
//         }
//
//         return .{
//             .allocator = allocator,
//             .handle_type = self.handle_type,
//             .started_at_ns = self.started_at_ns,
//             .finished_at_ns = self.finished_at_ns,
//             .status = self.status,
//             .stage_statuses = statuses_copy,
//             .stage_captures = captures_copy,
//         };
//     }
// };
//
// pub const SnapshotOptions = struct {
//     include_stdout: bool = false,
//     include_stderr: bool = false,
// };
//
// pub const SnapshotStream = enum {
//     stdout,
//     stderr,
// };
//
// /// ProcessSnapshot owns cloned stdout/stderr buffers so the original process
// /// handle can be dropped while scripts keep working with redirected bindings.
// pub const ProcessSnapshot = struct {
//     allocator: std.mem.Allocator,
//     stdout: ?[]u8,
//     stderr: ?[]u8,
//     status: ProcessStatus,
//     pid: std.process.Child.Id,
//     started_at_ns: i128,
//     finished_at_ns: i128,
//
//     pub fn deinit(self: *ProcessSnapshot) void {
//         if (self.stdout) |buf| self.allocator.free(buf);
//         if (self.stderr) |buf| self.allocator.free(buf);
//         self.* = undefined;
//     }
//
//     pub fn stdoutBytes(self: ProcessSnapshot) []const u8 {
//         return if (self.stdout) |buf| buf else &[_]u8{};
//     }
//
//     pub fn stderrBytes(self: ProcessSnapshot) []const u8 {
//         return if (self.stderr) |buf| buf else &[_]u8{};
//     }
//
//     pub fn durationNs(self: ProcessSnapshot) i128 {
//         return self.finished_at_ns - self.started_at_ns;
//     }
// };

// pub const ProcessStatus = struct {
//     ok: bool,
//     exit_code: ?ExitCode,
//     signal: ?u32,
//     failed_stage: ?usize,
//
//     pub fn fromStages(stages: []const StageStatus) ProcessStatus {
//         const last = stages[stages.len - 1];
//         var failure_index: ?usize = null;
//         var failure_exit: ?ExitCode = null;
//         var failure_signal: ?u32 = null;
//
//         for (stages) |stage| {
//             if (!stage.ok) {
//                 failure_index = stage.index;
//                 failure_exit = stage.exit_code;
//                 failure_signal = stage.signal;
//                 break;
//             }
//         }
//
//         return .{
//             .ok = failure_index == null,
//             .exit_code = failure_exit orelse last.exit_code,
//             .signal = failure_signal orelse last.signal,
//             .failed_stage = failure_index,
//         };
//     }
// };

pub const ExitCode = union(enum) {
    success,
    err: anyerror,
    term: std.process.Child.Term,

    pub fn fromByte(byte: u8) ExitCode {
        if (byte == 0) return .success;
        return .{ .term = .{ .Exited = byte } };
    }

    pub fn fromTerm(term: std.process.Child.SpawnError!std.process.Child.Term) ExitCode {
        if (term) |t| {
            return switch (t) {
                .Exited => |byte| fromByte(byte),
                else => .{ .term = t },
            };
        } else |err| {
            return .{ .err = err };
        }
    }

    pub fn getErrorCode(self: ExitCode) u8 {
        return switch (self) {
            .success => 0,
            .term => |term| switch (term) {
                .Exited => |byte| byte,
                else => 1,
            },
            .err => 1,
        };
    }

    pub fn format(self: ExitCode, writer: *std.Io.Writer) !void {
        try switch (self) {
            .success, .term => writer.print("{}", .{self.getErrorCode()}),
            .err => |err| writer.print("{} ({})", .{ self.getErrorCode(), err }),
        };
    }
};
//
// const Term = union(enum) {
//     process: std.process.Child.Term,
//     function: ExitCode,
// };
//
// pub const StageStatus = struct {
//     index: usize,
//     term: Term,
//     exit_code: ?ExitCode,
//     signal: ?u32,
//     ok: bool,
//
//     // pub fn fromTerm(index: usize, term: Term) StageStatus {
//     //     var exit_code: ?ExitCode = null;
//     //     var signal: ?u32 = null;
//     //     var ok = false;
//     //
//     //     switch (term) {
//     //         .process => |p| switch (p) {
//     //             .Exited => |code| {
//     //                 ok = code == 0;
//     //                 exit_code = if (ok) .success else .{ .byte = code };
//     //             },
//     //             .Signal => |sig| signal = sig,
//     //             .Stopped => |sig| signal = sig,
//     //             .Unknown => {},
//     //         },
//     //         .function => |exitCode| {
//     //             exit_code = exitCode;
//     //             ok = switch (exitCode) {
//     //                 .success => true,
//     //                 else => false,
//     //             };
//     //         },
//     //     }
//     //
//     //     return .{
//     //         .index = index,
//     //         .term = term,
//     //         .exit_code = exit_code,
//     //         .signal = signal,
//     //         .ok = ok,
//     //     };
//     // }
// };
//
// const default_max_capture_bytes = 1024 * 1024;
//
// pub const StageExecution = struct {
//     pid: ?std.process.Child.Id,
//     started_at_ns: i128,
//     finished_at_ns: i128,
//     status: StageStatus,
//     stdout_owned: []u8,
//     stderr_owned: []u8,
// };
//
// const StageRunArgs = struct {
//     spec: CommandRunner.CommandSpec,
//     index: usize,
//     stdin_data: ?[]const u8 = null,
// };
//
// pub const StageCapture = struct {
//     stdout: []u8,
//     stderr: []u8,
//
//     pub fn stdoutBytes(self: StageCapture) []const u8 {
//         return self.stdout;
//     }
//
//     pub fn stderrBytes(self: StageCapture) []const u8 {
//         return self.stderr;
//     }
// };
//
// pub const CommandDisplay = struct {
//     argv: []const interpreter.Value.String,
//
//     pub fn format(
//         self: CommandDisplay,
//         comptime _: []const u8,
//         _: std.fmt.FormatOptions,
//         writer: anytype,
//     ) !void {
//         for (self.argv, 0..) |arg, idx| {
//             if (idx > 0) try writer.writeByte(' ');
//             if (needsQuotes(arg)) {
//                 try writer.writeByte('"');
//                 try writer.writeAll(arg);
//                 try writer.writeByte('"');
//             } else {
//                 try writer.writeAll(arg);
//             }
//         }
//     }
// };
//
// fn destroyStageCaptures(allocator: std.mem.Allocator, captures: []StageCapture, count: usize) void {
//     var idx: usize = 0;
//     while (idx < count) : (idx += 1) {
//         const capture = captures[idx];
//         if (capture.stdout.len > 0) allocator.free(capture.stdout);
//         if (capture.stderr.len > 0) allocator.free(capture.stderr);
//     }
//     allocator.free(captures);
// }
//
// fn cloneBytes(allocator: std.mem.Allocator, bytes: []const u8) ![]u8 {
//     if (bytes.len == 0) {
//         return allocator.alloc(u8, 0);
//     }
//     const copy = try allocator.alloc(u8, bytes.len);
//     std.mem.copyForwards(u8, copy, bytes);
//     return copy;
// }
//
// fn needsQuotes(arg: []const u8) bool {
//     if (arg.len == 0) return true;
//     for (arg) |byte| {
//         if (std.ascii.isWhitespace(byte)) return true;
//     }
//     return false;
// }
//
// fn copyOwnedSlice(allocator: std.mem.Allocator, bytes: []const u8, include: bool) !?[]u8 {
//     if (!include) return null;
//     const copy = try allocator.alloc(u8, bytes.len);
//     std.mem.copyForwards(u8, copy, bytes);
//     return copy;
// }
