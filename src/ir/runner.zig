const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const ir = runic.ir;
const ast = runic.ast;
const ExitCode = runic.ExitCode;
const DocumentStore = runic.DocumentStore;
const CloseableReader = runic.closeable.CloseableReader;
const CloseableWriter = runic.closeable.CloseableWriter;
const ReaderWriterStream = runic.stream.ReaderWriterStream;
const rainbow = @import("../rainbow.zig");
const Tracer = runic.trace.Tracer;

const span_color = rainbow.beginBgColor(.red) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

pub const IRConfig = struct {
    io: std.Io,
    verbose: bool,
    dry_run: bool,
    script_args: []const []const u8 = &.{},
    env: *std.process.Environ.Map,
    stdin: *ReaderWriterStream,
    stdout: *ReaderWriterStream,
    stderr: *ReaderWriterStream,
    stdout_is_tty: bool = false,
    stderr_is_tty: bool = false,
    tracer: *Tracer,
    // stdin: CloseableReader(ExitCode),
    // stdout: CloseableWriter(ExitCode),
    // stderr: CloseableWriter(ExitCode),
};

pub const CompilationResult = union(enum) {
    err: struct {
        _diagnostics: []const ir.compiler.Diagnostic,

        pub fn diagnostics(self: @This()) []const ir.compiler.Diagnostic {
            return self._diagnostics;
        }
    },
    success: ir.context.IRProgramContext,

    pub fn err_(diagnostics: []const ir.compiler.Diagnostic) @This() {
        return .{ .err = .{ ._diagnostics = diagnostics } };
    }
};

pub const IRRunner = struct {
    allocator: Allocator,
    /// A freeing allocator (not the compile+runtime arena) used for resources
    /// that should be reclaimed during the run — currently runtime pipes (P1).
    /// Defaults to `allocator`; `runIR` overrides it to the process allocator.
    free_allocator: Allocator,
    document_store: *DocumentStore,
    script: *ast.Script,
    config: IRConfig,
    /// The source span of the instruction a generic runtime error faulted on, so
    /// `runIR` can render it as a proper `[error]:` diagnostic. Null when the
    /// faulting instruction carries no source location.
    runtime_error_span: ?ast.Span = null,
    /// Extra detail for the faulting runtime error (e.g. the missing command's
    /// name for `CommandNotFound`), used to build the diagnostic message.
    runtime_error_detail: ?[]const u8 = null,

    pub fn init(
        allocator: Allocator,
        document_store: *DocumentStore,
        script: *ast.Script,
        config: IRConfig,
    ) @This() {
        return .{
            .allocator = allocator,
            .free_allocator = allocator,
            .document_store = document_store,
            .script = script,
            .config = config,
        };
    }

    fn log(self: IRRunner, comptime fmt: []const u8, args: anytype) void {
        if (!self.config.verbose) return;
        std.log.debug(fmt, args);
    }

    pub fn compile(self: *IRRunner) !CompilationResult {
        var compiler = try ir.compiler.IRCompiler.init(
            self.config.io,
            self.allocator,
            self.document_store,
            self.script,
            self.config.script_args,
            self.config.env,
        );
        const result = try compiler.compile();

        return switch (result) {
            .err => |err| .err_(err.diagnostics()),
            .success => |shared| .{ .success = .init(self.config.io, self.allocator, shared) },
        };
    }

    pub fn run(self: *IRRunner, context: *ir.context.IRProgramContext) !ExitCode {
        var evaluator = ir.evaluator.IREvaluator.init(
            self.allocator,
            .{
                .io = self.config.io,
                .verbose = self.config.verbose,
                .stdin = self.config.stdin,
                .stdout = self.config.stdout,
                .stderr = self.config.stderr,
                .stdout_is_tty = self.config.stdout_is_tty,
                .stderr_is_tty = self.config.stderr_is_tty,
                .tracer = self.config.tracer,
                .pipe_allocator = self.free_allocator,
            },
            context,
        );

        while (evaluator.step() catch |err| {
            const current_thread = evaluator.context.getCurrentThreadPtr() orelse unreachable;
            const current_instr = current_thread.currentInstruction();

            // Capture the faulting instruction's source span (and any extra
            // detail, e.g. the missing command name) so `runIR` can render a
            // proper `[error]:` diagnostic (file:line:col + source line), the same
            // shape as a compile error — instead of a raw "Error evaluating …" /
            // "runic exited with error …" pair.
            if (current_instr) |ci| {
                self.runtime_error_span = if (ci.source) |source| source.span() else null;
            }
            self.runtime_error_detail = evaluator.error_detail;

            return err;
        }) |result| switch (result) {
            .cont, .cont_no_instr_counter_inc, .skip => continue,
            .exit => |exit_code| return exit_code,
            .process_exit => |exit_code| return exit_code,
        };

        return .success;
    }
};

pub const RunResult = union(enum) {
    err: struct {
        _diagnostics: []const ir.compiler.Diagnostic,
        arena: std.heap.ArenaAllocator,

        pub fn diagnostics(self: @This()) []const ir.compiler.Diagnostic {
            return self._diagnostics;
        }
    },
    success: ExitCode,

    pub fn deinit(self: *@This()) void {
        switch (self.*) {
            .err => |err| err.arena.deinit(),
            .success => {},
        }
    }

    pub fn err_(
        arena: std.heap.ArenaAllocator,
        diagnostics: []const ir.compiler.Diagnostic,
    ) @This() {
        return .{ .err = .{ ._diagnostics = diagnostics, .arena = arena } };
    }
};

pub fn runIR(
    allocator: Allocator,
    document_store: *DocumentStore,
    script: *ast.Script,
    config: IRConfig,
) !RunResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_allocator = arena.allocator();
    var runner = IRRunner.init(arena_allocator, document_store, script, config);
    // Pipes are created from the process (freeing) allocator, not the arena, so
    // their teardown reclaims. Compile and the rest of the runtime stay on `arena`.
    runner.free_allocator = allocator;
    const result = try runner.compile();

    if (result == .err) return .err_(arena, result.err.diagnostics());
    defer arena.deinit();
    var context = result.success;
    defer context.deinit();

    try context.addMainThread(config.env);

    runner.log("IR Compilation Results", .{});

    runner.log("\nRead-only Data ({} bytes):", .{context.dataSize()});
    for (context.readonlyData(), 0..) |page, i| {
        runner.log("Page {}:", .{i});
        runner.log("{s}", .{page});
    }

    runner.log("\nStruct Types:", .{});
    for (context.structTypes()) |struct_type| {
        runner.log("struct {s}:", .{struct_type.name});

        var it = struct_type.fields.iterator();
        while (it.next()) |entry| {
            runner.log("  {s}: {f}", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
    }

    runner.log("\nInstructions:", .{});
    var label_counter: usize = 0;
    for (context.instructions(), 0..) |is, instr_set| {
        runner.log("\nSet {}:", .{instr_set});
        for (is, 0..) |instr, i| {
            try logInstruction(&runner, &context, instr, instr_set, i, &label_counter);
        }
    }

    if (config.dry_run) return .{ .success = .success };

    runner.log("\nRunning...\n", .{});
    const exit_code = runner.run(&context) catch |err| {
        // Every runtime error is rendered as a proper `[error]:` diagnostic,
        // carrying the faulting instruction's source span (and any detail, e.g.
        // the missing command name) captured by `run`.
        return runtimeErrorResult(allocator, err, runner.runtime_error_span, runner.runtime_error_detail);
    };
    return .{ .success = exit_code };
}

/// Builds the single-diagnostic `RunResult` for a runtime error surfaced from a
/// runner or the debugger, so both render identically through `processResult`.
/// The diagnostic owns its own arena (the run arena is torn down on return).
fn runtimeErrorResult(
    allocator: Allocator,
    err: anyerror,
    span: ?ast.Span,
    detail: ?[]const u8,
) Allocator.Error!RunResult {
    var diag_arena = std.heap.ArenaAllocator.init(allocator);
    errdefer diag_arena.deinit();
    const da = diag_arena.allocator();
    const diags = try da.alloc(ir.compiler.Diagnostic, 1);
    diags[0] = .{
        .err = ir.compiler.Error.RuntimeError,
        ._span = span,
        .message = try runtimeErrorMessage(da, err, detail),
        ._severity = .@"error",
    };
    return RunResult.err_(diag_arena, diags);
}

/// The source span of the instruction an evaluator faulted on, or null when the
/// current thread/instruction has no source location. Used to anchor a runtime
/// error's diagnostic (the debugger path, which has no `IRRunner` to stash it).
fn evaluatorFaultingSpan(evaluator: *ir.evaluator.IREvaluator) ?ast.Span {
    const thread = evaluator.context.getCurrentThreadPtr() orelse return null;
    const instr = thread.currentInstruction() orelse return null;
    return if (instr.source) |source| source.span() else null;
}

/// A human-readable message for a runtime error surfaced as a diagnostic. Errors
/// whose site stashed a fully-formed `detail` (the missing command, the C library
/// that failed to load, …) use it verbatim; other user-facing errors get a fixed
/// explanation; anything else falls back to the error name so an internal fault
/// is still legible. `detail` is consulted only for the errors known to set it,
/// so a stale value can never leak into an unrelated error's message.
fn runtimeErrorMessage(allocator: Allocator, err: anyerror, detail: ?[]const u8) ![]const u8 {
    return switch (err) {
        error.CommandNotFound,
        error.CImportLoadFailed,
        error.CImportSymbolNotFound,
        => if (detail) |d|
            try allocator.dupe(u8, d)
        else
            try std.fmt.allocPrint(allocator, "runtime error: {t}", .{err}),
        error.CImportUnsupportedType => try allocator.dupe(u8, "unsupported type in a C FFI signature"),
        error.UnsupportedForward => try allocator.dupe(u8, "cannot consume a stream before it has been written to"),
        error.OutOfMemory => try allocator.dupe(u8, "out of memory"),
        else => try std.fmt.allocPrint(allocator, "runtime error: {t}", .{err}),
    };
}

pub fn debugIR(
    io: std.Io,
    allocator: Allocator,
    env_map: *std.process.Environ.Map,
    script: *ast.Script,
    document_store: *DocumentStore,
    stdin: *ReaderWriterStream,
    stdout: *ReaderWriterStream,
    stderr: *ReaderWriterStream,
    stdout_is_tty: bool,
    stderr_is_tty: bool,
    tracer: *Tracer,
    // stdin: CloseableReader(ExitCode),
    // stdout: CloseableWriter(ExitCode),
    // stderr: CloseableWriter(ExitCode),
) !RunResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_allocator = arena.allocator();
    var compiler = try ir.compiler.IRCompiler.init(io, arena_allocator, document_store, script, &.{}, env_map);
    const result = try compiler.compile();

    tracer.config.echo_to_stdout = true;

    if (result == .err) return .err_(arena, result.err.diagnostics());
    defer arena.deinit();
    const shared = result.success;

    var context = ir.context.IRProgramContext.init(io, arena_allocator, shared);
    defer context.deinit();
    try context.addMainThread(env_map);
    var debugger = try ir.debugger.IRDebugger.init(
        arena_allocator,
        .{
            .io = io,
            .verbose = false,
            .stdin = stdin,
            .stdout = stdout,
            .stderr = stderr,
            .stdout_is_tty = stdout_is_tty,
            .stderr_is_tty = stderr_is_tty,
            .tracer = tracer,
        },
        document_store,
        &context,
    );
    const exit_code = debugger.run() catch |err| {
        // Render a runtime error through the same diagnostic path as the normal
        // runner — read the faulting span and detail from the debugger's
        // evaluator (there is no `IRRunner` here to stash them).
        return runtimeErrorResult(
            allocator,
            err,
            evaluatorFaultingSpan(&debugger.evaluator),
            debugger.evaluator.error_detail,
        );
    };
    return .{ .success = exit_code };
}

fn logInstruction(
    runner: *IRRunner,
    context: *ir.context.IRProgramContext,
    instr: ir.Instruction,
    instr_set: usize,
    i: usize,
    label_counter: *usize,
) !void {
    var message_buffer: [1024]u8 = undefined;
    var writer = std.Io.Writer.fixed(&message_buffer);
    try writer.print("{x}: ", .{i});

    while (label_counter.* < context.labels().map.count()) {
        const key = context.labels().map.keys()[label_counter.*];
        const addr = context.labels().map.values()[label_counter.*];
        const label = ir.Label{ .key = key, .addr = addr.?.local_addr };
        if (addr.?.instr_set == instr_set and addr.?.local_addr == i) {
            try writer.print("{f} ", .{label});
            label_counter.* += 1;
        } else {
            break;
        }
    }

    try writer.print("{f}", .{instr});
    runner.log("{s}", .{writer.buffered()});
}
