const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const ir = runic.ir;
const ast = runic.ast;
const ExitCode = runic.command_runner.ExitCode;
const DocumentStore = runic.DocumentStore;
const CloseableReader = runic.closeable.CloseableReader;
const CloseableWriter = runic.closeable.CloseableWriter;
const ReaderWriterStream = runic.stream.ReaderWriterStream;
const rainbow = @import("../rainbow.zig");
const Tracer = runic.trace.Tracer;

const span_color = rainbow.beginBgColor(.red) ++ rainbow.beginColor(.black);
const end_color = rainbow.endColor();

pub const IRConfig = struct {
    verbose: bool,
    dry_run: bool,
    script_args: []const []const u8 = &.{},
    stdin: *ReaderWriterStream,
    stdout: *ReaderWriterStream,
    stderr: *ReaderWriterStream,
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
    document_store: *DocumentStore,
    script: *ast.Script,
    config: IRConfig,

    pub fn init(
        allocator: Allocator,
        document_store: *DocumentStore,
        script: *ast.Script,
        config: IRConfig,
    ) @This() {
        return .{
            .allocator = allocator,
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
            self.allocator,
            self.document_store,
            self.script,
            self.config.script_args,
        );
        const result = try compiler.compile();

        return switch (result) {
            .err => |err| .err_(err.diagnostics()),
            .success => |shared| .{ .success = .init(self.allocator, shared) },
        };
    }

    pub fn run(self: *IRRunner, context: *ir.context.IRProgramContext) !ExitCode {
        var evaluator = ir.evaluator.IREvaluator.init(
            self.allocator,
            .{
                .verbose = self.config.verbose,
                .stdin = self.config.stdin,
                .stdout = self.config.stdout,
                .stderr = self.config.stderr,
                .tracer = self.config.tracer,
            },
            context,
        );

        while (evaluator.step() catch |err| {
            const current_instr = (evaluator.context.getCurrentThread() orelse unreachable).currentInstruction();

            if (current_instr) |ci| {
                if (ci.source) |source| {
                    const span = source.span();
                    const file_name = span.start.file;
                    const line = span.start.line;
                    const column = span.start.column;
                    std.log.err("Error evaluating {s}:{}:{}: {}", .{
                        file_name,
                        line,
                        column,
                        err,
                    });
                } else {
                    const addr = (evaluator.context.getCurrentThread() orelse unreachable).getCurrentInstructionAddr();
                    std.log.err("Error evaluating instruction {f}: {}", .{ addr, err });
                }
            }

            return err;
        }) |result| switch (result) {
            .cont, .cont_no_instr_counter_inc, .skip => continue,
            .exit => |exit_code| return exit_code,
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
    const result = try runner.compile();

    if (result == .err) return .err_(arena, result.err.diagnostics());
    defer arena.deinit();
    var context = result.success;
    defer context.deinit();

    try context.addMainThread();

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
    return .{ .success = try runner.run(&context) };
}

pub fn debugIR(
    allocator: Allocator,
    script: *ast.Script,
    document_store: *DocumentStore,
    stdin: *ReaderWriterStream,
    stdout: *ReaderWriterStream,
    stderr: *ReaderWriterStream,
    tracer: *Tracer,
    // stdin: CloseableReader(ExitCode),
    // stdout: CloseableWriter(ExitCode),
    // stderr: CloseableWriter(ExitCode),
) !RunResult {
    var arena = std.heap.ArenaAllocator.init(allocator);
    const arena_allocator = arena.allocator();
    var compiler = try ir.compiler.IRCompiler.init(arena_allocator, document_store, script, &.{});
    const result = try compiler.compile();

    tracer.config.echo_to_stdout = true;

    if (result == .err) return .err_(arena, result.err.diagnostics());
    defer arena.deinit();
    const shared = result.success;

    var context = ir.context.IRProgramContext.init(arena_allocator, shared);
    defer context.deinit();
    try context.addMainThread();
    var debugger = try ir.debugger.IRDebugger.init(
        arena_allocator,
        .{
            .verbose = false,
            .stdin = stdin,
            .stdout = stdout,
            .stderr = stderr,
            .tracer = tracer,
        },
        document_store,
        &context,
    );
    return .{ .success = try debugger.run() };
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
