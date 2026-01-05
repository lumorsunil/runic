const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const ir = runic.ir;
const ast = runic.ast;
const ExitCode = runic.command_runner.ExitCode;
const DocumentStore = runic.DocumentStore;
const CloseableReader = runic.closeable.CloseableReader;
const CloseableWriter = runic.closeable.CloseableWriter;

pub const IRConfig = struct {
    verbose: bool,
    stdin: CloseableReader(ExitCode),
    stdout: CloseableWriter(ExitCode),
    stderr: CloseableWriter(ExitCode),
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

    pub fn compile(self: *IRRunner) !ir.context.IRProgramContext {
        var compiler = try ir.compiler.IRCompiler.init(
            self.allocator,
            self.document_store,
            self.script,
        );
        const shared = try compiler.compile();

        return .init(self.allocator, shared);
    }

    pub fn run(self: *IRRunner, context: *ir.context.IRProgramContext) !ExitCode {
        var evaluator = ir.evaluator.IREvaluator.init(
            self.allocator,
            .{
                .verbose = self.config.verbose,
                .stdin = self.config.stdin,
                .stdout = self.config.stdout,
                .stderr = self.config.stderr,
            },
            context,
        );

        while (evaluator.step() catch |err| {
            const current_instr = evaluator.context.getCurrentThread().currentInstruction();

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
                    const addr = evaluator.context.getCurrentThread().getCurrentInstructionAddr();
                    std.log.err("Error evaluating instruction {f}: {}", .{ addr, err });
                }
            }

            return err;
        }) |result| switch (result) {
            .cont, .cont_no_instr_counter_inc => continue,
            .exit => |exit_code| return exit_code,
        };

        return .success;
    }
};

pub fn runIR(
    allocator: Allocator,
    document_store: *DocumentStore,
    script: *ast.Script,
    config: IRConfig,
) !ExitCode {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    var runner = IRRunner.init(arena_allocator, document_store, script, config);
    var context = try runner.compile();
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
            try logInstruction(&runner, &context, instr, i, &label_counter);
        }
    }

    runner.log("\nRunning...\n", .{});

    return runner.run(&context);
}

pub fn debugIR(
    allocator: Allocator,
    script: *ast.Script,
    document_store: *DocumentStore,
    stdin: CloseableReader(ExitCode),
    stdout: CloseableWriter(ExitCode),
    stderr: CloseableWriter(ExitCode),
) !ExitCode {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    var compiler = try ir.compiler.IRCompiler.init(arena_allocator, document_store, script);
    const shared = try compiler.compile();
    var context = ir.context.IRProgramContext.init(arena_allocator, shared);
    try context.addMainThread();
    var debugger = try ir.debugger.IRDebugger.init(
        arena_allocator,
        .{ .verbose = false, .stdin = stdin, .stdout = stdout, .stderr = stderr },
        document_store,
        &context,
    );
    return debugger.run();
}

fn logInstruction(
    runner: *IRRunner,
    context: *ir.context.IRProgramContext,
    instr: ir.Instruction,
    i: usize,
    label_counter: *usize,
) !void {
    var message_buffer: [1024]u8 = undefined;
    var writer = std.Io.Writer.fixed(&message_buffer);
    try writer.print("{}: ", .{i});

    while (label_counter.* < context.labels().map.count()) {
        const key = context.labels().map.keys()[label_counter.*];
        const addr = context.labels().map.values()[label_counter.*];
        const label = ir.Label{ .key = key, .addr = addr.? };
        if (addr.? == i) {
            try writer.print("{f} ", .{label});
            label_counter.* += 1;
        } else {
            break;
        }
    }

    try writer.print("{f}", .{instr});
    runner.log("{s}", .{writer.buffered()});
}
