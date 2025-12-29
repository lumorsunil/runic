const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const ir = runic.ir;
const ast = runic.ast;
const ExitCode = runic.command_runner.ExitCode;
const DocumentStore = runic.DocumentStore;

pub const IRConfig = struct {
    verbose: bool,
};

pub const IRRunner = struct {
    allocator: Allocator,
    script: *ast.Script,
    config: IRConfig,

    pub fn init(
        allocator: Allocator,
        config: IRConfig,
        script: *ast.Script,
    ) @This() {
        return .{
            .allocator = allocator,
            .config = config,
            .script = script,
        };
    }

    fn log(self: IRRunner, comptime fmt: []const u8, args: anytype) void {
        if (!self.config.verbose) return;
        std.log.debug(fmt, args);
    }

    pub fn compile(self: *IRRunner) !ir.context.IRProgramContext {
        var compiler = try ir.compiler.IRCompiler.init(self.allocator, self.script);
        const shared = try compiler.compile();

        return .init(self.allocator, shared);
    }

    pub fn run(self: *IRRunner, context: *ir.context.IRProgramContext) !ExitCode {
        var evaluator = ir.evaluator.IREvaluator.init(
            self.allocator,
            .{ .verbose = self.config.verbose },
            context,
        );

        while (try evaluator.step()) |result| switch (result) {
            .cont => continue,
            .exit => |exit_code| return exit_code,
        };

        return .success;
    }
};

pub fn runIR(allocator: Allocator, config: IRConfig, script: *ast.Script) !ExitCode {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    var runner = IRRunner.init(arena_allocator, config, script);
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
    for (context.instructions(), 0..) |instr, i| {
        try logInstruction(&runner, &context, instr, i, &label_counter);
    }

    runner.log("\nRunning...\n", .{});

    return runner.run(&context);
}

pub fn debugIR(
    allocator: Allocator,
    script: *ast.Script,
    document_store: *DocumentStore,
) !ExitCode {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    var compiler = try ir.compiler.IRCompiler.init(arena_allocator, script);
    const shared = try compiler.compile();
    var context = ir.context.IRProgramContext.init(arena_allocator, shared);
    try context.addMainThread();
    var debugger = try ir.debugger.IRDebugger.init(
        arena_allocator,
        .{ .verbose = false },
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
