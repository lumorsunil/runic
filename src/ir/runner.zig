const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const ir = runic.ir;
const ast = runic.ast;
const ExitCode = runic.command_runner.ExitCode;

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

    pub fn compile(self: *IRRunner) !ir.IR.IRContext {
        var compiler = try ir.compiler.IRCompiler.init(self.allocator, self.script);
        return compiler.compile();
    }

    pub fn run(self: *IRRunner, context: *ir.IR.IRContext) !ExitCode {
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

    runner.log("IR Compilation Results", .{});

    runner.log("\nRead-only Data ({} bytes):", .{context.read_only.dataSize()});
    for (context.read_only.data, 0..) |page, i| {
        runner.log("Page {}:", .{i});
        runner.log("{s}", .{page});
    }

    runner.log("\nStruct Types:", .{});
    for (context.struct_types) |struct_type| {
        runner.log("struct {s}:", .{struct_type.name});

        var it = struct_type.fields.iterator();
        while (it.next()) |entry| {
            runner.log("  {s}: {f}", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
    }

    runner.log("\nInstructions:", .{});
    var label_counter: usize = 0;
    for (context.read_only.instructions, 0..) |instr, i| {
        try logInstruction(&runner, &context, instr, i, &label_counter);
    }

    runner.log("\nRunning...\n", .{});

    return runner.run(&context);
}

fn logInstruction(
    runner: *IRRunner,
    context: *ir.IR.IRContext,
    instr: ir.IR.Instruction,
    i: usize,
    label_counter: *usize,
) !void {
    var message_buffer: [1024]u8 = undefined;
    var writer = std.Io.Writer.fixed(&message_buffer);
    try writer.print("{}: ", .{i});

    while (label_counter.* < context.labels.map.count()) {
        const key = context.labels.map.keys()[label_counter.*];
        const addr = context.labels.map.values()[label_counter.*];
        const label = ir.IR.Label{ .key = key, .addr = addr.? };
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
