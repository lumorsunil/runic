const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");
const ir = runic.ir;
const ast = runic.ast;
const ExitCode = runic.command_runner.ExitCode;

pub const IRRunner = struct {
    allocator: Allocator,
    script: *ast.Script,

    pub fn init(allocator: Allocator, script: *ast.Script) @This() {
        return .{
            .allocator = allocator,
            .script = script,
        };
    }

    pub fn compile(self: *IRRunner) !ir.IR.IRContext {
        var compiler = ir.compiler.IRCompiler.init(self.allocator, self.script);
        return compiler.compile();
    }

    pub fn run(self: *IRRunner, context: *ir.IR.IRContext) !ExitCode {
        var evaluator = ir.evaluator.IREvaluator.init(self.allocator, context);

        while (try evaluator.step()) |result| switch (result) {
            .cont => continue,
            .exit => |exit_code| return exit_code,
        };

        return .success;
    }
};

pub fn runIR(allocator: Allocator, script: *ast.Script) !ExitCode {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    var runner = IRRunner.init(arena_allocator, script);
    var context = try runner.compile();

    std.log.debug("IR Compilation Results", .{});

    std.log.debug("\nRead-only Data:", .{});
    for (context.read_only.data, 0..) |page, i| {
        std.log.debug("Page {}:", .{i});
        std.log.debug("{s}", .{page});
    }

    std.log.debug("\nInstructions:", .{});
    for (context.read_only.instructions, 0..) |instr, i| {
        std.log.debug("{}: {f}", .{ i, instr });
    }

    std.log.debug("Running...", .{});

    return runner.run(&context);
}
