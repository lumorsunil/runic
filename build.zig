const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const runtime = b.addModule("runic", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const cli = b.addExecutable(.{
        .name = "runic",
        .root_module = b.createModule(.{
            .root_source_file = b.path("cmd/runic/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    cli.root_module.addImport("runic", runtime);
    b.installArtifact(cli);

    const run_cmd = b.addRunArtifact(cli);
    if (b.args) |args| run_cmd.addArgs(args);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Build and run the Runic CLI");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run all Zig tests");

    const runtime_tests = b.addTest(.{ .root_module = runtime });
    const runtime_runner = b.addRunArtifact(runtime_tests);
    test_step.dependOn(&runtime_runner.step);

    const cli_tests = b.addTest(.{ .root_module = cli.root_module });
    const cli_runner = b.addRunArtifact(cli_tests);
    test_step.dependOn(&cli_runner.step);
}
