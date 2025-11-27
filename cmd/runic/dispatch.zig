const std = @import("std");
const Allocator = std.mem.Allocator;
const utils = @import("main-utils.zig");
const runScript = @import("run_script.zig").runScript;
const repl = @import("repl.zig");

pub fn dispatch(
    allocator: Allocator,
    config: utils.CliConfig,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !u8 {
    switch (config.mode) {
        .script => |script| return try runScript(allocator, script, config, stdout, stderr),
        .repl => {
            try repl.run(allocator, .{
                .prompt = "runic> ",
                .continuation_prompt = "...> ",
                .history_limit = 256,
                .trace_topics = config.trace_topics,
                .module_paths = config.module_paths,
            });
            return 0;
        },
    }
}
