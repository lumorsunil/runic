//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

pub const token = @import("frontend/token.zig");
pub const lexer = @import("frontend/lexer.zig");
pub const ast = @import("frontend/ast.zig");
pub const diagnostics = @import("frontend/diagnostics.zig");
pub const types = @import("semantic/types.zig");
pub const symbols = @import("semantic/symbols.zig");
pub const type_checker = @import("semantic/type_checker.zig");
pub const command_runner = @import("runtime/command_runner.zig");
pub const bash_executor = @import("runtime/bash_executor.zig");
pub const scheduler = @import("runtime/scheduler.zig");
pub const match_executor = @import("runtime/match_executor.zig");
pub const module_loader = @import("runtime/module_loader.zig");
pub const stack_trace = @import("runtime/stack_trace.zig");
pub const tracing = @import("runtime/tracing.zig");

comptime {
    _ = @import("frontend/parser_tests.zig");
}
