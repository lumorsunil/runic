//! Host-function interface for embedding (roadmap Theme 7, `future/embedding.md`).
//!
//! A leaf module so both the embedding facade (`embed.zig`) and the evaluator
//! can share these types without an import cycle (embed → ir → evaluator, so the
//! evaluator must not import embed).
//!
//! Phase 2 model: a host function is a registered name the script calls like a
//! command (`greet "world"`). When the evaluator is about to spawn a command
//! whose name is registered, it calls the host callback instead — passing the
//! string arguments and writing the callback's returned string to the command's
//! stdout. This reuses the whole external-command pipeline (type-checked as an
//! executable, captured via binding, etc.); only the spawn is intercepted.

const std = @import("std");

/// What a host callback receives for one invocation.
pub const Call = struct {
    /// The call's arguments, already rendered to strings (the command model).
    args: []const []const u8,
    /// The opaque context the host registered alongside the function (its own
    /// state); null when none was given.
    ctx: ?*anyopaque,
    /// An allocator the callback may use for its `output` bytes; the runtime
    /// consumes `output` synchronously (writes it to stdout) during the call, so
    /// the callback need not keep it alive afterwards.
    arena: std.mem.Allocator,
};

/// What a host callback returns.
pub const Response = struct {
    /// Bytes written to the call's stdout (captured by a binding, piped onward,
    /// or printed). Empty for a function used only for its effect.
    output: []const u8 = "",
    /// The call's exit code (0 = success); surfaces through the normal
    /// command exit-code model.
    exit_code: u8 = 0,
};

pub const Callback = *const fn (Call) Response;

/// One registered host function.
pub const HostFn = struct {
    name: []const u8,
    callback: Callback,
    ctx: ?*anyopaque = null,
};

/// Finds the registered function named `name`, or null.
pub fn lookup(functions: []const HostFn, name: []const u8) ?*const HostFn {
    for (functions) |*f| {
        if (std.mem.eql(u8, f.name, name)) return f;
    }
    return null;
}
