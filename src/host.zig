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
    /// Bytes written to the call's stderr (e.g. an error message for a non-zero
    /// `exit_code`). Flows to the script's stderr like any command's.
    stderr: []const u8 = "",
    /// The call's exit code (0 = success); surfaces through the normal command
    /// exit-code model, so a non-zero code makes the call a catchable failure
    /// (`hostfn || …`, `hostfn catch |e| …`).
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

// --- Typed host functions (Phase 5) --------------------------------------
//
// Unlike the string/command host functions above, a typed host function is
// declared with a Runic signature and called as a function (`add 2 3`), passing
// and returning Runic *values* rather than strings. Phase 5a supports scalars.

/// A scalar type in a typed host-function signature.
pub const Ty = enum { int, float, bool, string };

/// A scalar value passed to / returned from a typed host function.
pub const Value = union(enum) {
    int: i64,
    float: f64,
    boolean: bool,
    string: []const u8,
};

/// One invocation of a typed host function.
pub const TypedCall = struct {
    /// Arguments, in declaration order, already marshalled to host values.
    args: []const Value,
    ctx: ?*anyopaque,
    /// Allocator for a `string` result; consumed synchronously by the runtime.
    arena: std.mem.Allocator,
};

pub const TypedResponse = struct {
    value: Value,
};

pub const TypedCallback = *const fn (TypedCall) TypedResponse;

/// A typed host function: a Runic signature `(params…) returns` bound to a Zig
/// callback over host values.
pub const TypedHostFn = struct {
    name: []const u8,
    params: []const Ty,
    returns: Ty,
    callback: TypedCallback,
    ctx: ?*anyopaque = null,
};

/// The registry index of the typed function named `name`, or null.
pub fn lookupTyped(functions: []const TypedHostFn, name: []const u8) ?usize {
    for (functions, 0..) |f, i| {
        if (std.mem.eql(u8, f.name, name)) return i;
    }
    return null;
}

/// Capability sandbox for an embedded run (roadmap Theme 7, Phase 3). Each flag
/// gates a side-effecting surface; a denied operation stops the run with a clear
/// diagnostic. The CLI and normal `runIR` use `.permissive` (everything on);
/// embedding defaults to `.strict` (an untrusted script computes and calls host
/// functions, but can't spawn processes, touch the filesystem, read/write the
/// environment, or load C libraries unless the host opts in). Registered host
/// functions are always callable — they are the sanctioned effect surface.
pub const Sandbox = struct {
    /// Spawn external processes (run a command that isn't a host function).
    allow_exec: bool,
    /// Filesystem access: file redirects (`> "f"`) and `cd`.
    allow_fs: bool,
    /// Read or write the environment (`$VAR`, `env.set`).
    allow_env: bool,
    /// Load a C dynamic library via `cimport`.
    allow_ffi: bool,

    /// Everything allowed — the CLI / normal-run default.
    pub const permissive: Sandbox = .{ .allow_exec = true, .allow_fs = true, .allow_env = true, .allow_ffi = true };
    /// Everything denied — the embedding default.
    pub const strict: Sandbox = .{ .allow_exec = false, .allow_fs = false, .allow_env = false, .allow_ffi = false };
};
