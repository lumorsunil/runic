//! The bundled Runic standard library, embedded into the binary at build time.
//!
//! Each module's source is compiled in via `@embedFile` and registered under a
//! virtual document path (`:std`, `:std/list`, …). The document store loads
//! these paths from this table instead of the filesystem, so the standard
//! library ships inside the single `runic` binary — no install-path lookup.
//!
//! Resolution: `import "std"` → `:std`; `import "std/<name>"` → `:std/<name>`
//! (see `resolveModulePath` in `document_store.zig`). The `:std` root module
//! re-exports each submodule as a public member (`std.list`, …).

const std = @import("std");

pub const Module = struct {
    /// Virtual document path, e.g. ":std" or ":std/list".
    path: []const u8,
    source: []const u8,
};

pub const modules = [_]Module{
    .{ .path = ":std", .source = @embedFile("std/std.rn") },
    .{ .path = ":std/list", .source = @embedFile("std/list.rn") },
    .{ .path = ":std/str", .source = @embedFile("std/str.rn") },
};

/// The embedded source for a virtual std path, or null if none matches.
pub fn source(path: []const u8) ?[]const u8 {
    inline for (modules) |module| {
        if (std.mem.eql(u8, module.path, path)) return module.source;
    }
    return null;
}

/// Whether `path` is a bundled-std virtual path (starts with ":std").
pub fn isStdPath(path: []const u8) bool {
    return std.mem.startsWith(u8, path, ":std");
}
