const std = @import("std");

pub const server = @import("server.zig");
pub const workspace = @import("workspace.zig");
pub const symbols = @import("symbols.zig");
pub const completion = @import("completion.zig");

// Aggregate the lsp module's in-file unit tests. `zig build test` only runs
// test declarations from files it pulls into the test binary; referencing each
// test-bearing file with `_ = @import(...)` in a `test` block does that. Add
// new test-bearing files here so their tests run.
test {
    _ = @import("completion.zig");
    _ = @import("types.zig");
}
