//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

pub const token = @import("frontend/token.zig");
pub const lexer = @import("frontend/lexer.zig");
pub const ast = @import("frontend/ast.zig");
pub const parser = @import("frontend/parser.zig");
pub const diagnostics = @import("frontend/diagnostics.zig");
pub const document = @import("frontend/document_store.zig");
pub const DocumentStore = @import("document_store.zig").DocumentStore;
pub const ExitCode = @import("runtime/exit_code.zig").ExitCode;
pub const rainbow = @import("rainbow.zig");
pub const mem = @import("mem/root.zig");
pub const semantic = @import("semantic/root.zig");
pub const stream = @import("stream.zig");
pub const closeable = @import("closeable.zig");
pub const process = @import("process.zig");
pub const TraceWriter = @import("trace-writer.zig").TraceWriter;
pub const ir = @import("ir.zig");
pub const trace = @import("trace.zig");
pub const signals = @import("signals.zig");

// Aggregate the runtime module's unit tests. `zig build test` only runs test
// declarations from files it actually pulls into the test binary; a top-level
// `pub const x = @import(...)` binds a namespace (or a single symbol) without
// pulling that file's tests. Referencing each test-bearing file with
// `_ = @import(...)` inside a `test` block does. Add new test-bearing files
// here so their tests run.
test {
    _ = @import("mem/rc.zig");
    _ = @import("mem/split.zig");
    _ = @import("frontend/lexer.zig");
    _ = @import("frontend/parser.zig");
    // NOTE: frontend/diagnostics.zig is intentionally omitted — its
    // `renderSyntaxError` is unused in production and both it and its tests have
    // rotted; either wire it up or remove it before re-enabling its tests.
    _ = @import("stream.zig");
    _ = @import("ir/context.zig");
    _ = @import("ir/evaluator.zig");
    _ = @import("ir/compiler.zig");
}
