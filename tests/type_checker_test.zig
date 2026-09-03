//! Unit tests for the semantic layer (the type checker).
//!
//! These parse in-memory source, type-check it, and assert on the outcome —
//! no CLI pipeline, no on-disk `.rn` fixtures — so a type-checking change has a
//! fast, targeted feedback loop. The file imports the `runic` module (rather
//! than the type checker's source directly) so only these tests run, not the
//! module's other (currently un-wired) in-file tests.

const std = @import("std");
const runic = @import("runic");

const TypeChecker = runic.semantic.TypeChecker;
const FrontendDocumentStore = runic.document.FrontendDocumentStore;

var test_threaded: std.Io.Threaded = undefined;
var test_threaded_ready = false;

fn testIo() std.Io {
    if (!test_threaded_ready) {
        test_threaded = .init(std.heap.page_allocator, .{});
        test_threaded_ready = true;
    }
    return test_threaded.io();
}

/// Parses and type-checks `source`, asserting it type-checks cleanly.
fn expectTypeChecks(source: []const u8) !void {
    const allocator = std.testing.allocator;
    var env = std.process.Environ.Map.init(allocator);
    defer env.deinit();
    var fds = FrontendDocumentStore.init(testIo(), allocator, &env);
    defer fds.deinit();

    const path = "<test>";
    const doc = try fds.putDocument(path, source);
    switch (doc.parser.parseScript(path)) {
        .success => {},
        .err => return error.TestSourceFailedToParse,
    }

    var tc = TypeChecker.init(testIo(), allocator, &fds.document_store, &env, false);
    defer tc.deinit();
    const result = try tc.typeCheck(path);
    if (result == .err) {
        for (result.err.diagnostics()) |d| {
            std.debug.print("unexpected type error: {s}\n", .{d.message});
        }
        return error.UnexpectedTypeError;
    }
}

/// Parses and type-checks `source`, asserting it reports a type error whose
/// message contains `needle` (pass "" to accept any error).
fn expectTypeError(source: []const u8, needle: []const u8) !void {
    const allocator = std.testing.allocator;
    var env = std.process.Environ.Map.init(allocator);
    defer env.deinit();
    var fds = FrontendDocumentStore.init(testIo(), allocator, &env);
    defer fds.deinit();

    const path = "<test>";
    const doc = try fds.putDocument(path, source);
    switch (doc.parser.parseScript(path)) {
        .success => {},
        .err => return error.TestSourceFailedToParse,
    }

    var tc = TypeChecker.init(testIo(), allocator, &fds.document_store, &env, false);
    defer tc.deinit();
    const result = try tc.typeCheck(path);
    if (result != .err) return error.ExpectedTypeError;

    for (result.err.diagnostics()) |d| {
        if (std.mem.indexOf(u8, d.message, needle) != null) return;
    }
    std.debug.print("no diagnostic matched \"{s}\"; got:\n", .{needle});
    for (result.err.diagnostics()) |d| std.debug.print("  - {s}\n", .{d.message});
    return error.DiagnosticNotFound;
}

test "annotated binding with a matching type" {
    try expectTypeChecks("const x: Int = 5");
}

test "annotated binding with a mismatched type is rejected" {
    try expectTypeError("var retries: Int = \"nope\"", "");
}

test "arithmetic on a bare sum is rejected" {
    try expectTypeError(
        \\const x: Int || String = 5
        \\echo "${x + 1}"
    , "sum");
}

test "a sum narrowed with is can be used as the member" {
    try expectTypeChecks(
        \\const x: Int || String = 5
        \\if (x is Int) echo "${x + 1}"
    );
}

test "orelse on an optional yields the payload type" {
    try expectTypeChecks(
        \\const x: ?Int = null
        \\const y: Int = x orelse 0
        \\echo "${y}"
    );
}

test "parseInt is a registered builtin pipeline stage" {
    try expectTypeChecks("const n = echo \"5\" | parseInt catch 0");
}

test "parseBool is a registered builtin pipeline stage" {
    try expectTypeChecks("const b = echo \"true\" | parseBool catch false");
}

test "a top-level try has no enclosing function to propagate to" {
    try expectTypeError(
        \\const E = error { Bad }
        \\fn String mayFail() E!String { yield E.Bad }
        \\const v = try mayFail
    , "propagate");
}

test "assigning to a field of an immutable binding is rejected" {
    try expectTypeError(
        \\const Point = struct { x: Int, y: Int }
        \\const p = Point{ .x = 1, .y = 2 }
        \\p.x = 9
    , "");
}

test "reset reclaims analysis memory across repeated re-checks" {
    const allocator = std.testing.allocator;
    var env = std.process.Environ.Map.init(allocator);
    defer env.deinit();
    var fds = FrontendDocumentStore.init(testIo(), allocator, &env);
    defer fds.deinit();

    const path = "<test>";
    const doc = try fds.putDocument(path,
        \\const a = 1
        \\const b = a + 2
        \\fn Int sq(n: Int) Int { yield n * n }
        \\echo "${sq b}"
    );
    switch (doc.parser.parseScript(path)) {
        .success => {},
        .err => return error.TestSourceFailedToParse,
    }

    var tc = TypeChecker.init(testIo(), allocator, &fds.document_store, &env, false);
    defer tc.deinit();

    // Simulate an editing session: many re-checks with a reset between each,
    // as the LSP now does. The arena must be reclaimed each round (bounded
    // memory) rather than growing with the number of edits.
    var peak: usize = 0;
    for (0..25) |_| {
        const result = try tc.typeCheck(path);
        try std.testing.expect(result == .success);
        const grown = tc.arena.queryCapacity();
        peak = @max(peak, grown);

        tc.reset();
        // After reset the arena is fully reclaimed and the module cache empty.
        try std.testing.expectEqual(@as(usize, 0), tc.arena.queryCapacity());
        try std.testing.expectEqual(@as(usize, 0), tc.modules.count());
    }

    // One re-check's footprint stays modest; the loop above would have grown
    // ~25x without the reset. Guard against runaway growth per check.
    try std.testing.expect(peak < 1024 * 1024);
}
