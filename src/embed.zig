//! Embedding facade — Phase 1 of the "embed Runic like Lua" direction
//! (`future/embedding.md`, roadmap Theme 7).
//!
//! This is the library entry point for a host program that wants to run a Runic
//! script from a source string and get a structured result back — captured
//! stdout/stderr, an exit code, and a list of diagnostics — instead of the CLI's
//! terminal rendering. It repackages the orchestration in
//! `cmd/runic/run_script.zig` (parse → resolve imports → type-check → run)
//! without the CLI-only concerns (print modes, env overrides, TTY detection).
//!
//! Later phases add host-function registration, a `host` module, a capability
//! sandbox, and richer value marshalling (see the design note).

const std = @import("std");
const Allocator = std.mem.Allocator;

const document = @import("frontend/document_store.zig");
const FrontendDocumentStore = document.FrontendDocumentStore;
const DocumentStore = @import("document_store.zig").DocumentStore;
const ast = @import("frontend/ast.zig");
const semantic = @import("semantic/root.zig");
const TypeChecker = semantic.TypeChecker;
const Stream = @import("stream.zig").Stream;
const closeable = @import("closeable.zig");
const ExitCode = @import("runtime/exit_code.zig").ExitCode;
const TraceWriter = @import("trace-writer.zig").TraceWriter;
const Tracer = @import("trace.zig").Tracer;
const ir = @import("ir.zig");
const host = @import("host.zig");

/// Host-function types, re-exported so a host program references them as
/// `runic.embed.HostFn` / `.Call` / `.Response` without reaching into `host.zig`.
pub const HostFn = host.HostFn;
pub const Call = host.Call;
pub const Response = host.Response;
pub const Sandbox = host.Sandbox;
// Typed host functions (Phase 5).
pub const TypedHostFn = host.TypedHostFn;
pub const TypedCall = host.TypedCall;
pub const TypedResponse = host.TypedResponse;
pub const Ty = host.Ty;
pub const Value = host.Value;

/// Severity of an embedding diagnostic, mirroring the frontend's.
pub const Severity = enum { @"error", warning, information, hint };

/// A compile- or run-time diagnostic, flattened for a host. All slices are owned
/// by the `Outcome`'s arena and live until `Outcome.deinit`.
pub const Diagnostic = struct {
    severity: Severity,
    message: []const u8,
    /// Source file the diagnostic points at, or null when it has no location.
    file: ?[]const u8,
    line: usize,
    column: usize,
};

/// The result of running a script. Owns every slice it hands back; call
/// `deinit` when done.
pub const Outcome = struct {
    arena: std.heap.ArenaAllocator,
    /// The script's exit code, or null when compilation failed (parse / import /
    /// type-check error) so the script never ran.
    exit_code: ?ExitCode,
    /// Everything the script wrote to stdout / stderr.
    stdout: []const u8,
    stderr: []const u8,
    /// Compile and runtime diagnostics (empty on a clean run).
    diagnostics: []const Diagnostic,

    /// True when the script compiled and ran without any error diagnostic.
    pub fn ok(self: Outcome) bool {
        if (self.exit_code == null) return false;
        for (self.diagnostics) |d| if (d.severity == .@"error") return false;
        return true;
    }

    pub fn deinit(self: *Outcome) void {
        self.arena.deinit();
    }
};

pub const Options = struct {
    /// Logical path for the script; used in diagnostics and import resolution.
    path: []const u8 = "<embedded>",
    /// `script.args` visible to the script (e.g. `$1`).
    args: []const []const u8 = &.{},
    /// Bytes fed to the script's stdin.
    stdin: []const u8 = "",
    verbose: bool = false,
    /// Host functions the script can call like commands (`greet "world"`). A
    /// call to one of these names is dispatched to its callback instead of
    /// spawning a process. The slice (and each `name`/`ctx`) must outlive `run`.
    host_functions: []const HostFn = &.{},
    /// Typed host functions the script calls as functions (`add 2 3`), passing
    /// and returning marshalled Runic values rather than strings (Phase 5).
    typed_host_functions: []const TypedHostFn = &.{},
    /// Capability sandbox. Defaults to `.strict` (deny-by-default): the script
    /// can compute and call host functions but can't spawn processes, touch the
    /// filesystem, read/write the environment, or load C libraries. Loosen it
    /// per capability (e.g. `.{ .allow_exec = true, ... }`) to grant access.
    sandbox: Sandbox = Sandbox.strict,
};

pub const Error = Allocator.Error || error{EmbedRunFailed};

/// Runs `source` as a Runic script and returns a structured `Outcome`. A
/// compile error yields an `Outcome` with `exit_code == null` and the
/// diagnostics; a successful compile runs the script and reports its exit code
/// plus any runtime diagnostic. `Error` is reserved for host-side failures
/// (allocation, or an unexpected internal error), not script errors.
pub fn run(gpa: Allocator, io: std.Io, source: []const u8, options: Options) Error!Outcome {
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    errdefer arena_state.deinit();
    const arena = arena_state.allocator();

    var diagnostics = std.ArrayList(Diagnostic).empty;

    // An embedded run gets its own empty environment for now; a later phase can
    // let the host seed it.
    var env = std.process.Environ.Map.init(gpa);
    defer env.deinit();

    var doc_store = FrontendDocumentStore.init(io, gpa, &env);
    defer doc_store.deinit();

    const entry = try doc_store.putDocument(options.path, source);
    const resolved_path = entry.path;

    // Parse the entry document.
    switch (entry.parser.parseScript(resolved_path)) {
        .success => {},
        .err => |e| {
            try collectDiagnostics(arena, &diagnostics, e.diagnostics());
            return compileFailure(arena_state, &diagnostics);
        },
    }
    const entry_ast = entry.ast orelse return compileFailure(arena_state, &diagnostics);

    // Resolve (parse) every imported module into the store, recursively — the
    // type checker expects them already present.
    if (!try resolveImports(io, gpa, &doc_store.document_store, entry_ast, arena, &diagnostics)) {
        return compileFailure(arena_state, &diagnostics);
    }

    // Type-check.
    var type_checker = TypeChecker.init(io, gpa, &doc_store.document_store, &env, false);
    type_checker.typed_host_functions = options.typed_host_functions;
    defer type_checker.deinit();
    const tc_result = type_checker.typeCheck(resolved_path) catch return error.EmbedRunFailed;
    switch (tc_result) {
        .success => {},
        .err => |e| {
            try collectDiagnostics(arena, &diagnostics, e.diagnostics());
            return compileFailure(arena_state, &diagnostics);
        },
    }

    // Wire stdin (from the provided bytes) and capture stdout/stderr.
    var tracer = Tracer.init(io, gpa, .{ .echo_to_stdout = false });
    defer tracer.deinit();

    var stdin_reader = std.Io.Reader.fixed(options.stdin);
    var stdout_capture = std.Io.Writer.Allocating.init(gpa);
    defer stdout_capture.deinit();
    var stderr_capture = std.Io.Writer.Allocating.init(gpa);
    defer stderr_capture.deinit();

    const stdin_stream = Stream(u8).initReaderWriter(gpa, "<<<embed_stdin>>>", .{
        .close_source = false,
        .disconnect_source = true,
        .close_destination = true,
        .disconnect_destination = true,
        .keep_open = true,
        .propagate_eof_on_source_close = true,
        .preserve_source_on_destination_close = true,
    }, &tracer) catch return error.EmbedRunFailed;
    defer stdin_stream.stream.deinit();
    const stdout_stream = Stream(u8).initReaderWriter(gpa, "<<<embed_stdout>>>", .{
        .close_source = true,
        .disconnect_source = true,
        .close_destination = false,
        .disconnect_destination = false,
        .keep_open = true,
    }, &tracer) catch return error.EmbedRunFailed;
    defer stdout_stream.stream.deinit();
    const stderr_stream = Stream(u8).initReaderWriter(gpa, "<<<embed_stderr>>>", .{
        .close_source = true,
        .disconnect_source = true,
        .close_destination = false,
        .disconnect_destination = false,
        .keep_open = true,
    }, &tracer) catch return error.EmbedRunFailed;
    defer stderr_stream.stream.deinit();

    var stdin_closeable = closeable.ManualCloseable(ExitCode){ .label = "<<<embed_stdin>>>" };
    var stdout_closeable = closeable.NeverCloses(ExitCode){ .label = "<<<embed_stdout>>>" };
    var stderr_closeable = closeable.NeverCloses(ExitCode){ .label = "<<<embed_stderr>>>" };

    var wrapped_stdout_buffer: [1024]u8 = undefined;
    var wrapped_stderr_buffer: [1024]u8 = undefined;
    var wrapped_stdout = TraceWriter.init(&wrapped_stdout_buffer, &stdout_capture.writer, null, "<<<embed_t_stdout>>>");
    var wrapped_stderr = TraceWriter.init(&wrapped_stderr_buffer, &stderr_capture.writer, null, "<<<embed_t_stderr>>>");

    stdin_stream.connectSource(closeable.CloseableReader(ExitCode).init(&stdin_reader, &stdin_closeable.closeable)) catch return error.EmbedRunFailed;
    stdout_stream.connectDestination(closeable.CloseableWriter(ExitCode).init(&wrapped_stdout.writer, &stdout_closeable.closeable)) catch return error.EmbedRunFailed;
    stderr_stream.connectDestination(closeable.CloseableWriter(ExitCode).init(&wrapped_stderr.writer, &stderr_closeable.closeable)) catch return error.EmbedRunFailed;

    var result = ir.runner.runIR(gpa, &doc_store.document_store, &entry.ast.?, .{
        .io = io,
        .verbose = options.verbose,
        .dry_run = false,
        .script_args = options.args,
        .env = &env,
        .stdin = stdin_stream,
        .stdout = stdout_stream,
        .stderr = stderr_stream,
        .stdout_is_tty = false,
        .stderr_is_tty = false,
        .tracer = &tracer,
        .host_functions = options.host_functions,
        .typed_host_functions = options.typed_host_functions,
        .sandbox = options.sandbox,
    }) catch return error.EmbedRunFailed;
    defer result.deinit();

    // Push any bytes still buffered in the trace writers into the capture
    // buffers before reading them.
    wrapped_stdout.writer.flush() catch {};
    wrapped_stderr.writer.flush() catch {};

    var exit_code: ?ExitCode = null;
    switch (result) {
        .success => |code| exit_code = code,
        .err => |e| try collectDiagnostics(arena, &diagnostics, e.diagnostics()),
    }

    // Allocate everything into the arena *before* moving it into the result:
    // struct fields initialize in order, so `.arena = arena_state` would snapshot
    // the arena before these allocations and strand them in the discarded local.
    const out = try arena.dupe(u8, stdout_capture.written());
    const err = try arena.dupe(u8, stderr_capture.written());
    const diags = try diagnostics.toOwnedSlice(arena);
    return .{
        .arena = arena_state,
        .exit_code = exit_code,
        .stdout = out,
        .stderr = err,
        .diagnostics = diags,
    };
}

/// Builds a compile-failure `Outcome` (no run; `exit_code == null`) with no
/// captured output and the collected diagnostics.
fn compileFailure(arena_state: std.heap.ArenaAllocator, diagnostics: *std.ArrayList(Diagnostic)) Error!Outcome {
    var state = arena_state;
    // Finish the arena allocation before the struct literal moves the arena (see
    // the ordering note in `run`).
    const diags = try diagnostics.toOwnedSlice(state.allocator());
    return .{
        .arena = state,
        .exit_code = null,
        .stdout = "",
        .stderr = "",
        .diagnostics = diags,
    };
}

/// Flattens any diagnostic list (parser / type-checker / IR all share the
/// `span()`/`severity()`/`message` interface) into the owned `Diagnostic` form,
/// duping every string into `arena`.
fn collectDiagnostics(arena: Allocator, out: *std.ArrayList(Diagnostic), diags: anytype) Allocator.Error!void {
    for (diags) |d| {
        const maybe_span: ?ast.Span = d.span();
        try out.append(arena, .{
            .severity = parseSeverity(d.severity()),
            .message = try arena.dupe(u8, d.message),
            .file = if (maybe_span) |s| try arena.dupe(u8, s.start.file) else null,
            .line = if (maybe_span) |s| s.start.line else 0,
            .column = if (maybe_span) |s| s.start.column else 0,
        });
    }
}

fn parseSeverity(name: []const u8) Severity {
    return std.meta.stringToEnum(Severity, name) orelse .@"error";
}

/// Parses every module imported (transitively) by `script` into the store, so
/// the type checker finds them. Returns false if any imported module failed to
/// parse (its diagnostics are collected). Walks top-level `import "…"` and
/// `const m = import "…"` statements — the positions imports occupy in practice.
fn resolveImports(
    io: std.Io,
    gpa: Allocator,
    ds: *DocumentStore,
    script: ast.Script,
    arena: Allocator,
    diagnostics: *std.ArrayList(Diagnostic),
) Error!bool {
    var ok = true;
    for (script.statements) |stmt| {
        const expr: ?*const ast.Expression = switch (stmt.*) {
            .expression => |e| e.expression,
            .binding_decl => |b| b.initializer,
            else => null,
        };
        const imp = if (expr) |e| switch (e.*) {
            .import_expr => |import_expr| import_expr,
            else => continue,
        } else continue;

        const module_path = document.resolveModulePath(io, gpa, imp.importer, imp.module_name) catch {
            ok = false;
            continue;
        };
        defer gpa.free(module_path);

        // Already parsed (also breaks import cycles).
        if ((ds.getAst(module_path) catch null) != null) continue;

        const module_parser = ds.getParser(module_path) catch {
            ok = false;
            continue;
        };
        switch (module_parser.parseScript(module_path)) {
            .success => |module_ast| {
                if (!try resolveImports(io, gpa, ds, module_ast, arena, diagnostics)) ok = false;
            },
            .err => |e| {
                try collectDiagnostics(arena, diagnostics, e.diagnostics());
                ok = false;
            },
        }
    }
    return ok;
}

test "runs a script and captures stdout" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    // `echo` is an external command; grant exec since this tests capture.
    var outcome = try run(gpa, threaded.io(), "echo \"hello embed\"\n", .{ .sandbox = .permissive });
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("hello embed\n", outcome.stdout);
    try std.testing.expectEqual(@as(usize, 0), outcome.diagnostics.len);
}

fn greetHost(call: Call) Response {
    const name = if (call.args.len > 0) call.args[0] else "stranger";
    const out = std.fmt.allocPrint(call.arena, "Hello, {s}!", .{name}) catch return .{ .exit_code = 1 };
    return .{ .output = out };
}

fn sumHost(call: Call) Response {
    var total: i64 = 0;
    for (call.args) |a| total += std.fmt.parseInt(i64, a, 10) catch return .{ .exit_code = 1 };
    const out = std.fmt.allocPrint(call.arena, "{d}", .{total}) catch return .{ .exit_code = 1 };
    return .{ .output = out };
}

fn boomHost(_: Call) Response {
    return .{ .stderr = "boom!", .exit_code = 3 };
}

fn rangeHost(call: Call) Response {
    const n = if (call.args.len > 0) std.fmt.parseInt(usize, call.args[0], 10) catch 0 else 0;
    var buf = std.Io.Writer.Allocating.init(call.arena);
    for (1..n + 1) |i| buf.writer.print("{d}\n", .{i}) catch return .{ .exit_code = 1 };
    return .{ .output = buf.written() };
}

test "a script calls a registered host function (unbound, to stdout)" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    const funcs = [_]HostFn{.{ .name = "greet", .callback = greetHost }};
    var outcome = try run(gpa, threaded.io(), "greet \"world\"\n", .{ .host_functions = &funcs });
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("Hello, world!", outcome.stdout);
}

test "a script captures a host function's result in a binding" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    const funcs = [_]HostFn{.{ .name = "greet", .callback = greetHost }};
    var outcome = try run(
        gpa,
        threaded.io(),
        "const msg = greet \"world\"\necho \"${msg}\"\n",
        .{ .host_functions = &funcs, .sandbox = .permissive },
    );
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("Hello, world!\n", outcome.stdout);
}

test "a host function receives multiple arguments and its result pipes onward" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    const funcs = [_]HostFn{.{ .name = "sum", .callback = sumHost }};
    var outcome = try run(
        gpa,
        threaded.io(),
        "const total = sum \"2\" \"3\" \"4\"\necho \"total=${total}\"\n",
        .{ .host_functions = &funcs, .sandbox = .permissive },
    );
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("total=9\n", outcome.stdout);
}

test "the default sandbox denies spawning an external command" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    // `echo` is an external process; the default (strict) sandbox forbids it.
    var outcome = try run(gpa, threaded.io(), "echo \"hi\"\n", .{});
    defer outcome.deinit();

    try std.testing.expect(!outcome.ok());
    try std.testing.expect(outcome.diagnostics.len >= 1);
    try std.testing.expect(std.mem.indexOf(u8, outcome.diagnostics[0].message, "sandbox") != null);
}

test "granting exec lets the script spawn a command" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    var outcome = try run(gpa, threaded.io(), "echo \"hi\"\n", .{ .sandbox = .{
        .allow_exec = true,
        .allow_fs = false,
        .allow_env = false,
        .allow_ffi = false,
    } });
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("hi\n", outcome.stdout);
}

test "host functions remain callable under the strict sandbox" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    // No exec/fs/env/ffi granted, yet a registered host function still runs —
    // it is the sanctioned effect surface.
    const funcs = [_]HostFn{.{ .name = "greet", .callback = greetHost }};
    var outcome = try run(gpa, threaded.io(), "greet \"world\"\n", .{ .host_functions = &funcs });
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("Hello, world!", outcome.stdout);
}

test "a host function reports a failure via stderr and a non-zero exit code" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    const funcs = [_]HostFn{.{ .name = "boom", .callback = boomHost }};

    // Unbound: the host function's stderr flows to the script's stderr.
    var out_err = try run(gpa, threaded.io(), "boom\n", .{ .host_functions = &funcs });
    defer out_err.deinit();
    try std.testing.expect(out_err.ok());
    try std.testing.expectEqualStrings("boom!", out_err.stderr);

    // The failure (non-zero exit) is catchable with `||`, recovering a fallback.
    var out_catch = try run(gpa, threaded.io(), "const x = boom || \"caught\"\nyield \"${x}\"\n", .{ .host_functions = &funcs });
    defer out_catch.deinit();
    try std.testing.expect(out_catch.ok());
    try std.testing.expectEqualStrings("caught", out_catch.stdout);
}

test "a host function's lines marshal to an array through the pipeline" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    const funcs = [_]HostFn{.{ .name = "range", .callback = rangeHost }};
    var outcome = try run(
        gpa,
        threaded.io(),
        "range \"3\" | lines | parseInt | { for (&0) |n| yield \"${n},\" }\n",
        .{ .host_functions = &funcs },
    );
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("1,2,3,", outcome.stdout);
}

fn addTyped(call: TypedCall) TypedResponse {
    return .{ .value = .{ .int = call.args[0].int + call.args[1].int } };
}

fn shoutTyped(call: TypedCall) TypedResponse {
    const name = call.args[0].string;
    const out = std.fmt.allocPrint(call.arena, "HI {s}", .{name}) catch return .{ .value = .{ .string = "" } };
    return .{ .value = .{ .string = out } };
}

test "a typed host function passes and returns Int values" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    const funcs = [_]TypedHostFn{.{ .name = "add", .params = &.{ .int, .int }, .returns = .int, .callback = addTyped }};
    var outcome = try run(
        gpa,
        threaded.io(),
        "const n: Int = add 2 3\nyield \"${n}\"\n",
        .{ .typed_host_functions = &funcs },
    );
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("5", outcome.stdout);
}

test "a typed host function passes and returns String values" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    const funcs = [_]TypedHostFn{.{ .name = "shout", .params = &.{.string}, .returns = .string, .callback = shoutTyped }};
    var outcome = try run(
        gpa,
        threaded.io(),
        "const s: String = shout \"bob\"\nyield \"${s}\"\n",
        .{ .typed_host_functions = &funcs },
    );
    defer outcome.deinit();

    try std.testing.expect(outcome.ok());
    try std.testing.expectEqualStrings("HI bob", outcome.stdout);
}

test "a type error comes back as a structured diagnostic, not a run" {
    const gpa = std.testing.allocator;
    var threaded = std.Io.Threaded.init(gpa, .{});
    defer threaded.deinit();

    var outcome = try run(gpa, threaded.io(), "const n: Int = \"not a number\"\n", .{});
    defer outcome.deinit();

    try std.testing.expect(!outcome.ok());
    try std.testing.expectEqual(@as(?ExitCode, null), outcome.exit_code);
    try std.testing.expect(outcome.diagnostics.len >= 1);
    try std.testing.expectEqual(Severity.@"error", outcome.diagnostics[0].severity);
}
