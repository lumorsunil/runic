//! The `runic cbind` subcommand: generate a Runic C-FFI binding from a C
//! header. It runs `zig translate-c <header>` and feeds the result to
//! `runic.cbind.generate`, which emits a `std.ffi` import, the header's enum
//! values / integer & string `#define`s as Runic `const`s, and one `cimport`
//! block of `extern fn`s. See future/c-ffi.md.
//!
//!   runic cbind <header.h> --lib <libname.so> [-o out.rn] [--name binding]
const std = @import("std");
const Allocator = std.mem.Allocator;
const runic = @import("runic");

const usage =
    \\usage: runic cbind <header.h> --lib <libname.so> [-o <out.rn>] [--name <binding>]
    \\
    \\  Generates a Runic C-FFI binding (a `cimport` block plus enum/#define
    \\  constants) from a C header, via `zig translate-c`. Writes to stdout
    \\  unless -o is given.
    \\
;

pub fn run(
    io: std.Io,
    allocator: Allocator,
    raw_args: []const [*:0]const u8,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
) !runic.ExitCode {
    var header: ?[]const u8 = null;
    var library: ?[]const u8 = null;
    var out_path: ?[]const u8 = null;
    var binding: ?[]const u8 = null;

    var i: usize = 0;
    while (i < raw_args.len) : (i += 1) {
        const arg = std.mem.span(raw_args[i]);
        if (std.mem.eql(u8, arg, "--lib")) {
            i += 1;
            if (i >= raw_args.len) return usageError(stderr, "--lib needs a value");
            library = std.mem.span(raw_args[i]);
        } else if (std.mem.eql(u8, arg, "-o")) {
            i += 1;
            if (i >= raw_args.len) return usageError(stderr, "-o needs a value");
            out_path = std.mem.span(raw_args[i]);
        } else if (std.mem.eql(u8, arg, "--name")) {
            i += 1;
            if (i >= raw_args.len) return usageError(stderr, "--name needs a value");
            binding = std.mem.span(raw_args[i]);
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            try stdout.writeAll(usage);
            try stdout.flush();
            return .success;
        } else if (std.mem.startsWith(u8, arg, "-")) {
            return usageError(stderr, "unknown flag");
        } else if (header == null) {
            header = arg;
        } else {
            return usageError(stderr, "unexpected extra argument");
        }
    }

    const header_path = header orelse return usageError(stderr, "a C header path is required");
    const library_name = library orelse return usageError(stderr, "--lib <libname.so> is required");
    const binding_name = binding orelse defaultBindingName(library_name);

    // Run `zig translate-c <header>` and capture its Zig output.
    const translated = std.process.run(allocator, io, .{
        .argv = &.{ "zig", "translate-c", header_path },
        .stdout_limit = .limited(64 * 1024 * 1024),
    }) catch |err| {
        try stderr.print("error: could not run `zig translate-c` ({t}); is zig on PATH?\n", .{err});
        try stderr.flush();
        return .fromByte(1);
    };
    defer allocator.free(translated.stdout);
    defer allocator.free(translated.stderr);

    if (translated.term != .exited or translated.term.exited != 0) {
        try stderr.print("error: `zig translate-c {s}` failed:\n{s}\n", .{ header_path, translated.stderr });
        try stderr.flush();
        return .fromByte(1);
    }

    // Baseline: translate-c an empty header to learn the compiler's predefined
    // macros, so they're filtered out of the generated constants. Best-effort —
    // if it fails, no filtering (the output is just noisier).
    const baseline = computeBaseline(io, allocator);
    defer if (baseline) |b| allocator.free(b);

    var result = try runic.cbind.generate(allocator, translated.stdout, .{
        .library_name = library_name,
        .binding_name = binding_name,
        .header_name = header_path,
        .baseline_source = baseline,
    });
    defer result.deinit(allocator);

    if (out_path) |path| {
        var file = try std.Io.Dir.cwd().createFile(io, path, .{});
        defer file.close(io);
        var buf: [4096]u8 = undefined;
        var writer = file.writer(io, &buf);
        try writer.interface.writeAll(result.source);
        try writer.interface.flush();
        try stderr.print(
            "wrote {s}: {d} extern fn, {d} struct, {d} const, {d} skipped\n",
            .{ path, result.extern_count, result.struct_count, result.constant_count, result.skipped_count },
        );
        try stderr.flush();
    } else {
        try stdout.writeAll(result.source);
        try stdout.flush();
        try stderr.print(
            "// {d} extern fn, {d} struct, {d} const, {d} skipped\n",
            .{ result.extern_count, result.struct_count, result.constant_count, result.skipped_count },
        );
        try stderr.flush();
    }

    return .success;
}

/// Runs `zig translate-c` on an empty header and returns its stdout (the
/// compiler's predefined macros), for filtering. Best-effort: returns null on
/// any failure, in which case predefined macros are not filtered out.
fn computeBaseline(io: std.Io, allocator: Allocator) ?[]u8 {
    const tmp_path = "/tmp/runic-cbind-empty.h";
    var f = std.Io.Dir.cwd().createFile(io, tmp_path, .{}) catch return null;
    f.close(io);
    defer std.Io.Dir.cwd().deleteFile(io, tmp_path) catch {};

    const res = std.process.run(allocator, io, .{
        .argv = &.{ "zig", "translate-c", tmp_path },
        .stdout_limit = .limited(64 * 1024 * 1024),
    }) catch return null;
    allocator.free(res.stderr);
    if (res.term != .exited or res.term.exited != 0) {
        allocator.free(res.stdout);
        return null;
    }
    return res.stdout;
}

fn usageError(stderr: *std.Io.Writer, message: []const u8) !runic.ExitCode {
    try stderr.print("error: {s}\n\n{s}", .{ message, usage });
    try stderr.flush();
    return .fromByte(2);
}

/// Derives a binding const name from a library name: strip the directory, a
/// leading "lib", and everything from the first '.'. "libm.so.6" → "m",
/// "libSDL2.so" → "SDL2". Falls back to "clib".
fn defaultBindingName(library_name: []const u8) []const u8 {
    var name = library_name;
    if (std.mem.lastIndexOfScalar(u8, name, '/')) |slash| name = name[slash + 1 ..];
    if (std.mem.startsWith(u8, name, "lib")) name = name["lib".len..];
    if (std.mem.indexOfScalar(u8, name, '.')) |dot| name = name[0..dot];
    return if (name.len == 0) "clib" else name;
}
