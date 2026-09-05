//! Generates a Runic C-FFI binding from the Zig that `zig translate-c` emits
//! for a C header. The output is a `std.ffi` import, any enum values / integer
//! or string `#define`s as Runic `const`s, and a single `cimport` block of
//! `extern fn`s — nothing is wrapped in a Runic function. See future/c-ffi.md.
//!
//! This module is the pure transform (Zig source in → Runic source out) so it
//! is unit-testable; the `runic cbind` CLI subcommand runs translate-c and
//! feeds its stdout here.
const std = @import("std");

pub const Options = struct {
    /// The library name written into the `cimport` (e.g. "libfoo.so.1").
    library_name: []const u8,
    /// The const the cimport binds to (e.g. "libfoo").
    binding_name: []const u8,
    /// The header path, for the generated header comment (optional).
    header_name: []const u8 = "",
    /// The `zig translate-c` output for an *empty* header. Its `pub const`
    /// names are the compiler's predefined macros (`__STDC__`, `__x86_64__`,
    /// …); constants with these names are dropped so only the header's own
    /// enum values and `#define`s remain. When null, nothing is filtered.
    baseline_source: ?[]const u8 = null,
};

pub const Result = struct {
    source: []u8,
    extern_count: usize = 0,
    constant_count: usize = 0,
    /// Functions dropped because a parameter or return type has no C-type
    /// mapping (struct-by-value, varargs, function pointers, …).
    skipped_count: usize = 0,

    pub fn deinit(self: *Result, allocator: std.mem.Allocator) void {
        allocator.free(self.source);
    }
};

/// Maps a `zig translate-c` type to the `std.ffi` member name (`c.Int`, …), or
/// null when it has no scalar C-type mapping. `[*c]const u8` (a C string) must
/// be recognized before the generic pointer rule.
fn cTypeName(raw: []const u8) ?[]const u8 {
    const t = std.mem.trim(u8, raw, " ");
    const eql = std.mem.eql;
    if (eql(u8, t, "[*c]const u8") or eql(u8, t, "[*c]u8")) return "Str";
    if (eql(u8, t, "c_int")) return "Int";
    if (eql(u8, t, "c_uint")) return "UInt";
    if (eql(u8, t, "c_long")) return "Long";
    if (eql(u8, t, "c_ulong")) return "ULong";
    if (eql(u8, t, "c_short")) return "Short";
    if (eql(u8, t, "c_ushort")) return "UShort";
    if (eql(u8, t, "c_char") or eql(u8, t, "u8") or eql(u8, t, "i8")) return "Char";
    if (eql(u8, t, "usize")) return "SizeT";
    if (eql(u8, t, "f32")) return "Float";
    if (eql(u8, t, "f64")) return "Double";
    if (eql(u8, t, "bool")) return "Bool";
    if (eql(u8, t, "void")) return "Void";
    // Any pointer shape maps to an opaque handle.
    if (std.mem.startsWith(u8, t, "?*") or std.mem.startsWith(u8, t, "*") or std.mem.startsWith(u8, t, "[*c]")) return "Ptr";
    return null;
}

/// Formats one `pub extern fn …;` line as a Runic `extern fn …`, or returns
/// null (skip) when any parameter/return type is unmappable or it is variadic.
fn formatExternFn(allocator: std.mem.Allocator, line: []const u8) !?[]u8 {
    const prefix = "pub extern fn ";
    if (!std.mem.startsWith(u8, line, prefix)) return null;
    const body = std.mem.trim(u8, line[prefix.len..], " ;");

    const open = std.mem.indexOfScalar(u8, body, '(') orelse return null;
    const close = std.mem.lastIndexOfScalar(u8, body, ')') orelse return null;
    if (close < open) return null;

    const name = std.mem.trim(u8, body[0..open], " ");
    const params_str = std.mem.trim(u8, body[open + 1 .. close], " ");
    const ret_str = std.mem.trim(u8, body[close + 1 ..], " ");

    if (std.mem.indexOf(u8, params_str, "...") != null) return null; // variadic

    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(allocator);
    try out.appendSlice(allocator, "    extern fn ");
    try out.appendSlice(allocator, name);
    try out.append(allocator, '(');

    if (params_str.len > 0) {
        var params = std.mem.splitSequence(u8, params_str, ", ");
        var first = true;
        while (params.next()) |param| {
            const colon = std.mem.indexOfScalar(u8, param, ':') orelse {
                out.deinit(allocator);
                return null;
            };
            const pname = std.mem.trim(u8, param[0..colon], " ");
            const ptype = cTypeName(param[colon + 1 ..]) orelse {
                out.deinit(allocator);
                return null;
            };
            if (!first) try out.appendSlice(allocator, ", ");
            first = false;
            try out.appendSlice(allocator, pname);
            try out.appendSlice(allocator, ": c.");
            try out.appendSlice(allocator, ptype);
        }
    }

    const ret = cTypeName(ret_str) orelse {
        out.deinit(allocator);
        return null;
    };
    try out.appendSlice(allocator, ") c.");
    try out.appendSlice(allocator, ret);

    return try out.toOwnedSlice(allocator);
}

/// Extracts a Runic `const NAME = value` from a `pub const …;` line when it is
/// an enum value or an integer/string `#define`; null for a type alias or an
/// expression we don't translate.
fn formatConstant(allocator: std.mem.Allocator, line: []const u8) !?[]u8 {
    const prefix = "pub const ";
    if (!std.mem.startsWith(u8, line, prefix)) return null;
    const body = std.mem.trim(u8, line[prefix.len..], " ;");

    const eq = std.mem.indexOfScalar(u8, body, '=') orelse return null;
    const lhs = std.mem.trim(u8, body[0..eq], " ");
    var rhs = std.mem.trim(u8, body[eq + 1 ..], " ");

    // `NAME: c_int = 0` — an enum value; drop the `: type` annotation.
    var name = lhs;
    if (std.mem.indexOfScalar(u8, lhs, ':')) |c| name = std.mem.trim(u8, lhs[0..c], " ");

    // `@as(c_int, 100)` — an integer #define; take the value argument.
    if (std.mem.startsWith(u8, rhs, "@as(")) {
        const comma = std.mem.indexOfScalar(u8, rhs, ',') orelse return null;
        rhs = std.mem.trim(u8, rhs[comma + 1 ..], " )");
    }

    const is_int = rhs.len > 0 and blk: {
        for (rhs, 0..) |ch, i| {
            if (i == 0 and (ch == '-' or ch == '+')) continue;
            if (!std.ascii.isDigit(ch)) break :blk false;
        }
        break :blk true;
    };
    const is_string = rhs.len >= 2 and rhs[0] == '"' and rhs[rhs.len - 1] == '"';
    if (!is_int and !is_string) return null; // type alias / unsupported expression

    return try std.fmt.allocPrint(allocator, "pub const {s} = {s}", .{ name, rhs });
}

/// The declared name in a `pub const NAME…` line (up to `:` or `=`), or null.
fn constName(line: []const u8) ?[]const u8 {
    const prefix = "pub const ";
    if (!std.mem.startsWith(u8, line, prefix)) return null;
    const rest = line[prefix.len..];
    const end = std.mem.indexOfAny(u8, rest, ":= ") orelse return null;
    return std.mem.trim(u8, rest[0..end], " ");
}

pub fn generate(
    allocator: std.mem.Allocator,
    zig_source: []const u8,
    options: Options,
) !Result {
    var constants = std.ArrayList([]u8).empty;
    defer {
        for (constants.items) |c| allocator.free(c);
        constants.deinit(allocator);
    }
    var externs = std.ArrayList([]u8).empty;
    defer {
        for (externs.items) |e| allocator.free(e);
        externs.deinit(allocator);
    }
    var skipped: usize = 0;

    // Names of the compiler's predefined macros (from an empty translate-c),
    // to exclude so only the header's own constants remain.
    var baseline = std.StringHashMap(void).init(allocator);
    defer baseline.deinit();
    if (options.baseline_source) |base| {
        var base_lines = std.mem.splitScalar(u8, base, '\n');
        while (base_lines.next()) |raw| {
            const line = std.mem.trim(u8, raw, " \t\r");
            if (constName(line)) |name| try baseline.put(name, {});
        }
    }

    var lines = std.mem.splitScalar(u8, zig_source, '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        if (std.mem.startsWith(u8, line, "pub extern fn ")) {
            if (try formatExternFn(allocator, line)) |formatted| {
                try externs.append(allocator, formatted);
            } else {
                skipped += 1;
            }
        } else if (std.mem.startsWith(u8, line, "pub const ")) {
            if (constName(line)) |name| {
                if (baseline.contains(name)) continue; // a predefined compiler macro
            }
            if (try formatConstant(allocator, line)) |formatted| {
                try constants.append(allocator, formatted);
            }
        }
    }

    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    if (options.header_name.len > 0) {
        try w.print("// Generated by `runic cbind` from {s}.\n", .{options.header_name});
    } else {
        try w.writeAll("// Generated by `runic cbind`.\n");
    }
    try w.writeAll("// Review the c.Str/c.Ptr choices and the skipped functions before use.\n\n");
    try w.writeAll("const c = import \"std/ffi.rn\"\n\n");

    for (constants.items) |constant| try w.print("{s}\n", .{constant});
    if (constants.items.len > 0) try w.writeAll("\n");

    try w.print("pub const {s} = cimport \"{s}\" {{\n", .{ options.binding_name, options.library_name });
    for (externs.items) |extern_fn| try w.print("{s}\n", .{extern_fn});
    if (skipped > 0) {
        try w.print("    // skipped {d} function(s): struct-by-value, variadic, or an unmapped type\n", .{skipped});
    }
    try w.writeAll("}\n");

    return .{
        .source = try out.toOwnedSlice(),
        .extern_count = externs.items.len,
        .constant_count = constants.items.len,
        .skipped_count = skipped,
    };
}

test "generate emits a cimport block with constants and skips unmapped fns" {
    const allocator = std.testing.allocator;
    const zig_source =
        \\pub const COLOR_RED: c_int = 0;
        \\pub const COLOR_BLUE: c_int = 4;
        \\pub const FOO_MAX = @as(c_int, 100);
        \\pub const FOO_NAME = "foo";
        \\pub const Color = c_uint;
        \\pub extern fn foo_add(a: c_int, b: c_int) c_int;
        \\pub extern fn foo_scale(x: f64) f64;
        \\pub extern fn foo_name(handle: ?*anyopaque) [*c]const u8;
        \\pub extern fn foo_free(handle: ?*anyopaque) void;
        \\pub extern fn foo_make(cfg: SomeStruct) c_int;
        \\pub extern fn foo_printf(fmt: [*c]const u8, ...) c_int;
    ;
    var result = try generate(allocator, zig_source, .{ .library_name = "libfoo.so", .binding_name = "libfoo" });
    defer result.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 4), result.extern_count); // add, scale, name, free
    try std.testing.expectEqual(@as(usize, 4), result.constant_count); // 2 enum + int + string
    try std.testing.expectEqual(@as(usize, 2), result.skipped_count); // struct-by-value + variadic

    const s = result.source;
    try std.testing.expect(std.mem.indexOf(u8, s, "const c = import \"std/ffi.rn\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "pub const COLOR_RED = 0") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "pub const FOO_MAX = 100") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "pub const FOO_NAME = \"foo\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "pub const libfoo = cimport \"libfoo.so\" {") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn foo_add(a: c.Int, b: c.Int) c.Int") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn foo_scale(x: c.Double) c.Double") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn foo_name(handle: c.Ptr) c.Str") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn foo_free(handle: c.Ptr) c.Void") != null);
    // The enum type alias is not emitted as a constant.
    try std.testing.expect(std.mem.indexOf(u8, s, "Color") == null);
    try std.testing.expect(std.mem.indexOf(u8, s, "skipped 2 function") != null);
}
