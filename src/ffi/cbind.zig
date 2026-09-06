//! Generates a Runic C-FFI binding from the Zig that `zig translate-c` emits
//! for a C header. The output is a `std.ffi` import, any enum values / integer
//! or string `#define`s as Runic `const`s, any by-value struct types as Runic
//! `struct`s, and a single `cimport` block of `extern fn`s — nothing is wrapped
//! in a Runic function. See future/c-ffi.md.
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
    /// By-value struct types emitted (all fields are scalar C types).
    struct_count: usize = 0,
    /// Functions dropped because a parameter or return type has no C-type
    /// mapping (a non-marshallable struct, varargs, function pointers, …).
    skipped_count: usize = 0,

    pub fn deinit(self: *Result, allocator: std.mem.Allocator) void {
        allocator.free(self.source);
    }
};

/// A `std.ffi` scalar C type, or a by-value struct (referenced by its Runic
/// name). The result token a parameter/return type maps to.
const TypeRef = union(enum) {
    scalar: []const u8, // the std.ffi member name, e.g. "Int"
    strct: []const u8, // the Runic struct name, e.g. "Color"
};

const StructField = struct {
    name: []const u8, // borrowed from the translate-c source
    raw_type: []const u8, // the translate-c field type, e.g. "u8" or "Vector2"
};

const StructDef = struct {
    /// The clean Runic name to emit (a typedef alias if one exists, else the
    /// translate-c name).
    runic_name: []const u8,
    fields: []StructField,
    /// Memoized marshallability (every field is a scalar C type or, recursively,
    /// a marshallable struct). Computed lazily since it depends on other structs.
    state: enum { unknown, computing, yes, no } = .unknown,
    /// Referenced by at least one emitted extern fn (only these, and the structs
    /// they transitively contain, are emitted).
    used: bool = false,
    /// Already written to the output (topological emit visits each once).
    emitted: bool = false,
    /// A struct synthesized for a fixed C array field (`float[4]` → a struct of
    /// four `Float` fields — same layout). Its `fields` live in the arena, not
    /// the main allocator.
    synthetic: bool = false,
};

/// The registry of struct types collected from the header, plus a map from any
/// type name a function might use (the clean typedef name or the `struct_X`
/// name) to its `structs` key.
const Structs = struct {
    /// Keyed by the translate-c struct name (`struct_Color`, or the typedef name
    /// for an anonymous struct).
    defs: std.StringHashMap(StructDef),
    /// A struct type name (a typedef) → the `defs` key it names.
    aliases: std.StringHashMap([]const u8),
    /// Any typedef name → its right-hand-side type expression, so a field or
    /// parameter written as a typedef (`ModelAnimPose` = `[*c]Transform`)
    /// resolves through to the underlying type.
    type_aliases: std.StringHashMap([]const u8),
    /// Backs the names and fields of synthesized array structs.
    arena: std.heap.ArenaAllocator,

    fn lookup(self: *Structs, type_name: []const u8) ?*StructDef {
        const key = self.aliases.get(type_name) orelse return null;
        return self.defs.getPtr(key);
    }

    /// Resolves a translate-c type to a `std.ffi` scalar or a marshallable
    /// struct, following typedef chains (`Camera` → `Camera3D`; `ModelAnimPose`
    /// → `[*c]Transform` → `c.Ptr`). Null if it maps to neither. `mark_used`
    /// records a referenced struct for emission.
    fn resolveType(self: *Structs, raw: []const u8, depth: usize, mark_used: bool) ?TypeRef {
        if (depth > 16) return null;
        const t = std.mem.trim(u8, raw, " ");
        if (cTypeName(t)) |scalar| return .{ .scalar = scalar };
        if (self.aliases.get(t)) |key| {
            if (!self.marshallable(key)) return null;
            const def = self.defs.getPtr(key).?;
            if (mark_used) def.used = true;
            return .{ .strct = def.runic_name };
        }
        if (self.type_aliases.get(t)) |rhs| return self.resolveType(rhs, depth + 1, mark_used);
        return null;
    }

    /// Follows typedefs to the struct key a type names, or null if it is not a
    /// struct — used to emit a struct's dependencies before itself.
    fn resolveStructKey(self: *Structs, raw: []const u8, depth: usize) ?[]const u8 {
        if (depth > 16) return null;
        const t = std.mem.trim(u8, raw, " ");
        if (self.aliases.get(t)) |key| {
            if (self.defs.contains(key)) return key;
        }
        if (self.type_aliases.get(t)) |rhs| return self.resolveStructKey(rhs, depth + 1);
        return null;
    }

    /// Whether the struct keyed `key` is marshallable — every field resolves to a
    /// scalar or (recursively) a marshallable struct. Memoized; a cycle (invalid
    /// by-value C) resolves to non-marshallable.
    fn marshallable(self: *Structs, key: []const u8) bool {
        const def = self.defs.getPtr(key) orelse return false;
        switch (def.state) {
            .yes => return true,
            .no, .computing => return false,
            .unknown => {},
        }
        def.state = .computing;
        var ok = def.fields.len > 0;
        for (def.fields) |field| {
            if (self.resolveType(field.raw_type, 0, false) == null) {
                ok = false;
                break;
            }
        }
        // getPtr again: a recursive call may have grown/rehashed the map.
        self.defs.getPtr(key).?.state = if (ok) .yes else .no;
        return ok;
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

/// The `std.ffi` scalar type or marshallable struct a parameter/return type
/// maps to, marking a referenced struct as used. Null when unmappable (skip the
/// function).
fn mapType(structs: *Structs, raw: []const u8) ?TypeRef {
    return structs.resolveType(raw, 0, true);
}

/// Whether a `pub const NAME = extern struct {` line opens a struct block, and
/// the declared NAME (borrowed from `line`).
fn structHeaderName(line: []const u8) ?[]const u8 {
    const prefix = "pub const ";
    if (!std.mem.startsWith(u8, line, prefix)) return null;
    if (!std.mem.endsWith(u8, line, "extern struct {")) return null;
    const rest = line[prefix.len..];
    const eq = std.mem.indexOfScalar(u8, rest, '=') orelse return null;
    return std.mem.trim(u8, rest[0..eq], " ");
}

/// Parses one struct field line (`    r: u8 = 0,` or `    offset: Vector2 = …,`)
/// into a name and its raw type string. Null for a non-field line (an inner
/// `pub const`, a blank line). Marshallability of the type is judged later.
fn parseStructField(line: []const u8) ?StructField {
    if (std.mem.startsWith(u8, line, "pub ")) return null;
    const colon = std.mem.indexOfScalar(u8, line, ':') orelse return null;
    const name = std.mem.trim(u8, line[0..colon], " ");
    if (name.len == 0) return null;
    for (name) |ch| if (!std.ascii.isAlphanumeric(ch) and ch != '_') return null;

    var type_str = line[colon + 1 ..];
    if (std.mem.indexOfScalar(u8, type_str, '=')) |eq| type_str = type_str[0..eq];
    if (std.mem.indexOfScalar(u8, type_str, ',')) |comma| type_str = type_str[0..comma];
    return .{ .name = name, .raw_type = std.mem.trim(u8, type_str, " ") };
}

/// Collects every `extern struct` block and typedef alias into `structs`.
/// Struct fields borrow `zig_source`; the field arrays are owned by `allocator`.
fn collectStructs(
    allocator: std.mem.Allocator,
    zig_source: []const u8,
    structs: *Structs,
) !void {
    var lines = std.mem.splitScalar(u8, zig_source, '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        if (structHeaderName(line)) |name| {
            var fields = std.ArrayList(StructField).empty;
            errdefer fields.deinit(allocator);
            while (lines.next()) |raw_body| {
                const body = std.mem.trim(u8, raw_body, " \t\r");
                if (std.mem.eql(u8, body, "};")) break;
                if (std.mem.startsWith(u8, body, "pub ")) continue; // inner decl
                if (parseStructField(body)) |field| try fields.append(allocator, field);
            }
            try structs.defs.put(name, .{
                .runic_name = name,
                .fields = try fields.toOwnedSlice(allocator),
            });
            try structs.aliases.put(name, name);
        }
    }

    // A second pass wires typedef aliases (`pub const Color = struct_Color;`) to
    // their struct. Aliases can chain (`Texture2D = Texture = struct_Texture`),
    // so collect the pairs and resolve to a fixpoint — order-independent. Every
    // typedef whose RHS is a *type* (a name, or a pointer/array shape — not a
    // value) is also recorded in `type_aliases` so a field or parameter written
    // through it (`ModelAnimPose` = `[*c]Transform`) resolves.
    const Pair = struct { alias: []const u8, target: []const u8 };
    var pairs = std.ArrayList(Pair).empty;
    defer pairs.deinit(allocator);
    var alias_lines = std.mem.splitScalar(u8, zig_source, '\n');
    while (alias_lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        const prefix = "pub const ";
        if (!std.mem.startsWith(u8, line, prefix)) continue;
        if (!std.mem.endsWith(u8, line, ";")) continue;
        const body = std.mem.trim(u8, line[prefix.len .. line.len - 1], " ");
        const eq = std.mem.indexOfScalar(u8, body, '=') orelse continue;
        const alias = std.mem.trim(u8, body[0..eq], " ");
        const target = std.mem.trim(u8, body[eq + 1 ..], " ");
        if (std.mem.indexOfScalar(u8, alias, ':') != null) continue; // typed const
        if (!isPlainIdentifier(alias)) continue;
        if (!looksLikeTypeExpr(target)) continue; // a value const, not a typedef
        try structs.type_aliases.put(alias, target);
        if (isPlainIdentifier(target)) try pairs.append(allocator, .{ .alias = alias, .target = target });
    }

    var changed = true;
    while (changed) {
        changed = false;
        for (pairs.items) |pair| {
            if (structs.aliases.contains(pair.alias)) continue;
            // A direct typedef (`Texture = struct_Texture`) also gives the struct
            // its clean emit name; a chained one only maps to the resolved key.
            if (structs.defs.getPtr(pair.target)) |def| {
                def.runic_name = pair.alias;
                try structs.aliases.put(pair.alias, pair.target);
                changed = true;
            } else if (structs.aliases.get(pair.target)) |key| {
                try structs.aliases.put(pair.alias, key);
                changed = true;
            }
        }
    }

    try synthesizeArrayStructs(structs);
}

/// A fixed C array field: its element count and element type expression.
const FixedArray = struct { len: usize, element: []const u8 };

/// Parses a translate-c fixed-array type (`[4]f32`, `[32]u8`, `[2]Matrix`) into
/// its length and element type. Null for a non-array (`[*c]…` has `*`, not a
/// digit, so it is rejected here and handled as a pointer).
fn parseFixedArray(raw: []const u8) ?FixedArray {
    const t = std.mem.trim(u8, raw, " ");
    if (t.len < 3 or t[0] != '[') return null;
    const close = std.mem.indexOfScalar(u8, t, ']') orelse return null;
    const n_str = t[1..close];
    if (n_str.len == 0) return null;
    for (n_str) |ch| if (!std.ascii.isDigit(ch)) return null;
    const len = std.fmt.parseInt(usize, n_str, 10) catch return null;
    const element = std.mem.trim(u8, t[close + 1 ..], " ");
    if (element.len == 0 or len == 0) return null;
    return .{ .len = len, .element = element };
}

/// Rewrites every fixed-array struct field (`float params[4]`) to a synthesized
/// struct of `len` identically-typed fields — byte-identical to the C array, so
/// the existing nested-struct marshalling handles it with no runtime change. The
/// element type is kept verbatim, so an array of structs (`[2]Matrix`) works and
/// a non-marshallable element leaves the array struct non-marshallable.
fn synthesizeArrayStructs(structs: *Structs) !void {
    const a = structs.arena.allocator();

    // Snapshot the keys — creating synthetic defs mutates (and may rehash) the map.
    var keys = std.ArrayList([]const u8).empty;
    var kit = structs.defs.keyIterator();
    while (kit.next()) |k| try keys.append(a, k.*);

    for (keys.items) |key| {
        const field_count = structs.defs.getPtr(key).?.fields.len;
        for (0..field_count) |fi| {
            const raw = structs.defs.getPtr(key).?.fields[fi].raw_type;
            const arr = parseFixedArray(raw) orelse continue;

            // A stable name per (len, element): `Arr_4_f32`, `Arr_2_Matrix`.
            const syn_name = try synthArrayName(a, arr);
            if (!structs.defs.contains(syn_name)) {
                const sfields = try a.alloc(StructField, arr.len);
                for (sfields, 0..) |*sf, i| {
                    sf.* = .{ .name = try std.fmt.allocPrint(a, "e{d}", .{i}), .raw_type = arr.element };
                }
                try structs.defs.put(syn_name, .{ .runic_name = syn_name, .fields = sfields, .synthetic = true });
                try structs.aliases.put(syn_name, syn_name);
            }
            structs.defs.getPtr(key).?.fields[fi].raw_type = syn_name;
        }
    }
}

/// A valid Runic identifier naming the array struct for `arr`, unique per
/// (length, element): non-identifier characters in the element become `_`.
fn synthArrayName(a: std.mem.Allocator, arr: FixedArray) ![]const u8 {
    const elem = try a.dupe(u8, arr.element);
    for (elem) |*ch| {
        if (!std.ascii.isAlphanumeric(ch.*) and ch.* != '_') ch.* = '_';
    }
    return std.fmt.allocPrint(a, "Arr_{d}_{s}", .{ arr.len, elem });
}

/// Formats one `pub extern fn …;` line as a Runic `extern fn …`, or returns
/// null (skip) when any parameter/return type is unmappable or it is variadic.
/// Marks any referenced struct as used.
fn formatExternFn(allocator: std.mem.Allocator, structs: *Structs, line: []const u8) !?[]u8 {
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
        var index: usize = 0;
        while (params.next()) |param| : (index += 1) {
            const colon = std.mem.indexOfScalar(u8, param, ':') orelse {
                out.deinit(allocator);
                return null;
            };
            const raw_name = std.mem.trim(u8, param[0..colon], " ");
            const ptype = mapType(structs, param[colon + 1 ..]) orelse {
                out.deinit(allocator);
                return null;
            };
            if (!first) try out.appendSlice(allocator, ", ");
            first = false;
            // A parameter name Runic can't bind (translate-c quotes C keywords
            // like `@"type"`) is renamed positionally — the name is cosmetic to a
            // C call.
            if (isPlainIdentifier(raw_name)) {
                try out.appendSlice(allocator, raw_name);
            } else {
                var name_buf: [16]u8 = undefined;
                try out.appendSlice(allocator, try std.fmt.bufPrint(&name_buf, "arg{d}", .{index}));
            }
            try out.appendSlice(allocator, ": ");
            try appendTypeRef(allocator, &out, ptype);
        }
    }

    const ret = mapType(structs, ret_str) orelse {
        out.deinit(allocator);
        return null;
    };
    try out.appendSlice(allocator, ") ");
    try appendTypeRef(allocator, &out, ret);

    return try out.toOwnedSlice(allocator);
}

/// Writes `const Name = struct { … }` for one by-value struct, emitting each
/// struct-typed field's dependency first so declarations resolve in order. A
/// struct *type* declaration cannot be `pub` (the parser rejects `pub const X =
/// struct {…}`); a plain `const` keeps it in scope for the extern signatures. An
/// importer that must *construct* one to pass in declares a matching struct
/// locally (qualified construction `lib.Color{…}` is not yet supported).
fn emitStruct(w: *std.Io.Writer, structs: *Structs, key: []const u8, count: *usize) std.Io.Writer.Error!void {
    const def = structs.defs.getPtr(key) orelse return;
    if (def.emitted) return;
    def.emitted = true; // set first: also breaks a (non-marshallable) cycle
    for (def.fields) |field| {
        if (structs.resolveStructKey(field.raw_type, 0)) |dep| try emitStruct(w, structs, dep, count);
    }
    count.* += 1;
    try w.print("const {s} = struct {{ ", .{def.runic_name});
    for (def.fields, 0..) |field, i| {
        if (i > 0) try w.writeAll(", ");
        switch (structs.resolveType(field.raw_type, 0, false).?) {
            .scalar => |s| try w.print("{s}: c.{s}", .{ field.name, s }),
            .strct => |s| try w.print("{s}: {s}", .{ field.name, s }),
        }
    }
    try w.writeAll(" }\n");
}

fn appendTypeRef(allocator: std.mem.Allocator, out: *std.ArrayList(u8), ref: TypeRef) !void {
    switch (ref) {
        .scalar => |s| {
            try out.appendSlice(allocator, "c.");
            try out.appendSlice(allocator, s);
        },
        .strct => |s| try out.appendSlice(allocator, s),
    }
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

    // Skip a name that is not a plain Runic identifier — translate-c emits
    // quoted identifiers for C keyword macros (`@"true"`, `@"false"`), which
    // Runic cannot bind.
    if (!isPlainIdentifier(name)) return null;

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

/// Whether a typedef's right-hand side is a *type* expression (a name, or a
/// pointer/array shape) rather than a value (`@as(…)`, a number, a string). Used
/// to tell `pub const Foo = Bar;` (a typedef) from `pub const N = 3;`.
fn looksLikeTypeExpr(rhs: []const u8) bool {
    if (rhs.len == 0) return false;
    if (isPlainIdentifier(rhs)) return true;
    return std.mem.startsWith(u8, rhs, "[*c]") or
        std.mem.startsWith(u8, rhs, "?*") or
        std.mem.startsWith(u8, rhs, "*") or
        std.mem.startsWith(u8, rhs, "[");
}

/// Whether `name` is a plain identifier (letter/underscore, then alphanumerics/
/// underscores) — the only names Runic can bind without `@"…"` escaping.
fn isPlainIdentifier(name: []const u8) bool {
    if (name.len == 0) return false;
    if (!std.ascii.isAlphabetic(name[0]) and name[0] != '_') return false;
    for (name[1..]) |ch| {
        if (!std.ascii.isAlphanumeric(ch) and ch != '_') return false;
    }
    return true;
}

/// Unwraps a translate-c scalar value (`@as(c_int, 200)` → `200`) and returns it
/// only when it is an integer literal; null otherwise.
fn unwrapIntValue(raw: []const u8) ?[]const u8 {
    var v = std.mem.trim(u8, raw, " ");
    if (std.mem.startsWith(u8, v, "@as(")) {
        const comma = std.mem.indexOfScalar(u8, v, ',') orelse return null;
        v = std.mem.trim(u8, v[comma + 1 ..], " )");
    }
    if (v.len == 0) return null;
    for (v, 0..) |ch, i| {
        if (i == 0 and (ch == '-' or ch == '+')) continue;
        if (!std.ascii.isDigit(ch)) return null;
    }
    return v;
}

/// Formats a compound-literal `#define` (a struct-valued macro such as raylib's
/// `RAYWHITE`) into a Runic struct-literal `const`. translate-c emits these as
/// `@import("std").mem.zeroInit(CLITERAL(Color), .{ @as(c_int, 245), … })`; we
/// pair the positional values with the struct's field names and mark the struct
/// used so it is emitted. Null unless the RHS is exactly that shape over a known
/// marshallable struct with a matching, all-integer value list.
fn formatStructConstant(allocator: std.mem.Allocator, structs: *Structs, line: []const u8) !?[]u8 {
    const prefix = "pub const ";
    if (!std.mem.startsWith(u8, line, prefix)) return null;
    const body = std.mem.trim(u8, line[prefix.len..], " ;");
    const eq = std.mem.indexOf(u8, body, " = ") orelse return null;
    const name = std.mem.trim(u8, body[0..eq], " ");
    if (!isPlainIdentifier(name)) return null;

    const zi = "@import(\"std\").mem.zeroInit(";
    const rhs = std.mem.trim(u8, body[eq + 3 ..], " ");
    if (!std.mem.startsWith(u8, rhs, zi)) return null;
    const inner = rhs[zi.len..];

    const sep = std.mem.indexOf(u8, inner, ", .{") orelse return null;
    var type_str = std.mem.trim(u8, inner[0..sep], " ");
    // `CLITERAL(Color)` (raylib's compound-literal helper) → `Color`.
    if (std.mem.startsWith(u8, type_str, "CLITERAL(") and std.mem.endsWith(u8, type_str, ")")) {
        type_str = std.mem.trim(u8, type_str["CLITERAL(".len .. type_str.len - 1], " ");
    }
    const vals_end = std.mem.lastIndexOfScalar(u8, inner, '}') orelse return null;
    const vals_start = sep + ", .{".len;
    if (vals_end <= vals_start) return null;
    const vals_str = std.mem.trim(u8, inner[vals_start..vals_end], " ");

    const key = structs.aliases.get(type_str) orelse return null;
    if (!structs.marshallable(key)) return null;
    const def = structs.defs.getPtr(key).?;

    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;
    // A struct-*literal* value can be `pub` (unlike a struct *type*), so an
    // importer can use the constant (e.g. `rl.RAYWHITE`) directly.
    try w.print("pub const {s} = {s}{{ ", .{ name, def.runic_name });

    // Split the value list on top-level commas (each value is `@as(c_int, N)`,
    // which itself contains a comma) and pair with the field names in order.
    var depth: usize = 0;
    var start: usize = 0;
    var count: usize = 0;
    var i: usize = 0;
    while (i <= vals_str.len) : (i += 1) {
        const at_end = i == vals_str.len;
        const ch = if (at_end) ',' else vals_str[i];
        if (!at_end and (ch == '(' or ch == '{')) depth += 1;
        if (!at_end and (ch == ')' or ch == '}')) depth -|= 1;
        if ((at_end or ch == ',') and depth == 0) {
            const piece = std.mem.trim(u8, vals_str[start..i], " ");
            if (piece.len == 0) break; // trailing comma
            if (count >= def.fields.len) {
                out.deinit();
                return null;
            }
            const value = unwrapIntValue(piece) orelse {
                out.deinit();
                return null;
            };
            if (count > 0) try w.writeAll(", ");
            try w.print(".{s} = {s}", .{ def.fields[count].name, value });
            count += 1;
            start = i + 1;
        }
    }
    if (count != def.fields.len) {
        out.deinit();
        return null;
    }
    try w.writeAll(" }");

    def.used = true;
    return try out.toOwnedSlice();
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
    var structs = Structs{
        .defs = std.StringHashMap(StructDef).init(allocator),
        .aliases = std.StringHashMap([]const u8).init(allocator),
        .type_aliases = std.StringHashMap([]const u8).init(allocator),
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    defer {
        var it = structs.defs.iterator();
        // Synthetic array structs own their fields in the arena, freed below.
        while (it.next()) |entry| if (!entry.value_ptr.synthetic) allocator.free(entry.value_ptr.fields);
        structs.defs.deinit();
        structs.aliases.deinit();
        structs.type_aliases.deinit();
        structs.arena.deinit();
    }
    try collectStructs(allocator, zig_source, &structs);

    var constants = std.ArrayList([]u8).empty;
    defer {
        for (constants.items) |c| allocator.free(c);
        constants.deinit(allocator);
    }
    // Struct-valued `#define`s (raylib's named colors, …). Emitted after the
    // struct types they construct, which must be in scope.
    var struct_constants = std.ArrayList([]u8).empty;
    defer {
        for (struct_constants.items) |c| allocator.free(c);
        struct_constants.deinit(allocator);
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
        // Skip over a struct block's body (collected already); its inner lines
        // must not be mistaken for constants.
        if (structHeaderName(line) != null) {
            while (lines.next()) |raw_body| {
                if (std.mem.eql(u8, std.mem.trim(u8, raw_body, " \t\r"), "};")) break;
            }
            continue;
        }
        if (std.mem.startsWith(u8, line, "pub extern fn ")) {
            if (try formatExternFn(allocator, &structs, line)) |formatted| {
                try externs.append(allocator, formatted);
            } else {
                skipped += 1;
            }
        } else if (std.mem.startsWith(u8, line, "pub const ")) {
            if (constName(line)) |name| {
                if (baseline.contains(name)) continue; // a predefined compiler macro
                if (structs.aliases.get(name) != null) continue; // a struct / its alias
            }
            if (try formatStructConstant(allocator, &structs, line)) |formatted| {
                try struct_constants.append(allocator, formatted);
            } else if (try formatConstant(allocator, line)) |formatted| {
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

    // Emit the by-value struct types referenced by a kept function (and the
    // structs they nest), each after its dependencies so it resolves in order.
    var struct_count: usize = 0;
    var it = structs.defs.iterator();
    while (it.next()) |entry| {
        if (!entry.value_ptr.used) continue;
        try emitStruct(w, &structs, entry.key_ptr.*, &struct_count);
    }
    if (struct_count > 0) try w.writeAll("\n");

    // Struct-valued constants, after the struct types they construct.
    for (struct_constants.items) |constant| try w.print("{s}\n", .{constant});
    if (struct_constants.items.len > 0) try w.writeAll("\n");

    try w.print("pub const {s} = cimport \"{s}\" {{\n", .{ options.binding_name, options.library_name });
    for (externs.items) |extern_fn| try w.print("{s}\n", .{extern_fn});
    if (skipped > 0) {
        try w.print("    // skipped {d} function(s): non-marshallable struct, variadic, or an unmapped type\n", .{skipped});
    }
    try w.writeAll("}\n");

    return .{
        .source = try out.toOwnedSlice(),
        .extern_count = externs.items.len,
        .constant_count = constants.items.len + struct_constants.items.len,
        .struct_count = struct_count,
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
    // The `= c_uint` enum type alias is not emitted as a constant.
    try std.testing.expect(std.mem.indexOf(u8, s, "pub const Color = 0") == null);
    try std.testing.expect(std.mem.indexOf(u8, s, "skipped 2 function") != null);
}

test "generate emits by-value struct types and their functions" {
    const allocator = std.testing.allocator;
    const zig_source =
        \\pub const struct_Color = extern struct {
        \\    r: u8 = 0,
        \\    g: u8 = 0,
        \\    b: u8 = 0,
        \\    a: u8 = 0,
        \\    pub const ColorToInt = __root.ColorToInt;
        \\};
        \\pub const Color = struct_Color;
        \\pub const struct_Vector2 = extern struct {
        \\    x: f32 = 0,
        \\    y: f32 = 0,
        \\};
        \\pub const Vector2 = struct_Vector2;
        \\pub const struct_Camera2D = extern struct {
        \\    offset: Vector2 = @import("std").mem.zeroes(Vector2),
        \\    zoom: f32 = 0,
        \\};
        \\pub const Camera2D = struct_Camera2D;
        \\pub extern fn GetColor(hex: c_uint) Color;
        \\pub extern fn ColorToInt(c: Color) c_int;
        \\pub extern fn Add(a: Vector2, b: Vector2) Vector2;
        \\pub extern fn BeginMode2D(camera: Camera2D) void;
    ;
    var result = try generate(allocator, zig_source, .{ .library_name = "libraylib.so", .binding_name = "rl" });
    defer result.deinit(allocator);

    const s = result.source;
    // Marshallable structs used by a kept function are emitted with c.X fields.
    try std.testing.expect(std.mem.indexOf(u8, s, "const Color = struct { r: c.Char, g: c.Char, b: c.Char, a: c.Char }") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "const Vector2 = struct { x: c.Float, y: c.Float }") != null);
    // A nested struct is emitted, referencing its inner struct by name, and its
    // function is kept.
    try std.testing.expect(std.mem.indexOf(u8, s, "const Camera2D = struct { offset: Vector2, zoom: c.Float }") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn BeginMode2D(camera: Camera2D) c.Void") != null);
    // A struct's inner struct is declared before it (Vector2 before Camera2D).
    try std.testing.expect(std.mem.indexOf(u8, s, "const Vector2").? < std.mem.indexOf(u8, s, "const Camera2D").?);
    // Struct-by-value params and returns are kept, referencing the struct name.
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn GetColor(hex: c.UInt) Color") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn ColorToInt(c: Color) c.Int") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn Add(a: Vector2, b: Vector2) Vector2") != null);
    try std.testing.expectEqual(@as(usize, 4), result.extern_count); // GetColor, ColorToInt, Add, BeginMode2D
    try std.testing.expectEqual(@as(usize, 3), result.struct_count); // Color, Vector2, Camera2D
    try std.testing.expectEqual(@as(usize, 0), result.skipped_count);
}

test "generate follows a chained typedef to a marshallable struct" {
    const allocator = std.testing.allocator;
    // A struct reached only through a chain of typedefs
    // (`Texture2D = Texture = struct_Texture`) must still resolve.
    const zig_source =
        \\pub const struct_Texture = extern struct {
        \\    id: c_uint = 0,
        \\    width: c_int = 0,
        \\    height: c_int = 0,
        \\};
        \\pub const Texture = struct_Texture;
        \\pub const Texture2D = Texture;
        \\pub extern fn DrawTexture(texture: Texture2D, x: c_int, y: c_int) void;
    ;
    var result = try generate(allocator, zig_source, .{ .library_name = "libraylib.so", .binding_name = "rl" });
    defer result.deinit(allocator);

    const s = result.source;
    // Emitted once under the direct typedef name; the function references it.
    try std.testing.expect(std.mem.indexOf(u8, s, "const Texture = struct { id: c.UInt, width: c.Int, height: c.Int }") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn DrawTexture(texture: Texture, x: c.Int, y: c.Int) c.Void") != null);
    try std.testing.expectEqual(@as(usize, 1), result.extern_count);
    try std.testing.expectEqual(@as(usize, 0), result.skipped_count);
}

test "generate synthesizes a struct for a fixed array field and follows pointer typedefs" {
    const allocator = std.testing.allocator;
    const zig_source =
        \\pub const ModelAnimPose = [*c]f32;
        \\pub const struct_Material = extern struct {
        \\    flags: c_uint = 0,
        \\    pose: ModelAnimPose = null,
        \\    params: [4]f32 = @import("std").mem.zeroes([4]f32),
        \\};
        \\pub const Material = struct_Material;
        \\pub extern fn LoadMaterial() Material;
    ;
    var result = try generate(allocator, zig_source, .{ .library_name = "lib.so", .binding_name = "m" });
    defer result.deinit(allocator);

    const s = result.source;
    // A fixed array becomes a struct of that many identically-typed fields,
    // declared before the struct that uses it.
    try std.testing.expect(std.mem.indexOf(u8, s, "const Arr_4_f32 = struct { e0: c.Float, e1: c.Float, e2: c.Float, e3: c.Float }") != null);
    // A pointer typedef resolves to an opaque pointer; the array field uses the
    // synthesized struct.
    try std.testing.expect(std.mem.indexOf(u8, s, "const Material = struct { flags: c.UInt, pose: c.Ptr, params: Arr_4_f32 }") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "const Arr_4_f32").? < std.mem.indexOf(u8, s, "const Material").?);
    try std.testing.expect(std.mem.indexOf(u8, s, "extern fn LoadMaterial() Material") != null);
    try std.testing.expectEqual(@as(usize, 0), result.skipped_count);
}

test "generate emits compound-literal struct constants after their type" {
    const allocator = std.testing.allocator;
    const zig_source =
        \\pub const struct_Color = extern struct {
        \\    r: u8 = 0,
        \\    g: u8 = 0,
        \\    b: u8 = 0,
        \\    a: u8 = 0,
        \\};
        \\pub const Color = struct_Color;
        \\pub const RAYWHITE = @import("std").mem.zeroInit(CLITERAL(Color), .{ @as(c_int, 245), @as(c_int, 245), @as(c_int, 245), @as(c_int, 255) });
        \\pub const GOLD = @import("std").mem.zeroInit(CLITERAL(Color), .{ @as(c_int, 255), @as(c_int, 203), @as(c_int, 0), @as(c_int, 255) });
        \\pub extern fn ColorToInt(c: Color) c_int;
    ;
    var result = try generate(allocator, zig_source, .{ .library_name = "libraylib.so", .binding_name = "rl" });
    defer result.deinit(allocator);

    const s = result.source;
    // The color macro becomes a `pub` struct-literal const, pairing each value
    // with the struct's field names in order.
    try std.testing.expect(std.mem.indexOf(u8, s, "pub const RAYWHITE = Color{ .r = 245, .g = 245, .b = 245, .a = 255 }") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "pub const GOLD = Color{ .r = 255, .g = 203, .b = 0, .a = 255 }") != null);
    // The struct type is declared before the constants that construct it.
    try std.testing.expect(std.mem.indexOf(u8, s, "const Color = struct").? < std.mem.indexOf(u8, s, "RAYWHITE").?);
    try std.testing.expectEqual(@as(usize, 2), result.constant_count); // RAYWHITE, GOLD
}
