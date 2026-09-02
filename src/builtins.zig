//! Registry of builtin pipeline functions (`parseInt`, `parseFloat`,
//! `parseBool`, `lines`). This is the single source of truth shared by the type
//! checker (which registers each builtin's signature) and the IR compiler (which
//! lowers each builtin and classifies its pipe boundaries). Adding a builtin is
//! a new entry in `all` plus, for a parse builtin producing a new scalar type,
//! the matching IR opcode in the compiler's dispatch.

const std = @import("std");
const ast = @import("frontend/ast.zig");

pub const Builtin = struct {
    name: []const u8,
    kind: Kind,
    output: Output,

    /// How the builtin transforms its stdin stream.
    pub const Kind = enum {
        /// Maps each input value to `output`, parsing text when needed
        /// (`parseInt`/`parseFloat`/`parseBool`). A bad parse is a `ParseError`.
        parse_map,
        /// Frames a byte stream into per-line values (`lines`). Never fails.
        framer,
    };

    /// The value type each input maps to.
    pub const Output = enum { integer, float, boolean, string };

    /// Whether the builtin can fail — a `parse_map` produces `ParseError!T`.
    pub fn fallible(self: Builtin) bool {
        return self.kind == .parse_map;
    }

    /// The concrete stdout type the builtin's stage produces per value.
    pub fn outputType(self: Builtin) ast.TypeExpr {
        return switch (self.output) {
            .integer => ast.TypeExpr.global(.integer),
            .float => ast.TypeExpr.global(.float),
            .boolean => ast.TypeExpr.global(.boolean),
            .string => string_type,
        };
    }
};

const byte_type = ast.TypeExpr{ .byte = .{ .span = .global } };

/// `String` is `[]Byte`.
pub const string_type = ast.TypeExpr{ .array = .{ .element = &byte_type, .span = .global } };

pub const all = [_]Builtin{
    .{ .name = "parseInt", .kind = .parse_map, .output = .integer },
    .{ .name = "parseFloat", .kind = .parse_map, .output = .float },
    .{ .name = "parseBool", .kind = .parse_map, .output = .boolean },
    .{ .name = "lines", .kind = .framer, .output = .string },
};

/// The builtin with the given name, or null if it names no builtin.
pub fn lookup(name: []const u8) ?Builtin {
    for (all) |b| {
        if (std.mem.eql(u8, b.name, name)) return b;
    }
    return null;
}
