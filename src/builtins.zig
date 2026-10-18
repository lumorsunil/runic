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
    /// For a `.limiter`, which limiter it is; null otherwise.
    limiter: ?Limiter = null,
    /// Number of value arguments the stage takes (`take n`, `until d` → 1).
    arity: u8 = 0,

    /// How the builtin transforms its stdin stream.
    pub const Kind = enum {
        /// Maps each input value to `output`, parsing text when needed
        /// (`parseInt`/`parseFloat`/`parseBool`). A bad parse is a `ParseError`.
        parse_map,
        /// Frames a byte stream into per-line values (`lines`). Never fails.
        framer,
        /// Reads a bounded prefix of the stdin stream and stops, closing
        /// upstream (`first`/`take`/`until`). Polymorphic: the stdout type comes
        /// from the upstream element type, not a fixed `output`.
        limiter,
    };

    /// Which stream limiter a `.limiter` builtin is.
    pub const Limiter = enum {
        /// One element, as a scalar: `?E` (null at EOF).
        first,
        /// Up to `n` elements: `[]E` (`String` for a byte stream).
        take,
        /// Elements up to (excluding) a delimiter: `[]E` (`String`).
        until,
    };

    /// The value type each input maps to.
    pub const Output = enum { integer, float, boolean, string };

    /// Whether the builtin can fail — a `parse_map` produces `ParseError!T`.
    pub fn fallible(self: Builtin) bool {
        return self.kind == .parse_map;
    }

    /// The concrete stdout type the builtin's stage produces per value. For a
    /// polymorphic `.limiter` this is only a placeholder for the static
    /// signature table; its real stdout type is resolved from the upstream
    /// element type by the type checker / IR compiler.
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
    .{ .name = "first", .kind = .limiter, .output = .string, .limiter = .first, .arity = 0 },
    .{ .name = "take", .kind = .limiter, .output = .string, .limiter = .take, .arity = 1 },
    .{ .name = "until", .kind = .limiter, .output = .string, .limiter = .until, .arity = 1 },
};

/// The builtin with the given name, or null if it names no builtin.
pub fn lookup(name: []const u8) ?Builtin {
    for (all) |b| {
        if (std.mem.eql(u8, b.name, name)) return b;
    }
    return null;
}
