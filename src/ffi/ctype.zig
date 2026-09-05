//! The C ABI types used in a `cimport` `extern fn` signature (see
//! `future/c-ffi.md`). This is the shared vocabulary the IR compiler records
//! from the AST and the evaluator uses to build a libffi call — its `ffi_type`,
//! and how a Runic `Value` marshals to/from it.
const std = @import("std");

pub const CType = enum {
    int,
    uint,
    long,
    ulong,
    short,
    ushort,
    char,
    size_t,
    float,
    double,
    bool,
    str,
    ptr,
    void,

    /// The `std.ffi` member name (`c.Double` → "Double") for this C type, and
    /// the reverse. Keep in sync with `std/ffi.rn` and the type checker's
    /// `cTypeToRunic`.
    pub fn fromName(name: []const u8) ?CType {
        const map = std.StaticStringMap(CType).initComptime(.{
            .{ "Int", .int },
            .{ "UInt", .uint },
            .{ "Long", .long },
            .{ "ULong", .ulong },
            .{ "Short", .short },
            .{ "UShort", .ushort },
            .{ "Char", .char },
            .{ "SizeT", .size_t },
            .{ "Float", .float },
            .{ "Double", .double },
            .{ "Bool", .bool },
            .{ "Str", .str },
            .{ "Ptr", .ptr },
            .{ "Void", .void },
        });
        return map.get(name);
    }

    /// Whether values of this type are passed/returned in a general-purpose
    /// (integer/pointer) register vs a floating-point register. Informational
    /// for now; libffi handles the ABI classification itself.
    pub fn isFloat(self: CType) bool {
        return self == .float or self == .double;
    }
};

test "CType.fromName maps std.ffi member names, rejects others" {
    try std.testing.expectEqual(CType.double, CType.fromName("Double").?);
    try std.testing.expectEqual(CType.int, CType.fromName("Int").?);
    try std.testing.expectEqual(CType.size_t, CType.fromName("SizeT").?);
    try std.testing.expectEqual(CType.ptr, CType.fromName("Ptr").?);
    try std.testing.expectEqual(CType.void, CType.fromName("Void").?);
    try std.testing.expect(CType.fromName("Nonsense") == null);
    // Every enum tag must be reachable by some name (fromName total over tags).
    inline for (std.meta.fields(CType)) |f| {
        var found = false;
        const names = [_][]const u8{ "Int", "UInt", "Long", "ULong", "Short", "UShort", "Char", "SizeT", "Float", "Double", "Bool", "Str", "Ptr", "Void" };
        for (names) |n| {
            if (CType.fromName(n)) |ct| {
                if (@intFromEnum(ct) == f.value) found = true;
            }
        }
        try std.testing.expect(found);
    }
}
