//! Thin binding over the statically-linked `libffi`, used to call C functions
//! whose signatures are only known at runtime — the core of the C FFI feature
//! (see `future/c-ffi.md`). `libffi` builds a call interface (`ffi_cif`) from a
//! runtime type list and performs the call (`ffi_call`), which is exactly the
//! runtime-signature problem an interpreted language faces.
//!
//! The richer marshalling layer (Runic `Value` ↔ C, per-extern `ffi_cif`
//! caching) is built on top of this in the IR/evaluator step. For now this
//! module exposes the raw `libffi` symbols and proves the mechanism links and
//! runs in-tree.
const std = @import("std");

/// The raw libffi C API (`ffi_cif`, `ffi_type`, `ffi_prep_cif`, `ffi_call`, and
/// the `ffi_type_*` globals). Resolved from the system header at build time;
/// the library itself is statically linked (see `build.zig`).
pub const c = @cImport({
    @cInclude("ffi.h");
});

test "libffi calls a C function through a runtime-built cif" {
    const S = struct {
        fn add(a: c_int, b: c_int) callconv(.c) c_int {
            return a + b;
        }
    };

    var cif: c.ffi_cif = undefined;
    var atypes = [_][*c]c.ffi_type{ &c.ffi_type_sint32, &c.ffi_type_sint32 };
    try std.testing.expect(c.ffi_prep_cif(&cif, c.FFI_DEFAULT_ABI, 2, &c.ffi_type_sint32, &atypes) == c.FFI_OK);

    var a: c_int = 3;
    var b: c_int = 4;
    var avalues = [_]?*anyopaque{ &a, &b };
    var result: c_int = 0;
    c.ffi_call(&cif, @ptrCast(&S.add), &result, &avalues);

    try std.testing.expectEqual(@as(c_int, 7), result);
}

test "libffi calls a dlopen'd libm function (pow) with doubles" {
    var lib = std.DynLib.open("libm.so.6") catch return error.SkipZigTest;
    defer lib.close();
    const pow_addr = lib.lookup(*anyopaque, "pow") orelse return error.SkipZigTest;

    var cif: c.ffi_cif = undefined;
    var atypes = [_][*c]c.ffi_type{ &c.ffi_type_double, &c.ffi_type_double };
    try std.testing.expect(c.ffi_prep_cif(&cif, c.FFI_DEFAULT_ABI, 2, &c.ffi_type_double, &atypes) == c.FFI_OK);

    var base: f64 = 2.0;
    var expn: f64 = 10.0;
    var avalues = [_]?*anyopaque{ &base, &expn };
    var result: f64 = 0;
    c.ffi_call(&cif, @ptrCast(pow_addr), &result, &avalues);

    try std.testing.expectEqual(@as(f64, 1024.0), result);
}
