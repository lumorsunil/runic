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
const Allocator = std.mem.Allocator;

/// The raw libffi C API (`ffi_cif`, `ffi_type`, `ffi_prep_cif`, `ffi_call`, and
/// the `ffi_type_*` globals). Resolved from the system header at build time;
/// the library itself is statically linked (see `build.zig`).
pub const c = @cImport({
    @cInclude("ffi.h");
});

pub extern "kernel32" fn LoadLibraryW(lpLibFileName: [*:0]const u16) callconv(.winapi) ?std.os.windows.HMODULE;
pub extern "kernel32" fn FreeLibrary(hLibModule: std.os.windows.HMODULE) callconv(.winapi) i32;
pub extern "kernel32" fn GetProcAddress(hModule: std.os.windows.HMODULE, lpProcName: [*:0]const u8) callconv(.winapi) ?std.os.windows.FARPROC;

pub const CrossDynLib = struct {
    const os = @import("builtin").os.tag;

    pub const DynLib = switch (os) {
        .windows => DynLibWindows,
        else => DynLibLinux,
    };

    pub const Error = error{
        DynLibLoadFailed,
    };

    pub const DynLibWindows = struct {
        allocator: Allocator,
        handle: std.os.windows.HMODULE,

        pub fn open(allocator: Allocator, path: []const u8) !@This() {
            var path_w_buffer: [1024]u16 = undefined;
            const len = try std.os.windows.wtf8ToWtf16Le(&path_w_buffer, path);
            const path_w: [:0]u16 = @ptrCast(path_w_buffer[0..len]);
            path_w_buffer[len] = 0;

            return .{
                .allocator = allocator,
                .handle = LoadLibraryW(path_w) orelse return CrossDynLib.Error.DynLibLoadFailed,
            };
        }

        pub fn close(self: *@This()) void {
            _ = FreeLibrary(self.handle);
        }

        pub fn lookup(self: *@This(), symbol: []const u8) !?std.os.windows.FARPROC {
            const symbol_z = try self.allocator.dupeZ(u8, symbol);
            defer self.allocator.free(symbol_z);
            return GetProcAddress(self.handle, symbol_z);
        }
    };

    pub const DynLibLinux = struct {
        allocator: Allocator,
        lib: std.DynLib,

        pub fn open(allocator: Allocator, path: []const u8) !@This() {
            return .{
                .allocator = allocator,
                .lib = try .open(path),
            };
        }

        pub fn close(self: *@This()) void {
            self.lib.close();
        }

        pub fn lookup(self: *@This(), symbol: []const u8) !?*anyopaque {
            const symbol_z = try self.allocator.dupeZ(u8, symbol);
            defer self.allocator.free(symbol_z);
            return self.lib.lookup(*anyopaque, symbol_z);
        }
    };
};

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

test "libffi passes a struct by value through a runtime-built struct type" {
    const Color = extern struct { r: u8, g: u8, b: u8, a: u8 };
    const S = struct {
        fn sum(color: Color) callconv(.c) c_int {
            return @as(c_int, color.r) + color.g + color.b + color.a;
        }
    };

    // A runtime-built FFI_TYPE_STRUCT of four bytes, mirroring how the
    // evaluator marshals a Runic struct into a by-value C argument.
    var fields = [_][*c]c.ffi_type{
        &c.ffi_type_uint8, &c.ffi_type_uint8, &c.ffi_type_uint8, &c.ffi_type_uint8, null,
    };
    var color_type: c.ffi_type = .{ .size = 0, .alignment = 0, .type = c.FFI_TYPE_STRUCT, .elements = &fields };

    var cif: c.ffi_cif = undefined;
    var atypes = [_][*c]c.ffi_type{&color_type};
    try std.testing.expect(c.ffi_prep_cif(&cif, c.FFI_DEFAULT_ABI, 1, &c.ffi_type_sint32, &atypes) == c.FFI_OK);

    // libffi computes the field offsets for the target ABI; the byte fields
    // are packed at 0..3, matching the struct's natural layout.
    var offsets = [_]usize{0} ** 4;
    try std.testing.expect(c.ffi_get_struct_offsets(c.FFI_DEFAULT_ABI, &color_type, &offsets) == c.FFI_OK);
    try std.testing.expectEqualSlices(usize, &.{ 0, 1, 2, 3 }, &offsets);

    var color = Color{ .r = 10, .g = 20, .b = 30, .a = 40 };
    var avalues = [_]?*anyopaque{&color};
    var result: c.ffi_arg = 0;
    c.ffi_call(&cif, @ptrCast(&S.sum), &result, &avalues);

    try std.testing.expectEqual(@as(c.ffi_arg, 100), result);
}

test "libffi returns a struct by value through a runtime-built struct type" {
    const Color = extern struct { r: u8, g: u8, b: u8, a: u8 };
    const S = struct {
        fn make(r: c_int, g: c_int, b: c_int, a: c_int) callconv(.c) Color {
            return .{ .r = @intCast(r), .g = @intCast(g), .b = @intCast(b), .a = @intCast(a) };
        }
    };

    var fields = [_][*c]c.ffi_type{
        &c.ffi_type_uint8, &c.ffi_type_uint8, &c.ffi_type_uint8, &c.ffi_type_uint8, null,
    };
    var color_type: c.ffi_type = .{ .size = 0, .alignment = 0, .type = c.FFI_TYPE_STRUCT, .elements = &fields };

    var cif: c.ffi_cif = undefined;
    var atypes = [_][*c]c.ffi_type{ &c.ffi_type_sint32, &c.ffi_type_sint32, &c.ffi_type_sint32, &c.ffi_type_sint32 };
    try std.testing.expect(c.ffi_prep_cif(&cif, c.FFI_DEFAULT_ABI, 4, &color_type, &atypes) == c.FFI_OK);

    // The return buffer is read back at libffi's computed offsets, mirroring how
    // the evaluator rebuilds a Runic struct from a by-value return.
    var offsets = [_]usize{0} ** 4;
    try std.testing.expect(c.ffi_get_struct_offsets(c.FFI_DEFAULT_ABI, &color_type, &offsets) == c.FFI_OK);

    var r: c_int = 10;
    var g: c_int = 20;
    var b: c_int = 30;
    var a: c_int = 40;
    var avalues = [_]?*anyopaque{ &r, &g, &b, &a };
    var ret: Color = undefined;
    c.ffi_call(&cif, @ptrCast(&S.make), &ret, &avalues);

    const bytes: [*]const u8 = @ptrCast(&ret);
    try std.testing.expectEqual(@as(u8, 10), bytes[offsets[0]]);
    try std.testing.expectEqual(@as(u8, 20), bytes[offsets[1]]);
    try std.testing.expectEqual(@as(u8, 30), bytes[offsets[2]]);
    try std.testing.expectEqual(@as(u8, 40), bytes[offsets[3]]);
}

test "libffi passes a nested struct by value through a nested struct type" {
    const Vector2 = extern struct { x: f32, y: f32 };
    const Camera = extern struct { offset: Vector2, target: Vector2, zoom: f32 };
    const S = struct {
        fn sum(cam: Camera) callconv(.c) f32 {
            return cam.offset.x + cam.offset.y + cam.target.x + cam.target.y + cam.zoom;
        }
    };

    // The inner struct type is reused for both nested fields; the outer type
    // nests it — the shape the evaluator builds for `Camera { Vector2 …; … }`.
    var vec_fields = [_][*c]c.ffi_type{ &c.ffi_type_float, &c.ffi_type_float, null };
    var vec_type: c.ffi_type = .{ .size = 0, .alignment = 0, .type = c.FFI_TYPE_STRUCT, .elements = &vec_fields };
    var cam_fields = [_][*c]c.ffi_type{ &vec_type, &vec_type, &c.ffi_type_float, null };
    var cam_type: c.ffi_type = .{ .size = 0, .alignment = 0, .type = c.FFI_TYPE_STRUCT, .elements = &cam_fields };

    var cif: c.ffi_cif = undefined;
    var atypes = [_][*c]c.ffi_type{&cam_type};
    try std.testing.expect(c.ffi_prep_cif(&cif, c.FFI_DEFAULT_ABI, 1, &c.ffi_type_float, &atypes) == c.FFI_OK);

    var cam = Camera{ .offset = .{ .x = 1, .y = 2 }, .target = .{ .x = 3, .y = 4 }, .zoom = 6 };
    var avalues = [_]?*anyopaque{&cam};
    var ret: f32 = 0;
    c.ffi_call(&cif, @ptrCast(&S.sum), &ret, &avalues);

    try std.testing.expectEqual(@as(f32, 16), ret);
}
