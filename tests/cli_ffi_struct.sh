#!/usr/bin/env bash
# End-to-end regression for C structs passed and returned by value through the
# `cimport` FFI: flat structs, nested structs, fixed-array fields (as a struct
# of that many fields), and round-tripping a returned struct back as an argument.
# A fixture C library is built with `zig cc`, so the test needs no system lib
# with a convenient by-value function.

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${RUNIC_REPO_ROOT:-$(cd "$script_dir/.." && pwd)}"
repo_root="$(cd "$repo_root" && pwd)"

if ! command -v zig >/dev/null 2>&1; then
  echo "zig is required for the struct FFI test but was not found on PATH." >&2
  exit 1
fi

(
  cd "$repo_root"
  zig build >/dev/null
)
runic_bin="$repo_root/zig-out/bin/runic"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

cat > "$tmp/fixture.c" <<'EOF'
#include <stdint.h>

typedef struct { unsigned char r, g, b, a; } Color;
typedef struct { float x, y; } Vector2;
typedef struct { Vector2 offset; Vector2 target; float zoom; } Camera;
typedef struct { int id; float params[4]; } Material;

// flat struct argument
int color_sum(Color c) { return (int)c.r + c.g + c.b + c.a; }
// flat struct return
Color make_color(int r, int g, int b, int a) {
    Color c = { (unsigned char)r, (unsigned char)g, (unsigned char)b, (unsigned char)a };
    return c;
}
// nested struct argument
float cam_sum(Camera c) { return c.offset.x + c.offset.y + c.target.x + c.target.y + c.zoom; }
// nested struct return
Camera make_cam(float ox, float oy, float tx, float ty, float zoom) {
    Camera c = { { ox, oy }, { tx, ty }, zoom };
    return c;
}
// fixed-array struct field argument
float mat_sum(Material m) { return m.params[0] + m.params[1] + m.params[2] + m.params[3] + (float)m.id; }
EOF

zig cc -shared -fPIC -o "$tmp/libfixture.so" "$tmp/fixture.c"

cat > "$tmp/struct.rn" <<EOF
const c = import "std/ffi.rn"

const Color = struct { r: c.Char, g: c.Char, b: c.Char, a: c.Char }
const Vector2 = struct { x: c.Float, y: c.Float }
const Camera = struct { offset: Vector2, target: Vector2, zoom: c.Float }
const Arr_4_f32 = struct { e0: c.Float, e1: c.Float, e2: c.Float, e3: c.Float }
const Material = struct { id: c.Int, params: Arr_4_f32 }

const lib = cimport "$tmp/libfixture.so" {
    extern fn color_sum(color: Color) c.Int
    extern fn make_color(r: c.Int, g: c.Int, b: c.Int, a: c.Int) Color
    extern fn cam_sum(cam: Camera) c.Float
    extern fn make_cam(ox: c.Float, oy: c.Float, tx: c.Float, ty: c.Float, zoom: c.Float) Camera
    extern fn mat_sum(m: Material) c.Float
}

// flat struct argument
echo "color_sum=\${lib.color_sum Color{ .r = 10, .g = 20, .b = 30, .a = 40 }}"

// flat struct return + field access + round-trip as argument
const col = lib.make_color 1 2 3 4
echo "make_color=\${col.r} \${col.g} \${col.b} \${col.a}"
echo "roundtrip=\${lib.color_sum col}"

// nested struct argument
const cam = Camera{ .offset = Vector2{ .x = 1.0, .y = 2.0 }, .target = Vector2{ .x = 3.0, .y = 4.0 }, .zoom = 5.0 }
echo "cam_sum=\${lib.cam_sum cam}"

// nested struct return + nested field access + round-trip
const built = lib.make_cam 10.0 20.0 30.0 40.0 50.0
echo "make_cam=\${built.offset.x} \${built.target.y} \${built.zoom}"
echo "cam_roundtrip=\${lib.cam_sum built}"

// fixed-array struct field
echo "mat_sum=\${lib.mat_sum Material{ .id = 100, .params = Arr_4_f32{ .e0 = 1.0, .e1 = 2.0, .e2 = 3.0, .e3 = 4.0 } }}"
EOF

out="$("$runic_bin" "$tmp/struct.rn" 2>&1)"

check() {
  if ! grep -qF -- "$1" <<<"$out"; then
    echo "FAIL: expected to find: $1"
    echo "--- output ---"
    echo "$out"
    exit 1
  fi
}

check 'color_sum=100'
check 'make_color=1 2 3 4'
check 'roundtrip=10'
check 'cam_sum=15'
check 'make_cam=10 40 50'
check 'cam_roundtrip=150'
check 'mat_sum=110'

echo "CLI struct FFI test passed."
