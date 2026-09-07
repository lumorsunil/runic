#!/usr/bin/env bash
# Smoke test for `runic cbind`: generate a Runic C-FFI binding from a C header
# and check the shape of the output (functions, enum values, #define constants,
# by-value struct types and their functions, and that variadic functions are
# skipped).

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${RUNIC_REPO_ROOT:-$(cd "$script_dir/.." && pwd)}"
repo_root="$(cd "$repo_root" && pwd)"

if ! command -v zig >/dev/null 2>&1; then
  echo "zig is required for the cbind CLI test but was not found on PATH." >&2
  exit 1
fi

(
  cd "$repo_root"
  zig build >/dev/null
)
runic_bin="$repo_root/zig-out/bin/runic"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

cat > "$tmp/foo.h" <<'EOF'
#define FOO_MAX 100
#define FOO_NAME "foo"
typedef enum { RED = 0, GREEN = 1, BLUE = 4 } Color;
int foo_add(int a, int b);
double foo_scale(double x);
const char *foo_name(void *handle);
void foo_free(void *handle);
typedef struct { int x; int y; } Point;
#define ORIGIN ((Point){ 0, 0 })
Point foo_make(int x, int y);
int foo_sum(Point p);
int foo_printf(const char *fmt, ...);
EOF

out="$("$runic_bin" cbind "$tmp/foo.h" --lib libfoo.so --name libfoo 2>/dev/null)"

check() {
  if ! grep -qF -- "$1" <<<"$out"; then
    echo "FAIL: expected to find: $1"
    echo "--- generated output ---"
    echo "$out"
    exit 1
  fi
}

check 'const c = import "std/ffi.rn"'
check 'pub const FOO_MAX = 100'
check 'pub const FOO_NAME = "foo"'
check 'pub const RED = 0'
check 'pub const BLUE = 4'
check 'pub const libfoo = cimport "libfoo.so" {'
check '    extern fn foo_add(a: c.Int, b: c.Int) c.Int'
check '    extern fn foo_scale(x: c.Double) c.Double'
check '    extern fn foo_name(handle: c.Ptr) c.Str'
check '    extern fn foo_free(handle: c.Ptr) c.Void'
# A by-value struct type is emitted (a plain `const`, not `pub` — the parser
# rejects `pub const X = struct {…}`), and its struct-arg / struct-return
# functions are kept.
check 'const Point = struct { x: c.Int, y: c.Int }'
check '    extern fn foo_make(x: c.Int, y: c.Int) Point'
check '    extern fn foo_sum(p: Point) c.Int'
# A compound-literal #define becomes a pub struct-literal constant.
check 'pub const ORIGIN = Point{ .x = 0, .y = 0 }'
# Only the variadic function is skipped now.
check 'skipped 1 function'

# The enum's underlying type alias (Color) is not emitted as a constant.
if grep -qE '(pub const Color|Color =)' <<<"$out"; then
  echo "FAIL: enum type alias 'Color' should not be emitted"
  echo "$out"
  exit 1
fi

echo "CLI cbind test passed."
