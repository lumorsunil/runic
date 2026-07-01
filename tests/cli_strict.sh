#!/usr/bin/env bash
#
# Verifies `--strict` mode: command failures (ExecutableError), which are exempt
# by default, must be handled. Runs a couple of inline scripts through the built
# binary with and without --strict.

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${RUNIC_REPO_ROOT:-$(cd "$script_dir/.." && pwd)}"
repo_root="$(cd "$repo_root" && pwd)"

if ! command -v zig >/dev/null 2>&1; then
  echo "zig is required for CLI strict tests but was not found on PATH." >&2
  exit 1
fi

(
  cd "$repo_root"
  zig build >/dev/null
)

runic_bin="$repo_root/zig-out/bin/runic"
tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

# A bare command whose failure is ignored.
bare="$tmp_dir/bare.rn"
printf 'fn Void @() Void\nls "/nonexistent-xyz-strict"\n' > "$bare"

# The same command, handled.
handled="$tmp_dir/handled.rn"
printf 'fn Void @() Void\nconst r = ls "/nonexistent-xyz-strict" catch "handled"\n' > "$handled"

run_status() {
  set +e
  (cd "$repo_root" && "$runic_bin" "$@") >/dev/null 2>&1
  local s=$?
  set -e
  echo "$s"
}

# Default mode: a bare command failure is exempt (runs, exit 0).
echo "-- bare command, default mode (expect exit 0)"
if [[ "$(run_status "$bare")" != "0" ]]; then
  echo "FAIL: bare command should be exempt without --strict" >&2
  exit 1
fi

# Strict mode: the same bare command is a compile error (exit 1).
echo "-- bare command, --strict (expect exit 1)"
if [[ "$(run_status --strict "$bare")" != "1" ]]; then
  echo "FAIL: bare command should be a compile error under --strict" >&2
  exit 1
fi

# Strict mode: handled command passes.
echo "-- handled command, --strict (expect exit 0)"
if [[ "$(run_status --strict "$handled")" != "0" ]]; then
  echo "FAIL: handled command should pass under --strict" >&2
  exit 1
fi

echo "CLI strict-mode tests passed (3 cases)."
