#!/usr/bin/env bash
#
# Smoke-runs every showcase script in examples/ and asserts a clean exit, so the
# curated examples can't silently rot. (The `*_inner.rn` helpers are excluded —
# they expect arguments / are invoked by other tests.)

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${RUNIC_REPO_ROOT:-$(cd "$script_dir/.." && pwd)}"
repo_root="$(cd "$repo_root" && pwd)"

if ! command -v zig >/dev/null 2>&1; then
  echo "zig is required for CLI example tests but was not found on PATH." >&2
  exit 1
fi

(
  cd "$repo_root"
  zig build >/dev/null
)

runic_bin="$repo_root/zig-out/bin/runic"
if [[ ! -x "$runic_bin" ]]; then
  echo "runic binary was not built at $runic_bin" >&2
  exit 1
fi

mapfile -t examples < <(
  cd "$repo_root" >/dev/null
  find examples -type f -name '*.rn' ! -name '*_inner.rn' -print 2>/dev/null | LC_ALL=C sort
)

if [[ ${#examples[@]} -eq 0 ]]; then
  echo "CLI example tests skipped: no scripts found under examples/."
  exit 0
fi

count=0
for example in "${examples[@]}"; do
  echo "-- running ${example}"
  out_tmp="$(mktemp)"
  set +e
  (cd "$repo_root" && "$runic_bin" "$example") >"$out_tmp" 2>&1
  status=$?
  set -e
  if [[ "$status" != "0" ]]; then
    echo "Example ${example} exited with ${status}, expected 0:" >&2
    cat "$out_tmp" >&2
    rm -f "$out_tmp"
    exit 1
  fi
  rm -f "$out_tmp"
  count=$((count + 1))
done

echo "CLI example tests passed (${count} scripts)."
