#!/usr/bin/env bash

set -euo pipefail

repo_root="${RUNIC_REPO_ROOT:-${1:-}}"
lang="${RUNIC_LANG:-${2:-unknown}}"

if [[ -z "$repo_root" ]]; then
  echo "RUNIC_REPO_ROOT must be set (or passed as the first argument)." >&2
  exit 1
fi

# The optimized fast paths only manifest in ReleaseFast, so the guard is
# meaningful only against a release build. Other toolchains have no equivalent.
if [[ "$lang" != "zig" ]]; then
  echo "Benchmark guard stage skipped: only runs for the Zig runtime (RUNIC_LANG=zig)."
  exit 0
fi

if ! command -v zig >/dev/null 2>&1; then
  echo "zig command not found in PATH. Install Zig to run the benchmark guard." >&2
  exit 1
fi
if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 not found in PATH. Install Python 3 to run the benchmark guard." >&2
  exit 1
fi

cd "$repo_root"

# Build a fresh ReleaseFast binary (earlier stages may have left a debug build).
zig build -Doptimize=ReleaseFast

RUNIC_BIN="$repo_root/zig-out/bin/runic" python3 "$repo_root/scripts/bench_guard.py"
