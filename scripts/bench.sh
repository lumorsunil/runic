#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REPS="${1:-10}"
RUNIC_BIN="${RUNIC_BIN:-$ROOT/zig-out/bin/runic}"
RUN_BASH="${RUN_BASH:-bash}"

if ! [[ "$REPS" =~ ^[0-9]+$ ]] || [[ "$REPS" -lt 1 ]]; then
  echo "usage: scripts/bench.sh [repetitions]" >&2
  exit 1
fi

if [[ ! -x "$RUNIC_BIN" ]]; then
  echo "missing runic binary: $RUNIC_BIN" >&2
  echo "build one first with: zig build -Doptimize=ReleaseFast" >&2
  exit 1
fi

run_bench() {
  local label="$1"
  local mode="$2"
  local path="$3"

  python3 - "$label" "$mode" "$path" "$REPS" "$RUNIC_BIN" "$RUN_BASH" <<'PY'
import statistics
import subprocess
import sys
import time

label, mode, path, reps, runic_bin, run_bash = sys.argv[1:]
reps = int(reps)

if mode == "runic":
    cmd = [runic_bin, path]
elif mode == "bash":
    cmd = [run_bash, path]
else:
    raise SystemExit(f"unknown mode: {mode}")

samples = []
for _ in range(reps):
    start = time.perf_counter()
    subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
    samples.append((time.perf_counter() - start) * 1000.0)

print(f"{label:24} {mode:5} mean_ms={statistics.mean(samples):8.3f} min_ms={min(samples):8.3f} max_ms={max(samples):8.3f}")
PY
}

echo "Benchmarking with $RUNIC_BIN"
echo "Repetitions: $REPS"
echo

run_bench "command_heavy" "runic" "$ROOT/tests/benchmarks/command_heavy.rn"
run_bench "command_heavy" "bash" "$ROOT/tests/benchmarks/command_heavy.sh"
echo
run_bench "evaluator_heavy" "runic" "$ROOT/tests/benchmarks/evaluator_heavy.rn"
run_bench "evaluator_heavy" "bash" "$ROOT/tests/benchmarks/evaluator_heavy.sh"
echo
run_bench "mixed_loop_exec" "runic" "$ROOT/tests/benchmarks/mixed_loop_exec.rn"
run_bench "mixed_loop_exec" "bash" "$ROOT/tests/benchmarks/mixed_loop_exec.sh"
