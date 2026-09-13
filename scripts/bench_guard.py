#!/usr/bin/env python3
"""Performance-regression guard for Runic's optimized fast paths.

Each benchmark exercises one optimization whose failure mode is an
*order-of-magnitude* blow-up — a fork per call (hundreds of MB), a scheduler
round-trip per instruction (10-100x time), or an O(n^2) array copy (both). The
budgets below are therefore deliberately loose tripwires, not precise targets:
the fast path uses ~5-13 MB and well under a second, so a 60-120 MB / 8-10 s
budget has a 10-100x margin. That catches real regressions while staying immune
to machine speed and normal timing noise. If a *new feature* legitimately makes
one benchmark heavier, bump that entry's budget in the same change (and say so),
rather than letting the guard rot.

Peak RSS is read per-process via os.wait4 (exact, not cumulative). Wall time is
a coarse ceiling. A benchmark also fails if its output is wrong — a "fast but
incorrect" regression must not pass.

Run against a ReleaseFast build (the optimizations only manifest there):
    RUNIC_BIN=zig-out/bin/runic scripts/bench_guard.py
"""
import os
import subprocess
import sys
import time

# name -> (script, expected stdout (stripped), max peak RSS in MB, max wall time in s)
# Fast-path baselines (ReleaseFast) noted in the comments for context.
BENCHMARKS = [
    # counted_loop: a bare `total += i` loop runs in one atomic native loop.
    # Regression (falls to the general jmp loop) => 10-100x slower.
    ("counted_loop", "counted_loop_heavy.rn", "1999999000000", 80, 8.0),  # ~0.16s / 4.7MB
    # ref_loop: a `const`-body loop still counts as a counted_loop.
    # Regression => per-iteration scheduler round-trip / syscall storm.
    ("ref_loop", "ref_loop_heavy.rn", "500000500000", 80, 8.0),  # ~0.24s / 4.9MB
    # call_heavy: a scalar-param sync call in a loop is fork-free.
    # Regression (fork per call) => hundreds of MB.
    ("call_heavy", "call_heavy.rn", "200010000", 80, 8.0),  # ~0.01s / 4.7MB (fork: ~566MB)
    # recursion: a recursive sync call runs its subtree atomically.
    # Regression (scheduler round-trip per call) => ~100x slower (fib25 27s).
    ("recursion", "recursion_heavy.rn", "75025", 60, 8.0),  # ~0.22s / 5.2MB
    # struct_call: a UFCS struct-param sync call is fork-free.
    # Regression (UFCS not sync-lowered) => fork per call, hundreds of MB.
    ("struct_call", "struct_call_heavy.rn", "5000000", 120, 8.0),  # ~0.41s / 5.0MB (fork: ~606MB)
    # error_union: an error-union-returning sync fn is fork-free.
    # Regression (error union excluded from sync) => fork per call.
    ("error_union", "error_union_heavy.rn", "20000100000", 120, 8.0),  # ~0.31s / 4.9MB (fork: ~540MB)
    # array_push: `xs = xs.push e` grows in place (amortized O(1)).
    # Regression (loses in-place) => O(n^2) copies, slow and memory-churny.
    ("array_push", "array_push_heavy.rn", "50000", 120, 10.0),  # ~0.48s / 13.4MB
]

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BENCH_DIR = os.path.join(ROOT, "tests", "benchmarks")


def measure(runic_bin, script_path):
    """Run the benchmark once; return (stdout, peak_rss_mb, elapsed_s, exit_code)."""
    start = time.perf_counter()
    proc = subprocess.Popen(
        [runic_bin, script_path],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
    )
    out = proc.stdout.read()
    _, status, rusage = os.wait4(proc.pid, 0)
    elapsed = time.perf_counter() - start
    code = os.waitstatus_to_exitcode(status)
    # ru_maxrss is in KB on Linux, bytes on macOS.
    rss_kb = rusage.ru_maxrss if sys.platform != "darwin" else rusage.ru_maxrss / 1024
    return out.decode("utf-8", "replace").strip(), rss_kb / 1024.0, elapsed, code


def main():
    runic_bin = os.environ.get("RUNIC_BIN", os.path.join(ROOT, "zig-out", "bin", "runic"))
    if not (os.path.isfile(runic_bin) and os.access(runic_bin, os.X_OK)):
        print(f"missing runic binary: {runic_bin}", file=sys.stderr)
        print("build one first with: zig build -Doptimize=ReleaseFast", file=sys.stderr)
        return 2

    print(f"bench_guard: {runic_bin}")
    print(f"{'benchmark':<14} {'time_s':>7} {'/budget':>8} {'rss_mb':>8} {'/budget':>8}  result")
    failures = []
    for name, script, expected, mem_budget, time_budget in BENCHMARKS:
        path = os.path.join(BENCH_DIR, script)
        out, rss_mb, elapsed, code = measure(runic_bin, path)

        problems = []
        if code != 0:
            problems.append(f"exit={code}")
        if out != expected:
            problems.append(f"output {out!r} != {expected!r}")
        if rss_mb > mem_budget:
            problems.append(f"RSS {rss_mb:.0f}MB > {mem_budget}MB budget")
        if elapsed > time_budget:
            problems.append(f"time {elapsed:.2f}s > {time_budget}s budget")

        ok = not problems
        result = "ok" if ok else "FAIL: " + "; ".join(problems)
        print(f"{name:<14} {elapsed:>7.2f} {time_budget:>8.1f} {rss_mb:>8.1f} {mem_budget:>8.0f}  {result}")
        if not ok:
            failures.append(name)

    print()
    if failures:
        print(f"bench_guard FAILED: {', '.join(failures)}")
        print("A fast path regressed (a 10-100x blow-up), or output changed. If a new")
        print("feature legitimately owns the slowdown, raise that benchmark's budget here.")
        return 1
    print(f"bench_guard passed ({len(BENCHMARKS)} benchmarks within budget).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
