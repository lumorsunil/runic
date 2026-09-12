# Flaky: block-producer pipeline + early-exiting consumer (EOF race)

Status: **investigated, not fixed**. Found while hardening the execution-optimization
work; this is a pre-existing concurrency bug, unrelated to those changes.

## Symptom

`tests/features/runtime_regression.rn` fails intermittently (~2–3%). The missing
line is always `right|7` from the `== block pipeline ==` section:

```
({
  echo "left|3"
  echo "right|7"
}) | grep "-m" "1" "right|7"
```

Expected `right|7`; occasionally empty.

## Minimal repro + rates (ReleaseFast/debug, 200 runs each)

| case | flake |
|---|---|
| `({echo a; echo b}) \| grep -m 1 b` (match the **last** line) | ~3% (6/200) |
| `({echo a; echo b}) \| grep b` (no `-m`) | 0 |
| `({echo a; echo b}) \| grep -m 1 a` (match the **first** line) | 0 |
| `({echo a; echo b}) \| cat` | 0 / 300 |
| `echo a \| grep a` (no block) | 0 |

## Mechanism (what's ruled in / out)

- **Producer side is fine.** `block | cat` delivers *both* lines 300/300 — the
  block always writes everything and the pipe always delivers it to a consumer
  that reads to true EOF.
- **Consumer side is the race.** It only flakes when the consumer (a) exits early
  (`grep -m 1`) and (b) needs a line produced by a **later** block statement.
  Matching the first line never flakes (the match happens before the window).
- A block runs its statements as **separate, sequentially-waited subprocesses**
  (IR: `fork <echo1>; wait; fork <echo2>; wait`). Between statement 1 finishing
  (its process reaped, its source on the inter-stage pipe removed) and statement 2
  starting, the downstream consumer's stdin transiently signals **EOF**; a
  to-true-EOF reader (`cat`) is unaffected, but a condition-terminating reader
  (`grep -m 1` that hasn't matched yet) treats the transient 0-byte read as EOF and
  exits with no match.
- The inter-stage pipe's `keep_open` is correct (it is TRUE during the block —
  note `keep_open 0` in IR dumps means *true*, per the bool→exit-code convention
  true=0). So this is **not** a `keep_open` timing bug; the transient EOF reaches
  the consumer's OS stdin through the per-statement subprocess fd lifecycle.

## Fix attempt (2026-09-12) — stopped; it is below the ReaderWriterStream layer

Tried to fix it; could not safely, and here is why, so the next attempt starts ahead:

- **It is consumer-side and a genuine no-match.** `… | grep -m 1 right|7 || echo
  NOMATCH` prints `NOMATCH` on the flaky runs (10/300) — grep exits non-zero, i.e.
  it read a stream that did **not** contain `right|7`. So the consumer receives a
  *truncated* stream (transient EOF after line 1), while `cat` on the same producer
  gets both lines 300/300.
- **Not an inter-statement window-duration issue.** Inserting `bash -c "sleep 0.1"`
  between the two echoes does **not** make it fail deterministically (9/10 still
  correct) — `grep` blocks correctly through a slow gap. The transient EOF is a
  momentary glitch at a specific instant, not "the gap is long enough to read EOF".
- **The ReaderWriterStream model says it should NOT EOF.** The inter-stage pipe is
  `keep_open=true` for the whole block (set false only at stage end, after both
  echoes — `keep_open 0` in dumps is *true*). With `keep_open=true`, `forward` with
  zero sources returns `.no_source` and does not close the destination, so the
  consumer's stdin write-end (held by runic as the pipe's destination) should stay
  open across the echoes. Each echo is spawned `.pipe` (a fresh OS pipe that runic
  reads and forwards), so an echo exiting does not close the consumer's stdin fd.
- **It is a Heisenbug.** Under `strace -f` the flake does not reproduce in 60 runs
  (tracing perturbs the timing). So it is a tight race that instrumentation hides —
  consistent with it living **below** the ReaderWriterStream layer, in the OS-fd /
  `std.process.spawn` juggling (dup2/close) as the block's sequential subprocesses
  are created, racing the consumer's read — not in the stream/keep_open logic.

Because it hides under tracing and the stream-level logic is provably correct, a
fix must target the OS-fd lifecycle, and any candidate can only be validated
statistically (≥1000 untraced runs before/after). I did not ship a speculative
change to the fd/spawn path — unverifiable root cause + high risk of shifting the
race or hanging. Left for a dedicated concurrency pass.

## Fix direction (deferred — deep, risky)

Keep the downstream consumer's stdin from signaling EOF across a multi-statement
block producer's sequential subprocesses — i.e. the inter-stage pipe's OS write
end (the fd handed to the consumer) must stay open continuously until the block
as a whole completes, not per-statement. This is in the stream-forward / OS-pipe-fd
lifecycle and is concurrency-sensitive (a naive change risks shifting the race or
hanging). It has no correctness impact on normal usage — only the determinism of
a regression test that exercises `grep -m 1` behind a block producer — so it is
left for a focused concurrency pass rather than fixed opportunistically. Do not
weaken the test to hide it; it correctly exposes a real bug.
