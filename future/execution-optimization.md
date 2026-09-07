# Execution optimization: interpreter wins, then a fork-free "sync" path

Status: **planning** (agreed direction, not started). Owner: Lumor.

## Goal

Make compute-heavy, in-process Runic programs much faster, while keeping the
plain interpreter as the zero-startup default. A native "compile for speed"
mode is a *later* option; the backend choice is explicitly deferred. The
foundation for both faster interpretation and eventual native codegen is the
same **synchrony (effect) analysis** described below.

## Key framing (from benchmarks, ReleaseFast)

- Startup is already ~3 ms; the plain interpreter does ~9.5M simple loop
  iterations/sec. So "trade startup for speed" only pays off for long-running,
  **compute-bound, in-process** work. Shell-shaped scripts (spawn processes,
  pipe bytes) are dominated by `fork`/`exec`/syscalls and gain ~nothing from any
  of this.
- The motivating workloads are exactly what the struct + C-FFI features enable:
  numeric/vector math, data transforms, a raylib-style per-frame game loop.

### The decisive measurement — function calls in loops

Each loop iteration does `const x = inc i` (forces the current call path: fork +
closure alloc + typed pipe + wait + dequeue):

| N calls | time | peak RSS |
|--------:|------|----------|
| 1,000   | 1.85 s | 103 MB |
| 2,000   | 3.47 s | 181 MB |
| 4,000   | 8.73 s | 347 MB |
| 8,000   | 15.04 s | 686 MB |
| inline, 100,000 | ~2 s | ~80 MB |

Time is super-linear; memory is strictly linear and **never reclaimed**
(~85 KB retained *per call*). Function calls inside loops are effectively
unusable at scale today. This is the single strongest motivation for the plan.

## Architecture facts that shape the options

- The IR is a **high-level, shell-oriented stack+heap VM** (`fork`, `pipe`,
  `exec`, `wait`, `stream`, `collect_stdin`, `enter_subshell`, `cimport_call`,
  string/float/int builtins) — not an SSA/register machine. See
  `src/ir/instruction.zig`.
- Runtime values are **dynamically tagged** (`src/ir/value.zig`), but the
  frontend is statically typed and the compiler **already monomorphizes**
  generics per concrete type (`maybeSpecialize` / `active_specializations` in
  `src/ir/compiler.zig`). This is the key asset for unboxed fast paths and any
  future native codegen.
- Concurrency is **cooperative green threads + CSP pipes**: a call `fork`s a
  thread, `yield` writes to a pipe, pipeline stages are concurrent threads,
  round-robin scheduled (`context.thread_counter`, `advanceThreadCounter`,
  `waiting_for`). This is the hard constraint — "native"/fast can't be plain
  straight-line codegen; it must preserve or sidestep these semantics.
- The maintainer already optimizes hot paths: `singleThreadFastPathEligible`
  and `tryRunFastRangeLoop` in `src/ir/evaluator.zig`.

## The core idea: synchrony (effect) analysis + a fork-free lowering

Classify every function/block by effect via a **fixpoint over the static call
graph**. A body is **sync** iff it needs no concurrency *and* yields a single
value:

- **Async** (keep the thread path): a pipeline; `exec`/command execution;
  stdin-stream / `&0` consumption; backgrounding (`&`); **yielding more than
  once, or yielding inside a loop** (a generator/producer); calling any async or
  indirect (function-valued / `run`) callee.
- **Sync-safe** (allowed): pure compute; `cimport_call` (a blocking C call, not
  a thread); calls to other sync functions; `cd`/env/subshell side effects
  (synchronous — allowed, revisit if problematic).
- Conservative default: **unknown ⇒ async**. Recursion via fixpoint: assume
  sync, demote on any async op, iterate.

**Lowering.** A new `compileSyncExpression` compiles a fully-sync expression
tree (including nested sync calls) to direct arithmetic + calls that leave the
result in a register — **no `fork`, no closure, no capture pipe, no
`wait`/`dequeue`**. Two mechanisms, both wanted:

1. A **synchronous call/return convention** — small VM addition: a per-thread
   return-address stack, `call`/`ret` that run the callee's instruction set in
   the *same* thread; args on the thread stack/frame; the single `yield`/`return`
   leaves the value in `%r` and returns. General, handles recursion, no code
   blowup.
2. **Inlining** small sync leaves on top, specialized to concrete arg types
   (reuse `maybeSpecialize`), which then unlocks constant folding.

This sidesteps the whole capture apparatus that the recent argument /
struct-literal-field capture fixes feed into — those remain for the async path;
sync calls never touch them.

## Phasing

- **Phase 0 — Guardrails & the leak.** Add call-in-loop benchmarks (absent
  today) + a memory-growth assertion. Investigate whether closed threads/pipes/
  heap are reclaimed and whether the scheduler is O(threads)/step (the O(N²)
  smell). Possibly a standalone bug worth fixing.
- **Phase 1 — Broad interpreter wins.** IR peephole/const-folding; less stack
  churn; more fast-path superinstructions (accumulators, map/filter). Benefits
  the default path and every workload.
- **Phase 2 — Synchrony analysis + fork-free lowering** (the big win). (a)
  classification pass, conservative; (b) inlining spike to de-risk the VM; (c)
  the sync call/ret convention for recursion/generality; (d) extend to sync
  `if`/`match`/blocks in value position.
- **Phase 3 (future) — synchronous generators.** A generator/iterator type +
  state-machine lowering so a multi-yield producer consumed synchronously
  (`for x in gen()`) also skips threading. This is the "generator that doesn't
  need its own thread" idea.
- **Phase 4 (deferred) — native compile mode.** Keep the interpreter as the
  default; add opt-in `runic build` for faster execution, trading compile time.
  Backend decided later (leading candidate: transpile the sync IR subset to Zig
  and invoke the Zig toolchain, as `cbind` already does; LLVM-as-a-library and a
  hand-rolled emitter considered and disfavored for a Zig-only project). The
  Phase 2 synchrony classification is the prerequisite — the sync subset is
  exactly what compiles cleanly to native.

## Open design questions (for when Phase 2 starts)

- Sync call/ret convention vs inline-only for the first milestone (recursion
  needs the convention).
- Exact single-yield rule (tail-position only for v1? how to treat a `yield`
  guarded by an `if`?).
- Whether commands can ever be made synchronously awaitable inside a sync body
  (would widen eligibility a lot; risky — deferred).
- How the classification is surfaced (metadata on the instruction set / fn_ref).
