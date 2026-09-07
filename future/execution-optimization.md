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

## Phase 0 findings (done)

- **Root cause of the call-in-loop blow-up, part 1 (fixed):** `spawnThread`
  heap-allocates each thread's `IRPrivateContext` (holding its `stack` and
  `subshell_context_stack`). `deinit` frees those, but only for threads still in
  the list; `removeThreadsSlatedToBeRemoved` `swapRemove`d a finished thread
  **without freeing its private context** — so every reaped forked call (one per
  function call) leaked its private context and stacks. Fixing this
  (`freeThreadPrivate` on removal) took the captured-call benchmark at N=8000
  from **15 s / 686 MB to 0.01 s / 2.3 MB**. Also fixed a latent bug there: the
  pipe-thread branch called `orderedRemove` on the wrong list (`threads` instead
  of `pipe_threads`).
- **Root cause part 2 (structural, not fixed):** the shared `heap` is
  append-only (`alloc` bumps `current_heap_addr`, never reclaims). Each call
  allocates closure + result slots that are never freed, so very large call
  counts still blow up (N=1,000,000 captured calls → ~15 GB). This is the
  lifetime problem the Phase 2 fork-free path avoids by not allocating a closure
  per call at all; a general fix would need heap reclamation / a different
  ownership model (see [[result-model-rethink]]).
- Added `tests/benchmarks/call_heavy.{rn,sh}` — the call-in-loop benchmark that
  was missing (only compute/command/mixed existed), so this stays measured.

## Phase 2 design: the sync call/return convention (agreed, next to build)

The classifier landed (`src/semantic/effects.zig`). Wiring it into a fork-free
lowering requires a VM change, because of how the VM actually models calls:

**VM reality (why calls fork today).** A thread models exactly *one* function
activation. Its stack begins with `[stdin, stdout, stderr, closure]` at slots
0–3 (set once at `fork`); `.closure` addressing is hardcoded to `stack[3]`, so a
function's arguments/captures are read from there. `.sf`/`.sc` provide only
*block* frames within that one activation (`pushFrame`/`popFrame` save `.sf`).
A result is produced by `yield`ing to the activation's stdout stream, collected
by the caller through a pipe. There is no intra-thread call stack — so the only
way to give a callee its own args + result channel is to `fork` a new thread.
That is the cost the sync path must avoid.

**The convention to add.**
1. New IR instructions `call target` and `ret`, plus a per-thread
   return-address stack in `IRPrivateContext` (each entry saves the return
   `instruction_counter`, `.sf`, and `.sc`). `call` pushes a return entry and
   jumps to `target`; `ret` pops it and restores. Recursion works for free.
2. A compiled **sync entry** instruction set per sync function: parameters are
   passed on the caller's stack as a new frame (not the heap `.closure`), read
   frame-relative via `.sf`; the single tail `yield expr` becomes `set %r = expr`
   then `ret`; block frames still work via `.sf`. No heap `alloc`, no pipe, no
   thread — so args are reclaimed on `ret`, which also fixes the append-only-heap
   growth for sync calls.
3. Call site: in `compileFunctionCall` (and the arg/struct-field/typed-value
   capture fast paths), when the callee `isSync` *and* has no closure captures
   *and* the arity matches, push args to a frame, emit `call sync_entry`, and
   take the result from `%r` — bypassing fork + closure + pipe + wait + dequeue
   entirely.

**v1 restrictions (widen later):** only sync functions with **no closure
captures** (pure leaves + params) take the sync path; capturing functions stay
on the fork path (this also neatly excludes the closure-capture-of-closeable FFI
blocker). Only a single tail `yield` to fd 1 — refine the classifier to treat a
`yield` to fd 2 (stderr) as threaded, since sync mode has no stderr stream.

**Validation targets:** `tests/benchmarks/call_heavy.rn` should collapse toward
inline speed and flat memory; `tests/features/recursive_regression.rn` must stay
correct; full CI green. Build it in tested increments:

- [x] **(a) `call`/`ret` + return stack, in isolation** — done. New IR
  instructions `call`/`ret`, a per-thread `call_stack` of `{return_addr, sf,
  sc}`; `ret` resizes the stack back to reclaim the callee frame. Unit-tested via
  `runInstruction` (save/restore, reclamation, LIFO recursion, empty-stack
  error). No emission yet.
- [ ] **(b) sync-entry emission for a nullary leaf.** The compiler-side work,
  and the harder part. A normal instruction set reserves slots 0–3 for
  stdin/stdout/stderr/closure (`addInstructionSet` does `rel_stack_counter +=
  4`) and a `yield` writes to `threadStdout()` (== `stack[1]`). A **sync entry**
  must NOT reserve those (it runs on the caller's stack, entered by `call` which
  pushes no I/O slots), and its single tail `yield expr` must compile to `set
  %r = expr` then `ret` — not a stdout write. So this is a genuine second
  lowering mode for a function body, not a reuse of the existing one. Gate the
  call site on `effects.isSync` + no captures + arity match.
- [ ] (c) params passed on the stack frame (read frame-relative, not `.closure`).
- [ ] (d) recursion end-to-end (`recursive_regression`).
- [ ] (e) wire the arg / struct-field / typed-value capture fast paths.

## Open design questions (for when Phase 2 starts)

- Sync call/ret convention vs inline-only for the first milestone (recursion
  needs the convention).
- Exact single-yield rule (tail-position only for v1? how to treat a `yield`
  guarded by an `if`?).
- Whether commands can ever be made synchronously awaitable inside a sync body
  (would widen eligibility a lot; risky — deferred).
- How the classification is surfaced (metadata on the instruction set / fn_ref).
