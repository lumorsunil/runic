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
  (`freeThreadPrivate` on removal) cut the captured-call benchmark at N=8000
  from **15 s / 686 MB to ~0.9 s / 660 MB** — a real fix (it stopped the
  per-reap leak of thread stacks). Also fixed a latent bug there: the pipe-thread
  branch called `orderedRemove` on the wrong list (`threads` instead of
  `pipe_threads`). (An earlier note claimed this reached 0.01 s / 2.3 MB; that
  was a mis-measurement against a cleared scratchpad file — the real post-fix
  figure is ~660 MB, because part 2 below still dominates a call-in-loop.)
- **Root cause part 2 (structural, not fixed) — the real remaining cost:** a
  loop runs its body as a **forked closure per iteration**, and the shared `heap`
  is **append-only** (`alloc` bumps `current_heap_addr`, never reclaims). So a
  call — or even a plain `const x = <computed>` — inside a loop still allocates
  per iteration and never frees (N=1,000,000 → ~15 GB). The Phase 2 sync call is
  fork-free and helps *non-loop* code greatly, but inside a loop the forked
  loop-body + append-only heap dominate both the sync and fork paths. **Making a
  loop run a sync body inline (no per-iteration fork) + heap reclamation is the
  next big win** (Phase 1 / loop work); see [[result-model-rethink]].
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
- [x] **(b) sync-entry emission for a nullary leaf** — done. `compileSyncEntry`
  emits a fork-free entry (no I/O/closure slots; a `set .sf = .sc` prologue bases
  its frame on the caller's stack top; `yield expr` → `set %r`/`ret` via a new
  sync branch in `compileYield`). `tryCompileSyncCall`, hooked at the top of
  `compileExpressionWithCapture`, emits `call <sync entry>` for a nullary sync
  callee and pads to `capture_temp_ref_count` to keep the capture path's stack
  contract. Gated on `effects.isSync` + nullary + capture-free +
  `syncReturnAllowed`. **v1 return-type restriction discovered:** error-union /
  optional / sum / execution returns need the typed-transport + try/catch/match
  machinery the sync return path doesn't replicate, so they stay on the fork
  path (`syncReturnAllowed`).
- [x] **(c) params on the stack frame** — done. `call` carries an `args` count;
  the frame base is `stacklen - args` (params are slots `0..N-1`, reclaimed by
  `ret`). The call site evaluates args into stable refs, pushes their values
  contiguously, then `call`s; `declareSyncParams` binds each param to its
  dereferenced frame slot. Eligibility tightened via `syncBodyLowerable` (a
  whitelist of body constructs — loops/pipelines/streams/try-catch/nested-fns
  fall back to fork), plus generic-`|T|` params/returns excluded (they
  monomorphize via the fork path). Correct for nested/branch/two-arg cases.
  **Did NOT speed up `call_heavy`** — see the critical finding below.
- [ ] **(next, higher priority than d/e) the jmp-fallback loop transient-alloc
  leak — the real blocker.** See "Critical finding" below.
- [ ] (d) recursion end-to-end (`recursive_regression`).
- [ ] (e) wire the arg / struct-field / typed-value capture fast paths; widen
  `syncReturnAllowed` (error unions/optionals) once the sync return path carries
  the discriminant.

## ROOT CAUSE FOUND: per-iteration stdin polling by the stdio stream threads

**It is not a memory leak — it is syscalls.** `strace -c` shows `poll` and
`read` scaling linearly (~8 each per loop iteration; N=3000→24k, N=6000→48k),
while `mmap`/`munmap` stay flat. `strace -k` pins the call:
`runInstruction → ReaderWriterStream.forward → PipeReader.stream → read(0,…,1)=0`
— i.e. **the stdin stream-forwarding thread busy-polls stdin (a `read(0)` that
returns EOF, then re-runs) once per scheduler round.**

Why loops split:
- Every program forks three background stdio threads (`fork <1:0>/<2:0>/<3:0>`
  → sets `stream [S0]/[S1]/[S2]`). The stdin one forwards stdin and never
  completes — its `forward` returns `.not_done`, so it re-polls forever, keeping
  ≥4 threads live (so `singleThreadFastPathEligible`, which needs exactly one
  thread, is never true here).
- `total += i` matches `tryRunFastRangeLoop`, which runs **all** iterations in
  one native loop **without yielding to the scheduler** — so the stdin thread
  barely runs → ~no syscalls → flat and fast.
- Any body needing a `ref` (any `const`/computed binding) drops to the general
  jmp loop, which **yields to the round-robin scheduler every iteration**;
  between iterations the scheduler runs the stdin thread, which does a
  `read(0)`+`poll` each round → ~8 syscalls/iter → the ~0.4 ms/iter slowdown.
  The "~20 KB/iter, freed at end" was transient io buffers for those reads, not
  a leak (teardown state is tiny; nothing scales through the interpreter
  allocator, the Value stacks stay cap=8, and context tables stay small).

**Fix directions (next task):**
1. Don't busy-poll stdin: when stdin is at EOF / not readable, the stream-forward
   thread should block or complete instead of re-running every scheduler round
   (`.not_done` → spin). This alone likely removes the per-round syscall.
2. Run a single-thread-safe loop body inline without a scheduler round-trip per
   iteration (widen the `counted_loop`/fast path to bodies containing
   `ref`/`ath`/`set`/sync-`call`, not just the one-ath shape) — so compute loops
   don't yield to the stdio threads at all. This is also exactly where the sync
   `call`/`ret` pays off (a fork-free call can run in that inline loop).

Both are tractable and well-scoped now that the cause is known. Only after this
does `call_heavy` show the sync-call win.

**Fix direction 1 — DONE.** The stdin stream-forward thread busy-polled because
`ReaderWriterStream.forward` re-`stream()`s every connected source each round,
and the stdin source stayed connected after EOF: the stdin stream ran with
`disconnect_source = false`, so the EndOfStream branch's `disconnectSources` was
a no-op and the *closed* source lingered in the list, re-polled forever
(`poll(0)`+`read(0)`=EOF each scheduler round). Fix: flip stdin's
`disconnect_source` to `true` (in `cmd/runic/run_script.zig`) so the closed
source is removed via the existing, well-tested EndOfStream→`disconnectSources`
path. Removal is safe because `propagate_eof_on_source_close` already closes and
disconnects the *destination* on the first EOF — so re-polling the stale source
could never deliver data anywhere; it was pure busy-poll.

A first attempt skipped any `isClosed()` source directly in the shared `forward`
loop; that hung command substitution (`const arr = .{ echo "one" }`): a command
source is closed by the child *exiting* **before** its pipe is drained, and
`forward` re-streaming it is exactly what drains the buffered bytes and yields
the terminal `.closed` that unblocks the consumer. `isClosed()` is therefore not
equivalent to "fully drained", so the fix must stay in stdin's config, not touch
shared `forward`.

Effect (ref-body loop `const x = i + 1; total = total + x`, N=200 000,
ReleaseFast, `</dev/null`):

| | time | peak RSS | syscalls |
|---|---|---|---|
| before | 5.0 s | 3.8 GB | ~8/iter `poll`+`read` (linear in N) |
| after  | 1.9 s | 1.7 GB | flat: 66 `poll`, 18 `read` total |
| ceiling (`counted_loop`, no `ref`) | 0.01 s | 4.9 MB | — |

~2.8× faster, ~2.6× less memory; the syscall storm is gone entirely. The
remaining gap to the ceiling is (a) the three stdio threads still *spin* in the
scheduler each round (now syscall-free, but still blocking `singleThreadFastPath`
and forcing a round-trip per iteration) and (b) the append-only heap never
reclaiming per-iteration slots — both belong to fix direction 2 below.

**Fix direction 2 (part a) — DONE.** Widened `counted_loop` to cover `ref`-body
loops (any `const`/computed binding). A `counted_loop` runs the whole loop in one
`runInstruction` call via `runAtomicInstructionSet`, never yielding to the
scheduler — so the stdio threads never run mid-loop (killing both remaining
costs). Previously `instructionSetIsCountedLoopSafe` excluded `.ref`/`.pop`/`.push`,
so a `const x = …` binding forced the general jmp loop. Two changes in
`src/ir/compiler.zig`:
1. Add `.ref`, `.pop`, `.push` to the counted-loop-safe whitelist.
2. **Frame alignment** (the subtle part): `addInstructionSet` gives the body its
   own compile frame based at 4 (a fresh activation reserves slots 0–3 for
   stdin/stdout/stderr/closure), but `runCountedLoop` runs the body in the
   *outer* runtime frame. So a body-local ref was numbered at slot 4 while the
   `.ref` instruction appends at the real outer stack top — the ref aliased the
   loop's limit slot and corrupted the loop (a `const x=i+1; total+=x` loop
   returned 1 instead of the sum). Fix: set the body frame's `rel_stack_counter`
   to `frame_before_body` so body-local refs are numbered from the outer top and
   line up with where `.ref` actually pushes them. Captured refs (`for_counter`)
   already carry outer addresses; `simple_exec` uses absolute slots 0–2; the
   language has no `break`, and any `if`/branch body emits `jmp` (not whitelisted)
   so conditional bodies still fall back correctly.

Effect (`const x = i + 1; total = total + x`, ReleaseFast, `</dev/null`):

| N | before fix 2 | after fix 2 |
|---|---|---|
| 200 000 | 1.9 s / 1.7 GB | 0.04 s / 5 MB |
| 1 000 000 | ~25 s / ~14 GB (orig) | 0.23 s / 5 MB |

Now at the `counted_loop` ceiling — flat memory, ~linear-fast time. Full suite
green (unit + 173 smoke + 57 diagnostics + 9 examples + FFI + strict-mode).

**Fix direction 2 (part b) — DONE.** Sync `call`/`ret` now runs fork-free inside
a counted_loop. A sync `call` returns `.cont_no_instr_counter_inc` (it jumps to
its entry set and returns via `ret`), which the flat `runAtomicInstructionSet`
rejects. Replaced it (for counted loops) with `runCountedLoopBody`, a mini
instruction-pointer loop: set the counter to the body set, run the instruction
at the counter, and on `.cont`/`.skip` advance it, on `.cont_no_instr_counter_inc`
follow whatever the instruction set it to (so `call`→entry and `ret`→caller both
work). The iteration ends when control falls off the end of the body set (a
trailing `ret` lands there too). Added `.call` to the counted-loop-safe
whitelist; the sync classifier guarantees the whole callee tree is sync
(ref/set/ath/call/ret, no yields), so nothing in the detour can block. The
counted_loop's own address is saved and restored around the loop (the body
runner clobbers the counter).

One real bug surfaced and was fixed: `counter_ptr` (the loop variable) is a
pointer into the value-stack ArrayList, and a call body grows the stack (args +
padding + locals, ~12 slots) past its capacity, reallocating the backing and
invalidating the pointer — so the post-body `counter_ptr.* += 1` wrote to freed
memory, the counter lost its first increment, and the loop ran one extra
iteration (a constant +1 in every sum). Fix: re-resolve the counter pointer
after the body. This also hardened part a (a ref body that pushes past capacity
had the same latent bug).

Effect (`const x = inc i; total += x`, `fn inc(n) { yield n+1 }`, ReleaseFast):

| N | before | after |
|---|---|---|
| 20 000 | 0.67 s / 566 MB | 0.01 s / 5 MB |
| 1 000 000 | (unusable) | 0.87 s / 5 MB (flat) |

`call_heavy` finally shows the sync-call win — fork-free calls, flat memory. Full
suite green (unit + 173 smoke + 57 diagnostics + 9 examples + FFI + strict-mode).

Remaining: (d) recursion (recursive sync fns — the driven loop already nests
call/ret, but the classifier must be allowed to sync-lower them), (e) capture
fast paths + widen `syncReturnAllowed`.

## (superseded) earlier framing: "jmp-fallback loop leaks ~20 KB/iter"

The reason `call_heavy` (and any compute-in-loop) is slow is **not** the call
convention and **not** a forked loop body. Measured cleanly (ReleaseFast):

| loop body (single-range `for (0..N)`) | 1M time | 1M peak RSS |
|---|---|---|
| `total = total + i` (no `ref`) | 0.09 s | 5 MB |
| `const x = i + 1; total = total + x` | ~25 s | ~14 GB |

Scaling of the `const` case is strictly linear: ~20 KB and ~0.4 ms **per
iteration**. Yet at teardown the process state is tiny (`heap.len=8`), so the
memory is **transient** — allocated and freed each iteration, but the allocator
retains the pages, so peak RSS (and time) grow linearly.

Root of the split:
- `total += i` compiles to a `counted_loop` (a tight native runtime loop,
  `tryRunFastRangeLoop`) because its body is `instructionSetIsCountedLoopSafe`
  (only `set`/`ath`/`cmp`/… — **no `ref`**).
- **Any** `const`/computed binding needs a `ref`, which disqualifies
  `counted_loop`, so the loop falls back to the general jmp loop that runs the
  body through `runInstruction` per instruction (the loop body IS inline — there
  is no per-iteration fork; the earlier "forked body" framing was wrong).
- Somewhere on that per-instruction path a ~20 KB transient allocation happens
  each iteration. **Not yet root-caused** — it is not an `alloc` in the IR
  (the compiled loop body is just `ref`/`ath`/`ath`/`pop`), so it is inside the
  runtime execution of those ops or the step loop. Next step: instrument the
  allocator (a counting wrapper) to find the ~20 KB/iter call site; candidates
  ruled out so far — the stack ArrayList (append+pop reuses capacity), the
  append-only heap (teardown `heap.len=8`), and per-instruction logging
  (compiled out / verbose-gated in ReleaseFast).

Fixing this is the highest-impact next task (it makes *all* compute-in-loops
fast, and only then does the sync call show its win in `call_heavy`). Options:
(a) find and remove the per-iteration transient allocation on the jmp loop path;
(b) widen `counted_loop` to accept `ref`/`call` bodies (run them in the tight
loop); (c) both.

## Open design questions (for when Phase 2 starts)

- Sync call/ret convention vs inline-only for the first milestone (recursion
  needs the convention).
- Exact single-yield rule (tail-position only for v1? how to treat a `yield`
  guarded by an `if`?).
- Whether commands can ever be made synchronously awaitable inside a sync body
  (would widen eligibility a lot; risky — deferred).
- How the classification is surfaced (metadata on the instruction set / fn_ref).
