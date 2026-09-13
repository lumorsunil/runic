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
- [x] **the jmp-fallback loop transient-alloc leak** — DONE (fix directions 1 &
  2 below). A `const`/computed loop body is now flat and fast: `const x=i+1;
  total+=x` runs 4M iterations in 0.87 s at 4.8 MB (was projected 14 GB).
- [x] **(d) recursion end-to-end** — DONE, and it was the last big win. Recursion
  was already fork-free and correct (only 1 OS `clone`; `recursion_value_regression`
  green) but ~11x slower per call than `call_heavy`'s sync calls: `fib 25` 27.6 s,
  `fib 30` 34.3 s. Root cause: a recursive sync call round-trips the round-robin
  scheduler after *every* instruction (the stdout/stderr stream-forward threads
  are always live, so `step()` never hits the single-thread fast path and services
  those idle threads per instruction — ~100 µs/call; without `</dev/null` it also
  triggered a stdin poll storm, but that was only ~10% of the cost). Fix: a sync
  `.call` in `step()` runs its whole subtree via `runSyncCallAtomic` (evaluator),
  driving the instruction counter through `call`/`ret`/`jmp` without yielding —
  the same technique `runCountedLoopBody` uses for loops. It fast-forwards only
  while instructions are scheduler-free (`instructionIsAtomicSafe`) and bails to
  normal scheduling at the first `pipe`/`fork`/`wait` — a sync entry can still
  contain a capture (a struct-param body like `magSq`), and running its `fork`+`wait`
  atomically would deadlock. Result: `fib 25` 27.6 s → **0.24 s**, `fib 30` 34.3 s
  → **3.6 s** (commit c854999). `call_heavy` and compute-loops were already fast
  via `counted_loop`; this extends the same atomic execution to recursion.
- [x] (e) UFCS / struct-param sync calls — DONE (the previous note was wrong: the
  fork wasn't a struct-field capture inside the entry — `magSq`'s entry is
  fork-free — it was the *call site*). `tryCompileSyncCall` required a bare
  identifier callee, so a UFCS method call `recv.method` (member callee) never
  sync-lowered and always forked. A struct-param sync fn is invoked that way
  (`p.magSq`), so it forked every call: 606 MB / 9.9 s at N=200000. Fix (commit
  ad69645): resolve the callee via `capturableCallInfo` (handles identifier *and*
  UFCS, prepending the receiver as frame slot 0 = `self`); arity checked against
  the full arg list; a field access (`p.x`) resolves to a non-fn name and bails.
  Result: 0.41 s / 5 MB (24x/120x), fork-free and atomic. Also fixed a correctness
  bug (a self-recursive UFCS method returned empty on the fork path). Test
  sync_call_ufcs_regression.rn. THEN (commit 172b966) the effects classifier:
  `callThreaded` blanket-threaded every member callee, so a fn wrapping a with-args
  UFCS sync call (`fn wrap(p) Int { yield p.scaledSum 2 }`) was threaded and forked
  (8.4 s / 713 MB). Since UFCS dispatches by method *name* to a free function,
  inherit that fn's effect for a `recv.method` callee (fields/builtins/module fns
  stay conservatively threaded — only a known-fn name is demoted, dup names already
  forced threaded → sound). wrap-with-args: 0.37 s / 5 MB. (A *nullary* UFCS `p.m`
  parses as a member node, not a call, so it was already non-threading.)
- [ ] (e-remaining) widen `syncReturnAllowed` for error unions once the sync return
  path carries the error discriminant (optionals already done, f51950f). A nullary
  UFCS call to a *threaded* fn is still classified sync (parses as a member node,
  bypassing `callThreaded`) — harmless today (its entry's fork makes
  `runSyncCallAtomic` bail to normal scheduling), but a latent looseness.

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

**Increment (d) — recursion, DONE for value-capture call sites.** A recursive
sync function now lowers to fork-free `call`/`ret` and recurses correctly (fib,
factorial, countdown), including inside a counted_loop. The classifier already
seeds candidates optimistically sync so self/mutual recursion converges, and
`syncBodyLowerable` already allows `if`/`match`/`block`; the missing piece was a
compile-ordering bug: `compileFnDecl` registered `sync_entries[fn] = entry` only
*after* `compileSyncEntry` compiled the body, so a self-call inside the body
looked up its own entry, found nothing, and fell back to fork (returning an
uncaptured thread handle → blank/wrong result, and deep fork-recursion then
failed outright). Fix: register the mapping inside `compileSyncEntry` right after
reserving the entry set, before compiling the body. `fib 10` in a binding →
55; `fib` summed over a loop → correct; all fork-free.

Measured: fib is now O(calls) with flat memory instead of forking a thread per
recursive call.

**Bare-call positions — DONE.** A sync call now lowers to `call`/`ret` in the two
value-consuming bare positions too, via `tryCompileSyncCall` hooks:
- **yield-value** (`yield (f n)`) — both the sync-mode branch (a nested sync call
  inside an entry, e.g. bare-`yield` recursion like `yield (countdown (n-1))`) and
  the normal branch (a forked body yielding a sync call's value to its stream).
- **command-argument** (`echo (f n)`) — the arg resolves to its entry and its
  value is materialized like any other argument.

**Statement position is deliberately left on the fork/output path.** A bare
statement call (`classify`) sends its `yield` to stdout — that is the value's
destination in statement position — so sync-lowering it (value → discarded `%r`)
would swallow the output. Confirmed by `if_bare_body_regression`. So the broad
`compileCall` hook was reverted in favor of the two value-position hooks above.
Covered by `tests/features/sync_call_bare_positions_regression.rn`.

**Increment (e), part 1 — optional returns, DONE.** `syncReturnAllowed` now
permits `?T`: an optional value rides back in `%r` with its null/present
discriminant intact, and its consumers (`if` capture, `orelse`, direct
interpolation, typed-pipe coercion) read it directly — no typed-transport needed.
Verified fork-free: a value-position `maybe n` call drops from 2 forks to 0 (only
the fixed stdio threads remain). Covered by
`sync_call_optional_return_regression.rn`; full suite green (174 smoke). Error
discriminants (error-union/error-set/sum/promise/execution) still fork — they
need the try/catch/match machinery the sync return path doesn't replicate yet.

**Counted loops with control flow — DONE (big win).** `instructionSetIsCountedLoopSafe`
now also allows `jmp`, so a for-loop body containing `if`/`else`/`else if`/`match`
or a nested `while` compiles to a `counted_loop` instead of dropping to the
general jmp loop. Safe because `runCountedLoopBody` follows the instruction
pointer: a lowerable body's only jumps are its own internal branch targets (labels
within the body set), so the runner walks them and still terminates by falling off
the end; anything that forks/waits/streams is absent (not whitelisted) and keeps
the body on the fork path. Combines with sync `call`/`ret` (a call inside an
`if`-branch inside the loop is fine).

Effect (if/else body, `total += (i>N ? 2 : 1)`, N=2 000 000, ReleaseFast): the
general jmp loop took **41 min / 15 GB** (per-iteration scheduler round-trips
through the general `step()` path accumulate); the counted_loop runs in **1.1 s /
5 MB**. Covered by `counted_loop_control_flow_regression.rn` (if/else, else-if,
match, while-in-for, value-position if, asymmetric branch temps, a sync call in a
branch). Full suite green (175 smoke).

Note: an optional call in a *loop* (`const v = maybe i orelse 0`) still does not
go flat. Root cause (investigated): a compound expression that merely *contains* a
call (`maybe i orelse 0`) reports `needs_stdio_capture = true` and
`compileExpressionWithCapture` forks the whole expression to capture a pipe — even
though the only call is a *sync* one that produces no stdio. A `fork` in the body
then disqualifies the counted_loop.

**Attempted and reverted (whack-a-mole — do not retry piecemeal).** Making
`callNeedsStdioCapture` return false for a call that has a sync entry does stop
the compound force-fork (optloop went to 0.0 s / 5 MB), but `needs_stdio_capture`
is *also* the flag every value context uses to choose between the sync-lowering
capture path (`compileExpressionWithCapture`, which runs `tryCompileSyncCall`) and
the plain `compileExpression` (no hook). Flipping it to false diverts sync-call
operands into the plain path, where they fork with the wrong type — regressing
arithmetic operands, `orelse` operands, and `${…}` interpolation
(`call_in_arithmetic_regression`, `array_element_typing_regression`). Patching each
value context (`compileArithmeticOperand`, `compileStableExpressionIntoRef`, …) to
re-add a sync check is unbounded — there are many such contexts.

The correct fix is systematic, not incremental: sync-lower a call in **one** place
(the call path itself, `compileCall`/`compileExpression`), so every value context
inherits it — which requires resolving the statement-position dual semantics first
(a bare statement call's `yield` is stdout output, not a discarded `%r`; a naive
`compileCall` hook swallows it — see `if_bare_body_regression`). Treat this as its
own focused project: give `compileCall` the sync hook, and have `compileStatement`
route a statement-position sync call's `%r` to stdout to preserve output.

**Runtime memory — the real driver was the tracer, DONE (big win).** The
"append-only heap" framing for deep-recursion memory was **wrong** (measured: fib
recursion leaves the heap at ~9 items and the runtime arena at ~1.3 MB). The
gigabytes were the **Tracer**: `Tracer.trace` unconditionally allocates a
`BasicTrace` + an `allocPrint`ed message and appends to `full_log` (never freed),
and `ReaderWriterStream.forward` calls it on every stream-thread `forward` — once
per scheduler round, i.e. per instruction in the general `step()` path. `full_log`
has no production consumer (only live echo reads traces), so gating `trace()` on
`echo_to_stdout` makes it a no-op when tracing is off (the debugger flips it on to
view traces). fib(22): 1.25 s / 1.12 GB → **0.33 s / 5.2 MB**; fib(28) now flat at
5 MB; the orelse fork-in-loop that used to run out of memory now completes.

**Fork-in-loop memory — investigated; NOT a heap-reclaim job (measured).** After
the tracer fix, the residual growth in a forking loop (`maybe i orelse 0`,
N=5000) breaks down as: heap ≈3 slots/iter, `thread_exit_codes` ≈3/iter, `pipes`
≈3/iter — all small — but the **runtime arena ≈10 KB/iter** (54 MB at N=5000).
The dominant cost is that each forked stage allocates a `ReaderWriterStream` +
buffers from the context allocator, and **the whole runtime runs on a
never-freeing `ArenaAllocator`** (`runIR`), so per-fork pipes/streams/closures
accumulate for the life of the program. Reclaiming just the reaped thread's
closure heap slots would recover ~1 % of this; the maps and pipe buffers are in
the arena and cannot be freed piecemeal.

So the real fix is architectural, not a bounded reclaim: either (a) give the
**runtime** a freeing allocator (or a resettable scratch arena for transient
per-fork resources) and free a reaped stage's pipes/streams/closure/map-entries,
or (b) make fewer things fork — the systematic sync-lowering call-path rework
above eliminates the fork (and thus the allocation) entirely for the common
compute cases. Treat (a) as its own project (the runtime currently relies on the
arena never freeing, so it needs a full free-correctness audit).

Remaining: (e) the rest — error-union/sum/promise returns (needs typed transport +
try/catch/match on the sync path); capture-bearing (closure) fns (the entry
requires `closure_captures.len == 0`); member/indirect calls stay conservatively
threaded.

## Runtime freeing-allocator rework — design notes (grounded in two probes)

**Current architecture.** `runIR` makes one `ArenaAllocator` (over the process
GPA) and uses it for *both* compile and runtime. Compile builds `IRSharedContext`
(instructions/data/labels/struct_types) — these must outlive the run. At runtime
the evaluator (via `runner.allocator`) and the context (`context.allocator`) share
that *same* arena and allocate threads, per-fork pipes/`ReaderWriterStream`s,
closures, the value heap, and the bookkeeping maps. The arena never frees, so all
of it lives to program end. `context.deinit` already contains correct, complete
teardown for every resource type — but every `.deinit(self.allocator)` in it is a
no-op because `self.allocator` is the arena; `arena.deinit()` does the real work
in one shot.

**Probe 1 (switch only `context.allocator` to the GPA):** immediate "Invalid
free" in `freeThreadPrivate`. Cause: the evaluator allocates a forked thread's
stack via `runner.allocator` (still the arena) while the context frees it via the
GPA. ⇒ the evaluator and context MUST share one runtime allocator.

**Probe 2 (switch both `runner.allocator` and `context.allocator` to the GPA):**
programs run correctly and there are **no invalid frees** — but the GPA reports
**leaks**: e.g. `materializeExecArgv` argv strings and `.zig_string` interpolation
dupes (`allocator.dupe(u8, …)`) are allocated and never freed. ⇒ the runtime is
written *leak-and-forget*; the arena is its safety net. A freeing switch is not a
one-liner: every such site needs a real lifetime.

**Options (each its own scoped effort):**
1. **Two arenas — persistent + resettable scratch (recommended).** Keep
   leak-and-forget. Persistent arena: main-thread-durable state (its stack,
   bindings, `shared`, the value heap, module cache). Scratch arena: per-fork
   transient (forked thread stacks/privates, per-fork pipes/streams/closures,
   transient dupes). Reset the scratch arena at a *quiescent point* — only the
   main thread live, `threads_to_remove` empty, no pipe threads — where every
   completed fork's machinery is provably dead. The audit is coarse (classify by
   allocation *site/purpose*, not per-object lifetime), plus one contained rule:
   a value that crosses the fork→main boundary (dequeued from a pipe / captured)
   must be copied into the persistent arena so it survives a reset. Bulk reclaim,
   no per-free correctness risk; wrong classification shows up as a dangling-read
   crash under the debug allocator (caught by tests), or as persistent growth.
2. **Full freeing rework.** One runtime allocator (freeing), plug every leak,
   free each reaped stage's pipes/closeables/map-entries incrementally. Most
   thorough (also fixes real leaks, enables long-running scripts) but the largest,
   riskiest audit — runtime values (`.zig_string`, slices) flow between threads
   and into bindings, so lifetimes are entangled.
3. **Targeted: pipes/streams only.** The measured bulk (~10 KB/iter) is per-fork
   `ReaderWriterStream`s. Give *pipes* a freeing allocator and `deinitParent` them
   when a stage's pipe is consumed/closed (machinery exists), leaving values on
   the arena. Narrowest; reclaims most of the fork-in-loop cost without touching
   value lifetimes — but needs a safe per-pipe "done" signal during the run.
4. **Reduce forks instead (orthogonal).** The sync-lowering call-path rework
   removes the fork — and thus the allocation — for the common compute cases; it
   does nothing for genuinely concurrent work (commands, real pipelines).

Recommendation: option 1 (two arenas) is the best effort/risk/reward — it reclaims
the bulk without a per-object free audit. Option 3 is a good smaller first step if
we want a bounded win before committing to the classification work.

### CHOSEN (2026-09-08): hybrid — RC-reclaim resources + scratch-arena values

Runic already has `RC(T)` (`src/mem/rc.zig`), and the stream types (pipes are
`ReaderWriterStream`) already carry an `RC(...).Ref`. Two reasons RC doesn't
reclaim today: the RC box is arena-allocated (its `deinit` free is a no-op), and
the `pipes` map holds a ref for the whole program so the count never hits 0.

Allocator layout for the runtime:
- **compile arena** — `shared` (instructions/data/labels/struct_types). Unchanged;
  persists to program end.
- **persistent** (freeing) — main-thread-durable state and any value that escapes
  the fork→main boundary; also backs the RC boxes for resources.
- **scratch arena** — per-fork/-iteration transients: forked thread stacks/privates,
  the transient part of the value heap, `.zig_string`/argv dupes. Reset in bulk at
  a *quiescent point* (only the main thread live, `threads_to_remove` empty, no
  pipe threads). Because these are bulk-reset, their leak-and-forget style needs
  **no** per-object free audit.
- **resources** (pipes/streams/closeables/cimports/subshell contexts/file sinks) —
  RC boxes allocated from `persistent`; each owner releases its ref when done, and
  the count reaching 0 frees the box. This is Tier 1.

The one cross-cutting rule: a value that crosses **fork→main** (dequeued from a
typed pipe, or a captured binding result) is copied into `persistent` so it
survives a scratch reset. The value heap is therefore split by address range into
a persistent region and a scratch region; a scratch reset truncates only the
scratch region.

**Phasing (each phase shippable + suite-green before the next):**
- **P0 — allocator seam.** Thread ONE `runtime_allocator` through the evaluator
  (`runner.allocator`) and context (`context.allocator`); keep compile on the arena.
  (Probe 2 showed this runs correctly but leaks — the leaks are exactly the values
  P2 moves to the scratch arena, so P0 lands with the runtime allocator = the
  compile arena still, i.e. a no-op refactor that just introduces the seam/param.)
- **P1 — Tier 1 RC resources.** Allocate resource RC boxes from a freeing allocator
  and release refs at the right points (thread reap releases its pipe refs; a pipe
  leaves the `pipes` map when disconnected from all ends). Needs a focused pass on
  pipe ref *ownership* (who holds strong refs vs the registry). Reclaims the
  measured ~10 KB/iter bulk. De-risk: fork-in-loop RSS should go flat; run under the
  debug allocator for double-free/leak.
- **P2 — scratch value heap.** Split the heap into persistent/scratch address
  ranges; route transient value allocs to scratch; add the fork→main copy-out;
  reset scratch at quiescent points. De-risk: dangling reads surface under the debug
  allocator + the feature/smoke suite.

Start with P1 (bounded, biggest single win, reuses RC). P2 is the follow-up.

### P1 investigation — pipe ownership (what the RC actually does today)

- `initReaderWriter` creates the `ReaderWriterStream` in an `RC` box with
  **refcount 1** (the self-ref stored in `.ref`), allocated from the context
  allocator (the arena). `addPipe` stores a **raw `*ReaderWriterStream`** in the
  `pipes` map keyed by handle — it does not take a counted ref.
- `connectSource`/`connectDestination` do **not** bump the count. Nothing does.
  So the count is always 1: RC is a single-owner box here, not multi-owner
  refcounting. Pipe *handles* are plain `usize`s inside `Value{.pipe=h}`, copied
  freely, so there is no general liveness tracking to hang a refcount on.
- `deinitParent` drops the ref (→ free), but is only called in `context.deinit`,
  and the arena makes that free a no-op. So pipes live to program end.

⇒ P1 is two concrete pieces, not "flip RC on":
1. **Freeing allocator for pipe boxes.** Allocate the `RC(ReaderWriterStream)`
   box (and its buffers) from a freeing allocator so `deinitParent` actually
   reclaims. (This is the P0 seam, scoped to pipes.)
2. **A `pipe_free` op + compiler-emitted teardown.** The compiler creates the
   per-stage plumbing pipes (stdout/stderr/merged for an exec/fork) as internal
   temporaries and already pops their stack refs at stage end — it *knows* those
   handles don't escape. Emit a `pipe_free h` there; at runtime it removes the
   handle from `pipes` (+ `typed_pipe_queues`/`consumed_pipes`) and `deinitParent`s
   the stream. Safety rests on the compiler only freeing pipes it created as
   non-escaping stage temporaries — never a pipe whose handle was bound/returned.

De-risk order: do (1) first (pipes on a freeing allocator; still freed only at
`deinit`) — pure refactor, suite stays green, no reclaim yet. **(1) is DONE
(commit bac0981).**

**Sub-step (2) as first specced — a fixed-point `pipe_free` — is UNSAFE. Do not
ship it.** Investigating it revealed why: the exec/fork result struct stores the
stdout/stderr/merged **pipe handles** in its `.stdout`/`.stderr`/`.merged` fields
(compiler ~line 2888), and `materializeString` reads a `.pipe` value's buffer
*lazily* (evaluator ~line 2828). The same handles are also how output **streams**
onward (`bash … | grep`). So a stage's pipes must live as long as the result
*value* that references them — which is first-class: it can be bound
(`const m = bash …; echo m.stdout`), stored in a struct, or returned. Freeing at
stage teardown would be a use-after-free; eager-materializing the buffers to
strings at stage end would break streaming. Pipe lifetime = the lifetime of the
`.pipe` **values** that reference the handle. There is no fixed IR point.

**Correct sub-step (2): refcount the pipe handle by its value references.** Keep
a per-handle count in the context. Retain when a `.pipe` value is stored into a
slot (`set`/`push`/heap-write/closure-write), release when a slot holding a
`.pipe` is overwritten or dropped (`pop`, `ret` stack truncation, rebind); at 0,
remove the handle from `pipes`/`typed_pipe_queues`/`consumed_pipes` and
`deinitParent` the stream. Pipes never reference pipes, so **no cycles** — RC is
complete here. The retain/release goes at the handful of central value-move sites
(each cheap-guarded by `value == .pipe`), so it does not touch integer-only hot
loops. Sub-step (1)'s freeing allocator is exactly what makes the count-0 free
reclaim. This is more like the Tier-2 discipline than a bounded op — but scoped
to `.pipe` values only, and cycle-free.

Implication for the plan: **pipes are not an independent "resource" tier** — they
are referenced by first-class values, so their reclaim is value-lifetime-driven,
same as the value tier. The hybrid's clean "RC resources vs scratch values" split
does not hold for pipes (you cannot escape-copy a live stream). Refcounting
`.pipe` values is the right mechanism; other pointer resources with no
value-reference (a subshell context, a file sink tied to a redirect) may still
suit a simpler owned-lifetime free.

**DECISION (2026-09-08): pipe reclaim PAUSED.** Sub-step (1) stays (commit
bac0981 — pipes on a freeing allocator, the correct foundation for either a later
RC or a simple owned free). Sub-step (2) is **not** pursued now: the dominant
runtime-memory cost was already removed (the tracer fix, commit 8bf8ff0), and the
remaining fork-in-loop cases are increasingly avoided by sync lowering / counted
loops. Value-lifetime refcounting of `.pipe` values (and, if unified, of all
boxed values) is the correct next mechanism but is a pervasive change touching the
central value-move sites — it deserves to be scoped as its own deliberate project
rather than bolted on here. Revisit when a real workload shows fork-in-loop memory
is the bottleneck. Higher-leverage bounded items to pick up first: the systematic
sync-lowering call-path rework (removes forks at the source) and in-place
non-forking `array.push` (quadratic + forking today).

### Measurement of loop heap/stack growth (decides whether P2 is worth it)

The "reset at program quiescence (only main thread)" trigger was **wrong**: the
three stdio stream threads live for the whole program, so that point is never
reached, and it wouldn't help a single long loop/recursion anyway. Before
building P2, measured heap+arena growth across representative loop shapes
(per-iteration deltas from N=1000→5000, or 5000→50000):

| loop shape | heap/iter | arena/iter | class |
|---|---|---|---|
| pure int `t+=i` (counted) | 0 | 0 | already flat |
| ref body `const x=i+1; t+=x` (counted) | 0 | 0 | already flat |
| sync call `const x=inc i; t+=x` (counted) | 0 | 0 | already flat |
| transient string `const s="x${i}"; t+=s.len` | ~3 slots | ~940 B | **garbage** |
| transient array `const a=.{i,i,i}; t+=a.len` | ~4 slots | ~270 B | **garbage** |
| array build `a = a.push i` | quadratic | quadratic + **forks** | pathology |

Conclusions:
- Compute in a counted_loop (int / ref / sync-call) is **already flat** — the
  biggest and most common case is done. Deep-recursion *stack* is inherent live
  data (bound it with tail-call frame reuse or a depth limit, not reclaim).
- **Garbage** = a transient value built and dropped inside a loop (string/array
  not kept). Real but moderate (~hundreds of B + a few slots/iter; ~GB only at
  N≥1e6). Reclaimable.
- `a.push` in a loop *was* a separate pathology (forked ~3 pipes/iter + copied the
  whole array each push → O(n²)). **FIXED** (commit below): the in-place
  linear-buffer path (`array_push_inplace`, amortized O(1), no fork) already
  existed but never fired at the **top level** (the script body was never analyzed
  by `analyzeLinearBuffers`) and `linScanExpr` bailed on a `for` loop whose source
  is a **range** (`0..N`) — so the analysis returned empty and every push copied.
  Two fixes: run `analyzeLinearBuffers` on the script statements, and handle
  `.range` in the scan. A top-level `var a = .{ }; for (0..1e6) { a = a.push i }`
  now runs in ~4.5 s / 136 MB (a live 1M-element array) instead of being
  effectively unrunnable. (A separate, pre-existing bug found while measuring this
  — `echo (build)` where `build` is a forking value-returning function yielded
  empty output — turned out to be the **command-argument capture path** handing
  the command a raw thread handle (materializes to "") and racing the still-running
  producer; only the slower array-loop producer exposed it. Fixed by capturing
  forking command args via `compileExpressionWithCapture`, like a binding does —
  see `command_arg_forking_value_regression.rn`.)

**Trigger fix for P2 (if built):** not program quiescence — a **high-water-mark**
scratch heap checkpointed at loop-iteration / call-frame boundaries (the
counted_loop is the natural driver: it already runs a net-neutral body per
iteration). Restore the mark at iteration end; a value that escaped this iteration
(bound to an outer var, accumulated, returned) is copied to persistent first.

**Reprioritization from the data:** P1 (RC resources / pipes) is confirmed
highest-value — pipes dominate every fork case (fork-in-loop AND `a.push`). The
value tier (P2) is real but moderate and can be **deferred**; when built, use the
checkpoint trigger above (full value-refcounting would also work and needs no
checkpoint, but costs per-boxed-value retain/release and leaks cycles — not
justified by the moderate garbage measured). A separate, high-value item surfaced:
**in-place non-forking `array.push`** (quadratic+forking today).

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

## Operand-capture correctness (found during the call-path investigation)

Sync lowering + stdio capture is hooked ad-hoc, so operand positions that call
bare `compileExpression` fork a producing operand and read a thread handle
instead of its value. Surveyed positions: arithmetic, interpolation, array
elements, index, if-condition, and command args (fixed b785dc4) all capture
correctly; **comparison** and **logical `&&`/`||`** did not.

- **Comparison — FIXED (commit 7b0eb88).** `compileComparisonBinary` now routes
  operands through `compileArithmeticOperand` like arithmetic does. Was a real
  wrong-answer bug: `const c = a < b` (a=3,b=5) yielded false. Guard:
  `tests/features/call_in_comparison_regression.rn`.
- **Logical `&&`/`||` with Bool call operands — FIXED (commit 8cbfe3a).** When an
  operand needs stdio capture, routing goes to `compileLogicalBinary(.value)`,
  which forked operands directly into the outer capture pipe, concatenating each
  operand's yielded bool (`yes && yes` → `"00"`). The fix adds a value path at
  the top of `.value` mode: when both operands are *Bool-valued* (a Bool-yielding
  call, a comparison, or a nested `!`/`&&`/`||`, via the recursive
  `exprIsBoolValued` predicate), capture each as a value and combine with `.log`
  — exactly like the non-capture value path. The result flows back through %r so
  the outer capture pipe is legitimately empty. Key boundaries that keep the
  change safe: a **command** operand (its output *is* the value) keeps the
  exit-code pipe path; **error-union** monadic `&&`/`||` route elsewhere
  (`compileLogicalOrValue`/`AndValue` via `lhs_captures_error`) and their
  operands aren't Bool-valued; and `.log` requires a Bool left (Int/exit-code is
  unsupported there, as for plain-var `a && b`), so non-Bool operands fall
  through to the existing lowering. Guard:
  `tests/features/call_in_logical_regression.rn`. This closes the operand-capture
  correctness gap for scalar values; command/exit-code logical semantics are
  unchanged.

## Open design questions (for when Phase 2 starts)

- Sync call/ret convention vs inline-only for the first milestone (recursion
  needs the convention).
- Exact single-yield rule (tail-position only for v1? how to treat a `yield`
  guarded by an `if`?).
- Whether commands can ever be made synchronously awaitable inside a sync body
  (would widen eligibility a lot; risky — deferred).
- How the classification is surfaced (metadata on the instruction set / fn_ref).
