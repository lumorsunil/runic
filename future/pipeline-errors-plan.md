# Pipeline error propagation — implementation plan

Living plan for the pipeline-error cluster (backlog #5 / #6 / #7 / #8 of
`future/error-handling-plan.md`). Branch: `error-pipeline` (worktree).

## Goal

Make `echo "1234" | parseInt catch 0` (and friends) work: error-union-typed
pipeline stages (`parseInt: ParseError!Int`) that **short-circuit** — a stage's
error skips past downstream stages to the nearest handler (trailing `catch`/
`||`, or the pipeline's bound result) — per the D7 model ("the type crossing
`|` is always `T`, never `E!T`; an error is a terminal event").

## Model recap (D7)

- A stage's stream type is `T`; an error aborts the stage and propagates.
- Today `parseInt` is `fn String parseInt() Int` and **hard-errors** (aborts the
  whole program) on bad input. It is a *map stage* (`compileParseMapStage`):
  loop { collect_stdin; if EOF break; parse; pipeWrite to stdout }.
- Pipelines fork each stage as a thread; the last stage's output is the
  pipeline's value (`compilePipeline` returns the last stage thread handle;
  capture goes through `compileExpressionWithCapture`'s byte/stream path).

## Phases

- [ ] **P1 — `E!T → T` pipeline boundary.** Let an error-union-typed stage feed
  a `T`-stage (payload crosses; error short-circuits). Type checker:
  `classifyPipeBoundary` + `validatePipeBoundary` accept upstream `E!T` vs
  downstream `T` (currently only `T → E!T`/`T → ?T`). Compiler: `classifyBoundary`
  / `boundaryUsesTypedTransport` keep **typed** transport for this case (so the
  `T` value still passes by-value, not stringified). Testable with a custom
  error-union-returning function stage feeding an `Int` stage on valid input.

- [ ] **P2 — mid-pipeline short-circuit.** When a stage reads an `.err` off `&0`
  (`collect_stdin`), forward it downstream and skip the stage body, so the error
  reaches the trailing position instead of corrupting an arithmetic/body op.
  (`compileParseMapStage` already loops on values; user/block stages need the
  guard.) Replaces today's hard-abort with structured propagation.

- [ ] **P3 — trailing pipeline result as error union.** `(pipeline) catch x` /
  `const r = pipeline`: surface the last stage's yielded error-union value so it
  is catchable — analogous to #12's typed value capture, but for a pipeline's
  final stage. Likely: typed-transport the final capture + dequeue the value.

- [ ] **P4 — flip `parseInt` / `parseFloat` to `ParseError!Int` / `…!Float`.**
  Add a builtin `ParseError` error set (like `ExecutableError`); `parse_int` /
  `parse_float` produce `.err` (`ParseError.Invalid`, payload = the input) on bad
  input instead of `Error.InvalidInt`; update the builtin function types. After
  this the spec example works end to end.

## Status / Notes

**2026-06-21 — P1 + P4 landed (foundation); P2/P3 remain. NOT merged to `error`
(the partial regresses unhandled bad-parse — see below).**

Done:
- **P4 (type + runtime):** `ast.TypeExpr.parseErrorType` (builtin `ParseError`
  set, payload-less `Invalid`). `parseInt`/`parseFloat` builtin types are now
  `ParseError!Int` / `ParseError!Float` (type-checker), registered `ParseError`
  as a global type. The `parse_int`/`parse_float` evaluator handlers produce a
  `.err{ "ParseError", "Invalid", null }` value on bad input instead of
  `Error.Invalid{Int,Float}` (no more hard abort).
- **P1 (boundary):** `classifyPipeBoundary` + `validatePipeBoundary` accept
  upstream `E!T` → downstream `T` (`short_circuit_error` kind). The compiler
  hardcodes `parseInt`/`parseFloat` as `Int`/`Float` stages
  (`stageStdoutType`, `classifyStageOutputKind`), so mid-pipeline typed
  transport is unaffected — **all existing valid-input parseInt/parseFloat
  pipeline tests still pass.**

Verified working: `const r = echo "10" | parseInt | doubler | inc` etc.;
`const r = echo "1234" | parseInt catch 0` → `r=1234` (ok value flows).

- **P3 (trailing-catch capture) — DONE.** `compileExpressionWithCapture` now
  detects a pipeline whose final stage yields an error union
  (`pipelineCaptureErrorUnionType`: parseInt/parseFloat → `ParseError!Int/Float`,
  or a user fn stage returning `E!T`), marks the capture's stdout pipe `typed`,
  and `pipe_dequeue`s the final value — returning it typed as the error union
  (reusing the byte path's 5-ref discipline, returning `%r`). So
  `const r = echo "abc" | parseInt catch 0` → `0`; `echo "1234" | parseInt catch 0`
  → `1234`; `… catch |e| match e { ParseError.Invalid => … }` works. Also
  registered the builtin `ParseError` set in the compiler's `error_sets`
  (`registerErrorSets`) so `ParseError.Invalid` is matchable. Tests:
  `parse_error_union_regression`. The obsolete hard-abort diagnostic fixtures
  (`parse{int,float}_invalid_runtime`) were removed (their premise is gone).

- **P2 (mid-pipeline short-circuit) — DONE for handled cases.** Arithmetic on an
  error value propagates it (`.ath` handler: `left/right == .err` → the error),
  so a mid-pipeline error flows *through* downstream arithmetic stages without
  crashing (previously `UnsupportedBinaryExpression`). Combined with P3's
  all-stages scan (`pipelineCaptureErrorUnionType` checks every stage for an
  error producer and uses the error producer's payload as the capture type's
  fallback when the final stage's output can't be inferred), a mid-pipeline
  error surfaces at the trailing `catch`/`try`: `echo "abc" | parseInt |
  { … yield n*2 } catch 99` → `99`; valid input → the computed value. Tests
  added to `parse_error_union_regression`.

**Resolved (Option A — compile-time enforcement, 2026):**
- **Unhandled errors are now a compile error**, not a runtime print. A bare
  statement whose result is a non-`ExecutableError` error (a bare error value, a
  call to an error-returning function, or a pipeline whose final stage yields an
  error union) is an `UnhandledError` — it must be `catch`/`try`/`||`'d. At the
  top level there is nothing to propagate to, so the error must be caught.
  Commands keep the exit-code model (`ExecutableError` exempt). Implemented in
  `runExpressionStatement` (`statementHasUnhandledError` / `isUnhandledErrorType`
  / `isExecutableErrorSet`). Diagnostics: `error_unhandled_call`,
  `error_unhandled_pipeline`.
- **Propagated-error observation**: `try` on an error now *yields* the error to
  the function's stdout (the same channel a yielded error uses) and halts,
  instead of `exit_with` (which lost the value). So a `try`-propagated error is
  observable by a capturing caller (`f catch x` / `match f { … }`) and chains
  through nested propagation and pipeline stages. (`compileTry`.)

**Pipeline-abort model (2026 — corrects the earlier "error rides the stream" mistake):**
- A yielded error now **aborts the pipeline**: the pipeline expression evaluates
  to that error, handled by a surrounding `catch`/`try`/`match`/`||`. Mechanism:
  `collect_stdin`, on dequeuing an `.err` from a typed pipe, **forwards it to the
  stage's stdout** (`thread.private.stack.items[1]`) so the error *leads* the
  stage's output stream — the consumer/capture reads it first and the whole
  pipeline evaluates to it, regardless of what the stage body does with the
  value. (The value is still returned to the body, so a value-op like `&0 * 2`
  propagates it rather than computing on `null`; `.ath` `.err`-propagation stays
  for that reason.) This **fixes the former comparison/`if` mis-evaluation** —
  `echo "abc" | parseInt | { if (&0 > 5) … } catch x` → the error, not a bogus
  branch. Type side: `resolveSubjectType` reports an error-producing pipeline's
  type as `E!<final stage's ok type>`, so `(… | parseInt | doubler | inc) catch 0`
  type-checks even though the final stage is `Int`. Tests in
  `parse_error_union_regression` (cases `i`–`l`).

**Remaining:**
**Design note — pipeline error semantics are bash `pipefail`-like (by design, not a limitation):**
bash runs a pipeline's stages **concurrently**; a failing stage does not start/stop
the others, and data already in flight still gets processed and written. `set -o
pipefail` makes the pipeline *report* failure if any stage fails, but still runs
every stage. Runic matches this: any stage that yields an error makes the whole
pipeline evaluate to that error (the saner `pipefail`-like default rather than
bash's "only the last stage's status counts"), while the other stages still run.

The only thing Runic *adds* over bash is that a pipeline is also a **value**: bash
keeps stdout (data) and exit status (error) on separate channels, but a Runic
pipeline-as-a-value must fold them into one. It does so by position:
- **value position** (`const r = pipe catch x`): the error *is* the value; the
  partial downstream output is captured and discarded (you asked for a value, it
  errored, you handle it).
- **statement position** (`echo "abc" | parseInt | doubler`): downstream output
  flows to stdout exactly as in bash (and the error currently prints too — the
  enforcement exemption below).

Consequences that follow from this (intended, bash-consistent):
- **Downstream stage bodies still execute** after the error leads the stream — a
  side-effecting downstream stage runs, just as bash's `pipefail` doesn't stop a
  downstream `rm`. Forcing producer-error to abort downstream would be a
  *departure* from bash (and re-introduces the halt/stack-divergence difficulty);
  not done.

**Remaining (genuine):**
- **Mid-pipeline transform exemption (enforcement).** A *bare* `echo "abc" |
  parseInt | doubler` statement is not a compile error (its ordinary result type
  is `Int`; only the `catch`-subject view is the error union). It prints the error
  and exits 0. Flagging it would force a `catch` on every `… | parseInt | …` and
  break the printing idiom (`catch` captures the output) — the trade-off accepted
  under Option A. Handled pipelines are fully correct.
- **Streaming mid-stream error**: in a multi-value stream where a *later* value
  errors, the forwarded error isn't first (earlier outputs precede it), so a
  single-value capture takes the first normal value. Inherent to single-value
  capture of a multi-value pipeline.

**Status:** merged to `error` (squashed) and built on since — `return` removed,
then Option A (compile-time enforcement of unhandled errors + propagated-error
observation), then `||`/`&&` discard/guard on error-producing calls/pipelines.
The cluster (#5/#6/#7/#8) is delivered; an *unhandled* error is now a compile
error (not a runtime print). `catch`/`try`/`match`/`||`/`&&` all observe errors
from values, calls, and pipelines.
