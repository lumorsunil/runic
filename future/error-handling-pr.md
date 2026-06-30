# PR: Structured error handling

Title and description for merging the `error` branch into `main`.

## Title

```
Structured error handling: error sets, unions, catch/try/||/&&/match, pipelines, and mandatory handling
```

## Description

Implements a complete, structured error-handling system for Runic — replacing
ad-hoc exit codes and `set -e` conventions with typed errors that flow uniformly
through values, function calls, and pipelines. Built up over several phases on
the `error` branch; full CI green (unit + 35 diagnostic fixtures + 95 CLI smoke
scripts).

### Highlights

- **Error sets & unions.** `const E = error { Bad, WithPayload: String }`;
  functions return `E!T` error unions, or `!T` to infer the set from the body.
  Construct values with `E.Variant` / `E{ .Variant = payload }`.
- **Handling: `catch`, `||`, `try`, `match`.** `catch` unwraps-or-handles
  (`|err|` binds the error), `||` discards to a fallback, `try` propagates,
  `match` dispatches on variants with payload capture and exhaustiveness checks.
- **Errors as values across the in-process boundary.** Function/pipeline results
  preserve the real error value (set/variant/payload) via typed in-process
  capture, so `catch`/`match`/`try`/`if`/`||`/`&&` operate on the structured
  error, not its flattened text. Extended to optional-returning functions.
- **Mandatory explicit handling.** An unhandled error is a **compile error** —
  it must be `catch`/`||`'d or propagated with `try`. `try` propagation must be
  *covered* by the enclosing function's error set (top-level `try` is rejected).
  Commands keep the exit-code model: `ExecutableError` is exempt (subset-based).
- **Commands & pipelines as catchable errors.** A command's value view is
  `ExecutableError!String`; `parseInt`/`parseFloat` are `ParseError!Int/Float`.
  Pipelines are **`pipefail`-style**: any stage yielding an error makes the whole
  pipeline evaluate to that error for a trailing handler to catch.
- **Inferred error sets (`!T`).** The body's error variants are collected so
  `match` exhaustiveness is enforced and callers see a concrete set, including
  **cross-function propagation** and `try`'d-command `ExecutableError`.
- **Error-set merge / sum type.** `const Merged = A || B` unions error sets by
  name (chains, payload-conflict check, dedup). General `Int || String` sum
  types are scoped out into `future/sum-types-plan.md`.
- **`return` keyword removed.** Output is via `yield` only.

### Notable design decisions

- **Value/yield propagation model** (not `return`): an error is the stage's
  output; `try` re-yields it.
- **Pipefail semantics are bash-consistent** by design — stages still run; the
  error becomes the pipeline's value (see `future/pipeline-errors-plan.md`).
- **`ExecutableError` exemption is a subset test** (every variant is a command
  failure mode), so a merged/mixed set carrying a user error is correctly *not*
  exempt.

### Testing

`zig build test`, `tests/diagnostics/` fixtures (status + stderr), and
`tests/features/*.rn` regression scripts with `.stdout` goldens. Full pipeline:
`zig build run -- scripts/run_ci.rn`.

### Deferred (tracked in `future/error-handling-plan.md`)

- Cross-**process** structured errors (external programs carry only exit code +
  bytes — by design; needs a wire format).
- `catch`/`try`/`match` on a **module member-access** call (`m.fn catch x`) and
  on functions with `pub` declarations inside them.
- Inferred-set **self-recursion** (needs a fixpoint pass).
- General sum types over arbitrary member types (`future/sum-types-plan.md`).
