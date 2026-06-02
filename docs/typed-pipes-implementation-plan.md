# Typed Pipes Implementation Plan

## Requirements

Typed pipes make function stdin and stdout types meaningful across pipeline
boundaries. The feature is described in `future/typed-pipes.md`, `docs/plan.md`,
and `todo.md`.

- Function signatures already carry stdin and stdout types:
  `fn StdinType name(params) StdoutType { ... }`.
- `Void` stdin or stdout means that side of the pipe is closed immediately.
- External executables use a catch-all boundary because Runic cannot infer each
  executable's real signature. The planned shape is equivalent to
  `fn String @(...String) ExecutableError!String`.
- The pipe operator must connect stages only when the upstream stdout type is
  compatible with the downstream stdin type.
- The type checker must reject mismatched function stdin/stdout flow, including
  functions that call another function with an incompatible stdin contract.
- Coercions such as `T` to `?T` and `T` to `E!T` need explicit rules before
  they are used by pipeline checking.
- The compiler/runtime must support non-byte-stream transport for fully typed
  function/expression pipelines; the existing byte-stream path remains required
  for executable stages.

## Current State (after Milestones 1–5)

- Parser support exists for function stdin and return types in
  `src/frontend/parser.zig`.
- AST function types include `stdin_type` and `return_type` in
  `src/frontend/ast.zig`.
- `Pipeline.resolveType` unwraps function types to their `return_type`.
- `TypeChecker.runPipeline` validates each `|` boundary; mismatches produce
  diagnostics. `TypeChecker` exposes `PipeBoundaryKind` and
  `classifyPipeBoundary` as a public API.
- `validatePipeBoundary` accepts exact matches, `T→?T`, and `T→E!T` coercions;
  rejects Void↔non-Void mismatches and all other incompatible boundaries.
- `TypeChecker.runFnDecl` validates function body stdout/stdin contracts and
  declares `@stdin` in the function scope with the declared stdin type.
- `compilePipeline` classifies each boundary and emits boundary-kind comments.
  All boundaries use byte pipes at runtime.
- `compileFnDecl` stores the function's `stdin_type`/`return_type` in the
  compiler scope binding, and pushes the stdin type onto
  `IRCompiler.stdin_type_stack` so `@stdin` is typed correctly (including the
  optional/error-union case needed by coercions).
- `@stdin` is implemented via the `collect_stdin` IR instruction: it marks the
  stdin pipe as `has_been_connected=true` (allowing the upstream stream thread
  to exit cleanly), waits for `keep_open=false`, then reads `buffer_writer`.
- `exit_with` correctly serializes heap-allocated strings (multi-segment string
  interpolation) and `zig_string` values to the stdout pipe.
- All boundary configurations work:
  - exec → exec (existing byte-pipe path, unchanged)
  - typed-fn → exec (upstream returns a value, exec reads from pipe)
  - exec → typed-fn (exec writes bytes, `@stdin` collects them)
  - typed-fn → typed-fn (both use return/`@stdin`, no exec)
  - typed-fn → ?T-stdin fn (T→?T coercion, `@stdin orelse default`)
  - mixed 3-stage pipelines in any combination of the above

## Implementation Steps

### 1. Freeze MVP Semantics

Objective: define a narrow, testable typed-pipes MVP before touching compiler
behavior.

- [x] Document the initial executable boundary as stdin `String` and stdout
  `ExecutableError!String`, or explicitly record any temporary deviation needed
  by the current `ExecutionResult` model.
  - Temporary deviation: executables use `ExecutionResult` as return type;
    `resolvePipelineStageStdoutType` maps `ExecutionResult` to `String` for
    pipe boundary validation. The `executableType` in `ast.zig` has
    `stdin_type = &globalStringType`.
- [x] Define canonical string type spelling for diagnostics and signatures
  (`String` vs current array-of-byte representation).
  - `String` is the alias for `[]Byte` declared in the global scope. Diagnostics
    show "String" because type identifiers resolve through `resolveTypeIdentifierToAlias`
    which wraps them as alias nodes that format as their alias name.
- [x] Define exact compatibility rules for the MVP:
  - [x] exact type match is accepted
  - [x] `Void` upstream to non-`Void` stdin is rejected
  - [x] non-`Void` upstream to `Void` stdin is rejected unless an explicit
    discard rule is chosen
  - [x] `T` to `?T` coercion: deferred to step 6
  - [x] `T` to `E!T` coercion: deferred to step 6
- [x] Decide whether the first stage of a function body receives the enclosing
  function's declared stdin implicitly.
  - Decision: the enclosing function's stdin type is validated against any calls
    made in the first expression position of the body via `validateFunctionBodyStdin`.
    The type checker walks the body and rejects calls to functions whose stdin
    type is incompatible with the enclosing function's declared stdin.
- [x] Decide whether the MVP supports only function/expression stages, or also
  executable-to-typed-function transitions such as `echo "123" | parseInt`.
  - Decision: the MVP supports both. Executable stages use the byte-stream path
    with `String` as their stdin/stdout type at the boundary. A typed function
    immediately following an executable must declare `String` stdin.
- [x] Add examples of accepted and rejected typed pipelines to the docs.
  - `docs/features.md` now includes `@stdin` examples, mixed exec/typed
    pipeline examples, and Void rejection examples with inline comments.

### 2. Add Type Model Helpers

Objective: centralize pipe boundary reasoning so parser, type checker, compiler,
and diagnostics do not duplicate ad hoc rules.

- [x] Add a helper that extracts a stage's stdout type from an expression.
- [x] Add a helper that extracts a stage's stdin type from callable/function
  expressions.
- [x] Represent external executable calls with the chosen catch-all function
  type, including stdin type.
- [x] Add `isPipeCompatible(upstream_stdout, downstream_stdin)` in the semantic
  layer.
- [x] Add a structured compatibility result that can report whether a boundary
  is exact, coerced, byte-stream, discarded, or invalid.
  - Added `PipeBoundaryKind` enum to `TypeChecker` with variants:
    `exact_typed`, `byte_stream`, `void_boundary`, `incompatible`.
  - Added public `classifyPipeBoundary` helper on `TypeChecker`.
  - Added parallel `PipeBoundaryKind` and `classifyBoundary` / `classifyStageOutputKind` /
    `classifyStageInputKind` helpers in `IRCompiler` so the compiler can
    classify each boundary without re-running the type checker.
  - `compileFnDecl` now stores the function's `stdin_type` and `return_type` in
    the compiler scope binding's `type_expr` field, giving the classifier access
    to each stage's declared types.
- [x] Reuse existing `validateTypeAssignment` behavior where possible instead
  of creating a separate incompatible type relation.
  - Decision: pipe-boundary compatibility intentionally uses a dedicated
    `pipeTypesEqual` + coercion check rather than `validateTypeAssignment`.
    Assignment is structurally similar but has different rules (e.g. it permits
    `Int→Float` widening and treats `null` specially) that are not desirable at
    pipe boundaries, where byte-stream transport and the executable
    `ExecutionResult→String` mapping dominate. The boundary rules
    (`exact`, `T→?T`, `T→E!T`, Void rejection) are deliberately narrower and
    centralized in `validatePipeBoundary` / `classifyPipeBoundary`.

### 3. Type-Check Pipeline Boundaries

Objective: make `runPipeline` validate each `|` boundary and give useful
diagnostics.

- [x] Update `TypeChecker.runPipeline` to preserve each stage's resolved output
  type after running the stage.
- [x] For every adjacent stage pair, resolve the downstream stdin type and call
  the compatibility helper.
- [x] Report a diagnostic at the pipe span or downstream stage span when types
  are incompatible.
- [x] Include both upstream stdout and downstream stdin types in the diagnostic.
- [x] Ensure `Pipeline.resolveType` returns the final typed stdout after all
  compatibility/coercion rules are applied.
  - Updated to unwrap function types to their `return_type` so assignment
    type checking sees the actual value type, not the function type.
- [x] Type-check function bodies against their declared stdout type.
- [x] Type-check side effects or calls in a function body against the enclosing
  function's declared stdin type, including the documented failing case:
  `fn Void hello() !String ...` called from `fn String greetings() !String hello`.
- [x] Add type-check-only tests for accepted and rejected boundaries.
  - [x] Added an exact successful function pipeline smoke fixture.
  - [x] Added a mismatched typed pipe diagnostic fixture.
  - [x] Added function stdout contract mismatch diagnostics.
  - [x] Added the documented function stdin contract mismatch diagnostic.

### 4. Establish Runtime Representation

Objective: give the compiler a concrete way to move typed values between stages
without forcing every pipeline through byte streams.

- [x] Define an internal typed pipe value or stage handoff representation for
  in-process values.
  - Decision: typed values are currently transported via the existing byte-pipe
    path for `String` types (since String IS bytes and `exit_with(string_slice)`
    already serializes the value to the stdout pipe in the evaluator). This is
    confirmed working by `typed_pipe_return_regression.rn`.
- [x] Decide whether typed handoff should reuse `Stream(T)`, introduce a
  one-shot typed channel, or pass values through closure parameters/registers.
  - Decision (for MVP): continue using byte pipes for all boundaries. True
    typed handoff (skipping serialization) requires `@stdin` support so
    downstream function bodies can access the received value as a typed register.
    This is deferred; the classification infrastructure (Step 2) is in place.
- [x] Define how `Void` closes stdin/stdout in the runtime representation.
  - Void stdout: the upstream stage's `exit_with(void/false)` emits a boolean
    exit code which does not write to the stdout pipe.
  - Void stdin: the stage closure simply does not read from its stdin pipe.
- [x] Define how error unions flow across typed pipeline boundaries.
  - Decision: for the MVP, error unions are not supported as typed pipe values.
    Functions that produce error unions should handle or propagate them within
    the function body. A pipeline boundary that exposes an error union is a
    type mismatch unless the downstream explicitly expects `E!T`. Coercions
    for error-union wrapping are deferred to step 6.
- [x] Define how typed values are converted to bytes when the next stage is an
  external executable.
  - For String values: `exit_with(string_slice)` in the evaluator writes the
    bytes directly to the stdout pipe. No explicit conversion needed.
  - For non-String typed values: deferred; requires `@stdin` and explicit
    serialization functions for non-String types.
- [x] Define how executable byte output is converted to typed values when the
  next stage expects a non-string type; for the MVP, require an explicit parsing
  function if automatic conversion is deferred.
  - Decision: automatic conversion from bytes to non-String types is deferred.
    The downstream function must declare `String` stdin when following an
    executable stage.
- [x] Add trace output that distinguishes byte-stream boundaries from typed
  boundaries.
  - `compilePipeline` now emits `comment("boundary N: {kind}")` for each
    adjacent stage pair using `classifyBoundary`. (Comments are no-ops at
    runtime but visible in verbose/debug IR output once comment emission is
    enabled.)

### 5. Compile Typed Pipeline Segments

Objective: split pipeline compilation into byte-stream and typed-value paths
without regressing existing command pipelines.

- [x] Teach `compilePipeline` to classify each boundary using the semantic
  compatibility result.
  - `compilePipeline` calls `classifyBoundary` for each adjacent stage pair and
    emits a comment with the kind. The classification correctly identifies
    `exact_typed` vs `byte_stream` boundaries.
- [x] Keep the existing byte-stream path for executable/execution-like stages.
  - All stages currently use the byte-pipe concurrent closure path regardless
    of boundary kind. This is correct behavior for the current MVP.
- [x] Add a typed segment path for adjacent stages whose boundary is a typed
  value handoff.
  - Implemented via the byte-pipe path with `collect_stdin`: `@stdin` signals
    the upstream stream thread to exit (via `has_been_connected=true`) and then
    reads the buffered bytes. Byte pipes are still used as the transport medium;
    the "skip serialization" optimization for non-String types is deferred.
- [x] Compile a function stage so its declared stdin comes from the typed
  handoff instead of `threadStdin()`.
  - Done via `@stdin`/`collect_stdin`: the function body reads from
    `threadStdin()` (the byte pipe) through the `collect_stdin` instruction
    rather than through an OS process.
- [x] Compile a function stage so its declared stdout becomes the next typed
  handoff instead of always writing to `threadStdout()`.
  - Done: `exit_with(value)` writes the value to the stdout pipe (the typed
    handoff medium). Multi-segment strings are serialized via
    `materializeString` via `maybeHeapSequenceLen`.
- [x] Preserve process-handle behavior for pipelines that still include external
  executables.
  - Current implementation always uses the process-handle path.
- [x] Ensure background pipelines still return an execution/thread-like handle
  and do not expose typed values synchronously.
  - Background pipelines compile via `compileBackgroundExpressionValue`, which
    is unchanged.
- [x] Add cleanup paths for mixed pipelines so byte pipes and typed handoffs are
  closed exactly once.
  - Current byte-pipe path handles cleanup via `pipeOpt keep_open=false` and
    the stream thread wait pattern.

### 6. Implement Coercions Incrementally

Objective: add only the coercions that are explicitly specified and tested.

- [x] Implement exact type matching first.
- [x] Add optional wrapping only after tests define `T | fn ?T ...` behavior.
  - `validatePipeBoundary` accepts `T→?T`: the upstream value flows through
    unchanged and the downstream `@stdin` is typed as `?T`, so
    `@stdin orelse "default"` type-checks and runs. The compiler tracks the
    enclosing function's declared stdin type via `stdin_type_stack` so
    `compileIdentifier` gives `@stdin` the correct (optional) type.
  - Fixture: `tests/features/typed_pipe_optional_coercion_regression.rn`.
- [x] Add error-union wrapping only after tests define `T | fn E!T ...`
  behavior.
  - `validatePipeBoundary` accepts `T→E!T` (same mechanism as `T→?T`). Runtime
    behavior cannot yet be exercised end-to-end because error-union stdin types
    (`fn E!String ...` / `fn !String ...`) do not parse in the current grammar;
    the acceptance path is in place for when error declarations land.
- [x] Add diagnostics for deferred or unsupported coercions that explain the
  required explicit conversion.
  - Superseded: `T→?T` and `T→E!T` are now accepted rather than rejected, so the
    deferral diagnostic was removed. Genuinely incompatible boundaries (e.g.
    `String→Int`, `Void→String`) still produce a clear mismatch diagnostic.
- [x] Keep coercion metadata available to the compiler so inserted conversions
  are not guessed later.
  - `IRCompiler.stdin_type_stack` carries the enclosing function's declared
    stdin type into `compileIdentifier`, so `@stdin` is typed correctly without
    the compiler re-deriving it.

### 7. Tests And Fixtures

Objective: cover type checking, runtime behavior, mixed byte/typed boundaries,
and regressions in existing pipelines.

- [x] Add diagnostic fixtures for mismatched `String -> Int`, `Void -> String`,
  and `String -> Void` boundaries.
  - [x] Added `String -> Int` (`tests/diagnostics/typed_pipe_mismatch.rn`).
  - [x] Added `Void -> String` (`tests/diagnostics/void_stdout_to_string_stdin.rn`).
  - [x] Added `String -> Void` (`tests/diagnostics/string_stdout_to_void_stdin.rn`).
- [x] Add a diagnostic fixture for a function body that violates its declared
  stdin contract.
- [x] Add a successful function-to-function typed pipeline fixture.
- [x] Add a successful executable-to-string-function pipeline fixture.
- [x] Add a mixed pipeline fixture that keeps existing `echo "x" | cat`
  behavior unchanged.
- [x] Add a `T→?T` coercion fixture
  (`tests/features/typed_pipe_optional_coercion_regression.rn`).
- [ ] Add a typed parse example once parsing functions or builtins exist.
  - BLOCKED: no `parseInt`/`parseFloat` builtins or user-defined parse functions
    exist in the language yet. The `T→?T` coercion fixture
    (`typed_pipe_optional_coercion_regression.rn`) is the closest available
    typed-transform example. Revisit when parse builtins land.
- [ ] Add feature-combination coverage in
  `tests/features/feature_combinations.rn` if that suite exists when the work
  starts.
  - N/A: `tests/features/feature_combinations.rn` does not exist. Typed-pipe
    coverage is instead spread across dedicated `typed_pipe_*` fixtures.
- [x] Run `zig build test`.
- [x] Run `bash tests/cli_smoke.sh`.
- [x] Run the full local regression pass before marking the feature complete.
  - 13 unit tests, 46 CLI smoke tests, 13 CLI diagnostic tests all pass.
  - Formatter (`zig fmt --check`) passes.

### 8. Documentation And Backlog

Objective: keep user-facing docs and backlog state aligned with the implemented
surface.

- [x] Update `future/typed-pipes.md` with final semantics and any deferred
  decisions.
- [x] Update `docs/features.md` and the language reference with typed pipeline
  examples, including `@stdin` usage, mixed exec/typed examples, and Void
  boundary rejection examples.
- [x] Update `README.md` if the canonical workflow or examples change.
  - Added typed pipeline boundary enforcement and `@stdin` to the Status list.
- [x] Update `todo.md` typed-pipes checklist items as they are completed.
- [x] Add changelog notes once behavior is implemented.
  - Added "Typed pipeline boundaries" section to `docs/CHANGELOG.md` under
    `[Unreleased]`, covering type checking, `@stdin`, mixed pipelines,
    coercion diagnostics, and the `exit_with` string serialization fix.

## Suggested Milestones

- [x] Milestone 1: type-check exact function-to-function pipeline boundaries
  with diagnostics, no runtime typed transport yet.
  - [x] Exact stage boundary checking is implemented in the semantic checker.
  - [x] Mismatch diagnostics are covered for `String -> Int`, `Void -> String`,
    and `String -> Void`.
  - [x] Function body stdin/stdout contract checking is implemented for the
    current direct-call and first-stage pipeline semantics.
  - [x] `Pipeline.resolveType` returns the final stdout type.
  - [x] All unit tests and CLI smoke/diagnostic tests pass.
- [x] Milestone 2: compile exact typed function-to-function pipelines using a
  typed handoff path.
  - [x] `PipeBoundaryKind` classification added to the semantic and compiler layers.
  - [x] Compiler stores function type metadata in bindings for boundary checks.
  - [x] `compilePipeline` emits boundary-kind trace comments.
  - [x] `typed_pipe_return_regression.rn` demonstrates that a function returning
    a typed String value (not via echo) passes correctly through a pipeline.
  - [x] `@stdin` built-in implemented: `collect_stdin` IR instruction reads all
    bytes from the thread's stdin pipe once the upstream signals completion
    (`keep_open=false`). The downstream stage signals "virtual consumer" presence
    by setting `has_been_connected=true` on the pipe, letting the upstream stream
    thread exit cleanly. Type checker declares `@stdin` in function scope with
    the function's declared stdin type.
  - [x] `typed_pipe_stdin_regression.rn` demonstrates end-to-end typed pipeline:
    `produce | transform` where `produce` returns a String via `return` and
    `transform` uses `@stdin` to receive it and appends "!".
  - [x] `exit_with` correctly serializes heap-allocated strings (multi-segment
    string interpolation) to the stdout pipe via `maybeHeapSequenceLen` check.
- [x] Milestone 3: support mixed executable/string/function pipelines without
  regressing existing command pipelines.
  - [x] exec→exec pipelines unchanged (45 smoke tests pass, no regressions).
  - [x] typed-fn→exec: `fn Void f() String { return "x" } | cat` works.
  - [x] exec→typed-fn: `echo "x" | fn String g() String { const s = @stdin; return s }` works.
  - [x] typed-fn→typed-fn: `return | @stdin` works (Milestone 2).
  - [x] 3-stage pipelines in all combinations work.
  - [x] New fixtures: `typed_pipe_exec_to_stdin_regression.rn`,
    `typed_pipe_three_stage_regression.rn`, `typed_pipe_mixed_regression.rn`.
- [x] Milestone 4: add documented coercions and error-union flow.
  - [x] T→?T coercion accepted by the type checker and working at runtime.
    `@stdin` is typed as `?T` inside a `?T`-stdin function (via
    `IRCompiler.stdin_type_stack`), so `@stdin orelse default` type-checks and
    runs. Fixture: `typed_pipe_optional_coercion_regression.rn`.
  - [x] T→E!T coercion accepted by the type checker. Runtime exercise is blocked
    on error-union stdin types parsing (`fn E!String ...` does not yet parse).
  - [x] Error-union pipeline boundary semantics documented.
  - [x] Non-coercible mismatches (incl. Void↔non-Void) still rejected with a
    clear diagnostic.
- [x] Milestone 5: update docs, examples, backlog, and full regression coverage.
  - [x] `docs/features.md` updated with `@stdin`, mixed pipelines, coercion
    guidance, and all boundary examples.
  - [x] `future/typed-pipes.md` rewritten with implemented semantics.
  - [x] `todo.md` updated with current state.
  - [x] Full regression pass: 13 unit tests, 45 smoke, 14 diagnostics pass.
- [x] Milestone 6: arbitrary typed values through pipes + `parseInt` builtin.
  - [x] `parseInt` builtin bridges `String → Int` as a pipeline stage.
  - [x] `Int`/`Float` values travel through pipes as canonical decimal text.
  - [x] `@stdin` is type-directed: an `Int`-stdin function parses its input into
    an `Int`, so `@stdin * 2` works.
  - [x] Multi-stage Int pipelines work:
    `echo "10" | parseInt | doubler | inc` → `21`.
  - [x] Int→String boundary still rejected by the type checker.
  - [x] No regressions: full suite passes (see Step 9 tasks).
  - See "Step 9" above for the design and task breakdown.

## 9. Arbitrary-Typed Pipe Values And `parseInt`

Objective: let non-`String` typed values (starting with `Int`) flow across pipe
boundaries, and add a `parseInt` builtin that bridges `String → Int` so that
`echo "1234" | parseInt | doubler` type-checks and runs.

### Design

- **Wire format stays canonical text.** A stage that outputs an `Int`/`Float`
  writes its decimal text representation to the stdout pipe (the same byte pipe
  used for strings). The *type* attached to each boundary tells the downstream
  stage how to interpret those bytes. No separate typed side-channel is needed
  for the MVP; this keeps mixed executable/typed pipelines working unchanged.
- **Type-directed `@stdin`.** When a function's declared stdin type is `Int`,
  `@stdin` parses the collected bytes into an `Int` value (via `collect_stdin`
  followed by a `parse_int` instruction). For `String` stdin it stays a string.
  The compiler already tracks the enclosing stdin type via `stdin_type_stack`.
- **Type-directed stage output.** `exit_with` is extended so that `.uinteger`
  and `.float` values write their decimal text to the stdout pipe (like strings)
  instead of only becoming an exit code. `Bool` (represented as `.exit_code`)
  is unchanged, so `match` predicates keep working.
- **`parseInt` builtin.** Recognized as a pipeline stage with the function type
  `fn String parseInt() Int`. It compiles to `collect_stdin` → `parse_int`,
  reading the upstream string and producing an `Int`. A user-defined `parseInt`
  in scope takes precedence.
- **Scope (deliberate limits).** Direct function-call-as-value
  (`const x = f()`) is already unsupported independently of typed pipes, so this
  milestone targets the pipe path only. `parseInt` returns `Int` (not `?Int` or
  `E!Int`) for the MVP; invalid input is a runtime error. Optional/error-union
  return types remain future work tied to error-declaration parsing.

### Tasks

- [x] Add a `parse_int` IR instruction (string in `%r` → `Int` in `%r`,
  trimming surrounding whitespace).
- [x] Implement `parse_int` in the evaluator (invalid input → `Error.InvalidInt`).
- [x] Extend `exit_with` so `.uinteger`/`.float` write decimal text to the
  stdout pipe when a stdout pipe is present. `Bool` (`.exit_code`) is unchanged.
- [x] Make a `Void`-stdout function discard its body value in `compileFnDecl`
  (so a numeric body value such as `x = 1` is no longer serialized to stdout).
- [x] Flush in the `pipe_write` evaluator handler so a non-waitable typed stage
  (e.g. `parseInt`) makes its bytes visible to a downstream `collect_stdin`.
- [x] Stabilize `@stdin`/`parseInt` results into a ref slot
  (`stabilizeRegisterResult`) so they behave like other value expressions when
  used as arithmetic operands (`@stdin * 2`).
- [x] Recognize `parseInt` in `compileIdentifier` (after user-scope lookup,
  before the executable fallback) and compile it to `collect_stdin` +
  `parse_int`.
- [x] Type `@stdin` according to the enclosing stdin type; emit `parse_int`
  when that type is `Int` (via `typeExprIsNamed`).
- [x] Declare `parseInt` in the type checker's global scope with type
  `fn String parseInt() Int` so boundary checking resolves its stdin/stdout.
- [x] Add a feature fixture `typed_pipe_parseint_regression.rn`
  (`echo "10" | parseInt | doubler | inc` → `21`).
- [x] Run the full regression suite (13 unit, 47 smoke, 13 diagnostics, fmt).
- [x] Update `docs/features.md`, `future/typed-pipes.md`, `todo.md`, and
  `docs/CHANGELOG.md`.

### Follow-up: `@stdin` from block expressions

`@stdin` is now usable from block expressions inside a function body (including
nested blocks and bindings such as `const n = @stdin` referenced later), not
just inline in the body's tail expression.

- [x] Root cause: `runFnDecl` resolved the function's stdin/stdout types in
  `fn_scope`, but the body's bindings live in a child scope that
  `runExpression`/`runBlockInNewScope` created and discarded. A binding
  referenced from the `return` expression (e.g. `n`) was therefore not found and
  fell back to the executable type, producing a spurious stdout mismatch once
  `@stdin` carried a real (non-`String`) type.
- [x] Fix: run a block body directly in a `body_scope` that `runFnDecl` keeps a
  handle to, and resolve stdin/stdout types in that scope. The compiler side
  already handled `@stdin` in blocks (the function-scoped `stdin_type_stack` is
  visible to nested blocks), so no codegen change was needed.
- [x] Fixture: `tests/features/typed_pipe_block_stdin_regression.rn`
  (`@stdin` inside a nested block, `echo "21" | parseInt | process` → `42`).

### Follow-up: infer a block stage's `@stdin` type from its upstream

A block (or bare expression) used directly as a pipeline stage — not a function
body — has no declared stdin type, so its `@stdin` defaulted to `String`. That
made `echo "3" | parseInt | { @stdin * @stdin }` fail at runtime with
`UnsupportedBinaryExpression` (`String * String`).

- [x] Fix: in `compilePipeline`, infer each non-first stage's stdin type from
  the upstream stage's stdout type and push it onto `stdin_type_stack` while
  compiling that stage. A new `stageStdoutType` helper resolves the upstream
  type (handling the `parseInt` builtin, function return types, and mapping an
  executable's `ExecutionResult` to `String`). `@stdin` inside the stage then
  picks up the inferred type (e.g. `Int`) and parses accordingly. Function-call
  stages are unaffected because their bodies are compiled at declaration time.
- [x] The first stage is not inferred (its input is the program/enclosing
  stdin), so `@stdin` there keeps the enclosing context's type or `String`.
- [x] Fixture: `tests/features/typed_pipe_block_stage_inference_regression.rn`
  (`echo "3" | parseInt | { @stdin * @stdin }` → `9`).

### Follow-up: bare `@stdin` as a pipeline stage (superseded by `yield`)

An earlier iteration treated a bare `@stdin` stage (`... | @stdin`) as an
identity passthrough (a `runPipeline` special-case, `isStdinStage`) that skipped
the boundary check and kept the upstream's type flowing.

Once stages stopped auto-pushing their value (see the `yield` follow-up below),
this became a footgun: a bare `@stdin` stage reads-and-discards (it does not
`yield`), so `parseInt | @stdin | doubler` would type-check but fail at runtime
with `InvalidInt` (the downstream got no input).

- [x] Removed the `isStdinStage` passthrough special-case. A bare `@stdin` stage
  is now type-checked like any expression stage (its stdin is the executable
  fallback `String`), so `parseInt | @stdin | doubler` is rejected at compile
  time (`Int → String`) instead of failing at runtime.
- [x] The explicit passthrough idiom is `{ yield @stdin }`, which infers its
  `@stdin` type from the upstream and re-emits it. Fixture:
  `typed_pipe_stdin_passthrough_regression.rn`
  (`echo "5" | parseInt | { yield @stdin } | doubler` → `10`).

### Follow-up: explicit `yield` for stdout output

Previously a function/stage's return (or body) value was automatically pushed to
stdout. That made a last stage emit output even when it shouldn't (e.g. a stage
that only consumes its input, or one that runs a side-effecting `echo`). Output
to stdout is now explicit via a new `yield` keyword.

Semantics:
- `yield expr` writes `expr` to the function/stage's stdout stream and continues.
- A function's `return`/body value is the return/exit value and is **not** pushed
  to stdout. `return` is for control flow / early exit.
- A stage that does not `yield` produces no stdout output (subprocess writes such
  as `echo` still write to stdout independently).
- The declared stdout type constrains `yield`ed values; `yield "x"` in an
  `Int`-stdout function is a compile-time error.

Implementation:
- [x] Lexer/token: add the `yield` keyword.
- [x] AST: add `YieldStmt`; parser: parse `yield expr` as a statement.
- [x] Type checker: `validateBodyYields` walks the body and checks each `yield`
  against the declared stdout type (replacing the old body-final-value stdout
  check, which is removed along with its now-dead resolution chain).
- [x] Compiler: `compileYield` writes the value to `threadStdout` via
  `pipe_write` (which now uses `materializeString`, so multi-segment strings are
  concatenated rather than space-joined).
- [x] Evaluator: `exit_with` no longer writes the value to stdout — it only sets
  the exit code (non-coercible values exit with success).
- [x] `compilePipeline` no longer auto-pushes a non-waitable stage's value;
  `parseInt` now `yield`s its parsed value internally.
- [x] Converted all value-producing fixtures/examples from `return`/auto-push to
  `yield` (typed-pipe fixtures, `return_statement_regression`,
  `string_interpolation_function_regression`, `return_env_regression`,
  `examples/error_budget.rn`); updated the `function_stdout_mismatch` diagnostic
  to a `yield` type mismatch.
- [x] New fixtures: `tests/features/yield_regression.rn` (yield → output) and
  `tests/features/yield_no_output_regression.rn` (no yield → no output).
- [x] Full suite green: 13 unit, 50 smoke, 13 diagnostics, fmt.

Deferred / future:
- [ ] Rename `@stdin` and introduce a general file-descriptor syntax (e.g. `&0`,
  `&1`, `&2`) for stdin/stdout/stderr. `yield` could become sugar for writing to
  `&1`. Not implemented yet.

### Follow-up: file-descriptor stream syntax (`&0`/`&1`/`&2`)

Replaced the `@`-prefixed `@stdin` with a uniform file-descriptor syntax for the
three standard streams: `&0` (stdin), `&1` (stdout), `&2` (stderr). `yield` is
the write verb (sugar for writing to `&1`, with `yield &2 expr` for stderr).

- [x] Lexer: `&` followed by a digit lexes as a new `.fd` token. (`>&` for
  redirects is consumed by `lexGreater`, so this never collides.)
- [x] AST: `FdExpr { fd: u8, span }`; `YieldStmt` gains a `fd: u8 = 1` target.
- [x] Parser: `.fd` parses as a `FdExpr` atom (in both the primary and
  binary-component paths); `parseYield` consumes a leading `&1`/`&2` as the
  target (a leading `&0` is read as the value, since stdin is not writable).
- [x] Type checker: `&0` is accepted as a read (typed by the enclosing function's
  stdin binding, now keyed by `FdExpr.stdin_binding_name = "&0"`, or inferred for
  block stages); `&1`/`&2` as read expressions are rejected. Only `yield &1` is
  checked against the declared stdout type; `yield &2` is unconstrained.
- [x] Compiler: `compileFd` emits the stdin-read path for `&0` (the former
  `@stdin` logic); `compileYield` writes to the target fd's pipe (stack slot 1
  or 2).
- [x] Migrated all fixtures, examples, and docs from `@stdin` to `&0`; removed
  the `@stdin` special-case from `compileIdentifier`.
- [x] New fixture `tests/features/fd_stderr_regression.rn` exercises `&0`,
  `yield`, and `yield &2` (stdout `40`, stderr `log: received 4`).
- [x] Full suite green: 13 unit, 53 smoke, 13 diagnostics, fmt.

Still future: a `&0 | cmd` form (piping the stdin value straight into a command),
and whether `&1`/`&2` should also be first-class redirect targets.

### Follow-up: in-process typed transport (no serialization)

The "skip serialization" optimization noted earlier is now implemented for
by-value scalars. An exact boundary carrying an `Int`/`Float` (no executable on
either side) passes the value in-process instead of serializing it to text and
re-parsing downstream.

- [x] Context: `IRProgramContext.typed_pipe_values` (a `PipeHandle → Value` map)
  with `putTypedPipeValue` / `getTypedPipeValue`.
- [x] Stream `Config.typed` flag + `PipeOption.typed` so a pipe can be marked
  typed via `pipe_opt`.
- [x] Compiler: `boundaryUsesTypedTransport` marks the inter-stage pipe `typed`
  when `classifyBoundary == .exact_typed` and the upstream stdout type is
  `Int`/`Float`. `classifyStageOutputKind` now recognizes the `parseInt` builtin
  so `parseInt → typed-fn` is classified `exact_typed`.
- [x] Evaluator:
  - `pipe_write` (yield) to a `typed` pipe stores the value (no bytes).
  - `collect_stdin` (`&0`) on a `typed` pipe returns the stored value directly;
    it stays in the map so `&0` is re-readable (`&0 * &0`).
  - `parse_int` passes a value through when it is already an `Int`.
- [x] `String`/executable boundaries keep the byte path unchanged.
- [x] Fixture: `tests/features/typed_pipe_inprocess_regression.rn`
  (`echo "6" | parseInt | { yield &0 * &0 } | add_one` → `37`); existing Int
  pipelines (e.g. `typed_pipe_parseint_regression`) now route through it.
- [x] Full suite green: 13 unit, 54 smoke, 13 diagnostics, fmt.

Still future: in-process transport for structured values (arrays/structs), and a
`parseFloat` builtin to make `Float` pipelines exercisable end-to-end.
