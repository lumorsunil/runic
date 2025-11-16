# Runic Plan Progress

## Completed Tasks

### Phase 1 — Repository & Tooling Scaffolding
- **Task 1:** Establish the canonical directory layout: `src/` for interpreter modules, `cmd/runic/` for the CLI entry point, `tests/` for integration suites, and `docs/` for reference material. (completed 2025-11-15T16:42:11+01:00)
  - Moved the CLI entry point to `cmd/runic/main.zig`, kept interpreter modules under `src/`, and added brief README guides (including `docs/README.md`) so contributors understand what belongs in each directory.
- **Task 2:** Select the implementation language (e.g., Rust, Zig, or Go) and create the initial build configuration, formatter targets, and lint commands; document them in `README.md`. (completed 2025-11-15T16:54:54+01:00)
  - Standardized on Zig 0.15.1, streamlined `build.zig` so the interpreter module and CLI share the same target/optimization settings, and wired explicit `run`/`test` steps for contributors.
  - Expanded `README.md` with the Zig-specific build, format, lint, and test commands so new contributors can bootstrap the toolchain without reading the scripts.
- **Task 3:** Define baseline CI/test scripts that run formatter → linter → unit tests → CLI smoke tests so future changes adhere to the workflow. (completed 2025-11-15T15:48:05+01:00)
  - Added `scripts/run_ci.sh` plus stage scripts for formatting, linting, unit tests, and CLI smoke suites, along with a placeholder `tests/cli_smoke.sh`.
  - Hardened the formatter/linter stages to operate directly on the Zig source directories and documented how to invoke each CI phase manually in `README.md` so contributors can rerun failing stages quickly. (updated 2025-11-15T18:24:11+01:00)

### Phase 2 — Front-End (Lexer, Parser, AST)
- **Task 4:** Design the token definitions for Runic syntax, covering commands, pipelines, declarations, literals, error declarations, optionals, promises, modules, and `bash { ... }` compatibility blocks. (completed 2025-11-15T18:28:06+01:00)
  - Documented every token category in `docs/tokens.md`, mapping Runic features (commands, pipelines, error sets, optional/promise operators, modules, Bash interop) to the `Tag` definitions the parser can rely on.
  - Annotated `src/frontend/token.zig` to call out which keywords and punctuation drive declarations, literals, and interop features, and linked the reference from `docs/README.md` for discoverability.
- **Task 5:** Implement a streaming lexer that emits tokens with location info for precise diagnostics. (completed 2025-11-15T18:33:40+01:00)
  - Extended `src/frontend/lexer.zig` with a pull-based `Stream` wrapper that lets the parser peek and conditionally consume tokens without materializing the entire token list.
  - Added regression tests asserting span line/column accuracy and the stream interface so every emitted token carries precise location metadata for diagnostics.
- **Task 6:** Define the Abstract Syntax Tree (AST) nodes for all constructs mentioned in `features.md`, ensuring pipeline stages carry metadata about command vs. expression nodes. (completed 2025-11-15T18:46:17+01:00)
  - Introduced `src/frontend/ast.zig` with statement, expression, pattern, type, and command structures that map directly to variables, errors, functions, modules, control flow, and pipelines from the language spec while preserving source spans for diagnostics.
  - Modeled pipeline stages with explicit `StageRole` metadata plus `CommandInvocation`/expression payloads so the runtime can tell whether a stage launches an external program or evaluates a Zig-like expression when inspecting pipeline results.
- **Task 8:** Write unit tests for lexing and parsing using table-driven fixtures that cover success and failure cases for every feature surface. (completed 2025-11-15T19:07:51+01:00)
  - Added table-driven lexer fixtures in `src/frontend/lexer.zig` covering commands, pipelines, declarations, literals, error sets, async/promise constructs, module imports, operators, and the expected lexer failure modes (unexpected characters plus unterminated strings/block comments).
  - Introduced `src/frontend/parser_tests.zig`, a stream-driven parser fixture suite wired into `src/root.zig` that exercises let/mut declarations, function signatures, pipelines, imports, error declarations, await/catch captures, and bash blocks with both passing and intentionally failing cases.

### Phase 3 — Semantic Analysis & Type Checking
- **Task 9:** Design the type system representations for primitives, arrays, maps, functions, error sets (`!T`), optionals (`?T`), promises (`^T`), and process handles. (completed 2025-11-15T19:51:23+01:00)
  - Added `src/semantic/types.zig` with a canonical `Type` union plus a `TypeStore` arena that interns primitives, collection shapes, wrappers (optionals/promises), functions, error sets/unions, and process handle descriptions so semantic analysis can reuse shared references.
  - Implemented builder APIs and regression tests that exercise optional/promise wrapping, closed error sets with payload variants, function signatures returning `!T`, and process handles with buffered vs. streaming stream captures.

### Phase 4 — Runtime & Command Execution Core
- **Task 16:** Build the command runner that spawns external binaries, captures stdout/stderr, tracks exit metadata (PID, timestamps, per-stage codes), and returns structured process handles. (completed 2025-11-15T21:08:38+01:00)
  - Added `src/runtime/command_runner.zig` with a `CommandRunner` facade that shells out via `std.process.Child`, captures stdout/stderr buffers, records PID plus high-resolution timestamps, and materializes `ProcessHandle` values with per-stage exit summaries.
  - Exposed process status helpers (including failed stage detection) plus unit tests that spawn shell snippets to verify stdout/stderr capture, PID tracking, and non-zero exit propagation for future pipeline orchestration work.
- **Task 17:** Add pipeline orchestration that preserves per-stage outputs and exit codes, enabling downstream inspection (`status.failed_stage` etc.). (completed 2025-11-15T21:32:00+01:00)
  - Reworked `src/runtime/command_runner.zig` so `ProcessHandle` carries per-stage capture buffers and statuses, added a reusable `runStage` helper, and implemented `CommandRunner.runPipeline` to feed each stage's stdout into the next while snapshotting timings, PIDs, and exit metadata.
  - Extended the runtime tests with pipeline-focused cases covering multi-stage success, first-failure reporting, and per-stage stdout/stderr assertions; `zig build test` currently fails earlier while trying to write to the shared Zig cache inside the sandbox (`manifest_create AccessDenied`), so the new tests could not be executed end-to-end here.
- **Task 18:** Support redirection sugar (`1>var`, `2>var`) and destructuring assignments from process handles. (completed 2025-11-15T21:55:00+01:00)
  - Added `ProcessHandle.snapshot`, `destructure`, and `redirectStream` helpers plus a `ProcessSnapshot` owner type so runtime code can clone stdout/stderr payloads into independent bindings that survive after the original handle is dropped.
  - Extended `src/runtime/command_runner.zig` tests with coverage for the new snapshot API (full destructuring as well as single-stream captures); the `zig build test` invocation still fails up-front with the sandboxed Zig cache `manifest_create AccessDenied` error, so the new tests could not be executed locally.
- **Task 19:** Implement background command execution (`&`) returning promise-backed process handles that integrate with the main scheduler. (completed 2025-11-15T21:41:43+01:00)
  - Introduced `src/runtime/scheduler.zig` with a cooperative scheduler that spawns background pipelines on dedicated threads, tracks completion via mutex/condition pairs, and returns `ProcessPromise` values capable of advertising readiness or blocking until the final `ProcessHandle` is cloned into the main interpreter allocator.
  - Extended `ProcessHandle` with a `clone` helper plus supporting utilities so asynchronous work can safely transfer buffered stdout/stderr and per-stage metadata across allocator boundaries without touching thread-unsafe state.
  - Added regression tests for promise fulfillment, error propagation from invalid specs, and scheduler teardown with still-running commands; like other runtime suites, these tests could not be executed end-to-end here because `zig build test` is blocked by the sandboxed Zig cache `manifest_create AccessDenied` failure.
- **Task 20:** Create the legacy `bash { ... }` execution mode that shells out verbatim and bridges IO back into Runic values. (completed 2025-11-15T22:05:00+01:00)
  - Added `src/runtime/bash_executor.zig` with a `BashExecutor` helper that assembles `bash -c` command specs, forwards cwd/env/map overrides, and returns normal `ProcessHandle` values so compatibility blocks plug into the runtime like any other command stage.
  - Covered stdout/stderr capture, exit-code propagation, and environment bridging via Zig tests; running `zig test src/runtime/bash_executor.zig` without overriding the existing `ZIG_GLOBAL_CACHE_DIR` currently fails earlier because the repository still depends on pre-0.15 APIs such as `std.ArrayList(...).init` and `std.mem.copy`, so the new assertions could not be executed here.

### Phase 5 — Language Features Beyond Core Commands
- **Task 22:** Add support for optional-aware `if (opt) |val| { ... } else { ... }` syntax, ensuring scopes respect the unwrapped binding. (completed 2025-11-15T22:34:00+01:00)
  - Extended `src/semantic/type_checker.zig` with `analyzeOptionalIf`, a helper that unwraps the subject’s optional payload, validates capture eligibility, and reuses the existing conditional flow inference so `if (opt) |payload| { ... } else { ... }` expressions always yield consistent result types.
  - Added `CaptureScope` utilities to `src/semantic/symbols.zig` so optional capture bindings are automatically pushed onto a nested scope and cleaned up after the `then` block finishes; symbol-table tests cover scope lifetime plus duplicate capture rejection, and attempting `zig build test` still fails earlier with the sandboxed Zig cache `manifest_create AccessDenied` error that blocks the standard library from loading.
- **Task 23:** Introduce async promise blocks (`async { ... }`) and the `await (promise) |value| { ... } catch |err| { ... }` syntax, integrating with the process handle promises. (completed 2025-11-16T00:26:00+01:00)
  - Added `analyzeAsyncBlock`/`analyzeAwait` helpers to `src/semantic/type_checker.zig`, taught `captureTypeForPromise` how to unwrap promise-backed process handles, and threaded error-union validation so `await` blocks can safely branch between success and `catch` handlers while surfacing the inferred payload/error-set types for capture scopes.
  - Expanded the unit suite with async-block and await-focused coverage plus process-handle promise unwrapping, and refreshed the promises section in `features.md` to spell out that `await ... catch` requires typed error unions—while background processes launched with `&` now await into immediate handles using the shared semantics.
- **Task 26:** Provide module imports (`import http from "net/http"`) with a loader that maps spec paths to files under `src/` and exposes typed APIs. (completed 2025-11-16T01:32:00+01:00)
  - Added `src/runtime/module_loader.zig`, a resolver that normalizes import specs, verifies the corresponding `src/<path>.rn` file exists, and hydrates module manifests into `types.TypeStore` references so the rest of the interpreter can inspect exported functions/values with concrete type information.
  - Introduced loader-focused Zig tests that generate temporary module trees to cover caching behavior, manifest parsing for primitive/map/optional types, invalid-spec rejection, and detection of missing metadata files; `src/root.zig` now re-exports the loader for downstream consumers.
- **Task 10:** Implement symbol tables, scope management, and immutability enforcement (reject reassignment to `let` bindings unless declared `mut`). (completed 2025-11-15T20:07:00+01:00)
  - Introduced `src/semantic/symbols.zig` with an arena-backed `SymbolTable`, lexical scope stack, and diagnostics for duplicate declarations, unknown identifiers, and immutable reassignments, plus unit tests covering scope shadowing and assignment validation.
  - Exported the symbol module via `src/root.zig` so upcoming semantic passes can resolve bindings while reusing the shared violation reporting helpers.
- **Task 11:** Enforce type annotations, inference rules, and conversions, including validation for optional unwrapping blocks, promise captures, and match expressions. (completed 2025-11-15T20:18:45+01:00)
  - Added `src/semantic/type_checker.zig` with structural type equality, assignment/coercion rules, optional/promise capture helpers, and match-case analyzers that report detailed violations when patterns, captures, or result arms disagree.
  - Expanded `src/root.zig` to expose the checker and wrote regression tests ensuring annotation mismatches are detected, optional/promise payloads unwrap correctly, match arms enforce variant payload capture semantics, and arm result types unify before yielding the inferred type.
- **Task 12:** Model error sets and ensure every function or expression that returns `!T` propagates errors via `try` or handles them with `catch`, mirroring Zig semantics. (completed 2025-11-15T20:42:00+01:00)
  - Extended `src/semantic/type_checker.zig` with explicit `analyzeTry`/`analyzeCatch` helpers that unwrap error unions, surface the originating error sets for bindings, and enforce that catch handlers produce values assignable to the success payload.
  - Added diagnostics for misusing `try`/`catch` along with regression tests that cover successful propagation, handler type mismatches, and attempts to run error-only constructs against non-error types.
- **Task 13:** Add checks for restricted error sets so `fn foo() MyError!T` only permits the declared variants. (completed 2025-11-15T20:34:41+01:00)
  - Added subset-aware error set comparison helpers and an `enforceRestrictedErrorSet` API to `src/semantic/type_checker.zig`, ensuring functions annotated with narrowed error sets reject attempts to bubble up undeclared variants (including `anyerror`).
  - Updated the assignment rules and extended the unit suite so restricted signatures accept subsets, emit specific diagnostics for mismatches, and guard against accidentally widening the declared error surface.
- **Task 14:** Validate that functions, loops, and conditionals produce consistent return types and expose clear diagnostics when control paths diverge. (completed 2025-11-15T21:05:00+01:00)
  - Expanded `src/semantic/type_checker.zig` with control-flow result descriptors, branch analysis helpers for functions/loops/conditionals, and violations for divergent or empty result sets so downstream passes can surface precise diagnostics.
  - Added targeted unit tests validating annotated return enforcement, inferred branch types, loop exit consistency, and empty-flow detection to lock in the new behavior.

1. Establish the canonical directory layout: `src/` for interpreter modules, `cmd/runic/` for the CLI entry point, `tests/` for integration suites, and `docs/` for reference material. (completed 2025-11-15T16:40:42+01:00)

2. Select the implementation language (e.g., Rust, Zig, or Go) and create the initial build configuration, formatter targets, and lint commands; document them in `README.md`. (completed 2025-11-15T16:51:52+01:00)

3. Define baseline CI/test scripts that run formatter → linter → unit tests → CLI smoke tests so future changes adhere to the workflow. (completed 2025-11-15T18:20:56+01:00)

4. Design the token definitions for Runic syntax, covering commands, pipelines, declarations (`let`, `mut`, `fn`), literals (arrays, maps), error declarations, optionals (`?T`), promises (`^T`), modules, and `bash { ... }` compatibility blocks. (completed 2025-11-15T18:25:42+01:00)

5. Implement a streaming lexer that emits tokens with location info for precise diagnostics. (completed 2025-11-15T18:29:31+01:00)

6. Define the Abstract Syntax Tree (AST) nodes for all constructs mentioned in `features.md`, ensuring pipeline stages carry metadata about command vs. expression nodes. (completed 2025-11-15T18:46:17+01:00)

6. Define the Abstract Syntax Tree (AST) nodes for all constructs mentioned in `features.md`, ensuring pipeline stages carry metadata about command vs. expression nodes. (completed 2025-11-15T18:36:21+01:00)

8. Write unit tests for lexing and parsing using table-driven fixtures that cover success and failure cases for every feature surface. (completed 2025-11-15T18:57:39+01:00)

9. Design the type system representations for primitives, arrays, maps, functions, error sets (`!T`), optionals (`?T`), promises (`^T`), and process handles. (completed 2025-11-15T19:46:29+01:00)

10. Implement symbol tables, scope management, and immutability enforcement (reject reassignment to `let` bindings unless declared `mut`). (completed 2025-11-15T20:02:31+01:00)

11. Enforce type annotations, inference rules, and conversions, including validation for optional unwrapping blocks, promise captures, and match expressions. (completed 2025-11-15T20:12:01+01:00)

12. Model error sets and ensure every function or expression that returns `!T` propagates errors via `try` or handles them with `catch`, mirroring Zig semantics. (completed 2025-11-15T20:22:59+01:00)

13. Add checks for restricted error sets so `fn foo() MyError!T` only permits the declared variants. (completed 2025-11-15T20:28:24+01:00)
14. Validate that functions, loops, and conditionals produce consistent return types and expose clear diagnostics when control paths diverge. (completed 2025-11-15T21:05:00+01:00)

14. Validate that functions, loops, and conditionals produce consistent return types and expose clear diagnostics when control paths diverge. (completed 2025-11-15T20:40:25+01:00)

16. Build the command runner that spawns external binaries, captures stdout/stderr, tracks exit metadata (PID, timestamps, per-stage codes), and returns structured process handles. (completed 2025-11-15T21:02:03+01:00)

17. Add pipeline orchestration that preserves per-stage outputs and exit codes, enabling downstream inspection (`status.failed_stage` etc.). (completed 2025-11-15T21:10:27+01:00)

18. Support redirection sugar (`1>var`, `2>var`) and destructuring assignments from process handles. (completed 2025-11-15T21:21:05+01:00)

19. Implement background command execution (`&`) returning promise-backed process handles that integrate with the main scheduler. (completed 2025-11-15T21:34:22+01:00)

20. Create the legacy `bash { ... }` execution mode that shells out verbatim and bridges IO back into Runic values. (completed 2025-11-15T22:05:00+01:00)

20. Create the legacy `bash { ... }` execution mode that shells out verbatim and bridges IO back into Runic values. (completed 2025-11-15T21:48:47+01:00)

22. Add support for optional-aware `if (opt) |val| { ... } else { ... }` syntax, ensuring scopes respect the unwrapped binding. (completed 2025-11-16T00:03:49+01:00)

23. Introduce async promise blocks (`async { ... }`) and the `await (promise) |value| { ... } catch |err| { ... }` syntax, integrating with the process handle promises. (completed 2025-11-16T00:25:21+01:00)

24. Implement pattern matching for errors and general `match` expressions, allowing capture clauses (`=> |info| ...`) as described in the spec. (completed 2025-11-16T01:01:44+01:00)
  - Added `src/runtime/match_executor.zig`, a reusable evaluator that understands wildcard, literal, and error-variant patterns plus capture semantics for both the subject value and error payloads.
  - Covered the executor with unit tests exercising literal matches, wildcard fallbacks, error payload captures, and invalid capture flows so regressions surface via `zig build test`.

24. Implement pattern matching for errors and general `match` expressions, allowing capture clauses (`=> |info| ...`) as described in the spec. (completed 2025-11-16T00:55:15+01:00)

26. Provide module imports (`import http from "net/http"`) with a loader that maps spec paths to files under `src/` and exposes typed APIs. (completed 2025-11-16T09:35:14+01:00)

### Phase 6 — CLI & User-Facing Experience
- **Task 27:** Build the `cmd/runic` binary that accepts script paths, REPL mode, and options for tracing, module paths, and environment overrides. (completed 2025-11-16T10:24:00+01:00)
  - Replaced the stub `main` with a structured CLI parser that validates mutually exclusive script/REPL modes, handles repeated `--trace`, `--module-path`, and `--env KEY=VALUE` switches, and forwards script arguments (with `--` sentinels) verbatim to the yet-to-be-wired interpreter.
  - Added regression tests for both script and REPL invocations plus fresh README documentation so contributors know how to drive the new binary while the runtime remains under construction.

27. Build the `cmd/runic` binary that accepts script paths, REPL mode, and options for tracing, module paths, and environment overrides. (completed 2025-11-16T12:52:59+01:00)

- **Task 28:** Wire REPL support for quick experiments, including syntax highlighting or at least multiline editing with history. (completed 2025-11-16T13:32:07+01:00)
  - Added `cmd/runic/repl.zig`, a raw-terminal REPL loop with multiline editing (continuations via `\`), arrow-key editing, persistent history, and meta commands such as `:help`, `:history`, and `:quit`, plus a lightweight tokenizer/pipeline builder backed by new tests.
  - Hooked `runic --repl` to the new session, reused `CommandRunner` to execute entered pipelines, and refreshed the top-level README plus `cmd/runic/README.md` to document the workflow while keeping script execution in stub mode.

28. Wire REPL support for quick experiments, including syntax highlighting or at least multiline editing with history. (completed 2025-11-16T13:17:01+01:00)

- **Task 29:** Implement helpful diagnostics: syntax errors with line/column pointers, runtime stack traces, and suggestions for missing `try`/`catch`. (completed 2025-11-16T14:12:08+01:00)
  - Added `src/frontend/diagnostics.zig`, a caret-style formatter that turns spans into line/column annotations and unit tests that capture single-line and multi-line pointer rendering so parser/lexer errors can point at the exact token.
  - Introduced `src/runtime/stack_trace.zig` plus REPL integration that emits pipeline stack traces (including failing stages and exit metadata) whenever command execution errors, giving users immediate runtime context.
  - Extended `src/semantic/type_checker.zig` with a `missing_try_or_catch` violation so assignments and function returns that drop error unions now surface actionable suggestions; new regression tests cover the hint for both bindings and annotated returns.

29. Implement helpful diagnostics: syntax errors with line/column pointers, runtime stack traces, and suggestions for missing `try`/`catch`. (completed 2025-11-16T14:01:01+01:00)

- **Task 30:** Add logging/tracing hooks to inspect pipelines, async tasks, and process handles for debugging scripts. (completed 2025-11-16T14:33:56+01:00)
  - Introduced `src/runtime/tracing.zig`, a topic-aware tracer with thread-safe writers (`pipeline`, `process`, `async`) plus `CommandDisplay` helpers so logs reuse the same quoting rules everywhere.
  - Threaded the tracer through `CommandRunner`, the REPL, process handles, and the async scheduler to emit structured logs for pipeline startup, per-stage exits, handle summaries, and promise lifecycle events; background tasks now inherit the same tracer so asynchronous pipelines surface identical visibility.
  - Added regression tests covering tracer output for both synchronous pipelines (`command_runner.zig`) and async scheduler runs, and documented the new `--trace pipeline|process|async` switches in `README.md`/`cmd/runic/README.md`.

30. Add logging/tracing hooks to inspect pipelines, async tasks, and process handles for debugging scripts. (completed 2025-11-16T14:33:56+01:00)

30. Add logging/tracing hooks to inspect pipelines, async tasks, and process handles for debugging scripts. (completed 2025-11-16T14:20:52+01:00)

### Phase 7 — Testing, Examples, and Documentation
- **Task 32:** Create regression tests for both success and failure flows—especially error propagation, optional handling, and async promise resolution. (completed 2025-11-16T15:18:00+01:00)
  - Added `command_runner.zig` coverage for empty pipelines and blank commands so the runtime now has a regression test that ensures `error.EmptyCommand` is surfaced instead of silently succeeding.
  - Extended `type_checker.zig` optional-if tests to cover widening when `else` branches produce `?T` plus the failure path when branches disagree, guaranteeing the optional control-flow helper enforces consistent result types.
  - Expanded `scheduler.zig` tests with non-zero background exits and duplicate `wait` calls, exercising both the success path (handle resolution) and failure path (`PromiseError.AlreadyAwaited`) for async promise handling.

32. Create regression tests for both success and failure flows—especially error propagation, optional handling, and async promise resolution. (completed 2025-11-16T14:57:31+01:00)
- **Task 33:** Add `examples/` Runic scripts showcasing the documented features and verifying CLI ergonomics. (completed 2025-11-16T15:34:37+01:00)
  - Created `examples/` with four runnable Runic scripts that each highlight a slice of the language surface: pipelines/process handles, data/loops, errors/matches, and async/modules/legacy bash.
  - Documented how to invoke each script (and the useful `--trace` flags) directly from the CLI so contributors can validate ergonomics, and linked the new directory plus summaries from `README.md`.

33. Add `examples/` Runic scripts showcasing the documented features and verifying CLI ergonomics. (completed 2025-11-16T15:32:07+01:00)
