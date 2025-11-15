# Runic Interpreter Implementation Plan

This plan decomposes the work required to build an interpreter that satisfies the behavior described in `features.md`. Each phase lists the concrete tasks and their expected outputs so progress can be tracked in order.

## Phase 1 — Repository & Tooling Scaffolding
1. Establish the canonical directory layout: `src/` for interpreter modules, `cmd/runic/` for the CLI entry point, `tests/` for integration suites, and `docs/` for reference material.
2. Select the implementation language (e.g., Rust, Zig, or Go) and create the initial build configuration, formatter targets, and lint commands; document them in `README.md`.
3. Define baseline CI/test scripts that run formatter → linter → unit tests → CLI smoke tests so future changes adhere to the workflow.

## Phase 2 — Front-End (Lexer, Parser, AST)
4. Design the token definitions for Runic syntax, covering commands, pipelines, declarations (`let`, `mut`, `fn`), literals (arrays, maps), error declarations, optionals (`?T`), promises (`^T`), modules, and `bash { ... }` compatibility blocks.
5. Implement a streaming lexer that emits tokens with location info for precise diagnostics.
6. Define the Abstract Syntax Tree (AST) nodes for all constructs mentioned in `features.md`, ensuring pipeline stages carry metadata about command vs. expression nodes.
7. Build a Pratt- or precedence-based parser that produces ASTs for scripts, including indentation/brace block handling, function definitions, pattern matching, capture clauses, and async/await expressions.
8. Write unit tests for lexing and parsing using table-driven fixtures that cover success and failure cases for every feature surface.

## Phase 3 — Semantic Analysis & Type Checking
9. Design the type system representations for primitives, arrays, maps, functions, error sets (`!T`), optionals (`?T`), promises (`^T`), and process handles.
10. Implement symbol tables, scope management, and immutability enforcement (reject reassignment to `let` bindings unless declared `mut`).
11. Enforce type annotations, inference rules, and conversions, including validation for optional unwrapping blocks, promise captures, and match expressions.
12. Model error sets and ensure every function or expression that returns `!T` propagates errors via `try` or handles them with `catch`, mirroring Zig semantics.
13. Add checks for restricted error sets so `fn foo() MyError!T` only permits the declared variants.
14. Validate that functions, loops, and conditionals produce consistent return types and expose clear diagnostics when control paths diverge.

## Phase 4 — Runtime & Command Execution Core
15. Implement an execution engine that walks the AST and evaluates expressions, maintaining an environment for variables, functions, and modules.
16. Build the command runner that spawns external binaries, captures stdout/stderr, tracks exit metadata (PID, timestamps, per-stage codes), and returns structured process handles.
17. Add pipeline orchestration that preserves per-stage outputs and exit codes, enabling downstream inspection (`status.failed_stage` etc.).
18. Support redirection sugar (`1>var`, `2>var`) and destructuring assignments from process handles.
19. Implement background command execution (`&`) returning promise-backed process handles that integrate with the main scheduler.
20. Create the legacy `bash { ... }` execution mode that shells out verbatim and bridges IO back into Runic values.

## Phase 5 — Language Features Beyond Core Commands
21. Implement literal evaluation for arrays, maps, and embedded expressions so data semantics stay predictable.
22. Add support for optional-aware `if (opt) |val| { ... } else { ... }` syntax, ensuring scopes respect the unwrapped binding.
23. Introduce async promise blocks (`async { ... }`) and the `await (promise) |value| { ... } catch |err| { ... }` syntax, integrating with the process handle promises.
24. Implement pattern matching for errors and general `match` expressions, allowing capture clauses (`=> |info| ...`) as described in the spec.
25. Add looping constructs: `for (iterable, range) |value, idx| { ... }` and `while iterator |val| { ... }`, ensuring iterators can come from arrays, generators, or IO streams.
26. Provide module imports (`import http from "net/http"`) with a loader that maps spec paths to files under `src/` and exposes typed APIs.

## Phase 6 — CLI & User-Facing Experience
27. Build the `cmd/runic` binary that accepts script paths, REPL mode, and options for tracing, module paths, and environment overrides.
28. Wire REPL support for quick experiments, including syntax highlighting or at least multiline editing with history.
29. Implement helpful diagnostics: syntax errors with line/column pointers, runtime stack traces, and suggestions for missing `try`/`catch`.
30. Add logging/tracing hooks to inspect pipelines, async tasks, and process handles for debugging scripts.

## Phase 7 — Testing, Examples, and Documentation
31. Populate `tests/` with integration suites mirroring the scenarios in `features.md` (commands, pipelines, errors, async, modules, bash blocks).
32. Create regression tests for both success and failure flows—especially error propagation, optional handling, and async promise resolution.
33. Add `examples/` Runic scripts showcasing the documented features and verifying CLI ergonomics.
34. Expand `README.md` (and potentially `docs/`) with instructions on building, running tests, authoring modules, and migrating from bash.
35. Document remaining roadmap items and backlog tasks uncovered during implementation.

## Phase 8 — Stabilization & Future Enhancements
36. Profile interpreter performance on representative scripts; optimize hot paths in the lexer, parser, and runtime.
37. Evaluate sandboxing and security boundaries for executing untrusted scripts (env inheritance, filesystem guards).
38. Plan for packaging/distribution (prebuilt binaries, installers) and versioning of the Runic language spec.
39. Gather feedback from early adopters using the CLI/examples and iterate on ergonomics before declaring a stable release.
