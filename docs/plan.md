# Runic Roadmap

This document tracks the current future-facing plan for Runic.

It is intentionally different from the historical bring-up notes that existed
earlier in the project. The parser, type checker, IR compiler, script runner,
and basic LSP all exist today, so the roadmap below focuses on the next major
areas of work rather than on bootstrapping the interpreter from scratch.

## Current State

Runic already has:

- a lexer, parser, AST, semantic/type-checking pipeline, and IR compiler
- script execution through the `runic` CLI
- feature and diagnostics regression suites under `tests/`
- a working `runic-lsp` binary with document management, diagnostics, hover,
  and basic completion support
- a command/process model with pipelines, execution-result values, redirects,
  imports, functions, closures, optionals, matching, and background execution

Runic is still experimental. Language design and implementation details are
expected to keep moving while the core model stabilizes.

## Planning Principles

- Keep the roadmap tied to implemented code, not speculative architecture.
- Prefer a small number of active themes over long sequential phase lists.
- Treat `todo.md` and `todo-ir.md` as engineering backlogs, not as public
  product-roadmap documents.
- Update this file when priorities change enough that a contributor would make
  the wrong decision by reading the old roadmap.

## Active Roadmap Themes

### 1. Language and runtime stabilization

The highest priority is tightening the semantics of features that already
exist.

Current focus areas:

- parser error recovery and better diagnostics after the first parse failure
- remaining gaps in function behavior, especially stdin/stdout semantics and
  piping through functions/blocks
- cleanup of execution-result behavior across more expression forms
- better handling of background execution, pipes, and edge-case cleanup
- reducing semantic mismatches between documented behavior and actual runtime
  behavior

The goal is not mainly to add new syntax, but to make the current language
surface predictable and regression-tested.

### 2. Core language growth

Once existing semantics are solid, the next layer is expanding the language in
areas that are already partially designed or partially implemented.

Likely near- to mid-term candidates:

- richer pattern matching
- more complete type-expression support
- additional operators and assignment forms
- improved function references / partial application support
- better user-defined struct/type support
- clearer command syntax decisions, if the current bareword model proves too
  ambiguous in practice

These items are intentionally broader because design decisions here are still
open.

### 3. Import/module model refinement

Imports currently work, but the surrounding module story is still evolving.

Current direction:

- keep `import` aligned with the actual implemented behavior
- continue refining module ergonomics around `pub` exports and parameterless
  imported modules
- decide whether the current direct-source-file module model remains the long-
  term design or whether a richer module packaging story is needed later

This area should be driven by real usage and friction, not by speculative
infrastructure.

### 4. LSP maturity

The language server is no longer an MVP proposal; it exists and needs to be
made more useful and more reliable.

Current priorities are tracked in more detail in `docs/lsp.md`, but the broad
goal is:

- improve completion quality
- add core navigation features
- fix long-running stability/performance issues
- keep diagnostics aligned with the parser/type checker used by the CLI

### 5. Developer workflow and documentation

The project now needs steady maintenance of its contributor-facing surface:

- keep `README.md`, `docs/features.md`, and `docs/language_reference.html`
  aligned with the implementation
- keep planning docs current enough that they remain useful
- expand examples and regression coverage whenever a feature becomes stable
- avoid accumulating speculative docs that are never brought back in sync

## What Is Not in This Document

This roadmap does not try to enumerate every open task.

Use:

- `todo.md` for broad language/runtime backlog items
- `todo-ir.md` for IR/runtime-oriented backlog items
- `docs/lsp.md` for the current LSP-specific roadmap

If one of those backlogs becomes the actual top-level project plan, this file
should be updated to say so explicitly.
