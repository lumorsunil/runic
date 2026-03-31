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
- Use focused design notes such as `future/error-handling.md` and
  `future/typed-pipes.md` to capture feature direction before that work is
  ready to land in the implemented language reference.
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

### 2. Typed dataflow and error model

Two major language directions are now explicit enough to count as roadmap
themes rather than loose backlog items.

#### Typed pipes

Planned direction:

- keep stdin/stdout types as meaningful parts of function signatures
- allow `|` to connect functions and expressions when the upstream stdout type
  matches the downstream stdin type
- model executable calls with a catch-all typed boundary rather than pretending
  external commands have precise static signatures
- define coercion rules carefully for cases such as optional wrapping and error
  unions
- extend the compiler/runtime so pipelines are not limited to byte-stream
  transport when the connected stages are fully typed

This is a significant feature, not a small type-checking tweak. It will affect
function semantics, pipeline compilation, and the runtime representation of
data flowing through pipes.

#### Error handling

Planned direction:

- move toward a Zig-like error model for both values and types
- support explicit error-set/error-union usage in bindings and function return
  types
- add `catch` and `try` semantics that work naturally with command and function
  expressions
- define how executable calls surface their inherent failure model, likely as a
  built-in error-union boundary such as `ExecutableError!String`
- support inference of error-union types where the implementation can determine
  them safely

This work should be coordinated with typed pipes because both features change
how command output, function output, and failures are represented in the type
system.

### 3. Core language growth

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

### 4. Import/module model refinement

Imports currently work, but the surrounding module story is still evolving.

Current direction:

- keep `import` aligned with the actual implemented behavior
- continue refining module ergonomics around `pub` exports and parameterless
  imported modules
- decide whether the current direct-source-file module model remains the long-
  term design or whether a richer module packaging story is needed later

This area should be driven by real usage and friction, not by speculative
infrastructure.

### 5. LSP maturity

The language server is no longer an MVP proposal; it exists and needs to be
made more useful and more reliable.

Current priorities are tracked in more detail in `docs/lsp.md`, but the broad
goal is:

- improve completion quality
- add core navigation features
- fix long-running stability/performance issues
- keep diagnostics aligned with the parser/type checker used by the CLI

### 6. Developer workflow and documentation

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
- `future/error-handling.md` and `future/typed-pipes.md` for feature-design
  notes that inform the roadmap but are not yet part of the implemented
  language reference

If one of those backlogs becomes the actual top-level project plan, this file
should be updated to say so explicitly.
