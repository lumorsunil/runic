# Runic Roadmap

This document tracks the current future-facing plan for Runic.

It is intentionally different from the historical bring-up notes that existed
earlier in the project. The parser, type checker, IR compiler, script runner,
and a full-featured LSP all exist today, so the roadmap below focuses on the
next major areas of work rather than on bootstrapping the interpreter from
scratch.

## Current State

Runic already has:

- a lexer, parser, AST, semantic/type-checking pipeline, and IR compiler
- script execution through the `runic` CLI
- feature and diagnostics regression suites under `tests/`, plus in-source unit
  tests for the lexer, parser, type checker, IR compiler, and evaluator
- a `runic-lsp` binary with document management, diagnostics, hover,
  completion (snippets, members, `$PATH`), go-to-definition, references and
  rename (binding-aware, workspace-wide), document symbols/highlight/links,
  workspace symbol search, folding ranges, inlay hints, and code actions
- a command/process model with pipelines, execution-result values, redirects,
  imports, functions, closures, optionals, sum types, error sets/unions,
  `match`, and background execution
- an expression surface with the usual arithmetic plus `**`, bit shifts
  `<<`/`>>`, bitwise Int methods (`.band`/`.bor`/`.bxor`/`.bnot`), compound
  assignment (`+=` … `%=`, `||=`, `&&=`), array/string slicing, and hex/octal/
  binary integer literals
- a small standard library (`std.map`, `std.list`, `std.str`, …) and pipeline
  builtins (`parseInt`/`parseFloat`/`parseBool`/`lines`)

Runic is still experimental. Language design and implementation details are
expected to keep moving while the core model stabilizes.

## Recently Landed (2026-09)

A cycle of feature work and engineering-health work:

- **Language:** exponent `**` and bit shifts `<<`/`>>` (with `>>` overloaded
  with append-redirect, resolved in the IR); bitwise ops as Int methods;
  `||=`/`&&=`; array/string slicing; hex/octal/binary literals; `$(a; b)`
  subshell statement sequences; a bare `&0` pipeline stage that forwards stdin;
  `parseBool`; unary-prefix-after-binary parsing.
- **Diagnostics:** top-level parser error recovery (report multiple errors per
  parse); clear diagnostics for unterminated strings/block comments;
  `command not found: '<name>'` instead of a bare `FileNotFound`.
- **Engineering health:** a shared builtin registry and a single binary-operator
  classification table (both replacing scattered per-site logic); and the unit
  test suite was resurrected and expanded — `zig build test` went from 13 tests
  (only the LSP protocol suite ran) to 70, after the runtime module's ~48
  in-file tests were wired to run and brought back up to date, and now stands
  at 112 as the `runic-lsp` protocol suite grew alongside the LSP work below.

A known constraint discovered this cycle: `compiler.zig` is large (~10k lines)
but cannot be cleanly split in current Zig — `usingnamespace` was removed and
non-`pub` methods are not callable across files, so a struct's methods can't be
spread across files. The mitigation is to keep the file from growing (the
registry/table work helps) rather than to shatter it.

## Planning Principles

- Keep the roadmap tied to implemented code, not speculative architecture.
- Prefer a small number of active themes over long sequential phase lists.
- Treat `todo.md` as the engineering backlog, not as a public product-roadmap
  document.
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

- ~~parser error recovery and better diagnostics after the first parse
  failure~~ — landed: top-level statement recovery reports multiple errors per
  parse; unterminated string/block-comment diagnostics; and a missing
  executable reports `command not found` instead of a bare `FileNotFound`.
  Nested (in-construct) recovery is still future work.
- remaining gaps in function behavior, especially stdin/stdout semantics and
  piping through functions/blocks (a bare `&0` stage can now forward stdin into
  a pipeline; first-class/anonymous blocks are still open — see Theme 3)
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

- ~~additional operators and assignment forms~~ — substantially done: `**`,
  `<<`/`>>`, bitwise Int methods, `||=`/`&&=`, slicing, hex/octal/binary
  literals. (Symbolic bitwise operators were deliberately *not* added — `&`/`|`/
  `^` collide with background/fd, pipe, and the promise prefix, so bitwise ops
  are methods instead.)
- richer pattern matching (regex/glob patterns in `match`)
- more complete type-expression support
- improved function references / partial application (`&add 5`) — needs a
  design call on the `&` sigil first
- first-class / anonymous blocks — bare `{ … }` is already an eager
  expression-block, so a lambda form needs distinct syntax; a design decision
- better user-defined struct/type support
- support escaping whitespace in bareword executable/identifier syntax so
  commands or names containing spaces can be represented without immediately
  collapsing to quoted-string behavior
- support invoking dotted executable names such as `cmd.exe`, with a parsing
  model that does not conflict with `.` as member access
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

The language server saw a major build-out this cycle and now offers a broad,
tested feature surface. Delivered:

- **Completion** — keyword snippets, member access (chained + trailing-dot
  recovery), signature/type detail, `$PATH` executables, resolve-on-focus.
- **Navigation** — go-to-definition, and binding-aware, workspace-wide
  references and rename (including cross-file module members).
- **Symbols & structure** — nested document outline, highlight, links,
  workspace symbol search, folding ranges.
- **Hints & actions** — inlay type and parameter hints, prepare-rename, an
  add-type-annotation code action.
- **Stability** — bounded per-edit analysis memory and re-check, and a
  document-close use-after-free fix.

Remaining work (tracked in `docs/lsp.md` and `todo.md`): more code actions
(add-missing-import, remove-unused), call hierarchy, richer formatting, and
semantic tokens. Diagnostics stay aligned with the CLI's parser/type checker.

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

- `todo.md` for backlog items across language, runtime, IR, tooling, and
  debugger work
- `docs/lsp.md` for the current LSP-specific roadmap
- `future/error-handling.md` and `future/typed-pipes.md` for feature-design
  notes that inform the roadmap but are not yet part of the implemented
  language reference

If one of those backlogs becomes the actual top-level project plan, this file
should be updated to say so explicitly.
