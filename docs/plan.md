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
- a **C FFI layer** — `cimport "lib" { extern fn … }` blocks that load a shared
  library and call externs through vendored, statically linked `libffi`, with a
  `std/ffi` C-type module (`c.Int`/`c.Double`/`c.Str`/…), scalar **and by-value
  struct** arguments and returns (including nested and module-qualified struct
  types), plus a `runic cbind <header.h>` binding generator

Runic is still experimental. Language design and implementation details are
expected to keep moving while the core model stabilizes.

## Recently Landed (2026-09)

A cycle of feature work and engineering-health work:

- **Language:** exponent `**` and bit shifts `<<`/`>>` (with `>>` overloaded
  with append-redirect, resolved in the IR); bitwise ops as Int methods;
  `||=`/`&&=`; array/string slicing; hex/octal/binary literals; `$(a; b)`
  subshell statement sequences; a bare `&0` pipeline stage that forwards stdin;
  `parseBool`; unary-prefix-after-binary parsing.
- **Language (random-fixes cycle):** tuple/record **destructuring** in bindings
  (nested, and heterogeneous over an array literal); **inferred struct literals**
  (`.{ .x = 3 }` typed from a binding annotation, a call argument, a return type,
  a struct field, or an array element); **tuples** — a heterogeneous `.{ … }` is
  an ordered per-position-typed collection (annotated `struct { A, B }`, with
  constant-index precision) while a homogeneous one stays an array; the empty
  `.{}` is an empty struct (appendable arrays annotate their element type);
  requiring `|T|` to introduce a generic type variable (a bare unknown uppercase
  type name is now an error); `break`/`continue`; string indexing; array `+`
  concat. Plus a batch of FFI/value-capture fixes and a compiler crash fix
  (self-referential type capture on a generic `var` parameter).
- **Diagnostics:** top-level parser error recovery (report multiple errors per
  parse); clear diagnostics for unterminated strings/block comments;
  `command not found: '<name>'` instead of a bare `FileNotFound`.
- **Engineering health:** a shared builtin registry and a single binary-operator
  classification table (both replacing scattered per-site logic); and the unit
  test suite was resurrected and expanded — `zig build test` went from 13 tests
  (only the LSP protocol suite ran) to 70, after the runtime module's ~48
  in-file tests were wired to run and brought back up to date, and now stands
  at 112 as the `runic-lsp` protocol suite grew alongside the LSP work below.
- **Performance (0.10.0):** a synchrony (effect) analysis lowers
  concurrency-free code to a fork-free "sync" call/return path — fork-free
  function calls (nullary, parameterized, UFCS/struct-param, in yield/arg
  positions), atomic execution of counted loops (incl. `const` bodies and
  control flow) and recursion, and `?T`/`E!T` returns on that path — plus
  in-place `xs = xs.push e` array growth and runtime memory/latency fixes.
  Compute-heavy in-process scripts now run at flat memory and near the
  interpreter's ceiling; the plain interpreter is still the zero-startup
  default, and native compilation stays deferred. A `bench_guard` CI stage
  protects these fast paths. Full history: `future/execution-optimization.md`.
- **C FFI (`cimport`):** the MVP landed end-to-end — dynamic-library loading and
  typed extern calls through vendored `libffi`, a `std/ffi` C-type module, and a
  `runic cbind` header-to-binding generator. Building it also delivered two
  independently useful language features it depends on: a module exporting a
  *type* (`pub const X = struct {…}`) and *qualified type references*
  (`module.Type` in annotation/expression position). Scalar and by-value struct
  args/returns work; remaining gaps are tracked in `future/c-ffi.md`.
- **FFI/module correctness (0.10.1):** a batch of fixes found driving real C
  (raylib) programs — module-struct field types resolved in the module's scope;
  every generator value piped into a param-coercion consumer; forward references
  and mutual recursion for top-level functions (incl. as pipeline/loop
  consumers that capture globals); `var` mutable parameters with an immutability
  guard for `const`; an array as a streaming pipeline source; and Runic-call
  arguments (incl. by-value struct returns) value-captured across the FFI
  boundary.

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
  a pipeline; forward references / mutual recursion, `var` mutable parameters,
  and piping every generator value into a consumer all landed this cycle;
  first-class/anonymous blocks are still open — see Theme 3)
- cleanup of execution-result behavior across more expression forms
- better handling of background execution, pipes, and edge-case cleanup
- reducing semantic mismatches between documented behavior and actual runtime
  behavior

The goal is not mainly to add new syntax, but to make the current language
surface predictable and regression-tested.

### 2. Typed dataflow and error model

Two related language directions, **both landed**: typed pipes (0.2.0) and the
error model (0.4.0). They were coupled because both shape how command output,
function output, and failures are represented in the type system. What remains in
this theme is a handful of small follow-ups (below), not new model work.

#### Typed pipes — landed (0.2.0)

The core feature shipped in 0.2.0 and is covered by ~16 `typed_pipe_*`
regression tests. Delivered:

- stdin/stdout types are meaningful parts of a function signature
  (`fn StdinType name() StdoutType`), and `&0`/`&1`/`&2` are the stream
  expressions (`yield` writes stdout);
- every `|` boundary is type-checked — the upstream stdout type must match the
  downstream stdin type, and a mismatch is a located compile-time error;
- external executables use a catch-all typed boundary
  (`fn String @(…String) ExecutableError!String`);
- coercions: `T → ?T` end-to-end, and `T → E!T` accepted by the type checker;
- non-byte transport: an exact scalar boundary (`Int`/`Float`, no executable on
  either side) passes the value in-process through a typed side channel instead
  of serializing to text and re-parsing; `String`/executable boundaries keep the
  byte path;
- multi-value streaming: a producer `yield`s many values, a consumer drains them
  with `for (&0) |v|`, live as they arrive; `parseInt`/`parseFloat` bridge a
  byte stream to typed values, and `lines` frames a byte stream per line.

Historical detail lives in `docs/typed-pipes-update.md` and
`docs/typed-pipes-implementation-plan.md` (both marked historical).

Remaining follow-ups (smaller than the feature itself):

- `lines` buffers its whole input before splitting (not line-by-line live);
- `T → E!T` is type-checked but runtime error-union stdin parsing was left
  pending;
- per-value framing for `String` streams is opt-in via `lines` (raw byte streams
  read whole);
- an untyped function parameter errors with a rough `error.TypeNotFound`
  ("Type checker failed to run") rather than a clean, located diagnostic.

#### Error handling — landed (0.4.0)

A Zig-like error model for both values and types shipped in 0.4.0 and is covered
by the `error_*` regression suite. Delivered:

- `error { Variant, Variant: PayloadType }` sets, `E!T` error unions, and a
  leading `!T` whose set is **inferred** from the body;
- explicit error-set/error-union usage in bindings and function return types;
- `catch` / `catch |err|` handlers, `||` (catch-and-discard), and `try`
  (re-yield to propagate), with a **superset check** on what `try` propagates;
- **mandatory handling** — an error that is neither caught nor propagated is a
  compile error (commands keep the implicit exit-code model);
- executable calls surface as `ExecutableError!String`; `parseInt`/`parseFloat`
  return `ParseError!Int`/`ParseError!Float`;
- `match` on an error value with payload capture, **exhaustiveness** checking,
  and paren-less / bare-body case syntax;
- error-set **merge** (`A || B`), pipeline `pipefail`-style errors, and
  in-process preservation of the structured error value across the call boundary
  (so `catch`/`match`/`try`/`||` see the real variant, not flattened text).

Spec: `future/error-handling.md`; implementation record: `error-handling-plan.md`
(+ `pipeline-errors-plan.md`); showcase: `examples/error_handling.rn`.

Remaining follow-ups (small; none block the language surface):

- **LSP** hover/completion for error sets and their variants — deferred as a
  separate subsystem (see Theme 5);
- a **cross-process** error wire format — external programs carry only exit code
  + bytes (i.e. `ExecutableError`); a real serialized error boundary is a
  separate, larger effort;
- inferred-set collection has a couple of niche edges (a mid-*stream* error in a
  multi-value pipeline isn't guaranteed first; a bare mid-transform pipeline
  isn't enforcement-flagged).

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
- **comptime functions returning types** (Zig-style) to supersede the current
  generic type-constructor form. Today a parameterized type is written
  `const Box(T) = struct { value: T }`; the intended long-term replacement is a
  comptime function that returns a type —
  `fn Box(comptime T: type) type { return struct { value: T } }` — unifying
  generic types with ordinary functions and comptime evaluation. (Generic type
  *variables* in signatures are now introduced explicitly with `|T|`; a bare
  unknown uppercase type name is an error rather than a silent generic.)
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

Landed this cycle: a module can export a *type* (`pub const X = struct {…}`),
types are referenceable *qualified* (`module.Type`) in annotation and expression
positions, and a module struct's field types resolve in the module's own scope
(so a field typed via the module's own imports works in an importer without that
import in scope). These unblocked the C FFI type surface.

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
