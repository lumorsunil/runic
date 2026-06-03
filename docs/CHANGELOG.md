# Changelog

All notable changes to Runic will be documented here.

Version numbers follow [Semantic Versioning](https://semver.org/): `MAJOR.MINOR.PATCH`

- **PATCH** — bug fixes and internal improvements; no breaking changes
- **MINOR** — new features or minor breaking changes (e.g. syntax adjustments, changed flag names)
- **MAJOR** — significant new features or breaking changes to the language, runtime, or CLI

---

## [Unreleased]

### Added

#### Typed pipeline boundaries
- **`parseFloat` builtin**: `parseFloat` (`fn String parseFloat() Float`) is the
  `Float` counterpart of `parseInt` — it maps each input value to a `Float`, so
  `Float` pipelines run end-to-end (`{ echo "1.5"; echo "2.5" } | lines |
  parseFloat | square` → `2.256.25`). Non-numeric input fails with the same
  single, source-located diagnostic style (`cannot parse "x" as Float`).
- **`lines` builtin + per-value `parseInt`**: `lines` (`fn String lines() String`)
  reads its whole byte stdin, splits on `\n`, and emits each non-empty line as a
  separate framed value onto its (typed-queue) stdout — turning a newline-
  delimited byte stream into a multi-value stream. `parseInt` now *maps* over its
  input (one `Int` per input value) instead of reading a single value, so it
  composes with a framed stream. Combined with a `for (&0)` filter, a whole
  stream flows through the pipeline: `{ for (0..5) |i| echo i } | lines |
  parseInt | square` (where `square` is `fn Int square() Int { for (&0) |in| {
  yield in * in } }`) emits `0 1 4 9 16`. Custom per-value filters use the
  `for (&0) |v| { ... }` form.
- **File-descriptor stream syntax (`&0`/`&1`/`&2`)**: the three standard streams
  are referenced with `&0` (stdin), `&1` (stdout), `&2` (stderr). `&0` is a value
  expression that reads stdin (replacing the previous `@stdin`); `&1`/`&2` are
  write streams. `&` followed by a digit lexes as a file-descriptor token.
- **`yield` keyword for explicit output**: functions and pipeline stages push
  values with `yield expr` (to stdout, `&1`) or `yield &2 expr` (to stderr). A
  function's `return`/body value is no longer auto-pushed to stdout, so a stage
  that consumes its input without yielding produces no output (e.g. a function
  that only runs a side-effecting `echo`). `return` now serves control flow / the
  function's exit value, and the declared stdout type constrains what may be
  `yield`ed to `&1` (`yield &2` carries untyped diagnostics).
- **Type checking at every `|`**: the type checker now validates that the
  upstream stdout type matches the downstream stdin type at each pipeline
  boundary. Mismatches are caught before execution with a clear diagnostic
  naming both sides.
- **Arbitrary typed pipe values + `parseInt`**: non-`String` typed values now
  flow across pipe boundaries. A function with an `Int` stdin receives `&0`
  already parsed into an `Int` (so `&0 * 2` works), and a stage returning an
  `Int`/`Float` serializes it as canonical decimal text. The new `parseInt`
  builtin (`fn String parseInt() Int`) bridges a `String` stage to an `Int`
  stage, e.g. `echo "10" | parseInt | doubler | inc` → `21`. `Int → String`
  remains a compile-time mismatch.
- **In-process typed transport**: an exact boundary carrying a by-value scalar
  (`Int`/`Float`, no executable on either side) now passes the value in-process
  instead of serializing it to text and re-parsing. The inter-stage pipe is
  marked `typed`; `yield` stores the value in a side-channel keyed by the pipe
  handle (writing no bytes) and `&0` reads it back directly. `String`/executable
  boundaries keep the byte path.
- **Function body contracts**: function bodies are checked against their
  declared `StdinType` and `StdoutType`. Calling a function whose stdin type
  is incompatible with the enclosing function's declared stdin produces a
  diagnostic.
- **`&0` access**: the built-in `&0` expression reads the function's stdin pipe
  as a typed value. Available in any function with a non-Void stdin type.
  Implemented via the `collect_stdin` IR instruction.
- **Consuming `&0` reads**: each `&0` read takes (consumes) the next value off
  the input stream; once the producer has closed, reading `&0` again yields EOF
  (`.null`) and `yield`ing an EOF value emits nothing. So a stage can read once
  per value and `yield` multiple times over its lifetime; to reuse a value, bind
  it (`const n = &0`).
- **Multi-value live streaming with `for (&0) |v|`**: a producer that `yield`s
  many values is drained by the downstream stage with a `for` loop over `&0`.
  Each iteration reads one value off the live stream (blocking until it arrives
  or the producer closes), so a consumer transforms an unbounded number of
  values as they arrive — `produce | double_each` where `double_each` is
  `for (&0) |v| { yield v * 2 }` emits each doubled value with the producer's
  timing. EOF ends the loop. Per-value iteration covers in-process typed
  (`Int`/`Float`) streams; a `String`/byte stream (no message framing) reads as
  a single value. Yield type-checking moved onto a stdout-type stack so a
  `yield` inside the loop validates the capture in its own scope.
- **Mixed exec/typed pipelines**: executable stages and typed Runic functions
  can be freely combined. The type checker enforces that an executable followed
  by a typed function must have `String` stdin (since executables output bytes).
- **Multi-stage typed pipelines**: three-or-more-stage pipelines with any
  combination of executable and typed-function stages are fully supported.
- **`T→?T` pipeline coercion**: a stage producing `T` can feed a downstream
  stage whose stdin is `?T`. The value flows through unchanged and `&0` is
  typed as `?T`, so `&0 orelse "default"` type-checks and runs. `T→E!T` is
  accepted by the type checker as well (runtime exercise pending error-union
  stdin type parsing). Genuinely incompatible boundaries (e.g. `String→Int`,
  `Void→String`, `String→Void`) still produce a clear mismatch diagnostic.
- **`Pipeline.resolveType`** now correctly unwraps function return types so
  pipeline expressions report the right value type in assignment contexts.

### Changed
- A `for` loop and `if`/`else` body may now be a bare statement without `{ }` —
  a single `yield`/`return`/`exit` (in addition to a bare expression, which
  already worked) is allowed directly: `for (&0) |in| yield in * in`,
  `if (cond) yield a else yield b`. A single such statement is desugared into a
  one-statement block. (`while` is not yet parsed at all, so it is unaffected.)
- `parseInt` (and an `Int`-typed `&0`) on non-numeric input now fails with a
  single, source-located diagnostic naming the offending value —
  `[error]: <file>:<line>:<col>: cannot parse "abc" as Int` — instead of dumping
  a raw `Error evaluating … error.InvalidInt` plus a generic CLI footer. The
  redundant generic lines are suppressed for this case.

### Fixed
- A producer block whose `yield` is nested inside a loop/`if`/`match`
  (`{ for (0..5) |i| { yield i } } | square`) is now correctly recognized as a
  scalar stage and gets framed (per-value) typed transport. Previously the
  stdout-type inference only looked at top-level `yield`s, so the boundary fell
  back to the byte path: the producer's values were concatenated into one blob
  and the consumer saw a single value (e.g. `0 1 2 3 4` became `01234` → parsed
  as `1234`, so `square` returned `1522756` instead of `0 1 4 9 16`). Inference
  now recurses into nested bodies and resolves loop captures (and the bare
  `yield i` value, which parses as a zero-arg call).
- `yield <binding>` (e.g. `for (&0) |v| { yield v }`, or any `yield v` where `v`
  is a loop capture or local) no longer crashes the compiler with an
  `integer overflow` panic. `yield` previously popped its value unconditionally
  when it looked like a stack location, but a bare binding yields a *borrowed*
  reference that must not be popped; doing so corrupted the frame and underflowed
  the loop compilers' per-iteration ref accounting. `yield` now pops only the
  temporaries that compiling its value actually pushed. Affected all three
  for-loop forms (live `for (&0)`, counted array, and range).
- Block pipeline stages carrying a scalar (`{ yield 1; ... } | { yield &0 }`)
  now use in-process typed transport with per-value framing, like named typed
  functions do. Previously a block stage was always classified as a byte stream,
  so the boundary buffered every `yield` into one text blob read after the
  producer closed — breaking live streaming (`{ yield 1; sleep "1"; yield 2 } |
  { yield &0; yield &0 }` waited a second and emitted `12` together instead of
  `1` immediately then `2`) and per-value framing (a single `&0` read returned
  the whole buffer instead of one value). The compiler now infers a block
  stage's stdout type from its first `yield &1` so the boundary is recognized as
  `Int`/`Float` typed transport.
- `&0` can now be referenced from block expressions inside a function body
  (including nested blocks and bindings like `const n = &0` used later). The
  type checker previously resolved a function's stdin/stdout types in a scope
  that did not contain the body's bindings, producing a spurious stdout-type
  mismatch once `&0` carried a non-`String` type.
- A block used directly as a pipeline stage now infers its `&0` type from
  the upstream stage, so `echo "3" | parseInt | { yield &0 * &0 }`
  evaluates `&0` as an `Int` (→ `9`) instead of failing on `String * String`.
- An explicit passthrough stage `{ yield &0 }` re-emits its input unchanged,
  preserving its type (`echo "5" | parseInt | { yield &0 } | doubler` →
  `10`). A bare `&0` used as a stage consumes-and-discards (it does not
  yield), so a type-incompatible chain like `parseInt | &0 | doubler` is
  rejected at compile time.
- `yield` of a multi-segment string (produced by interpolation like `"${x}!"`)
  serializes correctly — the segments are concatenated rather than space-joined.
- Chained fd redirects now preserve left-to-right shell semantics, so forms like `echo "hello" 1>&2 2>"/dev/null"` keep writing to the original stderr stream before the later redirect replaces fd `2`.
- Direct top-level executable calls now preserve TTY-aware stdout/stderr behavior when Runic itself is attached to a terminal, so scripts can keep color/ANSI output without breaking redirected or captured output paths.

### Changed
- Bound command expressions now preserve execution-result data more consistently across `&&`, `||`, and `;`, so `.stdout`, `.stderr`, and `.exit_code` remain available after sequencing command-producing expressions.
- `scripts/run_ci.sh` is now the preferred CI entrypoint. It wraps `scripts/run_ci.rn`, checks for expected progress output, and falls back to the direct shell stages if the Runic-driven CI path regresses.

## [0.1.0] — 2026-03-22

Initial versioned release. Establishes a baseline for tracking changes going forward.

### Added
- Versioning via `--version` / `-V` flag on the `runic` CLI
- `--version` flag on `runic-lsp` now reports the shared project version
- This changelog

### Language
- Typed variables: `const` (immutable) and `var` (mutable) bindings
- Primitive types: `String`, `Int`, `Float`, `Bool`, `Void`
- Array literals with Zig-style anonymous syntax: `.{ "a", "b" }`
- Structured pipelines and command execution
- String interpolation
- Control flow: `if`/`else`, `for`, `while`, `match`
- Functions with typed signatures: `fn StdinType name(params) StdoutType { ... }`
- Closures
- Pattern matching with predicate match cases
- Optional types, promise types (`^T`), and error sets
- Background process execution with `^` operator
- Module system via `.rn.module.json` manifests
- Bash interop via `bash { ... }` blocks
- LSP support (completions, diagnostics, hover)
