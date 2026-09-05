# Changelog

All notable changes to Runic will be documented here.

Version numbers follow [Semantic Versioning](https://semver.org/): `MAJOR.MINOR.PATCH`

- **PATCH** — bug fixes and internal improvements; no breaking changes
- **MINOR** — new features or minor breaking changes (e.g. syntax adjustments, changed flag names)
- **MAJOR** — significant new features or breaking changes to the language, runtime, or CLI

---

## [Unreleased]

## [0.8.0] - 2026-09-04

### Added

- **Exponent and bit shifts** — `**`, `<<`, `>>`. Integer operands stay `Int`
  (`**` saturates on overflow; shifting by 64+ clears; `>>` of a negative is
  arithmetic); a `Float` operand or negative exponent widens `**` to `Float`.
  `>>` is overloaded with append-redirect — a command on the left appends, a
  value shifts (resolved in the IR, like `>`).
- **Bitwise operations as `Int` methods** — `a.band b`, `a.bor b`, `a.bxor b`,
  `a.bnot` (the `&`/`|`/`^` symbols are taken by background/fd, pipe, and the
  promise prefix). Method calls work on integer literals too (`6.band 3`).
- **`||=` and `&&=`** compound assignment, alongside `+=`/`-=`/`*=`/`/=`/`%=`.
- **Array/string slicing** — `x[a..b]` (half-open), `x[a..]`, `x[..b]`, `x[..]`;
  bounds are clamped so an out-of-range or inverted range is empty.
- **Hex / octal / binary integer literals** — `0x1f`, `0o17`, `0b1010`. A
  leading zero is not octal (`042` is decimal).
- **`parseBool`** pipeline builtin (`fn String parseBool() ParseError!Bool`) —
  parses `"true"`/`"false"` (case-insensitive), mapping per value like
  `parseInt`/`parseFloat`.
- **`$(a; b)` subshell statement sequences** — a `$(...)` body is a statement
  sequence like `(...)`, not just a single expression.
- **`&0 | cmd`** — a bare `&0` used as a pipeline stage forwards the function's
  stdin into the pipeline (distinct from reading `&0` as a value).
- **Language server — major expansion.** `runic-lsp` gained a broad, tested
  feature surface (see `docs/lsp.md`):
  - _Completion:_ snippets for `const`/`var`/`fn`/`import` (gated on the
    client's snippet capability); member access, including chained `a.b.c` and
    recovery of a scope after a bare trailing dot (`obj.`); function-signature
    and struct-field-type detail text; executables found on `$PATH`; symlinked
    module paths; and `completionItem/resolve` filling documentation on focus.
  - _Navigation:_ go-to-definition for locals, parameters, the nearest
    shadowing binding, function calls, struct fields, and cross-file symbols;
    references and rename that are **binding-aware** (scope/binding identity,
    not name matching) and workspace-wide, including cross-file module members
    (`m.foo`) even in files that were never opened.
  - _Symbols & structure:_ a document outline nesting struct fields and
    function parameters; document highlight; document links for import paths;
    workspace symbol search; and folding ranges.
  - _Hints & actions:_ inlay hints for inferred binding types and for call
    parameter names (in nested positions and imported-module calls);
    prepare-rename; and an "add type annotation" code action.
  - _Workspace index:_ on initialize with a client-provided root, every `.rn`
    file is indexed, which is what makes cross-file navigation and search work.

### Changed

- A unary prefix is now accepted directly after a binary operator (`2 ** -2`,
  `3 - -2`), matching the leading-unary behavior.
- `.` before a letter on an integer literal is member access, not a decimal
  point, so `6.band 3` works (`6.5` is still a float).

### Fixed

- **Parser error recovery** — a failed top-level statement now resynchronizes
  and parsing continues, so multiple independent errors are reported per run.
- **Unterminated string / block comment** now produce a clear diagnostic
  pointing at the source, instead of exiting silently.
- **Missing executable** reports `command not found: '<name>'` with the call's
  source location, instead of a bare `error.FileNotFound`.
- **Lexer infinite loop on a lone `$` in a string** — a `$` inside a string
  that does not begin a `${…}` interpolation (e.g. `"$r"`, `"cost: $5"`) is now
  literal text; it previously spun forever because the character was never
  consumed. This hung both the CLI and the LSP (whose workspace scan parses
  every file at startup).
- **LSP stability** — the workspace type checker's analysis memory is now reset
  each edit (it previously grew unboundedly); the per-edit re-check is bounded
  to the open documents rather than every module ever touched; and a
  use-after-free when closing a document (stale cached scopes referencing the
  freed AST) was fixed.
- **LSP protocol encodings** — `SymbolKind`, `DiagnosticSeverity`,
  `DiagnosticTag`, `TextDocumentSyncKind`, and `InsertTextFormat` now serialize
  as their numeric codes; they were emitted as tag-name strings, which clients
  reject.

### Internal

- A shared **builtin registry** and a single **binary-operator classification
  table** replace logic that was duplicated across the compiler and type
  checker.
- The unit-test suite was **resurrected and expanded**: `zig build test` went
  from 13 to 70 tests after the runtime module's in-file tests (lexer, parser,
  type checker, IR compiler, evaluator) were wired to run and brought up to
  date, plus a new type-checker test harness. It now stands at 112 tests, the
  growth being an end-to-end `runic-lsp` protocol suite (49 tests) covering the
  completion, navigation, symbol, hint, and action features above.

## [0.7.0] — 2026-08-31

The **stdlib-foundations** release: the language features that let the standard
library be written in Runic itself (generics, comptime, `while`), plus the first
generic collection — a hashed `std.map` — and the performance and correctness
fixes discovered while building it.

### Added

- **Generic type constructors and monomorphization.** Declare generic types with
  `const Box(T) = struct { value: T }` and generic functions with `|T|` type
  captures (`fn wrap(x: |T|) Box(T) { … }`). Calls are **monomorphized** per
  concrete type — direct, multi-parameter, nested applications (`Box(Pair(K,V))`),
  and recursion. Construct with explicit type args, `Box(Int){ .value = 1 }`, or
  bare `Box{ … }`. A type in value position serializes to its name
  (`"${Box(Int)}"` → `Box(Int)`).
- **`std.map` — a generic hashed key/value map.** O(1)-average lookup via hashed
  buckets, with insertion-order iteration. `empty`/`set`/`get`/`has`/`remove`/
  `keys`/`values`/`len`, plus mutable in-place variants `setIn`/`removeIn`. `Int`
  and `String` keys (a String is hashed over its bytes; an interpolated key
  hashes identically to the same literal).
- **`comptime` value evaluation** — `comptime <expr>` interprets pure user
  functions at compile time.
- **`while` loop**, including `while (opt) |v| { … }` optional-unwrap capture.
- **`is`-type narrowing.** Inside `if (x is String) { … }`, `x` is treated as the
  tested type, so type-specific operations (`x.upper`, `x.bytes`) resolve there.
- **`s.bytes`** — a string's bytes as `[]Int`, enabling byte-level work (hashing,
  checksums) in pure Runic.
- **`arr.with i v`** — a new array with element `i` replaced (immutable element
  update, the analog of `arr[i] = v`).
- **Float math builtins** — `sqrt`/`floor`/`ceil`/`round`/`trunc` and `powF`,
  surfaced through `std.math`.
- **`setenv name value`** builtin (backs `std.env.set`) for dynamically-named
  environment writes.
- Standard-library additions: `std.math` Float surface, `std.env.set`, and
  completed `std.list` (`find`/`contains`/`sort`) and `std.path` (`normalize`).

### Changed

- **Integer remainder yields `Int`.** `int % int` was a `Float`; it is now an
  `Int` (like `+`/`-`/`*`), so it can index arrays and feed hashes. A float
  operand still widens the result.
- **Interpolated strings are real `String` values.** A built string (`"k${i}"`,
  concatenations) is now a contiguous `[]Byte` — recognized by `is String`,
  readable by `.bytes`, and consistent under `==` and hashing — instead of an
  internal segment rope.
- **Array indexing propagates the element type**, so `arr[i].field`, `arr[i][j]`,
  nested `[][]T`, and `const b = arr[i]` resolve the element's layout.

### Fixed

- Generic struct return-type equality and value-transport, so a generic function
  can return and reuse a generic struct.
- A `Void` function's nested call (including in an `if` branch) is awaited before
  the function returns, instead of racing the caller.
- Nullary module-member functions are auto-called in value position
  (`std.env.cwd`, not a function reference).
- Multiple interpolated command arguments keep their own values; an interpolated
  command argument combined with a file redirect no longer deadlocks.
- A function's / block's file redirects (`myFn > "out"`) are drained to the file.

### Performance

- **In-place linear array buffers.** A `var x = .{ }` grown only via
  `x = x.push e` / `x = x.with i e`, read only as `x[i]`/`for(x)`/`x.len`, and
  escaping only in a final `yield`, is uniquely owned and grown **in place**
  (amortized O(1)) instead of copied each push — turning O(n²) build loops into
  O(n) (building 3000 elements: ~5.6s → ~1.0s). Gated by a conservative analysis
  that leaves anything it can't prove unique untouched.

## [0.6.1] — 2026-08-20

### Fixed

- A function call (or UFCS method call) used directly as an **arithmetic
  operand** — e.g. `${five - 1}` or `${p.magSq - q.magSq}` — no longer crashes
  with "Could not dereference value of type thread". Such operands are now
  captured so they yield their value instead of a fork/thread handle, and two
  call operands in one expression are stabilized so they don't clobber each
  other.

## [0.6.0] — 2026-08-19

### Added

- **User-defined structs.** Declare a struct type with
  `const Point = struct { x: Int, y: Int }`, construct values with
  `Point{ .x = 3, .y = 4 }`, and read fields with `p.x`. Construction checks every
  field (unknown / missing / duplicate field and per-field type are compile
  errors). Structs nest, and can be passed to and returned from functions by
  value.
- **Struct methods via UFCS.** A method is a free function whose first parameter
  is the receiver; `recv.method args…` is shorthand for `method(recv, args…)`. A
  field of the same name takes precedence over a method.
- **Struct field mutation.** A field of a `var` struct can be reassigned with
  `p.field = value` (nested fields too); the field's declared type is enforced,
  and mutating a field of a `const` struct is a compile error.

### Fixed

- Struct field reads no longer alias when two are used in one expression (e.g.
  `p.x + q.x`, or two struct parameters read in a function body) — each read is
  now stable rather than sharing a scratch register.
- A typed binding whose initializer is a function-call result no longer
  false-positives (`const r: Int = someIntFn` previously reported "expected Int,
  actual: Int"); the assignment validators now normalize an alias-wrapped return
  type. Also fixed a latent stack-corruption crash when passing a struct value as
  a function argument through the value-capture path.

## [0.5.0] — 2026-07-01

### Added

- **Strict mode (`--strict` / `-e`).** A `set -e`-style opt-in that also requires
  handling of command failures (`ExecutableError`), which are exempt by default.
  Off by default, so existing scripts are unaffected; when on, a bare command
  whose failure isn't `catch`/`||`'d is a compile error.

## [0.4.0] — 2026-06-30

This release adds two major language features — structured **error handling** and
**sum types** — and removes the `return` keyword.

### Added

#### Structured error handling

A complete, typed error system replacing ad-hoc exit codes and `set -e`
conventions. See `docs/features.md`.

- **Error sets & unions.** `const E = error { Bad, WithPayload: String }`;
  functions return an error union `E!T`, or `!T` to infer the set from the body.
  Construct values with `E.Variant` / `E{ .Variant = payload }`.
- **Handling: `catch`, `||`, `try`, `match`.** `catch` unwraps-or-handles (`|err|`
  binds the error), `||` discards to a fallback, `try` propagates out of the
  enclosing function, and `match` dispatches on variants with payload capture and
  exhaustiveness checking.
- **Errors as values across the in-process boundary.** A function/pipeline result
  preserves the real error value (set/variant/payload) via typed in-process
  capture, so `catch`/`match`/`try`/`if`/`||`/`&&` operate on the structured
  error, not its flattened text. Also covers optional-returning functions.
- **Mandatory explicit handling.** An unhandled error is a **compile error** — it
  must be `catch`/`||`'d or propagated with `try`, and a `try`'s error must be
  covered by the enclosing function's declared set (a top-level `try` is
  rejected). Commands keep the exit-code model: `ExecutableError` is exempt.
- **Commands & pipelines as catchable errors.** A command's value view is
  `ExecutableError!String`; `parseInt`/`parseFloat` are `ParseError!Int`/`!Float`.
  Pipelines are **`pipefail`-style**: any stage yielding an error makes the whole
  pipeline evaluate to that error for a trailing handler to catch.
- **Inferred error sets (`!T`)** collect the body's error variants (including
  cross-function propagation and a `try`'d command's `ExecutableError`), so
  `match` exhaustiveness is enforced and callers see a concrete set.
- **Error-set merge.** `A || B` between two error sets builds a merged set (the
  union of their variants, with payload-conflict checking and dedup).

#### Sum types: `A || B`

A structural sum type — a value that is *one of* several member types — written
with `||` (any number of members: `A || B || C`). See `docs/features.md` and the
runnable `examples/sum_types.rn`.

- **Declaration & widening.** `const IntOrString = Int || String` (or inline as an
  annotation, parameter, or return type). Sums are unordered sets and normalized:
  `Int || String` equals `String || Int`, and nested/duplicate members flatten. A
  value of a member type widens into the sum implicitly.
- **Must narrow before use.** A bare (un-narrowed) sum cannot be used as one
  specific member — arithmetic and string interpolation on it are compile errors.
  You narrow it first.
- **Narrowing with `is`.** The new `x is T` operator is a runtime type test
  (works on any value) evaluating to `Bool`; in an `if` it refines the binding
  per branch (then: `T`; else: the remaining members, collapsing a two-member sum
  to the survivor).
- **Narrowing with comparisons.** `==`/`!=` and the relational operators narrow a
  sum-typed binding; comparing a sum to a value it can never equal (a non-member)
  is rejected as a likely mistake.
- **Narrowing with `match`.** `match x { Int => …, String => … }` dispatches on the
  member type, narrows the subject in each case body, enforces exhaustiveness
  (unless `_`), and a case may bind the narrowed value with `|name|`.
- **`var` flow narrowing.** A mutable sum binding narrows in branch conditions,
  and a reassignment refines its type from that point on — while the declared type
  still governs what may be assigned.
- **Functions** can take and return sums; the concrete member value survives the
  call boundary, so the caller can narrow the result.

(Note: `||` between two error *sets* builds an error-set merge instead of a sum —
see the error-handling section above.)

#### Tooling

- `examples/error_handling.rn` and `examples/sum_types.rn` showcases, and
  `tests/cli_examples.sh` which exit-checks every `examples/*.rn` in CI
  (previously no example was CI-verified).

### Changed

- **Error/value propagation is value/yield-based.** A function produces its
  result (including an error) via `yield`; there is no `return`. `try` propagates
  by re-yielding the error.
- Numeric literal typing is now spelled out: a literal with no decimal point (or
  exponent) is `Int`; one with a decimal point (even `0.0`) or an exponent is
  `Float`.

### Removed

- **The `return` keyword.** Functions output via `yield` only. To stop a function
  early, `yield` the result and place no further statements after it (or use
  `exit` to halt the stage).

### Fixed

- **Multi-digit literal crash**: any numeric literal with three or more digits
  (e.g. `100`) panicked the lexer (a fixed-size digit probe buffer was sliced out
  of bounds). Fixed.
- **`;` statement separator after a value binding**: `const z = y; echo "hi"`
  (and arithmetic-RHS bindings) silently swallowed the following statement as a
  command argument; only command-producing initializers now sequence with `;`.
- **`if`-branch stack drift**: a branch body that bound a value (e.g.
  `const n: Int = x`) leaked a runtime stack slot, corrupting a later statement;
  the branch now balances its stack.
- Type diagnostics render the string type as `String` instead of `[]Byte`.

## [0.3.0] — 2026-06-14

### Changed

#### Zig 0.16.0 migration
- **Minimum Zig version is now 0.16.0** (`build.zig.zon` `minimum_zig_version`).
  The migration is internal — no language, syntax, or CLI behavior changes — but
  it touches the entire I/O core.
- **`std.Io` threading**: Zig 0.16 moved the filesystem (`std.fs.File`/`Dir` →
  `std.Io.File`/`Dir`), process, and reader/writer APIs under `std.Io`, and every
  side effect (opening/reading/writing/closing files, spawning and waiting on
  processes, `realpath`, `isTty`, terminal mode) now requires an `std.Io`
  instance. An `io` value obtained from `std.process.Init` is threaded from the
  CLI/LSP entry points down through the runner, evaluator, IR context, process
  layer (`FileSink`, `PipeReader`/`PipeWriter`, `ProcessCloseable`,
  `CloseableProcessIo`), and the LSP server/workspace.
- **Process spawning rewritten**: `std.process.Child.init`/`spawn`/`waitForSpawn`
  and the `argv`/`env_map`/`cwd`/`term` fields were removed. Command execution now
  uses `std.process.spawn(io, .{ ... })` with `SpawnOptions` (the `Child.Cwd`
  union, `environ_map`, and the lowercase `.pipe`/`.inherit` `StdIo` variants) and
  reaps via `Child.wait(io)` with the new lowercase `Term` (`.exited`/`.signal`).
- **Environment map**: `std.process.EnvMap` became `std.process.Environ.Map`, now
  carried by value (no longer optional) on each subshell context.
- **Assorted std renames**: `std.mem.trimRight`/`trimLeft` → `trimEnd`/`trimStart`,
  `File.Reader`/`File.Writer`-based seeking and reading, and `std.posix.SIG` /
  `Sigaction` handlers becoming enum-typed.

### Fixed
- **Double-wait panic**: `Child.wait` is now single-shot and asserts the process
  has not already been reaped, so the thread-cleanup wait and a process's
  `ProcessCloseable` could no longer both reap the same child. Both paths now skip
  the wait when the process has already exited.
- **LSP message framing**: header lines are read with `takeDelimiterInclusive` so
  the trailing newline is consumed; the previous exclusive read left the delimiter
  in the stream, misaligning the request body and breaking every LSP request.
- **`realpath` allocation sizes**: results of `realPathFileAlloc` (sentinel-
  terminated `[:0]u8`) are re-duped into plain slices before being stored/freed,
  fixing allocator free-size mismatches in the LSP server, document store, and
  module-path resolution.
- **Diagnostic file paths**: corrected the `std.fs.path.relative` argument order so
  source locations again render relative to the working directory instead of
  `../../..`.

## [0.2.0] — 2026-06-04

> The **typed pipes** work below is summarized narratively in
> [typed-pipes-update.md](./typed-pipes-update.md).

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
- Bound command expressions now preserve execution-result data more consistently across `&&`, `||`, and `;`, so `.stdout`, `.stderr`, and `.exit_code` remain available after sequencing command-producing expressions.
- `scripts/run_ci.sh` is now the preferred CI entrypoint. It wraps `scripts/run_ci.rn`, checks for expected progress output, and falls back to the direct shell stages if the Runic-driven CI path regresses.

### Fixed
- `lines` followed by an external command (or any byte consumer) — e.g.
  `printf "a\nb\n" | lines | cat`, `… | lines | grep` — no longer silently
  produces empty output. `lines`/`emit_lines` only wrote to the in-process typed
  queue, so a downstream that reads bytes saw nothing. It now mirrors `yield`:
  on a `typed` boundary it enqueues framed values (so `lines | parseInt` keeps
  per-line framing), and on a byte boundary it writes each line back as `line\n`.
- `>` is now overloaded by operand rather than always being parsed as an output
  redirect. Previously `if (n > 2) { ... }` (and any `>` between values) was
  parsed as "redirect `n` to a file named `2`", so the condition was the value
  `n` and the runtime panicked (`access of union field 'exit_code' while field
  'uinteger' is active`). The parser now leaves `>` as an unresolved
  `.binary{.greater}` and the IR compiler decides from the **left operand**: a
  command (an external executable call, a Runic function call, a block, or a
  subshell) makes it an output redirect; a value or an in-scope value binding
  makes it the greater-than comparison. So `echo "x" > "f"` and `myFn > "file"`
  redirect, while `n > 2` and `count > limit` compare, in any context
  (condition, binding RHS, `yield`/`return` value, …). `>>` and `>&` are
  unaffected (they have no comparison meaning and always redirect). To compare a
  function's return value instead of redirecting it, bind it first
  (`const r = myFn; if (r > 2) ...`).
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
