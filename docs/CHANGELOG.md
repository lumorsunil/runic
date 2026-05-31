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
- **Type checking at every `|`**: the type checker now validates that the
  upstream stdout type matches the downstream stdin type at each pipeline
  boundary. Mismatches are caught before execution with a clear diagnostic
  naming both sides.
- **Arbitrary typed pipe values + `parseInt`**: non-`String` typed values now
  flow across pipe boundaries. A function with an `Int` stdin receives `@stdin`
  already parsed into an `Int` (so `@stdin * 2` works), and a stage returning an
  `Int`/`Float` serializes it as canonical decimal text. The new `parseInt`
  builtin (`fn String parseInt() Int`) bridges a `String` stage to an `Int`
  stage, e.g. `echo "10" | parseInt | doubler | inc` → `21`. `Int → String`
  remains a compile-time mismatch.
- **Function body contracts**: function bodies are checked against their
  declared `StdinType` and `StdoutType`. Calling a function whose stdin type
  is incompatible with the enclosing function's declared stdin produces a
  diagnostic.
- **`@stdin` access**: the built-in `@stdin` expression reads all bytes from
  the function's stdin pipe as a `String` value. Available in any function with
  a non-Void stdin type. Implemented via the `collect_stdin` IR instruction.
- **Mixed exec/typed pipelines**: executable stages and typed Runic functions
  can be freely combined. The type checker enforces that an executable followed
  by a typed function must have `String` stdin (since executables output bytes).
- **Multi-stage typed pipelines**: three-or-more-stage pipelines with any
  combination of executable and typed-function stages are fully supported.
- **`T→?T` pipeline coercion**: a stage producing `T` can feed a downstream
  stage whose stdin is `?T`. The value flows through unchanged and `@stdin` is
  typed as `?T`, so `@stdin orelse "default"` type-checks and runs. `T→E!T` is
  accepted by the type checker as well (runtime exercise pending error-union
  stdin type parsing). Genuinely incompatible boundaries (e.g. `String→Int`,
  `Void→String`, `String→Void`) still produce a clear mismatch diagnostic.
- **`Pipeline.resolveType`** now correctly unwraps function return types so
  pipeline expressions report the right value type in assignment contexts.

### Fixed
- `@stdin` can now be referenced from block expressions inside a function body
  (including nested blocks and bindings like `const n = @stdin` used later). The
  type checker previously resolved a function's stdin/stdout types in a scope
  that did not contain the body's bindings, producing a spurious stdout-type
  mismatch once `@stdin` carried a non-`String` type.
- A block used directly as a pipeline stage now infers its `@stdin` type from
  the upstream stage, so `echo "3" | parseInt | { @stdin * @stdin }` evaluates
  `@stdin` as an `Int` (→ `9`) instead of failing on `String * String`.
- A bare `@stdin` now works as a pipeline stage, acting as an identity
  passthrough that preserves the upstream value's type. `echo "5" | parseInt |
  @stdin | doubler` → `10`; a type-incompatible passthrough such as
  `echo "x" | @stdin | doubler` is still rejected.
- `exit_with` now correctly serializes heap-allocated strings (produced by
  multi-segment string interpolation like `"${x}!"`) to the stdout pipe, so
  typed functions that build strings via interpolation produce the right output.
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
