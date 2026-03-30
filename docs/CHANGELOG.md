# Changelog

All notable changes to Runic will be documented here.

Version numbers follow [Semantic Versioning](https://semver.org/): `MAJOR.MINOR.PATCH`

- **PATCH** — bug fixes and internal improvements; no breaking changes
- **MINOR** — new features or minor breaking changes (e.g. syntax adjustments, changed flag names)
- **MAJOR** — significant new features or breaking changes to the language, runtime, or CLI

---

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
