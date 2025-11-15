# Runic

Runic is an experiment in designing a modern bash-compatible scripting language. Its goal is to keep the terseness and command-focused workflow of traditional shell scripts while removing long-standing ergonomics issues around quoting, data safety, and program structure.

## Why another shell?

Classic bash is ubiquitous, but it is notoriously difficult to reason about:

- implicit stringly typed variables
- confusing quoting rules and word splitting
- inconsistent error handling defaults
- fragile functions and control structures bolted on over decades

Runic keeps the "just type commands" workflow but layers in contemporary programming language ideas so scripts are easier to read, write, and verify.

## Design goals

- **First-class command execution**: `echo "hi"` should run exactly as in bash. Command pipelines remain the core abstraction.
- **Predictable data semantics**: typed variables, immutable defaults, and opt-in mutation to avoid accidental string munging.
- **Structured flow control**: clean function, loop, and conditional syntax without legacy quirks.
- **Safer defaults**: explicit error propagation, no silent glob expansion surprises, and clear return values.
- **Interoperability**: seamless spawning of existing binaries and compatibility layers for embedding legacy bash snippets when needed.

## Core ideas

1. **Modern syntax surface**: indentation-aware blocks or braces with required keywords, eliminating the mix of `then`, `fi`, `do`, and `done`.
2. **Data primitives**: strings, numbers, booleans, arrays, and maps with predictable coercion rules and convenient literals.
3. **Command vs. expression separation**: explicit operators differentiate when you're invoking a program versus evaluating a language expression, reducing quoting headaches.
4. **Error-aware pipelines**: pipeline execution surfaces per-stage exit codes, allowing guarded chaining without `set -e` footguns.
5. **Module system**: reusable libraries and standard tooling for testing, formatting, and packaging scripts.

## Roadmap

1. Draft language reference covering syntax, semantics, and compatibility guarantees.
2. Build a parser and interpreter that can execute a useful subset of Runic scripts.
3. Provide conversion guidelines and tooling for migrating bash scripts.
4. Iterate on the design through real-world scripts, focusing on ergonomics and developer experience.

Runic aims to be familiar enough that a bash user can start using it immediately, yet principled enough to scale to large automation projects without the typical shell pitfalls.

## Development workflow

Run `./scripts/run_ci.sh` before pushing changes. The script enforces the formatter → linter → unit tests → CLI smoke tests flow described in `docs/plan.md` and automatically selects the toolchain based on the files in the repository:

1. **Formatter** — `cargo fmt`, `go fmt ./...`, or `zig fmt` depending on whether `Cargo.toml`, `go.mod`, or `build.zig` is present. Set `RUNIC_LANG=rust|go|zig` to override detection.
2. **Linter** — runs `cargo clippy --all-targets --all-features -- -D warnings`, `go vet ./...`, or `zig fmt --check` to ensure the code is clean before building.
3. **Unit tests** — executes `cargo test --lib --bins --tests`, `go test ./...`, or `zig build test`.
4. **CLI smoke tests** — executes every shell script that matches `tests/cli_*.sh` (a placeholder lives at `tests/cli_smoke.sh`). Replace the stub with real CLI invocations as soon as the interpreter is runnable.

Each stage stops the pipeline on failure so contributors get immediate feedback. Extend the stage scripts under `scripts/stages/` if a different toolchain or extra checks are required.
