# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Runic is a modern scripting language inspired by bash, written in Zig (~0.15.1+). It aims to fix scripting ergonomics with typed variables, structured pipelines, and a module system — while retaining the terse, command-focused style of bash.

## Build & Development Commands

```bash
# Build the CLI binary
zig build

# Run a script (rebuilds automatically)
zig build run -- path/to/script.rn -- <args>

# Run all tests (unit tests + module loader fixtures)
zig build test

# Build the language server
zig build runic-lsp

# Full CI pipeline (formatter → linter → unit tests → CLI smoke tests)
./scripts/run_ci.sh

# Individual CI stages
./scripts/stages/formatter.sh   # zig fmt src cmd tests
./scripts/stages/linter.sh      # zig fmt --check (fails on violations)
./scripts/stages/unit_tests.sh  # zig build test
./scripts/stages/cli_smoke.sh   # bash tests/cli_*.sh
```

Formatting is both formatter and linter: `zig fmt --check src cmd tests` must pass cleanly.

## Architecture

The interpreter runs in four sequential stages, orchestrated in `cmd/runic/run_script.zig`:

```
Source → Lexer → Parser → Type Checker → IR Compiler → IR Runner → Process Output
```

### Frontend (`src/frontend/`)
- **`lexer.zig`** — Streaming lexer with a context stack to handle string interpolation and nested blocks
- **`parser.zig`** — Pratt/precedence-based parser producing AST nodes
- **`ast.zig`** — AST node definitions (scripts, expressions, statements, control flow)
- **`diagnostics.zig`** — Error reporting with source span information
- **`document_store.zig`** — Caches parsed documents for the LSP

### Semantic Analysis (`src/semantic/`)
- **`type-checker.zig`** — Type inference, scope management, optional/promise/error-set validation, immutability enforcement
- **`scope.zig`** — Symbol table and scope stack

### IR Layer (`src/ir/`)
- **`compiler.zig`** — Lowers AST to a flat IR instruction stream
- **`instruction.zig`** — IR opcodes (push, pop, arithmetic, branch, call, etc.)
- **`runner.zig`** — Main execution engine; drives the IR program
- **`evaluator.zig`** — Evaluates individual IR operations
- **`context.zig`** — Execution state: threads, pipes, closeable handles, value stack
- **`value.zig`** — Runtime value types (String, Int, Bool, Float, Array, Map, Process handles, etc.)

### Runtime (`src/runtime/`)
- **`command_runner.zig`** — Spawns external processes, manages I/O capture and exit codes
- **`scheduler.zig`** — Async task scheduling for background processes (`^T` promises)
- **`module_loader.zig`** — Loads typed modules via `.rn.module.json` manifests
- **`bash_executor.zig`** — Embeds legacy bash blocks (`bash { ... }`)
- **`match_executor.zig`** — Pattern matching and destructuring logic

### CLI Entry Points (`cmd/`)
- **`cmd/runic/main.zig`** — Entry point, signal handling
- **`cmd/runic/dispatch.zig`** — Routes `--eval` vs file execution
- **`cmd/runic/run_script.zig`** — Orchestrates the full parse → typecheck → compile → execute pipeline
- **`cmd/runic-lsp/main.zig`** — Language server entry point

### Support Modules (`src/`)
- **`mem/`** — Reference-counted and arena allocators
- **`stream.zig`** — I/O stream abstractions
- **`process.zig`** — Process/pipe abstractions
- **`closeable.zig`** — Resource cleanup protocol
- **`lsp/`** — LSP protocol implementation (completion, diagnostics, hover)

## Testing

- **Unit tests**: embedded `test { ... }` blocks in Zig source files, run via `zig build test`
- **Feature regression tests**: `.rn` scripts in `tests/features/` — each covers a specific language feature
- **Diagnostic fixtures**: `tests/diagnostics/` — expected error output for invalid programs
- **CLI smoke tests**: `tests/cli_*.sh` — end-to-end shell scripts invoking the `runic` binary

To run a single regression script manually:
```bash
./zig-out/bin/runic tests/features/recursive_regression.rn
```

## Language Features Reference

See `docs/features.md` for the full language spec and `docs/plan.md` for the implementation roadmap. Example scripts in `examples/` demonstrate pipelines, functions, closures, and bash interop.

Key syntax rules:
- Command arguments must be quoted strings: `echo "hello"`, `ls "./src"`, `git "status" "--short"`
- Bindings: `const` (immutable), `var` (mutable) — there is no `let` or `mut`
- Primitive types: `String`, `Int`, `Float`, `Bool`, `Void`
- Array literals: `.{ "a", "b" }` (Zig-style anonymous syntax)
- Function syntax: `fn StdinType name(params) StdoutType { body }`
