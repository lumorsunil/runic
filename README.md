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

## Implementation language & tooling

The interpreter and CLI are implemented in Zig (tested with Zig 0.15.1). Zig's build system drives the project layout: `src/` hosts the reusable interpreter modules, while `cmd/runic/` exposes the CLI entry point that imports those modules.

### Build commands

- `zig build` — compiles the Runic CLI and installs it under `zig-out/bin/runic`.
- `zig build run -- --help` — builds (if needed) and runs the CLI with any arguments passed after `--`.
- `zig build test` — runs all Zig `test` blocks across the interpreter and CLI modules. Use the `ZIG_GLOBAL_CACHE_DIR` that is already configured in your shell (by Zig or the CI scripts) rather than overriding it for this command.

### Formatting and linting

- `zig fmt src cmd tests` formats every Zig source file and is enforced in CI before code review.
- `zig fmt --check src cmd tests` runs the same formatter in check mode and doubles as our lint step (scripts treat any diff as a failure).

All contributor tooling assumes Zig is installed and on the `PATH`. Optional environment variables `RUNIC_LANG=zig` and `RUNIC_REPO_ROOT=/path/to/runic` are set automatically by `scripts/run_ci.sh`, but they can be exported manually when invoking stage scripts directly.

## Development workflow

Run `./scripts/run_ci.sh` before pushing changes. The script enforces the formatter → linter → unit tests → CLI smoke tests flow described in `docs/plan.md` and automatically selects the toolchain (currently Zig because `build.zig` is present):

1. **Formatter** — runs `zig fmt` across `src/`, `cmd/`, and `tests/`.
2. **Linter** — runs `zig fmt --check` on the same set of Zig sources so misformatted files fail the build.
3. **Unit tests** — executes `zig build test` (without changing whatever `ZIG_GLOBAL_CACHE_DIR` is already exported), which compiles the interpreter as a module alongside the CLI and runs all `test` blocks.
4. **CLI smoke tests** — executes every shell script that matches `tests/cli_*.sh` (a placeholder lives at `tests/cli_smoke.sh`). Replace the stub with real CLI invocations as soon as the interpreter is runnable.

Each stage stops the pipeline on failure so contributors get immediate feedback. Extend the stage scripts under `scripts/stages/` if a different toolchain or extra checks are required.

Each stage script expects `RUNIC_REPO_ROOT` to point at the repository root and `RUNIC_LANG` to describe the toolchain; `scripts/run_ci.sh` exports both before invoking a stage. You can rerun an individual phase by calling, for example, `RUNIC_LANG=zig RUNIC_REPO_ROOT=$PWD ./scripts/stages/unit_tests.sh` when iterating on a specific failure.

## CLI usage

The `cmd/runic` binary accepts either a script path or `--repl` and exposes switches for tracing, module lookup paths, and environment overrides. Script execution still prints a structured summary while the interpreter lands, but `--repl` now launches an interactive shell so quick experiments feel closer to a finished experience.

A typical invocation looks like:

```bash
# run a script with inline args, capturing CLI options for the interpreter
zig build run -- path/to/script.rn --trace parser --module-path ./lib -- --flag value

# request the REPL stub (syntax-aware history lands in plan task 28)
zig build run -- --repl
```

Key flags:

- `--help`, `-h` — show the usage summary (also the default when no arguments are provided).
- `--repl` — enter REPL mode instead of executing a script. The REPL provides history, multiline editing (continue a command with a trailing `\`), and meta commands such as `:history`, `:help`, and `:quit`.
- `--trace <topic>` — enable structured tracing for the given interpreter subsystem. Current topics are `pipeline` (per-stage spawn/exit), `process` (handle summaries, stage outcomes, and captured IO sizes), and `async` (scheduler + promise lifecycle). Repeat the flag to collect multiple targets (e.g. `--trace pipeline --trace process`).
- `--module-path <dir>` — prepend an additional directory to the module loader search roots. This mirrors `import foo from "custom/foo"` scenarios from `features.md`.
- `--env KEY=VALUE` — override environment bindings that will eventually be forwarded to script executions and background commands.

After the script path, `runic` forwards every argument verbatim. Insert `--` between the script and its arguments when you need to pass values that look like CLI flags.

### Debug tracing

Tracing is the fastest way to inspect how Runic pipelines, async tasks, and process handles behave while the runtime is still under construction. Enable one or more topics via `--trace`:

- `pipeline` — prints `[trace pipeline]` records when a pipeline starts, when each stage exits (including PID, status, duration, and capture sizes), and when the pipeline finishes.
- `process` — captures the resulting handle summary: PID, duration, failed stage (if any), and a per-stage status recap. This is useful when destructuring handles in scripts.
- `async` — follows the scheduler lifecycle. Every background task logs its spawn, completion/error, promise wait events, and the resolved handle summary so you can see exactly when async work finishes.

The REPL inherits these switches, so you can run `zig build run -- --repl --trace pipeline --trace async` to watch commands execute in real time while experimenting interactively.

## Examples

Sample Runic scripts now live under `examples/` so you can get a feel for the CLI ergonomics before wiring your own programs. Execute them with `zig build run -- examples/<script>.rn` and pass extra flags to the script after `--`.

- `examples/pipelines_and_handles.rn` — demonstrates pipelines, tee capture, and process-handle destructuring with `--trace pipeline --trace process`.
- `examples/data_and_flow.rn` — highlights typed bindings, array/map literals, optional-aware flow control, and loops.
- `examples/errors_and_match.rn` — exercises error declarations, `try`/`catch`, and pattern matching for recovery.
- `examples/async_and_legacy.rn` — covers async promises, background processes, module imports, and the `bash { ... }` escape hatch.

## Neovim syntax plugin

Syntax highlighting for Runic lives under `editor/neovim/`. Run
`./scripts/neovim_plugin.sh install` to symlink the plugin into your local
`nvim/site/pack` tree (customize the destination via `--pack-root`, `--slot`, or
`--target`). Call `./scripts/neovim_plugin.sh open-example` to launch Neovim
with the plugin preloaded and one of the sample programs from `examples/*.rn`.
See `editor/neovim/README.md` for additional commands, a `lazy.nvim` spec
snippet, and troubleshooting tips.
