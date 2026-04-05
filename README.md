# Runic

Runic is an experiment in designing a modern scripting language heavily inspired by bash. Its goal is to keep the terseness and command-focused workflow of bash scripts while removing long-standing ergonomics issues around quoting, data safety, and program structure.

Runic is still in an experimental phase, and major breaking changes may occur at any time as the design and implementation evolve.

## Quick examples

Runic scripts keep the familiar "just run commands" flow you get in bash, but with clearer data handling and explicit types:

```runic
#!/usr/bin/env runic

// receive script arguments
fn Void @(name: String) String
echo "you entered name: ${name}"

// capture stdout and status from a pipeline
const recent = ls "./src" | head "-n" "5"
if (recent) {
  echo "first few entries:"
  echo "${recent.stdout}"
}

// simple function that reuses existing tools
fn Void check_git() String {
  const summary = git "status" "--short"
  if (summary.stdout != "") {
    echo "working tree changes:"
    echo "${summary.stdout}"
  }
}

check_git
```

## Why another scripting language?

Bash is ubiquitous, but it is notoriously difficult to reason about:

- implicit stringly typed variables
- confusing quoting rules and word splitting
- inconsistent error handling defaults
- fragile functions and control structures bolted on over decades

Runic is not a shell — it does not replace bash as an interactive environment. It is a scripting language that borrows bash's command-first style and layers in contemporary programming language ideas so scripts are easier to read, write, and verify.

## Design goals

- **First-class command execution**: `echo "hi"` should run exactly as in bash. Command pipelines remain the core abstraction.
- **Predictable data semantics**: typed variables, immutable defaults, and opt-in mutation to avoid accidental string munging.
- **Structured flow control**: clean function, loop, and conditional syntax without legacy quirks.
- **Safer defaults**: explicit error propagation, no silent glob expansion surprises, and clear return values.
- **Interoperability**: seamless spawning of existing binaries and compatibility layers for embedding legacy bash snippets when needed.

## Core ideas

1. **Modern syntax surface**: brace-delimited blocks with required keywords, eliminating the mix of `then`, `fi`, `do`, and `done`. Bindings use `const` (immutable) and `var` (mutable).
2. **Data primitives**: strings, numbers, booleans, arrays, and maps with predictable coercion rules and convenient literals. Type names must always begin with a capital letter (`String`, `Int`, `Float`, `Bool`, `Void`) so they stand out from value identifiers. Array and record literals use Zig-style anonymous syntax: `.{ "a", "b" }` and `.{ .key = value }`.
3. **Command vs. expression separation**: explicit operators differentiate when you're invoking a program versus evaluating a language expression, reducing quoting headaches.
4. **Functions as pipeline stages**: function declarations carry both a stdin type and a stdout type — `fn StdinType name(params) StdoutType { ... }` — making data flow through pipelines explicit. Functions are called like commands: `check_git` or `greet "world"`.
5. **Error-aware pipelines**: pipeline execution surfaces per-stage exit codes, allowing guarded chaining without `set -e` footguns.
6. **Module system**: reusable libraries imported with `const m = import "spec"`, resolved relative to the importing file, with `pub` declarations exposed on the imported value.

## Status

A working parser, type checker, and IR-based runtime are in place. The following features run end to end:

- Command execution and pipelines (`|`, `&&`, `||`)
- `const` and `var` bindings with optional type annotations
- `if`/`else`, `for` (range and collection), `while` with capture clauses
- Functions with stdin/stdout types, closures, and recursion
- String interpolation (`${ }`) and block capture
- Process handle access (`.stdout`, `.stderr`, `.status.ok`, `.status.exit_code`)
- File redirection (`>`, `>>`) and stream capture (`1>var`, `2>var`)
- Builtin `cd` with subshell-local working-directory updates
- Explicit environment access via `$NAME`, with mutable updates scoped to the current subshell
- Optional types via `?T`, `null`, `orelse`, `.?`, and captured `if` unwrapping
- Script argument handling via the `@` entry-point function
- Error declarations (`error Foo = enum/union`), `try`/`catch`, and exact-value `match`
- Background process execution via trailing `&`, with `.wait` on captured execution values
- Module imports via `import "spec"`

Runic aims to be familiar enough that a bash user can start using it immediately, yet principled enough to scale to large automation projects without the typical bash scripting pitfalls.

## Implementation language & tooling

The runtime and CLI are implemented in Zig (tested with Zig 0.15.1). Zig's build system drives the project layout: `src/` hosts the reusable language/runtime modules, while `cmd/runic/` exposes the CLI entry point that imports those modules.

### Toolchain requirements

- Install Zig 0.15.1 (matching the `build.zig` defaults). Confirm with `zig version`.
- Ensure `zig` and `zig fmt` are on your `PATH`; every script in `scripts/` expects the CLI to be invocable directly.
- Optional environment variables `RUNIC_LANG=zig` and `RUNIC_REPO_ROOT=/path/to/runic` are exported automatically by `scripts/run_ci.rn`, but you can set them manually when calling the stage scripts yourself.

### Building from source

1. From the repository root, run `zig build`. This produces `zig-out/bin/runic`.
2. Use `zig build run -- --help` (or `zig-out/bin/runic --help` after building once) to confirm the CLI wiring.
3. When experimenting with scripts, prefer `zig build run -- path/to/script.rn -- <args>` so Zig automatically rebuilds if any sources changed.
4. For optimized binaries, pass `-Doptimize=ReleaseSafe` (or `ReleaseFast`) to `zig build`. Cross-compilation follows standard Zig flags (`-Dtarget=x86_64-linux-gnu`).
5. Build the language server with `zig build runic-lsp`. The resulting `zig-out/bin/runic-lsp` binary speaks LSP over stdio and is ready to be registered with editors.
6. Keep `ZIG_GLOBAL_CACHE_DIR` untouched so repeated builds share the same cache location (the CI scripts configure it for you).

### Running tests

- `zig build test` — executes every Zig `test` block, covering lexer/parser/runtime modules along with module-loader fixtures. Run this command before sending any PR.
- `zig build run -- scripts/run_ci.rn` — enforces formatter → linter → `zig build test` → CLI smoke suites (`tests/cli_*.sh`). Use this script for a full pre-push verification.
- `bash tests/cli_smoke.sh` — runs the positive end-to-end feature scripts under `tests/features/` through the CLI.
- `bash tests/cli_diagnostics.sh` — runs the negative CLI diagnostic fixtures under `tests/diagnostics/` against the built `zig-out/bin/runic` binary, with ANSI stripped before diffing.
- Stage-specific reruns: `./scripts/stages/unit_tests.sh`, `./scripts/stages/cli_smoke.sh`, etc., respect the same `RUNIC_REPO_ROOT`/`RUNIC_LANG` environment variables.
- Add new CLI regression tests by dropping shell scripts under `tests/cli_*.sh`. They run inside `run_ci.rn` and should invoke `zig build run -- ...` to interact with the current binary.

### Benchmarking

Use optimized binaries for performance work. Debug builds are useful for correctness, but their timings are too noisy to compare with bash or to judge runtime changes reliably.

1. Build an optimized CLI:
   `zig build -Doptimize=ReleaseFast`
2. Run the benchmark harness:
   `bash scripts/bench.sh`
3. Increase repetitions when comparing close changes:
   `bash scripts/bench.sh 25`

The harness compares Runic and bash on two representative workloads:
- `tests/benchmarks/command_heavy.*` for repeated external-command spawning
- `tests/benchmarks/evaluator_heavy.*` for pure arithmetic/control-flow evaluation
- `tests/benchmarks/mixed_loop_exec.*` for loops that also spawn commands each iteration

Override the binary path with `RUNIC_BIN=/path/to/runic bash scripts/bench.sh` if you want to compare multiple builds.

### Formatting and linting

- `zig fmt src cmd tests` formats every Zig source file and is enforced in CI before code review.
- `zig fmt --check src cmd tests` runs the same formatter in check mode and doubles as our lint step (scripts treat any diff as a failure).

### Language server

- Build with `zig build runic-lsp` to produce `zig-out/bin/runic-lsp`. Editors should launch it with the default `--stdio` transport.
- Set `RUNIC_LSP_LOG=1` when debugging conversations; logs go to stderr so protocol responses on stdout remain untouched.
- A placeholder `--tcp <port>` flag exists for upcoming transport introspection. Until then, prefer stdio and invoke `zig-out/bin/runic-lsp --stdio` directly when iterating locally.

## Development workflow

Run `zig build run -- scripts/run_ci.rn` before pushing changes. The script enforces the formatter → linter → unit tests → CLI smoke tests flow and automatically selects the toolchain (currently Zig because `build.zig` is present):

1. **Formatter** — runs `zig fmt` across `src/`, `cmd/`, and `tests/`.
2. **Linter** — runs `zig fmt --check` on the same set of Zig sources so misformatted files fail the build.
3. **Unit tests** — executes `zig build test` (without changing whatever `ZIG_GLOBAL_CACHE_DIR` is already exported), which compiles the runtime modules alongside the CLI and runs all `test` blocks.
4. **CLI smoke tests** — executes every shell script that matches `tests/cli_*.sh`. Today that includes:
   `tests/cli_smoke.sh` for successful end-to-end script execution and
   `tests/cli_diagnostics.sh` for expected compiler/runtime diagnostics.

Each stage stops the pipeline on failure so contributors get immediate feedback. Extend the stage scripts under `scripts/stages/` if a different toolchain or extra checks are required.

Each stage script expects `RUNIC_REPO_ROOT` to point at the repository root and `RUNIC_LANG` to describe the toolchain; `scripts/run_ci.rn` exports both before invoking a stage. You can rerun an individual phase by calling, for example, `RUNIC_LANG=zig RUNIC_REPO_ROOT=$PWD ./scripts/stages/unit_tests.sh` when iterating on a specific failure.

## Authoring modules

Runic modules are currently regular `.rn` files imported from other `.rn`
files:

1. Place the implementation at `<script_dir>/<spec>.rn` and import it with `const m = import "spec.rn"` or another relative spec that resolves from the importing file.
2. Keep imported modules parameterless. A file that declares parameters in its `@(...)` signature cannot currently be imported.
3. Export reusable bindings with `pub` so they are visible on the imported value.
4. Be aware that importing a module executes its top-level body, captures its execution result, and exposes both execution-result fields and `pub` declarations on the imported value.
5. Validate module behavior by running `zig build test` and by invoking a small importing script via `zig build run -- path/to/script.rn`.

## Migrating from bash

Runic keeps the familiar pipeline mindset while removing bash-specific hazards. To migrate existing scripts:

- Port declarations/functions to typed Runic syntax (`const`, `var`, `fn`) before touching command pipelines so behavior stays verifiable.
- Replace `source`-style helper files with plain `.rn` modules imported via `import "..."`, and move reusable entry points behind `pub` declarations.
- Handle failures with `try`/`catch` and typed status objects rather than `set -e` and `$?`.
- Exercise the CLI via `zig build run -- path/to/script.rn --trace pipeline` and add CLI smoke tests to `tests/` as soon as a migration lands.

`docs/migrating-from-bash.md` includes a checklist that walks through the recommended workflow, highlights common traps, and links back to `features.md` entries that replace legacy bash behaviors.

## CLI usage

The `cmd/runic` binary accepts either a script path or `--eval` and exposes switches for tracing, module lookup paths, and environment overrides. Script execution parses `.rn` files directly, honors import/module lookups, `const`/`var` bindings, and pipelines, and forwards diagnostics and command output through the IR runtime.

A typical invocation looks like:

```bash
# run a script with inline args
zig build run -- path/to/script.rn --trace parser --module-path ./lib -- --flag value

# evaluate inline source
zig build run -- --eval 'echo "${1 + 2}"'
```

Key flags:

- `--help`, `-h` — show the usage summary (also the default when no arguments are provided).
- `--trace <topic>` — enable structured tracing for the given runtime subsystem. Current topics are `pipeline` (per-stage spawn/exit), `process` (handle summaries, stage outcomes, and captured IO sizes), and `async` (background task lifecycle). Repeat the flag to collect multiple targets (e.g. `--trace pipeline --trace process`).
- `--module-path <dir>` — prepend an additional directory to the module loader search roots. This mirrors `const foo = import "custom/foo"` scenarios from `features.md`.
- `--env KEY=VALUE` — seed or override environment bindings for the initial script context. Read them explicitly as `$NAME`, reassign them with `$NAME = ...`, and child processes inherit the current subshell-local values.

After the script path, `runic` forwards every argument verbatim. Insert `--` between the script and its arguments when you need to pass values that look like CLI flags.

### Debug tracing

Tracing is the fastest way to inspect how Runic pipelines, background tasks, and process handles behave while the runtime is still under construction. Enable one or more topics via `--trace`:

- `pipeline` — prints `[trace pipeline]` records when a pipeline starts, when each stage exits (including PID, status, duration, and capture sizes), and when the pipeline finishes.
- `process` — captures the resulting handle summary: PID, duration, failed stage (if any), and a per-stage status recap. This is useful when destructuring handles in scripts.
- `async` — follows background execution lifecycle. Every detached task logs its spawn, completion/error, and the resolved handle summary so you can see exactly when background work finishes.

Use these switches with either a script path or `--eval` to watch commands execute in real time.

## Examples

Sample Runic scripts live under `examples/`. Run them with `zig build run -- examples/<script>.rn`.

- `examples/pipelines_and_handles.rn` — command pipelines, process handle inspection (`.stdout`, `.stderr`, `.status`), and `&&`/`||` chaining. Use `--trace pipeline --trace process` to see per-stage detail.
- `examples/data_and_flow.rn` — `const`/`var` bindings with type annotations, `.{ }` array literals, `for` loops over ranges and arrays, arithmetic, and comparisons.
- `examples/functions_and_closures.rn` — function declarations with stdin/stdout types, single-expression bodies, closures over outer variables, and recursion.

## Neovim syntax plugin

Syntax highlighting for Runic lives under `editor/neovim/`. Run
`./scripts/neovim_plugin.sh install` to symlink the plugin into your local
`nvim/site/pack` tree (customize the destination via `--pack-root`, `--slot`, or
`--target`). Call `./scripts/neovim_plugin.sh open-example` to launch Neovim
with the plugin preloaded and one of the sample programs from `examples/*.rn`.
See `editor/neovim/README.md` for additional commands, a `lazy.nvim` spec
snippet, and troubleshooting tips.
