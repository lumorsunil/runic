# Migrating Bash Scripts to Runic

Runic targets bash users who want safer defaults without abandoning the command-first workflow. This guide outlines a lightweight methodology for porting existing `.sh` scripts while preserving their behavior. Use it alongside `features.md` (language semantics) and the module authoring guide when extracting reusable helpers.

## Prerequisites

1. Install Zig 0.15.1 and build the CLI (`zig build`). The resulting binary lives at `zig-out/bin/runic`.
2. Skim the `README.md` sections on building, running tests, and authoring modules so you know how to iterate locally.
3. Read through your target script and note where it relies on bash-specific features such as implicit word splitting, `set -euo pipefail`, or sourced helper files.

## Recommended porting workflow

1. **Stage the script in `examples/` (optional).** Keeping the original and Runic versions next to each other makes diffing behavior easier while the interpreter is still evolving.
2. **Translate declarations and control flow first.** Replace `VAR=value` with typed bindings (`let home: Str = env.HOME`) and re-express functions with `fn`. Use the explicit `if`/`for` syntax shown in `features.md`.
3. **Keep commands command-like.** Runic preserves the “bare words launch binaries” rule, so start by copying pipelines verbatim. When you need to transform data, pivot into expressions explicitly—`let files = ls src | lines()`—to avoid quoting pitfalls.
4. **Handle errors intentionally.** Instead of `set -e`, rely on typed errors and `try`/`catch`. Pipelines expose rich status objects, so branch on failures instead of reading `$?`.
5. **Convert sourced helpers into modules.** Shared functions should move into `src/<spec>.rn` with a matching `.module.json` manifest (see `docs/module_authoring.md`). Consumers can then `let helpers = import("ci/helpers")` and the loader will enforce interface contracts.
6. **Leverage `bash { ... }` for anything pending.** When a section uses advanced bashisms (arrays of associative expansions, `[[` tests, etc.), wrap it in a compatibility block so the rest of the script can already benefit from Runic semantics.
7. **Validate incrementally.** Run `zig build run -- path/to/script.rn --trace pipeline` to confirm each stage behaves as expected. Add CLI smoke tests under `tests/cli_*.sh` to lock in regressions as soon as the interpreter reaches parity with your use case.

## Migration tips

- Environment variables are explicit fields under an `env` namespace; e.g., `env["HOME"]` or helper APIs once the runtime lands.
- Prefer modules over ad-hoc `source` calls. They provide typed signatures and play nicely with the loader cache.
- Structured data literals (`[]`, `{}`) eliminate most quoting/IFS juggling. Use them early to reduce the amount of legacy shell escaping you have to reason about.
- Pair every migration with notes in `docs/progress.md` or a commit description so the broader team understands which bash features still require interop blocks.

Once the interpreter core lands, this workflow becomes even more powerful: you can mix native Runic functions with background processes, use promises to coordinate async tasks, and trust the type system to prevent whole classes of bash bugs.
