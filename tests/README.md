# tests/

Integration suites and CLI smoke tests belong here. Mirror real Runic usage
scenarios so changes to the interpreter or tooling are always exercised end to
end before release.

## CLI harnesses

- `tests/cli_smoke.sh` runs positive end-to-end `.rn` fixtures from
  `tests/features/` and compares stdout/stderr/status against adjacent
  `.stdout`, `.stderr`, and `.status` files.
- `tests/cli_diagnostics.sh` runs negative `.rn` fixtures under
  `tests/diagnostics/` against the built `zig-out/bin/runic` binary, strips ANSI
  color codes, and compares the rendered diagnostics against `.stdout` / `.stderr`
  fixtures.
- Use `__RUNIC_PWD__` inside `.stdout` or `.stderr` fixtures when the output
  should include the repository root used for that test run.

## Feature coverage suites

`tests/features/` now hosts a battery of single-purpose `.rn` scripts—each one
drives a single Runic feature (pipelines, optionals, background execution, module imports,
etc.) so regressions show up immediately. Execute any of
them with the CLI to validate behavior, e.g.

Notable current coverage includes:
- `tests/features/env_vars_regression.rn` and `tests/features/env_var_lookup_regression.rn` for explicit `$NAME` environment access, child-process inheritance, and subshell-local environment updates.
- `tests/features/match_regression.rn` plus `tests/diagnostics/match_capture_unsupported.rn` for exact-value `match`, predicate matcher cases, and the current lack of match-case captures.
- `tests/features/async_background_regression.rn` for trailing `&`, bound background execution capture, and `.wait`.
- `tests/features/optional_if_capture_regression.rn` for `if (optional) |value|` capture semantics.
- `tests/features/optional_regression.rn` for `?T`, `null`, and `orelse`.
- `tests/features/optional_unwrap_regression.rn` plus `tests/diagnostics/optional_unwrap_non_optional.rn` for postfix `.?` unwrap semantics.
- `tests/features/subshell_regression.rn` for subshell cwd isolation and captured command output.

```
zig build run -- tests/features/error_handling_required.rn
zig build run -- tests/features/pipeline_capture.rn
```

Run the whole directory via a simple loop when iterating locally:

```
for file in tests/features/*.rn; do
  zig build run -- "$file" || exit 1
done
```

Run the documented regression harnesses locally with:

```
bash tests/cli_smoke.sh
bash tests/cli_diagnostics.sh
```

## Benchmarks

`tests/benchmarks/` holds measurement-oriented scripts rather than pass/fail regressions.
They are intended for local performance work and should be run with an optimized
binary, not a debug build.

Use the helper from the repository root:

```
zig build -Doptimize=ReleaseFast
bash scripts/bench.sh
```

Current workloads:
- `tests/benchmarks/command_heavy.rn` and `.sh` stress repeated external command execution.
- `tests/benchmarks/evaluator_heavy.rn` and `.sh` stress pure arithmetic and loop evaluation.
- `tests/benchmarks/mixed_loop_exec.rn` and `.sh` stress loops that also spawn commands each iteration.
