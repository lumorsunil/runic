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

## Feature coverage suites

`tests/features/` now hosts a battery of single-purpose `.rn` scripts—each one
drives a single Runic feature (pipelines, optionals, promises, module imports,
legacy bash blocks, etc.) so regressions show up immediately. Execute any of
them with the CLI to validate behavior, e.g.

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
