# Runic Feature Tests

Each `.rn` script in this directory exercises exactly one language feature from
`features.md` so regressions are easy to isolate. Run any script via
`zig build run -- tests/features/<file>.rn`.

- `command_first.rn` – command-first execution and pipelines
- `data_semantics.rn` – immutable vs mutable bindings plus literals
- `type_annotations.rn` – typed bindings/functions
- `errors_first_class.rn` – custom error unions and pattern matching
- `error_handling_required.rn` – explicit `try` / `catch`
- `restricted_error_sets.rn` – constrained error return types
- `optionals.rn` – Zig-style optionals with capture clauses
- `promises.rn` – async promises and `await`
- `structured_flow_control.rn` – keyword-driven branching
- `native_iteration.rn` – `for` / `while` over iterators
- `command_vs_expression.rn` – separation between processes and expressions
- `process_handles.rn` – synchronous and asynchronous process handles
- `pipeline_capture.rn` – capturing stdout/stderr from pipelines
- `error_aware_pipeline.rn` – per-stage pipeline errors
- `module_system.rn` – module imports via `src/`
- `legacy_compatibility.rn` – `bash { }` compatibility blocks
