# cmd/runic/

This directory houses the Runic CLI entry point. Build it via `zig build run -- --help` (or `zig build`) to exercise the binary exactly the way users will. The current implementation wires argument parsing to the script runner so contributors can execute `.rn` files while the interpreter continues to expand:

- `runic path/to/script.rn [-- <script args>...]` — selects script mode. Arguments after the script path are forwarded verbatim; add `--` if you need to pass values that resemble CLI options.
- `runic --repl` — switches to REPL mode with multiline editing, persistent history, and simple pipeline execution (type `:help` inside the REPL for built-in commands).
- `--trace <topic>` (repeatable) — enables tracing for specific interpreter subsystems. Current topics: `pipeline` (per-stage logging), `process` (handle summaries), and `async` (scheduler/promise lifecycle).
- `--module-path <dir>` (repeatable) — adds directories to the module loader search roots so imports like `import http from "custom/http"` can point at additional trees.
- `--env KEY=VALUE` (repeatable) — collects environment overrides that are injected when scripts spawn commands.

Script execution now loads the requested file, applies imports/`let` bindings, and runs the pipelines it contains with the same tracing hooks as the REPL. The REPL remains handy for interactive experiments, but both modes exercise the same command runner.
