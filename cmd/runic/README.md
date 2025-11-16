# cmd/runic/

This directory houses the Runic CLI entry point. Build it via `zig build run -- --help` (or `zig build`) to exercise the binary exactly the way users will. The current implementation focuses on argument parsing so the runtime scaffolding can mature independently:

- `runic path/to/script.rn [-- <script args>...]` — selects script mode. Arguments after the script path are forwarded verbatim; add `--` if you need to pass values that resemble CLI options.
- `runic --repl` — switches to REPL mode with multiline editing, persistent history, and simple pipeline execution (type `:help` inside the REPL for built-in commands).
- `--trace <topic>` (repeatable) — enables tracing for specific interpreter subsystems. Current topics: `pipeline` (per-stage logging), `process` (handle summaries), and `async` (scheduler/promise lifecycle).
- `--module-path <dir>` (repeatable) — adds directories to the module loader search roots so imports like `import http from "custom/http"` can point at additional trees.
- `--env KEY=VALUE` (repeatable) — collects environment overrides that will eventually be injected when scripts spawn commands.

Script execution still prints a stub summary while we continue wiring the interpreter. The REPL now provides a friendlier surface for quick experiments until the full runtime lands.
