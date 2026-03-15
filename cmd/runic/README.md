# cmd/runic/

This directory houses the Runic CLI entry point. Build it via `zig build run -- --help` (or `zig build`) to exercise the binary exactly the way users will. The current implementation wires argument parsing to the IR-backed script runner so contributors can execute `.rn` files the same way users will:

- `runic path/to/script.rn [-- <script args>...]` — selects script mode. Arguments after the script path are forwarded verbatim; add `--` if you need to pass values that resemble CLI options.
- `runic --eval 'echo "${1 + 2}"'` — executes inline Runic source directly from the command line.
- `--trace <topic>` (repeatable) — enables tracing for specific runtime subsystems. Current topics: `pipeline` (per-stage logging), `process` (handle summaries), and `async` (scheduler/promise lifecycle).
- `--module-path <dir>` (repeatable) — adds directories to the module loader search roots so bindings like `let http = import("custom/http")` can point at additional trees.
- `--env KEY=VALUE` (repeatable) — collects environment overrides that are injected when scripts spawn commands.
- `--print-ast` — parses the script and prints each recorded AST snippet for debugging before running it.
- `--print-tokens` — lexes the provided script and prints every token/span pair before execution so you can inspect how the lexer sliced the source.

Script execution now loads the requested file, applies imports/`let` bindings, and runs the pipelines it contains through the IR runtime.
