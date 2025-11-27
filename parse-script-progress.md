# Parse Script Progress

| Step | Description | Status | Notes |
| --- | --- | --- | --- |
| 1 | Map script grammar to AST requirements | Complete | Added `docs/parse-script-constructs.md` summarizing every script construct ↔ AST type with parser status so we can reference it while implementing the parser. |
| 2 | Build foundational parsing helpers (patterns, types, interpolation, expression & pipeline parsing) | Complete | Extended the lexer with `$` tokens and taught the parser how to detect pipeline stages, parse command invocations (env assignments, `${expr}` args, redirects, capture directives, background `&`), and mix command/expr stages. Added regression tests covering these pipelines so later work can rely on them. |
| 3 | Implement statement parsers (let/mut, fn, error, control flow, bash, import) | Complete | Parser now builds dedicated statement nodes (including import detection, loops, return, bash blocks) and `docs/parse-script-constructs.md` reflects the coverage. |
| 4 | Implement `Parser.parseScript` + arena plumbing | Complete | Added `Parser.parseScript`, which iterates statements, copies them into an `ast.Script`, tracks script spans, and calls `expectEnd` to ensure the source is fully consumed. |
| 5 | Extend/add parser tests covering scripts | Complete | Added `parseScript` fixtures for let/pipeline/expression mixes, async fn + return, error decls, import bindings, loop + bash blocks, and command pipelines plus failure cases. Documented usage by running `ZIG_GLOBAL_CACHE_DIR="$PWD/zig-cache" zig test src/frontend/parser_tests.zig`. |
| 6 | Track follow-ups & integration hooks for ScriptExecutor/CLI | Complete | Added `docs/parse-script-integration.md` describing the runtime files to rewire, staged follow-ups (ScriptExecutor, evaluator expansion, CLI wiring), and the open compatibility questions we still need to answer. |
