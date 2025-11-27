1. Implement and test `Parser.parseScript` + supporting statement parsers (unit tests can live in `src/frontend/parser_tests.zig`; run with `zig test src/frontend/parser_tests.zig`).
2. Introduce a `ScriptExecutor` that accepts `ast .Script` and bridges each statement variant to ei ther the interpreter or the existing context/modu le helpers; add tests that feed small scripts thr ough this executor without touching the CLI yet.
3. Update `cmd/runic/run_script.zig` to call the new parser once per file and execute the resultin g AST instead of looping over strings; keep `prin tScriptTokens` as an optional debug output since it now mirrors the real token stream.
4. Remove or simplify the string parsers in `cmd/ runic/main-utils.zig` once their responsibilities are covered by AST execution, then run the curre nt CLI smoke tests (`zig build run` or whichever command exists in README) to validate behavior.

This staged plan lets us migrate from ad-hoc stri ng parsing to a single lexer→AST→interpreter pipe line while reusing the runtime components already in `src/frontend` and `src/interpreter`.
