# Runic Plan Progress

## Completed Tasks

### Phase 1 — Repository & Tooling Scaffolding
- **Task 1:** Establish the canonical directory layout: `src/` for interpreter modules, `cmd/runic/` for the CLI entry point, `tests/` for integration suites, and `docs/` for reference material. (completed 2025-11-15T16:42:11+01:00)
  - Moved the CLI entry point to `cmd/runic/main.zig`, kept interpreter modules under `src/`, and added brief README guides (including `docs/README.md`) so contributors understand what belongs in each directory.
- **Task 3:** Define baseline CI/test scripts that run formatter → linter → unit tests → CLI smoke tests so future changes adhere to the workflow. (completed 2025-11-15T15:48:05+01:00)
  - Added `scripts/run_ci.sh` plus stage scripts for formatting, linting, unit tests, and CLI smoke suites, along with a placeholder `tests/cli_smoke.sh`.

### Phase 2 — Front-End (Lexer, Parser, AST)
- **Task 5:** Implement a streaming lexer that emits tokens with location info for precise diagnostics. (completed 2025-11-15T16:03:30+01:00)
  - Added `src/frontend/token.zig` for token/tag definitions with keyword mapping plus location/span helpers.
  - Implemented `src/frontend/lexer.zig`, a streaming lexer with comment skipping, literals, punctuation, and newline tokens, along with unit tests documenting representative lexing output.

1. Establish the canonical directory layout: `src/` for interpreter modules, `cmd/runic/` for the CLI entry point, `tests/` for integration suites, and `docs/` for reference material. (completed 2025-11-15T16:40:42+01:00)
