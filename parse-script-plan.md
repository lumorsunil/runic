# Parser.parseScript Implementation Plan

## Current State
- `src/frontend/parser.zig` currently exposes expression parsing for identifiers, literals, and `if` expressions plus the module document parser, but it **does not** offer a script entry point (`parseScript`) or statement-level parsing (`parseStatement` only emits expression statements; see lines 197-238).
- The AST already models script constructs such as `ast.Script`, `ast.Statement`, `ast.LetDecl`, loops, `ImportStmt`, pipelines, etc. (see `src/frontend/ast.zig` near lines 424-660), so the parser must start producing those nodes.
- Type expressions, binding patterns beyond single identifiers, interpolated strings, pipelines, and command invocations are all represented in the AST but have no parsing support yet. The module parser currently *skips* these structures (e.g. `skipTypeExpression`), which is insufficient for scripts where we need the actual nodes.
- `src/frontend/parser_tests.zig` only covers low-level helpers (let/mut snippets, function decl fixtures, if-expression AST checks, and module document parsing). There are no tests that assert script parsing behavior or statement coverage.

## Step-by-step Task List
1. **Map Script Grammar to AST requirements**
   - Catalogue every statement and expression form scripts currently rely on (`features.md`, `examples/async_and_legacy.rn`, and `cmd/runic/main-utils.zig` heuristics).
   - Document which AST types correspond to each surface (e.g. `let` → `ast.LetDecl`, `bash { ... }` → `ast.BashBlock`, env assignments and redirects → `ast.CommandInvocation` fields) so we know the parser features we must implement before runtime code can consume the AST.
   - Outcome: a checked list of constructs with links into the AST so future contributors know which pieces still block full script parsing.

2. **Build foundational parsing helpers**
   - Expand binding pattern parsing to cover tuples/records/rest bindings so `let`/loop captures match `ast.BindingPattern` (currently only identifiers/`_` are handled inside capture clauses).
   - Implement a real type-expression parser that can construct `ast.TypeExpr` nodes for prefix modifiers (`?`, `!`, `^`), named paths, tuples, records, arrays, and function types (needed for `let` annotations, function signatures, and error union payloads). Replace the module parser’s `skipTypeExpression`/`skipInitializerExpression` usage with the new helpers where possible.
   - Teach `parseStringLiteral` how to split segments and parse `${expr}` interpolations so the resulting `ast.StringLiteral` matches the runtime interpolation semantics that currently happen in `cmd/runic/main-utils.zig`.
   - Introduce expression-precedence helpers (e.g. `parseUnary`, `parseMultiplicative`, `parseAdditive`, logical comparisons) plus post-fix parsing for calls/members/indexing, arrays/maps/ranges, async/await, match/try/catch, and block/closure literals. These functions will feed every statement that embeds an expression.
   - Implement pipeline + command parsing: detect command stages vs. expression stages, parse env assignments (`NAME=value`), arguments (words, strings, interpolations), redirects (`1>var`, `2>>expr`), capture directives (`capture = { stdout: :stream }`), and trailing `&` for background processes (`ast.CommandInvocation`) so that bare command statements and command-containing expressions parse correctly.

3. **Implement statement parsers**
   - `let` / `mut` declarations: parse optional destructuring patterns, optional type annotations, and initializer expressions. Special-case `let name = import("spec")` into `ast.Statement.import_stmt` by looking for `kw_import` to avoid double-parsing module imports.
   - Function declarations: handle optional `async` prefix, parameter lists (with default values and `mut` flags), optional return type, and bodies (block vs. expression) into `ast.FunctionDecl`.
   - Error declarations: support both `error Name = enum { ... }` and `error Name = union { Variant, Variant: Type }` forms (`ast.ErrorDecl`).
   - Control-flow statements: `return` with optional expression, `for`/`while` loops with capture clauses and bodies, and optional capture on `while` (per AST). Reuse the binding-pattern improvements from Step 2.
   - Bash blocks: parse `bash { ... }` by tracking braces and slicing the raw body text (`ast.BashBlock`).
   - Expression statements/pipelines remain the fallback when none of the keywords match, leveraging the expanded expression parser from Step 2.
   - Ensure `parseBlock` and any block-expression helpers call the upgraded `parseStatement` so nested blocks share the same syntax as the script root.

4. **Implement `Parser.parseScript` and allocation plumbing**
   - Iterate tokens until EOF, skipping terminators, and collect pointers to statements allocated out of the arena (mirroring `parseBlock`/`parseModuleDocument` but targeting `ast.Script`).
   - Track the script span (from first non-newline token to the final statement or EOF) so diagnostics can highlight the full file.
   - Expose helpers like `sliceForRange`/`sliceForSpan` when statements (e.g. bash blocks) need to capture raw text slices from the source buffer.
   - Ensure `Parser.expectEnd` still works by invoking it at the end of script parsing to confirm we consumed everything except trailing newlines.

5. **Extend and add tests (`src/frontend/parser_tests.zig`)**
   - Introduce fixtures that call `Parser.parseScript` and assert the resulting AST for representative snippets:
     - Basic script mixing `let`, command pipelines, and expression statements.
     - Function declaration + return statement recovery.
     - Error declaration with enum/union payloads.
     - Module import detection.
     - `for`/`while` loops with capture clauses.
     - Bash block body extraction.
     - Pipelines exercising env assignments, redirects, captures, and background markers.
   - Add failure fixtures that surface parse errors for unterminated blocks, malformed capture clauses, invalid command syntax, etc.
   - Update any existing tests that relied on the limited expression parser to account for the richer AST nodes.
   - Run and document `zig test src/frontend/parser_tests.zig` as the verification command contributors should use.

6. **Follow-up tracking & integration hooks**
   - Note in the plan which runtime pieces (`cmd/runic/run_script.zig`, `cmd/runic/main-utils.zig`, interpreter) will start consuming the AST after `parseScript` exists so the next tasks (ScriptExecutor, CLI wiring) have clear entry points.
   - Capture open questions (e.g. how much legacy command syntax we need to support immediately vs. staged rollout) to revisit once the parser scaffolding is in place.
   - Concrete notes now live in `docs/parse-script-integration.md`, which enumerates the runtime touch points, staged follow-ups, and open compatibility decisions.

## Reimplementation Strategy & Milestones

Given that `parseScript` and the surrounding statement parser no longer exist in `src/frontend/parser.zig`, we will restore the entry point in three concrete phases:

1. **Foundations (binding patterns, types, expressions)** – tracked by Tasks 1–2 above. This milestone concludes when:
   - `parseBindingPattern` handles tuples/records/rest and is reusable from `parseOptionalCaptureClause`, `parseLet`, and loop captures.
   - A new `parseTypeExpr` family replaces `skipTypeExpression`, with unit tests covering optional/promise/error-union syntax.
   - Expression parsing supports precedence, postfix operations (call/member/index), literal collections, async/await/match/try/catch, pipelines, and command invocations (env assignments, redirects, capture directives, background flag).
2. **Statements & script blocks** – covers Task 3. Success criteria:
   - Dedicated parsers for each `ast.Statement` variant, including specialized handling for `let name = import("spec")` → `ast.Statement.import_stmt`.
   - `parseBlock` reuses `parseStatement` so nested scopes provide the same syntax as top level.
   - Bash block bodies slice the original source via helper functions (`sliceForRange`/`sliceForSpan`).
3. **Script entry point & tests** – Tasks 4–5.
   - Implement `Parser.parseScript` that iterates statements until EOF, tracks the script span, and calls `expectEnd`.
   - Update `src/frontend/parser_tests.zig` with the existing script fixtures (let+pipeline, async fn, errors, imports, loops, bash blocks, pipelines) plus new failure tests, and ensure `zig test src/frontend/parser_tests.zig` passes.

Each milestone should land in its own PR to keep reviews manageable. After Milestones 1–3 complete, we will revisit Task 6 to double-check integration touch points (ScriptExecutor, CLI wiring) before deleting the ad-hoc runtime parsers.
