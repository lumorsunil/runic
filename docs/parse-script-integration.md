# Parse Script Integration Hooks

## Overview
The parser now produces full script ASTs (`Parser.parseScript`) plus tests that assert pipelines, functions, loops, and bash blocks. The CLI side still evaluates scripts by slicing raw text (`cmd/runic/run_script.zig`) and delegating each trimmed chunk to `executeScriptCommand` inside `cmd/runic/main-utils.zig`. This document captures where the new AST entry points should thread into the runtime and outlines the follow-up work so the ScriptExecutor and CLI wiring tasks have concrete targets.

## Runtime entry points to rewire

### `cmd/runic/run_script.zig`
- Currently streams the file line-by-line, handles continuations, skips inline function bodies, and forwards each finished chunk to `executeScriptCommand`.
- Once a script executor exists, this loop should be replaced with: lex+parse the whole buffer via `Parser.parseScript`, pass the AST (or parse errors) through `utils.printScriptTokens/printAst`, and invoke the executor with the resulting `ast.Script`.
- Module prelude code already lives here (env map creation, module registry, inline function registration), so the new flow only needs to hand the `ScriptContext`, `ModuleRegistry`, and `CommandRunner` handles to the executor instead of looping over strings.

### `cmd/runic/main-utils.zig`
- This file currently does *everything*: parsing let/mut/error declarations, parsing module invocations (`parseModuleInvocation`), evaluating inline `ensure` clauses, compiling regex-y snippets into ASTs via `recordAstSnippet`, tokenizing pipelines (`runPipelineInternal`), and invoking the interpreter via `InterpreterState`.
- Follow-ups should split these roles:
  - Build a `ScriptExecutor` (likely lives next to `InterpreterState`) that walks `ast.Script`, reuses `ScriptContext` for bindings, drives `ModuleRegistry` for `import` statements, and routes `ast.CommandInvocation` stages to the command runner.
  - Keep helper code such as module loading, env overrides, and stack trace rendering here, but delete ad-hoc parsers (`parseLetBindingWithOptions`, `parseEnsureCall`, pipeline tokenization) once AST execution takes over.
  - Update `recordAstSnippet`/`InterpreterState.executeSnippet` to accept direct AST nodes instead of reparsing strings when statements already come from `parseScript`.

### `src/interpreter/evaluator.zig`
- Evaluator only supports let declarations, expression statements, literals, identifiers, simple block expressions, and pipelines without env assignments or redirects. It also fabricates pipelines manually before calling `CommandExecutor`.
- To back the script executor, Evaluator (or a sibling component) must implement the rest of the `ast.Statement` and `ast.Expression` variants that Step 3 unlocked: import statements, function declarations, loops, error decls, bash blocks, capture clauses, async/await, match, unary/binary operators, etc.
- Pipeline support must be upgraded to honor `ast.CommandInvocation.env_assignments`, redirects, capture directives, background execution, and expression stages so CLI behavior matches `runPipelineInternal`.

### `cmd/runic/pipeline.zig`
- The current token + spec builder is still used by `runCommandPipeline` and module invocation execution to talk to `CommandRunner`. Once Evaluator emits the same `CommandRunner.CommandSpec` data directly from `ast.CommandInvocation`, this helper becomes redundant except for legacy paths (REPL fallback, error reporting). The integration work should plan either to delete it or keep it only for legacy CLI paths.

### Module + REPL hooks
- `modules.registerInlineFunctions` already parses functions via the module parser; once scripts run through AST execution, their definitions should land in the same `ModuleRegistry`, so script-level functions can be called by later statements without manual string parsing.
- `cmd/runic/repl.zig` reuses `executeScriptCommand` for each entered line. When the script executor exists, ensure the REPL can either parse per-line fragments or still access the legacy helpers until incremental parsing is available.

## Follow-up tasks to stage
1. **ScriptExecutor bridging (`cmd/runic/main-utils.zig` or new module)** – Accepts `ast.Script`, `ScriptContext`, and `CommandRunner`, drives statements in order, and reports diagnostics using the existing stderr helpers.
2. **Evaluator expansion (`src/interpreter/evaluator.zig`)** – Implement remaining statements/expressions plus full command invocation semantics so the executor can stay thin.
3. **CLI wiring (`cmd/runic/run_script.zig` + `cmd/runic/repl.zig`)** – Replace the line-based loop with `parseScript`, thread parse errors through the current `print_ast/print_tokens` switches, and delegate to the ScriptExecutor.
4. **Module/ensure/import integration** – Map `ast.Statement.import_stmt` nodes to `ModuleRegistry.ensureImport` calls, translate `ensure` statements/functions into AST-friendly forms, and drop the custom `parseEnsureCall` path.
5. **Legacy pipeline teardown** – After AST execution handles env assignments, redirects, captures, and background commands, delete `runCommandPipeline`, `TokenList.populate`, and related token interpolators, keeping any display helpers required for error rendering.

## Open questions
- How much of the legacy command syntax (implicit continuations, escaped newlines, inline `ensure`, module invocation shortcuts) must be supported on day one versus migrated gradually? We need a compatibility matrix before deleting `executeScriptCommand`.
- What is the mapping between `ScriptContext` bindings and interpreter scopes? Today we only reseed string bindings; once AST statements introduce other value types, should ScriptContext move into the interpreter entirely?
- Should `ensure` remain a special form (so it keeps precise error locations) or become a normal function call handled by the interpreter?
- When pipelines capture handles or destructure `{ stdout, stderr }`, should Evaluator return process snapshots directly, or should the executor keep delegating to `runPipelineInternal` until we have feature parity?
