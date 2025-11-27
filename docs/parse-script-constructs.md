# Script Grammar → AST Coverage

Step 1 of `parse-script-plan.md` maps the surface syntax used by real scripts (`features.md`, `examples/async_and_legacy.rn`) and the current CLI heuristics (`cmd/runic/main-utils.zig`) to the AST types declared in `src/frontend/ast.zig`. The checkboxes indicate whether the parser already produces those nodes. Almost every construct is still unchecked because `src/frontend/parser.zig:222-235` only emits expression statements backed by identifiers, literals, and `if` expressions today.

## Legend
- `[ ]` – AST type exists but parser never emits it yet.
- `[x]` – Parser already emits this node (note any limitations beside the entry).

## Statement forms
- [x] **Let/Mut declarations** → `ast.Statement.let_decl` / `ast.LetDecl` (`src/frontend/ast.zig:533-566`). Required for the basic bindings in `features.md:19-36` and the module alias in `examples/async_and_legacy.rn:4`. Parser must also recognize destructuring patterns and optional type annotations.
- [x] **Import statements** → `ast.Statement.import_stmt` / `ast.ImportStmt` (`src/frontend/ast.zig:533-626`). Scripts use `let alias = import("spec")` (`features.md:258-276`), and plan step 3 calls out special-casing these bindings into `ImportStmt` nodes so modules can be resolved before evaluation.
- [x] **Function declarations** → `ast.Statement.fn_decl` / `ast.FunctionDecl` (`src/frontend/ast.zig:533-582`). Needed for user functions, defaults, return types, and async modifiers highlighted in `features.md:146-176`.
- [x] **Error declarations** → `ast.Statement.error_decl` (`src/frontend/ast.zig:533-614`). Scripts declare both enums and tagged unions (`features.md:42-79`, `examples/async_and_legacy.rn:6-16`).
- [x] **Return statements** → `ast.Statement.return_stmt` / `ast.ReturnStmt` (`src/frontend/ast.zig:533-630`). Required inside functions illustrated across `features.md:49-80`.
- [x] **For loops** → `ast.Statement.for_stmt` / `ast.ForStmt` (`src/frontend/ast.zig:533-637`). Examples at `features.md:160-177` rely on tuple captures, multi-source loops, and block bodies.
- [x] **While loops** → `ast.Statement.while_stmt` / `ast.WhileStmt` (`src/frontend/ast.zig:533-645`). Supported syntax appears in the same iteration section of `features.md:160-177`.
- [x] **Bash blocks** → `ast.Statement.bash_block` / `ast.BashBlock` (`src/frontend/ast.zig:522-525`, `533-555`). Scripts embed legacy shell snippets as shown in `features.md:304-317` and `examples/async_and_legacy.rn:39-42`.
- [x] **Expression statements** → `ast.Statement.expression` / `ast.ExpressionStmt` (`src/frontend/ast.zig:533-649`). Currently the only emitted statement shape, but expression coverage is still limited to identifiers, literals, and `if` expressions (`src/frontend/parser.zig:222-312`), so richer expressions, pipelines, and commands still cannot appear at the top level.

## Command & pipeline surfaces
- [ ] **Pipeline expressions** → `ast.Expression.pipeline` / `ast.Pipeline` (`src/frontend/ast.zig:197-244`, `424-446`). Scripts rely on bare pipelines and expression-mixed stages (`features.md:5-14`, `features.md:218-251`). Interpreter tests already fabricate these nodes (`src/interpreter/evaluator.zig:463-555`), so the parser must construct identical structures.
- [ ] **Command invocations** → `ast.CommandInvocation` plus `ast.CommandPart` variants (`src/frontend/ast.zig:448-504`). Real scripts use word args, quoted strings, interpolations, and nested expressions (`features.md:192-236`). The current CLI emulates parsing by tokenizing raw source at runtime (`cmd/runic/main-utils.zig:1698-2055`), highlighting the env assignments, redirects, captures, and handles the AST eventually needs.
- [ ] **Env assignments** → `ast.EnvAssignment` (`src/frontend/ast.zig:469-473`). Needed for syntax such as `NAME=value cmd` and capture shorthand in `features.md:222-235`. Runtime tokenization already exposes them via `TokenList.populate` and `pipeline_builder.build` (`cmd/runic/main-utils.zig:1909-1975`).
- [ ] **Redirects** → `ast.Redirection` / `ast.StreamRef` / `ast.RedirectionTarget` (`src/frontend/ast.zig:475-497`). Feature doc uses `1>var` and `2>$status_stderr` (`features.md:195-215`).
- [ ] **Capture directives** → `ast.CaptureDirective` / `ast.CaptureEntry` (`src/frontend/ast.zig:499-519`). Scripts specify capture policies (`features.md:218-236`).
- [ ] **Background execution** → `CommandInvocation.background` (`src/frontend/ast.zig:448-455`), needed for `cmd &` usages (`features.md:203-216`, `examples/async_and_legacy.rn:28`).
- [ ] **Process handle destructuring** – part of normal binding patterns but tied to command nodes: destructuring `let { stdout, stderr } = cmd` requires the parser to support record binding patterns (see the Binding Pattern section below).

## Expression & control-flow forms
- [x] **Identifiers and literals** → `ast.Expression.identifier` / `.literal` (`src/frontend/ast.zig:197-236`). Supported today, but string literals need interpolation-aware segments (see String Literal entry).
- [ ] **String interpolation** → `ast.StringLiteral.Segment.interpolation` (`src/frontend/ast.zig:148-175`). Parser currently emits a single `.text` segment per string (`src/frontend/parser.zig:289-312`), so `${expr}` never becomes AST.
- [x] **If expressions with capture clauses** → `ast.Expression.if_expr` (`src/frontend/ast.zig:197-244`, `363-374`). Already parsed, including optional capture blocks (`src/frontend/parser.zig:111-176`), enabling optional-aware `if` cases from `features.md:103-118`.
- [ ] **Match expressions & patterns** → `ast.Expression.match_expr` / `ast.MatchPattern` (`src/frontend/ast.zig:376-405`). Needed for the `match err { ... }` blocks at `features.md:52-59` and `examples/async_and_legacy.rn:22-26`.
- [ ] **Await expressions** → `ast.Expression.await_expr` (`src/frontend/ast.zig:351-360`). Scripts await promises with success/failure captures (`features.md:119-141`, `examples/async_and_legacy.rn:18-27`).
- [ ] **Async blocks** → `ast.Expression.async_expr` (`src/frontend/ast.zig:339-355`), required for `async { ... }` in `features.md:121-134`.
- [ ] **Try/Catch expressions** → `ast.Expression.try_expr` / `.catch_expr` (`src/frontend/ast.zig:407-422`). Scripts use `try` and `catch` inline (`features.md:66-80`, `examples/async_and_legacy.rn:10-26`).
- [ ] **Function literals / closures** → `ast.Expression.fn_literal` (`src/frontend/ast.zig:339-349`). Needed for snippets like `fn (line) => ...` inside command captures (`features.md:229-235`).
- [ ] **Call / member / index expressions** → `ast.CallExpr`, `ast.MemberExpr`, `ast.IndexExpr` (`src/frontend/ast.zig:280-302`). Required across the examples that call module methods or access fields (`features.md:147-208`, `examples/async_and_legacy.rn:10-37`).
- [ ] **Binary/unary operations** → `ast.BinaryExpr`, `ast.UnaryExpr` (`src/frontend/ast.zig:304-337`). Loops and guards use arithmetic and comparisons (`features.md:147-175`, `examples/async_and_legacy.rn:11-33`).
- [ ] **Array/map/range literals** → `ast.ArrayLiteral`, `ast.MapLiteral`, `ast.RangeLiteral` (`src/frontend/ast.zig:257-279`). Found in `features.md:19-35`, `features.md:160-167`, and destructuring examples.
- [ ] **Block expressions** → `ast.Expression.block` (`src/frontend/ast.zig:197-236`). Needed for inline blocks returned from expressions (function bodies, capture blocks).
- [ ] **Pipeline expressions mixed with expressions** – the AST already allows expression stages (`ast.StageRole.expression`, `src/frontend/ast.zig:432-446`), which scripts require for `(build | tee).status` and `let files = ls ./src | lines()` (`features.md:183-188`, `240-248`).

## Binding patterns, captures, and types
- [ ] **Tuple & record binding patterns** → `ast.BindingPattern.tuple` / `.record` (`src/frontend/ast.zig:27-61`). Necessary for destructuring process handles (`features.md:199-214`) and loop captures (`features.md:160-176`). Parser currently only supports identifiers/discards (`src/frontend/parser.zig:178-193`).
- [ ] **Capture clauses** → `ast.CaptureClause` and `ast.CaptureBlock` (`src/frontend/ast.zig:63-76`). Although the parser can read capture bars (used by `if`), it must reuse the same helper for loops, await blocks, and catch handlers.
- [ ] **Type expressions** → `ast.TypeExpr` variants (`src/frontend/ast.zig:78-146`). Scripts use optional (`?T`), promise (`^T`), error unions (`!T`), arrays, tuples, and inline records for structs (`features.md:28-34`, `features.md:121-140`, `features.md:318-329`). Module parsing currently skips type syntax entirely (`src/frontend/parser.zig:302-338` via `skipTypeExpression`), so a full type parser is still outstanding.
- [ ] **Function parameters** → `ast.Parameter` (`src/frontend/ast.zig:568-575`). Needed for parsing `mut` flags, annotations, and default values described across the function examples.

## Runtime heuristics to replace
`cmd/runic/main-utils.zig:1698-2055` shows how the CLI currently tokenizes script source using `pipeline.TokenList`, rewrites env assignments, resolves `${...}` against bindings, and builds pipelines on the fly before invoking `CommandRunner`. Once `Parser.parseScript` exists, this ad-hoc pipeline builder should be replaced by AST-driven execution, so the parser must produce every structure listed above (including the same capture policies and background markers) for the interpreter to consume directly.
