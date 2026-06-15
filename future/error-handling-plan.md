# Error Handling — Implementation Plan

Tracking document for implementing Zig-like error handling in Runic.
Spec: [`future/error-handling.md`](./error-handling.md).

Branch: `error`.

This is a living document. Each AI session should:
1. Read this file + the spec first.
2. Pick the next unchecked task (respect phase ordering).
3. Update checkboxes and the **Status / Notes** of the phase as work lands.
4. Record any decisions made under **Open Decisions**.

---

## Current State (baseline audit)

Re-verified against branch `error` @ `de78261` (post-0.16.0 merge). `zig build test` is **green** (exit 0). A surprising amount of scaffolding already exists. Confirmed present:

| Area | What's there | File |
| --- | --- | --- |
| Tokens | `kw_error`, `kw_enum`, `kw_union`, `kw_struct`, `kw_try`, `kw_catch`, `bang` (`!`), `kw_match`, `kw_orelse` | `src/frontend/token.zig` |
| Lexer | keyword map populated for all of the above | `src/frontend/lexer.zig` |
| AST type exprs | `error_union {err_set, payload}`, `error_set {error_types[]}`, `err {error_payload}` — with formatters; `slotSize` returns 1 for each | `src/frontend/ast.zig:154-156, 217-246, 497` |
| AST expr nodes | `try_expr {subject}`, `catch_expr {subject, handler: CatchClause}`, `CatchClause {binding, body}`, `match_expr` (+`MatchCase`/`MatchPattern`) | `src/frontend/ast.zig:687-689, 1228-1241` |
| Type checker (resolve) | `resolveTypeExpr` (`:612`) + `runTypeExpression` (`:1031`) recurse into `error_union`; `error_set`/`err` are no-ops in `runTypeExpression` (`:1051`) | `src/semantic/type-checker.zig` |
| Type checker (assignment) ⚠️ | **Already scaffolded but unreachable/untested:** `validateTypeAssignment` dispatches error arms → `validateTypeAssignmentErrorUnion` (`:2186`), `validateTypeAssignmentErrorSet` (`:2213`), `validateErrorInSet` (`:2227`), `validateTypeAssignmentErrorType` (`:2249`). Covers union→union (err_set subset + payload), error→union, and ok-value→union coercion. `pipeTypesEqual` handles `error_union` (`:1682`). Caveats: variant matching is **pointer equality** (`error_set_type == err` — relies on interning that doesn't exist yet); has TODOs (`:2258`). | `src/semantic/type-checker.zig` |
| `match` | fully wired: parse, typecheck (`runMatchExpr` `:1112`), compile (`compileMatch` `:4145`), comptime eval | `parser.zig:1722`, `type-checker.zig:1112`, `compiler.zig:1830,4145` |
| Tree walker | `populateBlockExpr` descends into `try_expr`/`catch_expr`; type-checker's stdin validation also walks them (`type-checker.zig:848-851`) — but **only** the stdin pass, NOT `runExpression` | `cmd/runic/run_script.zig:319-341` |

**Not yet implemented (the gaps):**
- Parser: ~~`error { ... }` declarations~~ ✅ done in Phase 1 (`parseErrorTypeExpr`); still missing: postfix `!` error-union types; leading `!T` inferred-error-set types; `try <expr>`; `<expr> catch <default>` / `<expr> catch |err| <body>`; error value construction `E.Variant` / `E{ .Variant = payload }`. (The assignment-validation scaffolding is now partially reachable via error-set declarations but still untested for unions until `!` parsing lands in Phase 2.)
- Type checker: `runExpression` (`:993`) has **no** `try_expr`/`catch_expr` arms → falls to `error.UnsupportedExpression`. Missing: registering error sets as named types; variant interning (so pointer-equality matching works); `catch`/`try` semantics; inferred error sets; executable-call inherent error unions; switch/match exhaustiveness over error variants with payload capture.
- Runtime: `Value` has **no** error representation (`src/ir/value.zig:13`). Need the `.err` arm from D2.
- IR: no `compileTry`/`compileCatch`; no error-set decl compilation; no error-value construction; no error-union wrapping; no is-error/extract-payload instructions; no error-set type registry.

---

## Open Decisions

Resolve these as we go; record the choice + rationale inline.

- **D1 — `switch` vs `match` for error dispatch.** ✅ **RESOLVED: reuse `match`.** No `switch` keyword will be introduced; error dispatch uses `catch |err| match err { ... }`. The spec (`future/error-handling.md`) has been updated to match. `match` already handles path patterns, captures, and exhaustiveness scaffolding.
- **D2 — Runtime representation of an error union.** ✅ **RESOLVED.** There is *no* dedicated runtime error-union object — the type `E!T` is erased at runtime to a **single `Value` slot** holding whichever case occurred, discriminated by the `Value`'s own tag (mirrors how optionals use `.null`). Confirmed: `Value` is already a uniform tagged union with heap indirection (`Struct` holds `fields: []const Value`, `String` is a `slice` addr/len), so big payloads are already boxed and `slotSize(error_union) == 1` stays correct — no layout/max-size math needed.

  Add one arm to the `Value` union(enum) in `src/ir/value.zig`:
  ```zig
  err: Err,

  pub const Err = struct {
      set_id: usize,        // indexes the error-set type registry (cf. Struct.type: usize)
      variant_index: usize, // indexes the set's variant list
      payload: ?*Value,     // boxed; null for payload-less variants
  };
  ```
  **Discrimination rule:** tag is `.err` ⟹ error branch; any other tag ⟹ the ok payload. Stays unambiguous for nested cases like `MyError!?String` (`.err`, `.null`, `.slice` are distinct tags).

  Runtime examples for `const x: MyError!String`:
  - ok `"hello"` → `Value{ .slice = … }` (just the String — its error-union type is known only statically).
  - `MyError.UnknownError` → `Value{ .err = .{ .set_id = <MyError>, .variant_index = 0, .payload = null } }`.
  - `MyError{ .ErrorWithMessage = "boom" }` → `Value{ .err = .{ .set_id = <MyError>, .variant_index = 1, .payload = &Value{ .slice = "boom" } } }`.

  Consumers: `catch`/`try` test `value == .err`; `match err { MyError.X => … }` compares `err.set_id` + `err.variant_index`; payload capture `|m|` dereferences `err.payload`. `Value.deinit` gains an `.err` arm to free the boxed payload.

  Implies a new **error-set type registry** (parallel to the struct-type table) so `set_id` resolves to a set and `variant_index` to a variant — to be built in Phase 1/3.
- **D3 — `ExecutableError` definition.** Spec says all executable calls have stdout type `ExecutableError!String`. Where is `ExecutableError` defined — a builtin error set injected into the prelude/global scope, or a synthetic compiler type? **Recommendation:** define it as a builtin error set in global scope so it participates in inference and switch. Decision: _TBD_.
- **D4 — Error set member access vs payload struct literal.** `E.Variant` (no payload) reads like member/path access; `E{ .Variant = x }` reads like a struct literal. Confirm parser can disambiguate from existing struct-literal / member parsing. Decision: _TBD_.
- **D5 — Inferred error-set representation.** For `!String` and `fn ... !T`, how is the inferred set stored during checking (open set accumulated from body, then frozen)? Decision: _TBD_.

---

## Phases

Ordering matters: types before values before control flow before inference.

### Phase 0 — Foundations & test harness
- [x] Decide D1 (switch vs match) → reuse `match` (spec + plan updated).
- [x] Decide D2 (runtime representation) → flat `.err` arm on `Value`, type erased to one slot (see D2).
- [ ] Add empty regression test files under `tests/features/` (e.g. `error_sets.rn`, `error_catch.rn`, `error_try.rn`, `error_switch.rn`) and diagnostic fixtures under `tests/diagnostics/` to drive TDD.
- [ ] Confirm `zig build test` green before starting.

**Status / Notes:** _not started_

### Phase 1 — Error set type declarations ✅ COMPLETE
Goal: `const MyError = error { UnknownError, ErrorWithMessage: String }` parses, type-checks, and registers `MyError` as a named type.
- [x] Restructured `ast.TypeExpr.ErrorSet` to `variants: []const Variant` where `Variant = {name: Identifier, payload: ?*TypeExpr, span}` (+ `ErrorSet.variant(name)` helper); `ErrorType` similarly gained `{name, payload?}`. (`src/frontend/ast.zig`)
- [x] Implemented `parseErrorTypeExpr` and enabled the `.kw_error` dispatch in `parseMaybeTypeExpr` — parses `error { Variant, Variant: PayloadType, ... }` (commas and/or newlines separate; trailing comma OK). (`src/frontend/parser.zig`)
- [x] Registration works via the existing type-binding path: `parseTypeBinding` → `runTypeBindingDecl` → `scope.declare`. No new registration code needed.
- [x] Validation: `runErrorSet` (new) resolves each variant's payload type and reports duplicate variant names (new `Error.DuplicateErrorVariant`). Wired via `runTypeExpression`'s `.error_set` arm.
- [x] Switched membership checks to **name-based** (`validateErrorInSet` now takes `variant_name`/`span`), removing the pointer-equality hack — this also knocks out the interning concern flagged for Phase 2.
- [x] Tests: `tests/features/error_set_declaration_regression.rn` (+`.stdout`) and `tests/diagnostics/error_duplicate_variant.rn` (+`.status`/`.stderr`). Full suite green: `zig build test`, `zig fmt --check`, 68 smoke + 16 diagnostic scripts.

**Status / Notes:** ✅ Complete. Caveats deferred to later phases: (1) variant **payload types are not deeply validated** — `runTypeIdentifier` is a no-op across the whole type checker, so an unknown payload type name isn't yet caught (consistent with existing behavior). (2) `resolveTypeExpr` returns `error_set` via its `else` branch, so variant payloads are **not alias-resolved** yet (fine for primitive payloads; revisit in Phase 3). (3) The dead `Statement.error_decl`/`EnumBody`/`UnionBody` nodes were left untouched — candidates for removal in Phase 9.

### Phase 2 — Error union type syntax
Goal: `MyError!String`, and bare `!String`, parse as `ast.TypeExpr.error_union`.
- [ ] In `parseMaybeTypeExpr`/`parseTypeExpr`, handle postfix `!`: after parsing a type, if next token is `bang`, wrap into `error_union { err_set = <parsed>, payload = <parse next type> }`.
- [ ] Handle leading `!T` (no explicit error set) → `error_union` with an inferred/placeholder error set (ties into Phase 6).
- [ ] Type checker already resolves `error_union` recursively (`resolveTypeExpr`/`runTypeExpression`) and has assignment validation (`validateTypeAssignmentErrorUnion` et al.) — verify these actually fire once the parser produces the nodes; fix the pointer-equality variant matching (needs interning, see Phase 1).
- [ ] Test: `const x: MyError!String = ...` annotation parses & resolves.

**Status / Notes:** _not started — note: assignment validation is pre-built but unreachable/untested today; this phase is the first that can exercise it._

### Phase 3 — Error value construction + runtime representation
Goal: construct error values and represent them at runtime.
- [ ] Define the runtime `Value` error representation per D2 in `src/ir/value.zig` (add variant; update `deinit`, `slotSize` assumptions, serialize/deserialize as needed).
- [ ] Parse + compile `E.Variant` (payload-less) construction.
- [ ] Parse + compile `E{ .Variant = payload }` construction (resolve D4).
- [ ] Type-check that variant exists in the set and payload type matches.
- [ ] Compile "ok" values implicitly coercing into an error union when assigned to `E!T`.
- [ ] Test: construct each form; round-trip through a binding.

**Status / Notes:** _not started_

### Phase 4 — `catch` operator
Goal: `expr catch default` and `expr catch |err| body`.
- [ ] Parse `catch` as a postfix/binary operator on an expression, producing `ast.catch_expr`. Support both default-value and `|capture| block` forms (`CatchClause` already models `binding` + `body`).
- [ ] Type check: LHS must be an error union `E!T`; result type is `T`; default branch must be assignable to `T`; capture binds `err` to error-set type `E`. Non-error LHS → type error (per spec).
- [ ] Compile `compileCatch`: branch on is-error; if error, evaluate handler (with capture bound); else use payload.
- [ ] Test: `parseInt catch 0`; capture form; type error on non-error LHS.

**Status / Notes:** _not started_

### Phase 5 — `try` keyword
Goal: `try expr` desugars to `expr catch |err| return err`.
- [ ] Parse `try <expr>` → `ast.try_expr` (or desugar directly to `catch_expr` at parse time).
- [ ] Type check: enclosing function's return type must be an error union whose set is a superset of the LHS error set (drives Phase 6 inference).
- [ ] Compile (reuse Phase 4 catch machinery with an implicit `return err` handler).
- [ ] Test: `try` propagates error to caller; `try` in non-erroring fn → type error.

**Status / Notes:** _not started_

### Phase 6 — Inferred error sets
Goal: `const result = echo "hello"` infers `ExecutableError!String`; `fn ... !T { ... }` infers the union of error sets that can escape the body.
- [ ] Inference for bindings without annotation from an error-union RHS.
- [ ] Inference for fn return type `!T`: accumulate error sets from `try`/returned errors in the body, freeze into a concrete set (resolve D5).
- [ ] Test: examples from spec lines 42-49.

**Status / Notes:** _not started_

### Phase 7 — Executable calls carry inherent error unions
Goal: every executable call's stdout type is `ExecutableError!String`; non-zero exit becomes an error value at runtime.
- [ ] Define `ExecutableError` builtin error set (resolve D3).
- [ ] Type checker: give executable/pipeline call expressions stdout type `ExecutableError!String`.
- [ ] Runtime: map process exit code → ok(String) vs error value. Coordinate with `src/runtime/exit_code.zig` and process/stream layers.
- [ ] Test: `echo "1234" | parseInt catch 0`; failing command caught.

**Status / Notes:** _not started — touches process/stream/exit_code; likely the riskiest phase._

### Phase 8 — Switch/match on error values with payload capture
Goal: dispatch on error variants, capturing payloads (spec lines 73-80).
- [ ] Extend `match` to match error-set variant paths `MyError.Variant` (D1 resolved: `match`, not `switch`).
- [ ] Support payload capture in a case: `MyError.ErrorWithMessage => |message| ...`.
- [ ] Exhaustiveness checking across the error set.
- [ ] Compile variant test + payload extraction (reuse Phase 3 representation).
- [ ] Test: full `catch |err| match err { ... }` example.

**Status / Notes:** _not started_

### Phase 9 — Diagnostics, docs, polish
- [ ] Good diagnostics: `catch` on non-error, unknown variant, payload type mismatch, non-exhaustive switch, `try` outside erroring fn.
- [ ] Update `docs/features.md` with the error-handling surface.
- [ ] Add `examples/` script demonstrating errors end-to-end.
- [ ] LSP: hover/completion for error sets & variants (`src/lsp/`), if in scope.
- [ ] Run full CI: `zig build run -- scripts/run_ci.rn`.

**Status / Notes:** _not started_

---

## Key Files Reference

- `src/frontend/token.zig` — tokens (all needed keywords already present)
- `src/frontend/lexer.zig` — keyword map
- `src/frontend/parser.zig` — type-expr parsing (`parseMaybeTypeExpr` ~2925, stubs ~2933-2936), expression dispatch (~498), `match` (`parseMatchExpression` ~1722) as a template for `catch`/`try`
- `src/frontend/ast.zig` — type/expr node defs (error type nodes 154-156/217-246; expr nodes 687-689/1228-1241)
- `src/semantic/type-checker.zig` — `resolveTypeExpr` (612), `runTypeExpression` (1031), `runExpression` switch (~975-1000, needs try/catch arms), `runMatchExpr` (1112), assignment validation `validateTypeAssignmentErrorUnion` (2186)/`ErrorSet` (2213)/`validateErrorInSet` (2227)/`ErrorType` (2249)
- `src/semantic/scope.zig` — named-type registration (error-set registry)
- `src/ir/value.zig` — runtime `Value` (needs `.err` arm, D2)
- `src/ir/compiler.zig` — `compileExpression` switch (~1830), `compileMatch` (4145)
- `src/ir/evaluator.zig`, `src/ir/instruction.zig` — new opcodes for is-error / extract-payload
- `src/runtime/exit_code.zig` — for Phase 7
- `cmd/runic/run_script.zig` — `populateBlockExpr` (341) already walks try/catch (319-329)

## Testing

- Feature scripts: `tests/features/*.rn` (run via `./zig-out/bin/runic tests/features/<x>.rn`)
- Diagnostic fixtures: `tests/diagnostics/`
- Unit tests: inline `test { }` blocks, `zig build test`
- Full CI: `zig build run -- scripts/run_ci.rn`
