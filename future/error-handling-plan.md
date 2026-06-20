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

  **Identification is NAME-BASED** (revised 2026-06-15 after auditing the IR layer; the original index-based scheme was rejected because the compiler has no type/scope access and treats all type decls as no-ops, so a global registry would be extra machinery — and Phases 1-2 type-checking is already name-based). Add one arm to the `Value` union(enum) in `src/ir/value.zig`:
  ```zig
  err: Err,

  pub const Err = struct {
      set: []const u8,        // error set name, e.g. "MyError"
      variant: []const u8,    // variant name, e.g. "UnknownError"
      payload: ?*const Value, // boxed; null for payload-less variants
  };
  ```
  Names are arena/Zig-side strings (consistent with `zig_string`/`fn_ref`). No global error-set registry needed; `match`/`catch` compare strings, resolved statically by the compiler.

  **Discrimination rule:** tag is `.err` ⟹ error branch; any other tag ⟹ the ok payload. Stays unambiguous for nested cases like `MyError!?String` (`.err`, `.null`, `.slice` are distinct tags).

  Runtime examples for `const x: MyError!String`:
  - ok `"hello"` → `Value{ .slice = … }` (just the String — its error-union type is known only statically).
  - `MyError.UnknownError` → `Value{ .err = .{ .set = "MyError", .variant = "UnknownError", .payload = null } }`.
  - `MyError{ .ErrorWithMessage = "boom" }` → `Value{ .err = .{ .set = "MyError", .variant = "ErrorWithMessage", .payload = &Value{ .slice = "boom" } } }`.

  Consumers: `catch`/`try` test `value == .err`; `match err { MyError.X => … }` compares `err.set`/`err.variant`; payload capture `|m|` dereferences `err.payload`. `Value.deinit` gains an `.err` arm to free the boxed payload.

  **Serialization caveat:** the dead `Value.serialize`/`deserialize`/`deinit` methods reference non-existent variants (`.location`, `.register`) and are not analyzed by Zig (never called), so they don't block adding `.err`. If error values ever need to cross a pipe/process boundary, `.err` serialization must be implemented then.

  Implies a new **error-set type registry** (parallel to the struct-type table) so `set_id` resolves to a set and `variant_index` to a variant — to be built in Phase 1/3.
- **D3 — `ExecutableError` definition.** ✅ **RESOLVED (Phase 7a): builtin error set in global/prelude scope** (`NonZeroExit: Int`, `Signalled: Int`, `SpawnFailed`), so it participates in checking like a user error set.
- **D7 — Execution error model.** ✅ **RESOLVED (2026-06-16).** Errors short-circuit (not stream data); handling is enforced (catch/`||`/propagate, else compile error); booleans-as-exit-codes reinterpreted as ok-vs-error (`||` = catch-and-discard); command value view is `ExecutableError!String` while `ExecutionResult` remains the explicit handle. Full model + staging in the Phase 7 section.
- **D4 — Error set member access vs payload struct literal.** ✅ **RESOLVED (Phase 3a/3b).** `E.Variant` rides member access; `E{ .Variant = x }` is a new struct-literal node, disambiguated by an **uppercase identifier immediately followed by `{`** (lowercase command parsing untouched). Both implemented.
- **D5 — Inferred error-set representation.** ✅ **RESOLVED (Phase 6): empty error set = inferred.** A leading-`!T` parses to an `error_union` whose `err_set` is an empty `error_set` (`variants.len == 0`); `isInferredErrorSet` treats that as "inferred", and yield validation accepts any error into it (**open-set** model). Concrete accumulation of the body's exact union (and propagation to callers, for `match` exhaustiveness) is deferred until Phase 8 needs it.
- **D6 — Error propagation model.** ✅ **RESOLVED (2026-06-15): value/yield-based.** `catch`/`try` operate on the error union a function/pipeline **produces** (via `yield`→stdout), not on `return`. `try` re-yields the error to propagate. See the Phase 3c notes for full rationale. `catch` on directly-produced error unions is model-independent and is built first.

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

### Phase 2 — Error union type syntax ✅ COMPLETE
Goal: `MyError!String`, and bare `!String`, parse as `ast.TypeExpr.error_union`.
- [x] Split `parseMaybeTypeExpr` into a primary parser (`parseMaybePrimaryTypeExpr`) + postfix `!` handling: after a primary type, if next is `bang`, wrap into `error_union { err_set = primary, payload = parseTypeExpr() }`. (`src/frontend/parser.zig`)
- [x] Leading `!T` → `parseInferredErrorUnionTypeExpr`: builds an `error_union` whose `err_set` is an **empty `error_set` placeholder** (`variants = &.{}`) meaning "infer me" (Phase 6 fills it in).
- [x] Verified the type checker resolves + validates these once produced: `const x: MyError!String = "hi"` and `const y: !String = "hi"` type-check & run; error-union fn return types parse (`fn Void getOne() MyError!Int { return 1 }`); payload type mismatch (`MyError!String = 5`) is rejected with a good diagnostic. Name-based membership (from Phase 1) is in place — no interning needed.
- [x] Tests: `tests/features/error_union_types_regression.rn` (+`.stdout`); `tests/diagnostics/error_union_payload_mismatch.rn` (+`.status`/`.stderr`). Full suite green (69 smoke + 17 diagnostic).

**Status / Notes:** ✅ Complete (type **syntax + assignment** only). Important caveats for Phase 3:
- **Runtime error-union values don't work yet.** A fn returning `E!Int` does not unwrap its ok value — `${getOne}` interpolates empty. That's the Phase 3 runtime representation (D2).
- **Latent panic in `validateTypeAssignmentErrorUnion`:** the `.error_union`/`.err` arms access `assignee.err_set.error_set` directly, but after resolution `err_set` is often an `.alias` (or `.identifier`) wrapping the set, not a raw `.error_set` — accessing the wrong union field will panic. Not hit in Phase 2 (plain-value assignment uses the `else` branch only), but **must unalias before `.error_set` access in Phase 3** when error/union values start flowing.

### Phase 3 — Error value construction + runtime representation
Goal: construct error values and represent them at runtime. Split into 3a/3b/3c (decided 2026-06-15).

#### Phase 3a — Payload-less `E.Variant` construction + runtime `Value.err` ✅ COMPLETE
- [x] Added `Value.err: Err{ set, variant, payload: ?*const Value = null }` (name-based, per D2) with a `format` method. Only **two** analyzed switches needed `.err`: `compiler.zig` call-callee (pass-through via `.from(v)`, like other non-callable values) and `evaluator.zig` `materializeString` (prints `Set.Variant`); plus an explicit `.err` arm in `Value.format`. (The dead `deinit`/`serialize`/`deserialize` aren't analyzed, so untouched.)
- [x] Compiler: added `error_sets: StringHashMapUnmanaged(ErrorSet)` + `registerErrorSets()` pre-pass over top-level `type_binding_decl`s with `error_set` type_expr.
- [x] Compiler: `compileMember` special-cases `member.object` = identifier in `error_sets` → `compileErrorVariant` emits `Result.fromValue(.{ .err = ... })`. Validates the variant exists (else diagnostic) and rejects payload variants with a "use `E{ .V = ... }` (3b)" message.
- [x] Type checker: added `runErrorSetMemberAccess` + an `.error_set` arm in `MemberExpr.resolveType` (so `MyError.Variant` has the error-set type). **Caveat:** for `const e = MyError.Nope` this type-check path doesn't currently fire — the **compiler** catches unknown variants first (diagnostic on **stdout**, like other compiler errors). Type-check member typing for error sets is wired and correct-by-construction for when consumed (Phases 4/8); the resolve path that bypasses it is unexplained and deferred.
- [x] Observable: `MyError.UnknownError` constructs, binds, and prints as `MyError.UnknownError` (both bound and inline).
- [x] Tests: `tests/features/error_value_construction_regression.rn` (+`.stdout`); `tests/diagnostics/error_unknown_variant.rn` (+`.stdout`/`.status`). Full suite green (70 smoke + 18 diagnostic).

#### Phase 3b — Payloaded `E{ .Variant = payload }` construction ✅ COMPLETE
- [x] **New struct-literal parsing.** Added `ast.Expression.struct_literal: StructLiteral{ name, fields: []FieldInit{name, value} }`. The binary-expression parser intercepts an **uppercase (type-like) identifier immediately followed by `{`** → `parseStructLiteral` parses `{ .field = expr, ... }` (commas/newlines, trailing comma OK). Uppercase gating keeps lowercase command parsing untouched. Threaded the new variant through the two exhaustive expr walks (`run_script.zig` `populateStackExpr`, `type-checker.zig` `validateFunctionBodyStdin`).
- [x] Type checker: `runStructLiteral` → for error-set names, `runErrorValueLiteral` validates exactly one field, the variant exists, the variant has a payload, and the value type matches the variant's (resolved) payload type. `StructLiteral.resolveType` returns the error-set type. (Non-error-set names → clear "only supported for error values" diagnostic.)
- [x] Compiler: `compileStructLiteral` looks up the name in `error_sets`, validates the variant/payload, compiles the payload expression, and boxes its `.value` into a heap `*const Value` on the `.err` arm. Constant payloads only for now (`.location` source → "payloads must currently be constant values"); runtime-valued payloads deferred.
- [x] Evaluator: `materializeString`'s `.err` arm recurses into the payload so it prints `E.WithMsg(boom)` (resolving slices via the evaluator, not static format).
- [x] Observable: `E{ .WithMsg = "boom" }` → `E.WithMsg(boom)` bound, inline (`${...}`), and yielded into an error union. Negative cases (type mismatch, payload on payload-less variant, unknown variant, here all caught at **type-check → stderr**).
- [x] Tests: `tests/features/error_payload_construction_regression.rn` (+`.stdout`); `tests/diagnostics/error_payload_type_mismatch.rn` (+`.stderr`/`.status`). Full suite green (72 smoke + 20 diagnostic).

**Note:** ✅ **Runtime-valued payloads now supported** (done in the 7c worktree as the prerequisite for `ExecutableError.NonZeroExit(code)`). New `make_err` instruction constructs an error value at runtime, boxing the resolved payload (constant *or* runtime) via the evaluator's allocator — no `.err` representation change. `compileStructLiteral` now emits `make_err` for all payloaded construction. Verified: `E{ .Msg = runtimeVar }`, computed `E{ .Msg = "hello ${name}" }`, and constants. Test: `error_runtime_payload_regression`.

#### Phase 3c — Ok/error value coercion into an error-union stdout type ✅ COMPLETE
- [x] `runYield` now coerces into an `E!T` declared stdout type: a bare ok payload value (`T`) **or** an error value (an error set whose variants are all in `E`) both satisfy it (new `yieldCoercesToErrorUnion` helper, mirroring the existing `coerced_error_union` pipe boundary). Before this, a function with an error-union stdout type could not `yield` *anything*.
- [x] Verified: `fn Void okFn() E!String { yield "ok value" }` and `fn Void errFn() E!String { yield E.Bad }` both type-check and print (`ok value` / `E.Bad`) via bare calls / pipelines. Wrong type (`yield "str"` into `E!Int`) is rejected.
- [x] Tests: `tests/features/error_union_yield_regression.rn` (+`.stdout`); `tests/diagnostics/error_union_yield_mismatch.rn` (+`.stderr`/`.status`). Full suite green (71 smoke + 19 diagnostic).

**Findings that reframed this phase:**
- The Phase 2 "empty `${getOne}`" was **not** an error bug. Two orthogonal causes: (1) `return` is control-flow/exit (`exitWith`), not output — functions emit their value via **`yield` → stdout**, consumed by `${fn}`/pipes; (2) `${fn}` command-substitution capture of **typed (non-String) stdout** is a **pre-existing** limitation — plain `fn Void f() Int { yield 1 }` also leaks `1` to program stdout instead of being captured. Error-union stdout inherits this. Fixing typed-value `${}` capture is general typed-pipe machinery, **out of scope** for error handling.
- **Propagation model — ✅ RESOLVED (2026-06-15): value/yield-based.** Errors are ordinary output values. A function with an `E!T` stdout type produces its ok-or-error value via `yield` (already enabled in 3c); `catch`/`try` operate on the **produced value** (binding / pipeline / `${}` substitution), not on `return`. `try` propagates by re-yielding the error. This is consistent with the language's value-production model and builds directly on 3c, at the cost of diverging from the spec's `return ...` wording (the spec's `return` is treated as aspirational phrasing; functionally the error is the stage's output). Note: `catch` on a *directly-produced* error union (inline `E.Bad`, a binding) is model-independent and can be built first; the value/yield decision only governs how function/pipeline results are consumed.

**Status / Notes:** Phase 3 ✅ COMPLETE (3a ✅ · 3b ✅ · 3c ✅).

### Phase 4 — `catch` operator ✅ COMPLETE (direct values; function consumption deferred)
Goal: `expr catch default` and `expr catch |err| body`.
- [x] Reshaped `ast.CatchExpr` to `{ subject, capture: ?CaptureClause, handler: *Expression }` (mirrors `IfExpr`; removed the unused `CatchClause`). Parsing: `catch` binds looser than pipelines — `parseExpression` now wraps an inner expression via `parseMaybeCatch`; added `kw_catch` to `isExprTerminator` so the binary parser stops before it. Handler uses `parseControlFlowBody` (so `catch 0`, `catch |err| match ...`, `catch |err| { ... }` all work).
- [x] Type check (`runCatch`): runs subject + handler (handler in a child scope with the optional `|err|` bound to the error-set type via `catchErrorSetType`); LHS must be error-like (`error_union`/`error_set`/`err`/`failed`) else a "catch requires an error union or error value" diagnostic. `CatchExpr.resolveType` yields the payload type.
- [x] New runtime primitive: **`is_err` instruction** (a `Neg`/UnaryOperation that sets a bool = `value == .err`) — `cmp` couldn't test an error (not a singleton like `null`). Wired through evaluator + effects analysis.
- [x] Compile (`compileCatch`): stabilize subject into a ref → `is_err` → jump to handler when error, else use the subject's ok value; returns the **result ref** (not `r2`, so const-bindings get stable storage). Capture binds `|err|` to the subject in the handler scope.
- [x] Fixed the **latent err_set-aliasing panic** flagged in Phase 2: `validateTypeAssignmentErrorUnion`/`ErrorSet` now unalias the error set before reading variants, and a new `.error_set` arm lets an error value coerce into `E!T` (`const bad: E!String = E.Bad`).
- [x] Verified: pure-error `catch` default, ok-path + err-path via `E!String` bindings, capture form (`|err|` materializes), non-error LHS → diagnostic. Tests: `error_catch_regression` (feature) + `error_catch_non_error` (diagnostic). Full suite green (72 smoke + 20 diagnostic).

**Status / Notes:** ✅ Complete for **directly-produced** error unions (inline values, bindings). **Deferred:** consuming a *function/pipeline* result (`echo "x" | parseInt catch 0`) needs the value/yield result to be captured as a value — blocked on the **pre-existing typed-value `${}`/capture limitation** (non-String stdout doesn't materialize into a value), not on catch itself. Revisit alongside that capture work / Phase 7.

### Phase 5 — `try` keyword ✅ COMPLETE
Goal: `try expr` propagates an error out of the enclosing function (value/yield model, D6), else evaluates to the ok value.
- [x] Parse `try <expr>` → `ast.try_expr` (`parseTryExpression` in `parseExpressionInner`; binds tightly via `parseExpressionInner`, so `(try X) catch Y` and `yield try X` parse correctly — parenthesize pipelines).
- [x] Type check (`runTry`): subject must be error-like (else diagnostic); `TryExpr.resolveType` yields the payload type. **Deferred:** validating that the enclosing function's error set is a superset of the propagated error (needs Phase 6 inference).
- [x] Compile (`compileTry`): mirrors `compileOptionalUnwrap` — stabilize subject → `is_err` → on error `exit_with` the error (propagate as the function's result), else evaluate to the ok value (returns the result ref).
- [x] Verified: `try ok` unwraps and continues; `try error` stops the enclosing function (post-`try` code doesn't run); `yield try ok` works; `try` on a non-error → diagnostic. Tests: `error_try_regression` (feature) + `error_try_non_error` (diagnostic). Full suite green (74 smoke + 22 diagnostic).

**Status / Notes:** ✅ Complete. Propagation uses `exit_with` (the function's result). Observing the propagated error from a *caller* shares the deferred function-result-consumption gap from Phase 4 (typed-value capture); the control-flow (stop the function) is fully working and tested.

### Phase 6 — Inferred error sets ✅ COMPLETE (open-set model; concrete collection deferred)
Goal: `const result = ...` infers an error-union type from the RHS; `fn ... !T { ... }` accepts errors inferred from the body.
- [x] **Binding inference** already works via `resolveExprType(initializer)` — `const b = a` where `a: E!String` gives `b: E!String`; `const e = E.Bad` gives `e: <error set>`. Verified `b catch ...` / `e catch ...`.
- [x] **Leading-`!T` inference (open-set model, D5):** an empty error set (the placeholder produced by parsing `!T` in Phase 2) marks an *inferred* set. `yieldCoercesToErrorUnion` now treats an inferred (empty) union set as accepting **any** error value (new `isInferredErrorSet` helper). So `fn Void f() !String { yield E.Bad }` and `{ yield "ok" }` and `{ yield try bad }` all type-check and run.
- [x] Verified explicit (non-empty) error sets are **unaffected**: `fn Void f() E!String { yield F.Other }` is still rejected; `yield E.Bad` accepted.
- [x] Test: `error_inferred_set_regression` (feature). Full suite green (75 smoke + 22 diagnostic).

**Status / Notes:** ✅ Complete as an **open-set** model: a leading-`!T` set accepts any body-produced error, but its *concrete* members are not yet collected into the function's recorded type. **Deferred (D5 follow-up):** accumulating the exact union of body error sets and propagating it to callers — needed for Phase 8 `match` exhaustiveness and for the spec's executable-inference examples (which also need Phase 7's `ExecutableError`). The spec's `const result = echo "hello"` / `grep "--invalid-flag"` examples specifically depend on Phase 7.

### Phase 7 — Executable error unions + error short-circuit/enforcement (REDESIGNED)
This phase was redesigned with the user (2026-06-16) into something larger and more principled than the spec. Model decisions recorded as **D7** below. Staged 7a–7d to de-risk; reassess between stages.

**Model (D7):**
- **Errors short-circuit; they are not stream data.** A stage's output stream is `T`; an error is a terminal event that aborts the stage and propagates outward. The type crossing `|` is always `T`, never `E!T` — errors skip *past* remaining stages to the nearest handler.
- **Handling is enforced (Zig-like):** an expression that can error must be `catch`'d / `||`'d or propagated (function declares `E!T` + `try`/yield), else a compile error. Uncaught at top level → program exit code.
- **Booleans-as-exit-codes are reinterpreted as ok-vs-error:** `if (cmd)` = "cmd ok"; `cmd && next` = run next if ok; `cmd || fallback` = **catch-and-discard** (fallback if error). `success = ok`, `nonzero = error(code)`.
- **`ExecutableError`** covers today's `ExitCode` variants: `NonZeroExit: Int`, `Signalled: Int`, `SpawnFailed`. A command's *value view* is `ExecutableError!String`; **`ExecutionResult` stays** as the explicit detailed handle (`.stdout`/`.stderr`/`.wait`/background) — the two coexist.

#### Phase 7a — `ExecutableError` builtin error set
- [ ] Define `ExecutableError` (the set above) in global/prelude scope so it participates in checking like a user error set (resolves D3).
- [ ] Test: reference `ExecutableError`/its variants; construct + `catch`.

#### Phase 7b — error-handling enforcement ✅ COMPLETE (bare error values)
- [x] Enforce "an error must be handled": a **bare expression statement** whose value is an error union / error value (and isn't `catch`/`try`) → `UnhandledError` diagnostic ("use catch (or || to discard) or propagate it"). Implemented in `runExpressionStatement`.
- [x] Scoped to error **values** (`E.Bad`, `E{...}`); `call`/`pipeline` statements are skipped — their error enforcement is tied to the execution model and lands in 7c/7d. (The error→non-error *coercion* rejections in yields/assignments are already enforced via existing type-mismatch checks.)
- [x] Fixed `CatchExpr.resolveType` to return the handler's type for pure-error subjects (so `const x = E.Bad catch "y"` types `x` as the handler type, not the error set).
- [x] Test: `error_unhandled` (diagnostic). Full suite green (76 smoke + 23 diagnostic).
- [ ] **Moved to 7c:** `||` as error-discard (pairs with the boolean→ok/error reinterpretation).

#### Phase 7c — command value = `ExecutableError!String` (touches execution core)
**Investigation (2026-06-16):** this is a deep refactor, not a retype. Findings:
- Commands have **no value-as-string form today** — even `const r: String = echo "hi"` fails ("expected String, actual ExecutionResult"). A command is an `ExecutionResult` struct (`execution_result_struct_type`); stdout is only reachable via `${cmd}` substitution or `.stdout`/`.exit_code`.
- The capture path is `compileExpressionWithCapture` (`compiler.zig:2074`, ~120 lines of pipe/fork/wait) → produces the `ExecutionResult`; `${cmd}` materializes the stdout pipe to a string (evaluator `materializeString`).
- `if`/`&&`/`||` are built throughout on `exit_code.toBoolean()` (`compileLogicalBinary` ~5822, `compileIfCondition` ~4175, unary-not ~1991/5409, comptime fold ~1942/1971).

**Staged sub-plan (each step must keep the suite green):**
- [x] 7c-i: type-checker coercion `.execution` → `ExecutableError!String` ✅ — `validateTypeAssignmentErrorUnion` accepts a command (`.execution`) assigned where an error union is expected, validating the union's payload accepts `String`. (Bound-with-annotation site; per the 7b enforcement, a command assigned to a plain `String` is still an error — you must handle it.)
- [x] 7c-ii: runtime command → `ExecutableError!String` ✅ — `compileExecutionToErrorUnion` runs at a binding with an error-union annotation: `resolve_exit_code` on the captured `ExecutionResult`, then branch — exit 0 → the merged (captured) output as the ok value; non-zero → `make_err("ExecutableError", "NonZeroExit", <exit code>)`. Verified: `const r: ExecutableError!String = echo "x"` → output; `= grep "zzz" /dev/null` → caught via `catch`/`||`; `r` materializes as `ExecutableError.NonZeroExit(1)` (exit code carried via the runtime payload). Test: `executable_error_union_regression`. Suite green (79 smoke).
  - **Known edge:** a `catch |err| ... ${err} ...` handler that *interpolates* the captured error on a command-converted binding currently prints nothing (works for user errors and for static/non-interpolating handlers). Capture-context interaction; deferred.
  - **Scope:** entry point is binding-with-annotation. Direct command-in-expression typing (`echo | parseInt catch 0` without an annotation) is 7c-iv/7d.
- [x] 7c-iii: `||` as error-discard on error-union values ✅ — `errUnion || fallback` / `E.Bad || fallback` yields the ok value, or the fallback when the LHS is an error. Value mode, non-capture operands: `BinaryExpr.resolveType` types `logical_or` over an error-like LHS as the payload (so 7b treats it as handled); `compileLogicalOrValue` does the `is_err` branch (mirrors `compileOrelseBinary`), detecting error-like LHS by type **or** a constant `.err` value source. Exit-code `||` unchanged; suite green (77 smoke). Test: `error_or_discard_regression`. (Statement-mode and capture-operand `||`-discard later fixed — see backlog item 5.)
- [~] 7c-iv: **Part A done** — `catch`/`try` accept a direct command/pipeline subject (treated as `ExecutableError!String`): `runCatch`/`runTry` accept `.execution`; `compileCatch`/`compileTry` convert an execution-typed subject via `compileExecutionToErrorUnion` before the `is_err` branch. So `grep "zzz" /dev/null catch "no-match"`, `${cmd catch "x"}`, and `try cmd` (in an error-returning fn) all work directly — no annotation needed. Test: `executable_error_direct_regression`. Suite green (80 smoke).
  - **Part B (mostly done):** `if (errorUnion)` is true when ok (not error); `if (errorUnion) |value|` binds the ok payload (`compileIfCondition` does `is_err` → `neg`; `runIfCapture` binds the payload). `errorUnion || fallback` was done in 7c-iii. Commands in `if`/`&&`/`||` already behave as ok-vs-error via exit code (success=ok). Test: `error_if_condition_regression`. Suite green (81 smoke).
    - **Deferred (niche/risky):** `errorUnion && x` (semantically muddy — would propagate the error); `cmd || fallback` / `cmd && next` as a *value* yielding output (needs the capture-path logical lowering, risky). Note: the spec's `echo "1234" | parseInt catch 0` needs `parseInt` to return an error union (the builtin returns plain `Int`) — separate from command errors.
- [ ] Keep `ExecutionResult` as the explicit handle (`.stdout`/`.stderr`/`.wait`/background) — coexists with the value view.

**Note:** best tackled as a dedicated focused session (ideally a git worktree) — it's the riskiest, most-embedded change in the whole feature.

#### Phase 7d — pipeline error short-circuit + trailing `catch`/`||` ✅ (trailing handler)
- [x] A pipeline ending in a command is handled at the **trailing** `catch`/`||`/`try`: since the pipeline's result type is `.execution` and (from 7c-iv A) `catch`/`try` accept `.execution` subjects, the pipeline's final result (the last stage's exit/output) is converted to `ExecutableError!String` and handled. `catch` binds looser than `|`, so `(echo | grep) catch x` catches the whole pipeline. Verified: `echo "hi" | grep "zzz" catch "no-match"`, `... || "discarded"`, `try (echo | grep ...)` in an error-returning fn. Test: `error_pipeline_regression`. Suite green (82 smoke).
- **Deferred:** *true mid-pipeline* short-circuit (a failing upstream stage aborting downstream stages) — current pipelines fork stages concurrently and the trailing handler catches the aggregate final result, which covers the user-facing need; aborting downstream is a deeper execution-model change. Error-*typed* function stages propagating mid-pipeline tie into the typed-pipe model. The spec's exact `echo "1234" | parseInt catch 0` needs the `parseInt` builtin to return an error union (it returns plain `Int` and currently hard-errors on bad input) — a separate builtin-design change.

**Status / Notes:** ✅ **Phase 7 COMPLETE** (7a–7d). Errors short-circuit to the nearest handler; handling is enforced; `ExecutableError` builtin; commands carry the `ExecutableError!String` value view (binding, direct `catch`/`try`/`||`, pipelines) while `ExecutionResult` remains the explicit handle; `if`/`||` work as ok-vs-error; runtime error payloads. Merged `error-7c` → `error` (ff). Deferred niche items noted per sub-phase.

### Phase 8 — match on error values with payload capture ✅ COMPLETE
Goal: dispatch on error variants, capturing payloads (spec lines 73-80).
- [x] `match` matches error-set variant paths `MyError.Variant`: when the subject is error-like (`matchErrorSet` detects it, seeing through the zero-arg bare-call wrapper via `resolveSubjectType`), each `.path` case is a variant pattern. Compiler emits a new `match_err` instruction (tests set+variant); type-checker `runErrorMatch` validates the variant exists.
- [x] Payload capture `MyError.ErrorWithMessage => |message| ...`: new `err_payload` instruction extracts the payload; `compileMatchCaseBody` binds it in a fresh scope; `runErrorMatch` binds the capture to the variant's payload type. Wildcard `_` supported.
- [x] Compile variant test + payload extraction: `match_err` / `err_payload` instructions (name-based, per D2). Per-case test results use a **register** (not a stack ref) so the case loop pushes no transient slots — fixed a stack-counter drift that crashed/mis-addressed subsequent statements; `compileMatchCaseBody` restores the counter after a capture body (carried in r2).
- [x] Test: `error_match_regression` (payload-less, payload capture, `catch |err| match (err)` + wildcard) and `error_match_unknown_variant` (diagnostic). Full suite green (83 smoke + 24 diagnostic).

**Status / Notes:** ✅ Complete. **Deferred (backlog):** (a) **exhaustiveness checking** across the set (no-match falls through to runtime exit, as plain `match` does) — also limited by the open-set inference (item 3); (b) the spec's `match err { … }` uses **paren-less** subject + **bare-expression** case bodies, but the current `match` requires `match (err) { … => { … } }` (parens + block bodies) — a general `match`-syntax relaxation, tracked separately.

### Phase 9 — Diagnostics, docs, polish ✅ COMPLETE
- [x] Diagnostics in place: `catch`/`try` on a non-error, unknown variant (construction + match), payload type mismatch, unhandled error value, `try` outside an erroring fn. (Non-exhaustive `match` checking is deferred — backlog 13.)
- [x] Rewrote `docs/features.md` "Errors as first-class types" + "Error-aware pipelines" to the **actual implemented** syntax (`const E = error {…}`, `E!T`/`!T`, `E.Variant` / `E{ .Variant = … }`, `catch`/`||`/`try`, `match (err) {…}`, command error unions) — the old sections were aspirational/incorrect.
- [x] Added `examples/error_handling.rn` (runs clean): error sets, `catch`, `if (errorUnion) |v|`, command `catch`/`||`, `match` with payload capture.
- [ ] LSP hover/completion for error sets & variants — **deferred** (separate subsystem; not required for the language surface).
- [x] Full CI green: `zig build run -- scripts/run_ci.rn` (formatter, linter, unit tests, 83 CLI smoke scripts).

**Status / Notes:** ✅ Complete (LSP polish deferred). Exposed and documented backlog item 15 (stack-counter drift when sequencing capture-binding error constructs) — a real robustness bug to fix next.

---

## Overall status

**Phases 1–9 COMPLETE.** The error-handling spec (`future/error-handling.md`) is implemented end-to-end and tested: error set declarations, error union types (`E!T`, inferred `!T`), value construction (payload-less + payloaded, runtime payloads), `catch`, `try`, `||`, `if (errorUnion)`, inferred error sets (open-set), `ExecutableError` + commands/pipelines as catchable error unions, and `match` on variants with payload capture. Enforcement requires errors be handled. Full CI green. Remaining work is the **Deferred / Follow-ups** backlog above (notably item 15, the sequencing stack-drift bug).

---

## Deferred / Follow-ups (backlog)

Niche or risky items intentionally deferred along the way, collected here so they can be picked up later. Each notes where it came from.

1. ✅ **FIXED — `catch |err|` handler on a binding subject** (7c): two issues, both fixed. (a) `compileCatchHandler` pop-balances the capture-binding slot (same fix as #15) so the handler *runs*. (b) The catch result is typed as the error union's payload, which masked an `execution_handles` handler from the statement-level wait — so a capturing handler (`echo "${err}"`) as the **last statement** raced its fork against program exit. Fixed by draining a waitable handler in `compileCatch`'s error path (`if (isWaitable(handler)) |loc| self.wait(...)`, mirroring `compileIfNoElse`). Now `v catch |err| echo "${err}"` and command-binding catch handlers are deterministic (verified 5×). Tests: `error_catch_capture_binding_regression`, `error_catch_handler_output_regression`. (`match` case bodies don't have this race.)
2. ✅ **FIXED — `try` superset check** (5/6): `runTry` now verifies the enclosing function's declared error set is a superset of what `try` propagates — every variant the subject can raise must be a member, else a `ErrorNotInErrorSet` diagnostic (`validateTryPropagation`). Uses item 3's inferred-set substitution (`resolveInferredErrorSet`), so `try <inferred fn>` is checked against its collected set too. Skipped when the enclosing function declares an inferred (`!T`) set (it collects instead) or no error union, and for `execution`/`err`/`failed` subjects (command `ExecutableError` propagation isn't superset-checked yet). Test: `tests/diagnostics/error_try_superset`; valid propagation still covered by `error_try_regression`.
3. 🟡 **PARTIAL — Concrete inferred error sets** (6): leading-`!T` previously used a pure *open-set* model (accepts any error, never collected). **MVP done:** the type checker now collects the variants a function body can produce into a side table (`inferred_error_sets`, keyed by the inferred `error_set` placeholder node) and `matchErrorSet` substitutes that concrete set, so **`match` exhaustiveness is enforced on inferred-fn results** and callers see a real set. Collection sources: `yield <error value>` and `try <error union>`; an `InferredErrorCollector` is pushed per function in `runFnDecl` (no AST mutation → LSP-safe; rebuilt each pass). Test: `tests/diagnostics/error_inferred_match_non_exhaustive`.
   - **Still deferred:** (a) **set-granularity, not per-variant** — `yield E.Bad` collects *all* of `E`'s variants (`E.Bad` resolves to the whole set `E`, ast.zig:966), a sound over-approximation; (b) **no cross-function propagation** — calling another inferred-`!T` function whose set isn't yet finalized (forward refs / mutual recursion are checked before population); (c) **`ExecutableError` from a `try`'d command** isn't collected (command `execution` type isn't an error set here); (d) ~~the recorded set isn't surfaced to #2 yet~~ — **done**: #2's `validateTryPropagation` consumes it via `resolveInferredErrorSet`. Note: matching on a *function-call* error-union result currently hits a **separate pre-existing** runtime bug (`UnsupportedDereferenceValueType`, affects concrete error unions too) — unrelated to inference; the exhaustiveness check runs at type-check time before that.
4. **`errorUnion && x`** (7c-iv): not implemented — semantically muddy (would propagate the error). Decide semantics first.
5. **`cmd ||`/`cmd &&` as a value yielding output** (7c-iv): commands in `||`/`&&` still use exit-code logical lowering (capture path), not output-or-fallback value semantics. Needs the capture-path logical lowering reworked. (Pre-existing: `const x = false || echo "cap"` leaves `x` empty — same in the error-union case, which now matches this baseline rather than crashing.)
  - ✅ **FIXED — `errorUnion || <command>` crash**: `errUnion || cmd` (or any error-like LHS where an operand needs stdio capture) routed to `compileLogicalBinary`, which did an exit-code `jmp`/`log` on the error value → runner panic (`access of union field 'exit_code' while field 'err' is active`). 7c-iii only covered the non-capture value path; statement mode and the value-capture path still crashed. Fixed by detecting an error-like LHS (`resultIsErrorLike`) in **both** the `.statement` mode and the value-capture path of `compileLogicalBinary` and lowering them as error-discard (`is_err` → fallback) instead of the exit-code jmp. `compileLogicalOrValue` shares the same helper. Regression: `error_or_discard_regression` (statement-level `d || echo "ran-on-error"` + ok-value no-run).
6. **True mid-pipeline short-circuit** (7d): a failing upstream stage aborting downstream stages. Pipelines fork stages concurrently; only the trailing handler catches the aggregate result today.
7. **Error-typed function stages mid-pipeline** (7d): tie into the typed-pipe model.
8. **`parseInt` (and other builtins) returning an error union** (7d): the spec's `echo "1234" | parseInt catch 0` needs `parseInt` to be `…!Int` (it returns plain `Int` and hard-errors on bad input). Builtin-design change.
9. ✅ **FIXED — Unknown-variant diagnostic location** (3a): `const e = MyError.Nope` was caught by the **compiler** (stdout) not the type checker (stderr). Root cause: member access parses as a `.member` *binary op* (`initBinaryExpression`), not a `.member` `MemberExpr`, so `runMember`'s error-set arm was dead for it — `runBinary` recursed but never validated the variant. Fixed by validating error-set variant access in `runBinary`'s `.member` branch (calls `runErrorSetMemberAccess`), so an unknown variant is now a type-checker `ErrorNotInErrorSet` diagnostic on stderr (and the program no longer reaches codegen). Fixture `error_unknown_variant` moved from `.stdout` to `.stderr`.
10. **Variant payload type validation** (1): `runTypeIdentifier` is a no-op codebase-wide, so an unknown payload type name in a variant isn't caught; variant payloads aren't alias-resolved (`resolveTypeExpr` returns `error_set` via its `else`).
11. ✅ **FIXED — Dead `Statement.error_decl` / `EnumBody` / `UnionBody` AST nodes** (1): removed (`ErrorDecl`/`ErrorBody`/`EnumBody`/`EnumVariant`/`UnionBody`/`UnionVariant` and the no-op switch arms in `run_script.zig`, `type-checker.zig`, `lsp/symbols.zig`). They were never parsed; the type-expr `error_set` is the single source of truth.
12. 🟡 **PARTIAL — error values across the in-process boundary** (D2): a function returning an error union `yield`s a structured value; a byte capture flattened it to text (`"E.Bad"`), losing the error. **Done (in-process / function capture):** `tryCompileTypedErrorUnionCapture` (`compiler.zig`) runs an error-union-returning function call with a `typed` stdout pipe (so `yield` enqueues the value via the existing typed transport), waits, and dequeues it (new `pipe_dequeue` IR instruction) — returning the value typed as the error union. So `catch`/`match`/`try` on such a call now operate on the real error value (`mayFail catch "fb"` → `"fb"`; `match mayFail` → its variant; `try mayOk` → unwraps). Mirrors the byte path's stack contract (pads to `capture_temp_ref_count`, result in `%r`). Regression: `error_function_value_capture_regression`.
    - ✅ **`if (fn) |v|` capture** now works: the parser's `rewriteBareIdentifierCallForIfCapture` (which unwrapped a zero-arg call condition to a bare identifier — right for a variable, wrong for a function, whose identifier resolves to `.function` not its return type) was removed; the compiler already unwraps zero-arg calls of both variables and functions, and the type checker now resolves the condition via `resolveConditionType` (sees through a zero-arg call: a variable's own type, or a function's return type). `if (mayOk) |v|` binds the ok payload; the error path takes `else`. Covered by `error_function_value_capture_regression`. (Optional-returning functions are also supported now — see item 17.)
    - ✅ **`match` payload-capture on a function call** now works, both directly (`match fn { E.X => |c| … }`) and through `catch |err| match err { … }`. Root cause was the raw-return-type problem again: a call's return type came back with an unresolved `identifier` err_set, so the `catch` capture / `match` subject didn't resolve to an error set and the generic (capture-less) match path rejected it. `resolveSubjectType` now `resolveTypeExpr`s its result, so catch/try/match all see a resolved error set. Covered by `error_function_value_capture_regression`.
    - **Still deferred:** (a) **cross-process** boundaries (external commands) — those genuinely carry only exit code + bytes (that's `ExecutableError`); a real wire format is a separate, larger effort. (b) Observing an error a callee **propagated** via `try` (it exits via `exitWith`/exit code, not a stdout `yield`, so the typed capture doesn't see it). (c) Only no-pub-export functions with a direct `fn_ref` binding take the typed path; others fall back to the byte path.
13. ✅ **FIXED — `match` exhaustiveness** (8): a `match` on an error value without a `_` case must cover every variant of the set, else a `NonExhaustiveMatch` diagnostic per missing variant (`runErrorMatch`). An inferred/open set has no concrete variants, so it's not enforced there (item 3). Tests: `error_match_non_exhaustive`.
14. ✅ **FIXED — Paren-less `match` + bare-expression case bodies** (8): `match` now accepts an unparenthesized subject (`match err { … }`) and bare-expression case bodies (`MyError.X => echo "…"`, wrapped into a single-statement block), matching the spec; the parenthesized + block form still works. Regression test: `error_match_bare_regression`.
15. ✅ **FIXED — Stack-counter drift when sequencing error constructs** (8/9): the `.ref` instruction *appends* a runtime slot, so `compileMatchCaseBody` bare-resetting the counter after a payload-capture body left the slots on the runtime stack (the counter then diverged), crashing a `match`-with-capture **followed by** another ref-using statement (`if (errorUnion) |v|`, `make_err`, …). Fixed by emitting real `pop`s (which pop at runtime *and* decrement the counter) after stashing the body result in `r2`. Regression test: `error_sequencing_regression`; `examples/error_handling.rn` restored to natural order.
16. 🟡 **PARTIAL — using a zero-arg function call as a value** (found during item 3):
    - ✅ **FIXED (crash):** `const r = greeting` / `match greeting { … }` / `greeting catch x` (canonical bash-style zero-arg call — a bare identifier) crashed with `UnsupportedDereferenceValueType` (`Could not dereference address 0x…`). Root cause in `callNeedsStdioCapture` (`compiler.zig`): a bare identifier parses as a zero-arg call, and a *known* identifier returned `needs_stdio_capture = false` (right for a variable read like `const y = x`, **wrong for a function** — its output must be captured), so the binding stored the raw thread handle. Fixed by capturing when the known binding `isFunctionRef()`. Plain-value capture now works (`const a = greeting` → `a=hello`). Regression: `function_call_value_capture_regression`. (Note: `greeting()` with empty parens is a *different* parse — a `()` empty-group argument — and is a separate question; canonical Runic zero-arg calls are bare.)
    - ✅ **FIXED (semantics) via #12:** `catch`/`match`/`try` on an error-union-returning function now preserve the yielded error value (in-process typed capture — see item 12). `mayFail catch "fb"` → `"fb"`, `match mayFail` → its variant, `try mayOk` → unwraps. Regression: `error_function_value_capture_regression`. (Remaining edges — propagated-error observation, match payload-capture through catch-of-fn — tracked under item 12.)
17. ✅ **FIXED — optional-returning functions** (found extending #12): three gaps, now closed. (a) **yield coercion** — `fn …() ?T { yield "x" }` was rejected ("yields []Byte, declared ?String"); `runYield` now coerces a bare `T` (or `null`) into `?T` via `yieldCoercesToOptional` (mirrors the error-union coercion). (b) **value preservation** — extended the in-process typed capture (item 12) to optional returns (`tryCompileTypedValueCapture` now accepts `.optional` as well as `.error_union`), so `null` vs value survives the capture; `if (fn) |v|`, binding, and `orelse` all see the real optional. The `if`-optional compile arm now stashes the condition into a stable ref first (a volatile register condition — from the typed capture — was clobbered before the `|v|` binding read it). (c) **`orelse` on a function call** crashed the type checker (`UnresolvedTypeLiteral`): a call's return type comes back raw, so `?String`'s child was an unresolved `identifier`; `runBinary`'s orelse arm now `resolveTypeExpr`s the optional child (and unaliases the left). Regression: `optional_function_value_capture_regression`.

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
