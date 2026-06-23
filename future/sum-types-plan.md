# General sum types — plan

Living plan for **structural sum types** spelled `A || B` (and `A || B || C`),
e.g. `const IntOrString = Int || String`. A value of `A || B` is *either* an
`A` or a `B`; you narrow it back to a concrete type before use. Spun out of the
error-handling backlog (the error-set merge, `future/error-handling-plan.md`
item 18, is the narrow special case that ships first and independently).

Status: **Phase 1 done; Phase 2 (widening) started; narrowing model agreed
(2026-06-23).** Branch: `sum-types` (off `error`).

## Key architectural fact

Runic's runtime `Value` (`src/ir/value.zig`) is **already a dynamic tagged
union** (`.int`, `.zig_string`, `.bool`, `.float`, `.array`, `.map`, `.err`, …).
A runtime value already carries its own tag, so a sum type needs **no new
runtime representation** — `Int || String` at runtime is just a `Value` that is
either `.int` or `.zig_string`. The cost is almost entirely in (1) the static
type system and (2) a consumption construct that narrows a sum. The IR/runtime
work is small: one type-discriminant test op (analogous to the existing
`is_err`).

## Scope / non-goals (initial)

- **Structural & unordered**, with flatten + dedup: `Int || String` ≡
  `String || Int`; `(Int || String) || Int` ≡ `Int || String`.
- **Keep optionals (`?T`) and error unions (`E!T`) as their own special types**
  for now. `?T` is *conceptually* `T || null` and `E!T` is *conceptually*
  `E || T`, but folding the existing, working error/optional machinery into the
  general sum engine is a large refactor and the riskiest possible starting
  point. Add general sums **in parallel**; unify later if it proves clean.
- **Explicit-only, no join inference** initially: `if (c) "a" else 5` does *not*
  auto-infer `String || Int`. A sum type only arises from an explicit
  annotation / declaration. (Join inference can come later.)
- **No member-specific operations on an un-narrowed sum**: you must narrow it
  first (via flow narrowing or type-`match` — see "Narrowing model"). A bare sum
  supports assignment, passing, equality/`is` tests, and narrowing.

## Resolved design decisions (2026-06-23)

1. **Parser disambiguation of `||`** — ✅ resolved in Phase 1: `||` in *type*
   position parses to `type_merge`; value-position `||` (logical-or) is untouched.
2. **Narrowing** — ✅ **flow-sensitive (occurrence typing)** via `is`, `==`/`!=`,
   and relational operators, **plus** an explicit type-`match`. Full rules in
   "Narrowing model" below.
3. **`null` / optionals overlap** — disallow `null` as a sum member; use `?T`.
   (Unchanged; revisit only if unification happens.)
4. **Error sets / error unions as members** — keep out of general sums for now
   (`E!T` already models error-or-value). `buildSumType` is reached only when at
   least one operand is *not* an error set; mixing an error set into a general
   sum is deferred (decide: reject, or treat the set as an opaque member).
5. **Coercion** — member → sum (widen) is implicit; sum → member is **only** via
   narrowing (flow or match), never implicit. Plus **literals are coercible
   untyped constants** (decision 6).
6. **Literals are coercible (untyped-constant model).** A literal's type is the
   set of types it can represent; as a literal it coerces to *any single member
   that fits* the context — this is distinct from a runtime sum value (a binding),
   which never narrows implicitly. Applies to **all** literals uniformly; only
   numeric literals are multi-target:
   - integer literal (`0`, no decimals) → coercion type **`Int || Float`**;
   - float literal (`0.5`) → `Float`; string literal → `String`; bool → `Bool`.
   So `const n: Int = 0` keeps working (0 → Int), `arr[0]` works, `0.5` is
   Float-only, and the bare `x == 0` comparison sees `0 : Int || Float`. An
   **unannotated** `const x = 0` has type `Int || Float` and, being a `const`
   bound directly to a coercible literal, stays coercible (Zig `comptime_int`
   style). **High blast radius** (touches every numeric literal) → sequenced as
   its own late phase, after core narrowing, to avoid destabilizing numeric code.

## Narrowing model

A sum-typed **binding** has a *declared type* (governs what's assignable) and a
*flow type* (the refined type at a given program point). Narrowing refines the
flow type within a scope; it is **purely static** — the runtime `Value` is
already tagged, so a narrowed `Int` access just reads the `Int` already in the
slot. Only bindings (named, in scope) narrow — `if (foo() == 0)` narrows nothing.

**`is` type test** — `x is T` (works on any type, not just sums), compiles to a
runtime tag test (like `is_err`), result `Bool`:
- then-branch: `x : T`;
- else-branch: `x : (sum − T)` — the tested member is removed; if one member
  remains it collapses to that type (`Int || String` → `String`).

**Equality `==` / `!=`** — `x == E` where `E`'s (literal) type shares members
with `x`'s sum:
- `==` then-branch: `x` narrows to the **intersection** of its members with
  `E`'s type's members (`x: Int||String == 0: Int||Float` → `Int`); else-branch
  unchanged (value inequality doesn't exclude a whole type).
- `!=` is symmetric (narrows the else-branch). Comparing against a value with no
  shared member (`x == true` on `Int||String`) is a type error.
- Requires allowing `==` between a sum and a member type (returns `Bool`; false
  when the runtime tags differ).

**Relational `<` `>` `<=` `>=`** — narrow to the sub-sum of members that
*support* the operator (the comparison type-checks for them): `Int||String` with
`x > 0` → `Int` (String has no `>`); `Int||Float` with `x > 0` → stays
`Int||Float`. else-branch unchanged.

**Flow through assignment (`const` and `var`).** Both narrow. For a `var`, an
assignment refines the flow type from that point on (`x = "s"` → `x : String`
afterward), but the **declared type stays the sum**, so any member is still
assignable later (`x = 5` → `x : Int` afterward). At a branch join the flow type
is the union of the incoming branch-end flow types.

**Type-`match`** — `match x { i: Int => …, s: String => … }`: multi-way narrowing
with exhaustiveness over the member set (or `_`), each arm binding the narrowed
value. Mirrors the error-variant `match`.

Implementation note: the core new machinery is a per-binding **flow-type
override** in the scope (set on entering a narrowed branch, on assignment, and
merged at joins), plus a condition analyzer that extracts {then-facts,
else-facts} from a guard. Start with single conditions and `&&` conjunctions;
defer `||` / negation composition.

## Phases

- [x] **Phase 1 — Parse & represent. DONE (2026-06-23).** Reused route **(ii)**:
  the `A || B` type-expr parsing already exists from the error-set merge (#18) as
  the `ast.TypeExpr.type_merge` node (so `const X = …` works, and inline
  `(A || B)` via the paren path). Added `ast.TypeExpr.sum` (slice of member
  `*const TypeExpr`). `resolveTypeMerge` now splits: both operands error sets →
  merged `error_set` (#18, `mergeErrorSets`); otherwise → a structural `sum`
  (`buildSumType`). Normalization on construction: `appendSumMembers` flattens a
  nested sum and dedups members by `pipeTypesEqual`. Exhaustive `TypeExpr`
  switches got `.sum` arms (`runTypeExpression` recurses into members; member
  access → `UnsupportedMemberAccess`). Tests: `sum_type_regression`.
  - Canonical *order* for equality isn't defined yet — equality currently relies
    on member-wise comparison and would treat `Int || String` and
    `String || Int` as distinct. Order-insensitive equality is part of Phase 2.

- [~] **Phase 2 — Type equality & assignability. STARTED.** **Widening done**:
  `validateTypeAssignmentSum` accepts a value whose type is one of the members,
  or a sub-sum whose members are all present (`sumHasMember`); a non-member is a
  clear "expected type … actual …" mismatch. Tests: `sum_type_regression`
  (widen Int/String/Float, flatten+dedup), `sum_type_widen_mismatch` (Bool ∉
  `Int || String`). **Remaining**: order-insensitive sum *equality* in
  `pipeTypesEqual` (so `Int || String` == `String || Int` and sum-typed params /
  returns / function signatures compare correctly); reject the reverse (sum →
  member) consistently everywhere; interactions with optionals/error-unions.

- [~] **Phase 3 — `is` type test + flow-type infrastructure. 3a DONE (operator);
  3b TODO (narrowing).**
  - **3a (done, 2026-06-23):** the `x is T` operator end to end. Token `kw_is`;
    `ast.IsExpr` (resolves to `Bool`); parser folds `is` as a tight postfix in the
    binary parser's `.op` state (so `x is Int && y is String` parses correctly);
    type checker `runIs`; IR `compileIs` + a new `is_type` instruction with a
    `TypeTag` (`int`/`float`/`string`/`boolean`), evaluated against the runtime
    `Value` tag (Int=`uinteger`, Float=`float`, Bool=`exit_code`, String=
    `zig_string`/`slice`). Works on any value, composes with `&&`. Test:
    `is_operator_regression`. (Note: a bare `is` result printed via `${}` shows
    the pre-existing bool-as-exit_code quirk — `true`→`0`; use it in control flow.)
    Limitation: `typeTagOf` matches builtin primitive names / resolved forms; an
    alias-to-primitive or a non-primitive `T` isn't testable yet.
  - **3b (todo):** the per-binding **flow-type override** in `Scope` + the
    condition analyzer that turns a guard into {then-facts, else-facts}, wiring
    `if`/`else` to install facts per branch and restore/merge at the join.
    `is`-based narrowing for `const` first (then `T`, else `sum − T`).

- [ ] **Phase 4 — Comparison & relational narrowing.** Allow `==`/`!=` and
  relational ops between a sum and a member (type check + evaluator: compare
  underlying values, false on tag mismatch). Derive the narrowing facts:
  `==` → intersection in the then-branch; relational → members supporting the
  operator. Reject comparisons with no shared/with an unsupported member.

- [ ] **Phase 5 — `var` flow narrowing + assignment refinement.** Extend flow
  typing to `var`: an assignment refines the flow type from that point; the
  declared type still governs assignability; branch joins union the flow types.

- [ ] **Phase 6 — Type-`match`.** Extend `match` to dispatch on a sum member type
  with typed-pattern binding and exhaustiveness over the member set (or `_`);
  lowering mirrors the error-variant `match` using the Phase 3 tag-test op.

- [ ] **Phase 7 — Coercible literals (untyped constants).** Implement decision 6:
  a numeric integer literal is `Int || Float` and coerces to any fitting member;
  other literals coerce to their single natural type; a `const` bound to a literal
  stays coercible. **Sequenced last** because it touches every numeric literal —
  must keep all existing Int/Float code working (`const n: Int = 0`, indices,
  ranges, arithmetic). Until this lands, narrowing works with `0 : Int` (which
  already narrows `Int||String` → `Int` on `== 0`), so Phases 3–6 don't block on it.

- [ ] **Phase 8 — Interactions, diagnostics, polish.** Sums in arrays/maps,
  string interpolation of a bare (un-narrowed) sum (error: must narrow), typed-pipe
  transport (Values already carry tags, so likely free), function params/returns,
  order-insensitive equality finished (Phase 2 remainder), clear diagnostics
  ("cannot use `Int || String` directly; narrow it with `is`/match"), LSP
  hover/completion (deferrable), docs in `docs/features.md`, regression + diagnostic
  fixtures.

## Relationship to error sets / error unions

- **Error-set merge** (`error-handling-plan.md` #18, `A || B` where both are
  error *sets*) ships first and independently: it unions variant *name lists*
  into one ordinary `error_set`, needs no runtime change and no new consumption
  construct (`match`-on-variant already exists). It is the narrow precedent for
  the `||` type-merge spelling.
- **Folding `?T` / `E!T` into general sums** is explicitly out of scope here; a
  future unification could express `?T` as `T || null` and `E!T` as `E || T`,
  but only after the general machinery is proven.

## Effort estimate

Large feature. The dominant cost is now the **flow-typing infrastructure**
(Phases 3–5) — a per-binding flow-type override threaded through scopes,
condition analysis, and branch-join merging — plus the **coercible-literal**
change (Phase 7), which is high-blast-radius across all numeric code. Runtime/IR
stays small (the `Value` tag already exists; only a tag-test op and relaxed
sum-vs-member comparison are new). Design decisions are now locked (above).
