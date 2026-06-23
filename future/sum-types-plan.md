# General sum types — plan

Living plan for **structural sum types** spelled `A || B` (and `A || B || C`),
e.g. `const IntOrString = Int || String`. A value of `A || B` is *either* an
`A` or a `B`; you narrow it back to a concrete type before use. Spun out of the
error-handling backlog (the error-set merge, `future/error-handling-plan.md`
item 18, is the narrow special case that ships first and independently).

Status: **Phase 1 done; Phase 2 (widening) started.** Branch: `sum-types` (off `error`).

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
- **No operations on an un-narrowed sum**: you must narrow (via type-`match`)
  before doing anything type-specific. The bare sum supports only assignment,
  passing, and narrowing.

## Open design decisions (resolve before coding)

1. **Parser disambiguation of `||`.** In value position `||` is logical-or; in
   type position it is sum. How does `const IntOrString = Int || String` get
   recognized as a *type* binding when its RHS parses as a value `.logical_or`?
   (See Phase 1.)
2. **Narrowing syntax.** Proposed: `match x { i: Int => …, s: String => … }`
   (typed patterns binding the narrowed value). Confirm the `name: Type =>`
   spelling vs. an alternative.
3. **`null` / optionals overlap.** Is `Int || null` allowed, and is it the same
   type as `?Int`? (Initial answer: disallow `null` as a sum member; use `?T`.)
4. **Error sets / error unions as members.** Can a sum mix an error set and a
   value type (`MyError || Int`)? Initially: **no** — error unions already model
   "error or value". Keep them out of general sums at first.
5. **Coercion direction.** Member → sum (widen) is implicit; sum → member is
   only via narrowing. Confirm no implicit narrowing anywhere.

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

- [ ] **Phase 3 — Narrowing via type-`match`.** Extend `match` (today dispatches
  on error variants) to dispatch on a sum's member type, binding the narrowed
  value: `match x { i: Int => …, s: String => … }`. Exhaustiveness over the
  member set (or `_`). IR: a type-discriminant test op reading the runtime
  `Value` tag (analogous to `is_err`); the match lowering mirrors the error
  `match`. Decide narrowed-binding scoping.

- [ ] **Phase 4 — Interactions, diagnostics, polish.** Sums in arrays/maps,
  string interpolation of a bare sum (error: must narrow), typed-pipe transport
  (Values already carry tags, so likely free), function params/returns, clear
  diagnostics ("cannot use `Int || String` directly; narrow it with match"),
  LSP hover/completion (deferrable), docs in `docs/features.md`, regression +
  diagnostic fixtures.

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

Large feature — roughly **4–6× the error-set merge**, dominated by Phase 2
(type-compat integration) and Phase 3 (the new narrowing construct). Runtime/IR
is small because the `Value` tag already exists. Design decisions above should be
locked before coding starts.
