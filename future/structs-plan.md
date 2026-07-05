# User-defined structs — plan

Living plan for user-defined struct types: declaration, construction, field
access, pass/return/assign, nesting, and **methods**. Roadmap Theme 3
("better user-defined struct/type support"). `docs/features.md` already promises
the syntax, but it isn't wired up yet.

Status: **design agreed; implementation starting.** Scope: MVP + methods
(explicit receiver param, UFCS). Branch: `structs` (off `main`).

## Current state (what already exists vs. the gap)

A lot of infrastructure is already present from the error-payload and
execution-result work:

- **AST** — `TypeExpr.struct_type` (`StructType` with `fields` and `memberType`),
  `Expression.struct_literal` (`StructLiteral`), `StructField`.
- **Parser** — `parseStructLiteral` parses `Name{ .field = v }` construction
  (used today for error values). `parseStructTypeExpr` **is commented out**
  (`parser.zig` ~3087), so `const Point = struct { … }` fails at parse time
  ("expected value, actual: kw_struct"). This is the primary gap.
- **Type checker** — `runStructLiteral`, `runStructMemberAccess`,
  `validateTypeAssignmentStruct`, `StructType.memberType` already exist (exercised
  by execution-result structs and error payloads).
- **IR / runtime** — `compileStructLiteral`, `Value.strct` (`Struct` + a
  `struct_types` layout table), `TypeExpr.slotSize` for structs. So the runtime
  representation and layout already work.

So the feature is largely **"connect the dots + close gaps + add tests"** rather
than greenfield. The runtime is the part that already exists; the work is in the
frontend (parsing the type) and in wiring a *user-declared* struct through the
existing construction / access / assignment paths (which so far only saw builtin
execution-result and error-payload structs).

## Design decisions

1. **Declaration.** `const Point = struct { x: Int, y: Int }` binds a struct
   *type* (a type binding, like `const E = error { … }`). Fields are
   `name: Type`, separated by commas and/or newlines, trailing comma allowed.
2. **Construction.** `Point{ .x = 3, .y = 4 }` (the existing `struct_literal`
   syntax). All fields required initially; default field values are deferred.
3. **Field access.** `p.x` (already parses as member access; `runStructMemberAccess`
   types it). Read-only in the MVP.
4. **Field mutation.** `var p; p.x = 5` — deferred to a later phase (needs an
   lvalue-assignment path for a struct field); not in the first milestone unless
   cheap.
5. **Nesting.** A field may itself be a struct type; construction/access nest.
6. **Methods — the crux (needs a decision, see below).**

## Methods: explicit receiver param + UFCS (DECIDED 2026-07-01)

**Methods are ordinary free functions whose first parameter is the receiver, and
`p.method args…` is uniform-function-call (UFCS) sugar for `method(p, args…)`.**
Chosen for its simplicity and explicitness — no method-in-struct-body parsing, no
new `self` binding concept, no receiver channel outside the normal call path.

```runic
const Point = struct { x: Int, y: Int }

fn Void mag(self: Point) Float {
    yield self.x * self.x + self.y * self.y
}

const p: Point = Point{ .x = 3, .y = 4 }
p.mag        # == mag(p)
```

- `self` is not a keyword — it's just the conventional name of the first
  parameter; any name works.
- Resolution: `p.method` first tries a *field* named `method` on `p`'s struct
  type; if there is none, it resolves a *function* named `method` in scope whose
  first parameter type matches `p`'s type, and calls it with `p` as arg 0 (plus
  any further args). Field access wins over UFCS on a name collision.
- Trailing args attach normally: `p.method a b` → `method(p, a, b)`.
- This is not limited to structs in principle (UFCS could apply to any type),
  but the milestone only needs it for struct receivers; keep the resolution
  general but land it with struct tests.

## Phases

- [x] **Phase 1 — Parse struct type declarations. DONE (2026-07-02).**
  `parseStructTypeExpr` (`struct { field: Type, … }`, comma/newline separated),
  wired into `parseMaybePrimaryTypeExpr`; produces the existing `struct_type` AST
  (empty `decls` — methods are free functions).
- [x] **Phase 2 — Construct + field read, end to end. DONE (2026-07-02).**
  `runStructValueLiteral` (type checker) validates `Name{ .f = v }` construction
  (unknown field, missing field, duplicate field, per-field type). `compileStructValueLiteral`
  (IR) builds the value: compile fields into refs, `alloc` the slots, write each
  at its layout offset, return the base typed as the struct. Field access reuses
  the existing offset-based `compileMember` (extended to resolve an identifier
  object type against the user-struct registry).
- [x] **Phase 3 — Nesting + functions. DONE (2026-07-02).** Nested struct fields
  + nested access work. Struct params work (fixed `.member` type resolution:
  `BinaryExpr.resolveType` and member handling now unalias and resolve a field's
  type instead of returning the object type). Struct returns survive the call
  boundary via `tryCompileTypedValueCapture` (extended to struct / struct-name-
  identifier returns). Test: `struct_regression`.
- [x] **Phase 4 — Methods (UFCS). DONE (2026-07-04), with a deferred typed-capture edge.**
  `recv.method args…` compiles to `method(recv, args…)` when `method` is a
  function in scope. Compiler: `tryUfcsRewrite` synthesizes the call (receiver
  reused as arg 0 → evaluated once); wired into `compileCall` (with-args, method
  wins in call position) and `compileMember`'s field-not-found path (no-args,
  field wins). `analyzeExpressionEffects` flags a UFCS member as needing stdio
  capture so `${p.mag}` captures instead of leaking to stdout. Works: no-arg
  methods, methods with args, receiver that is a nested field (`l.to.mag`), field
  precedence. Tests: `struct_methods_regression`.
  - **Deferred (edges):**
    1. **Typed-value capture of a UFCS result.** `const m: Int = p.mag` byte-captures
       (stringifies) the result, so typed use (`m + 1`) fails — `tryCompileTypedValueCapture`
       handles `.call` callees, not `.member` UFCS. Same class as the sum/error
       typed-capture work; extend it to UFCS members. Interpolation/statement use
       works today.
    2. ✅ **Typed-context validation of a UFCS result — DONE (2026-07-04).** Fixed
       the underlying **pre-existing** normalization bug: the scalar/array
       assignment validators (`validateTypeAssignmentInteger`/`Float`/`Boolean`/
       `Byte`/`Null`/`Array`) compared `assignment_type.*` without unaliasing, so
       a function's alias-wrapped `Int`/`String` return false-positived against a
       resolved annotation (broke a plain `const r: Int = someIntFn` too, not just
       structs). They now unalias first. With that fixed, `BinaryExpr.resolveType`
       resolves a UFCS `recv.method` to the method's return type, so a wrong
       annotation is caught (`const bad: String = p.mag` → Int/String mismatch)
       and a correct one passes. Tests: `typed_call_binding_regression`,
       `typed_call_binding_mismatch`.
- [ ] **Phase 5 — Polish.** Field mutation on a `var` (if not already), docs in
  `docs/features.md`, a `examples/structs.rn` showcase, and the regression +
  diagnostic suites (there are currently **no** struct feature tests).

## Open questions / deferred

- Default field values; partial construction.
- Field mutation lvalue path (Phase 5 or its own milestone).
- Generic/parameterized structs — out of scope.
- Equality / comparison / printing of struct values (how does `${p}` render?).
- Interaction with the "files are structs" model (a file's members vs a struct's
  members share machinery — keep them consistent).
