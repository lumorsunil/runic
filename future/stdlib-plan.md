# Language features for a shallow standard library — plan

Living plan for the language work needed before a useful standard library can be
written *in Runic*. The stdlib's shape (modules, import model, conventions) lives
in `docs/standard-library.md`; **this** doc is about the missing/broken language
features that currently block writing those modules.

Status: **design agreed (2026-08-20); implementation not started.**

## Why: grounded gap analysis

The stdlib doc's modules (`fs`, `path`, `env`, `process`, `str`, `testing`) are
mostly thin wrappers over external commands + `$NAME`, which already work. The
real blockers are **strings**, **signed math**, **composability (HOF/generics)**,
and the **`std` import mechanism**. Probed against 0.6.1:

**Works today:** arrays (literals, `.len`, `for`-iterate, `xs[i]` on inferred
arrays), `for` over `0..n` ranges, structs, sum types, error handling, optionals,
typed pipes, UFCS methods, closures (capture works), modules with `pub`, string
interpolation. Builtins: `parseInt`, `parseFloat`, `lines`, `len`, `cd`, `wait`.

**Broken / missing (verified):**

Correctness landmines
- **`Int` is unsigned** — `0 - 5` evaluates to `0`. Breaks all subtraction/math.
- **Recursion + arithmetic crashes** — `n + sumTo (n - 1)` → deref error.
- **Function values crash** — `const f = dbl; f 5` → error.
- **Typed array element access mis-types** — a param `xs: []Int` makes `xs[0]`
  resolve to `[]Int`, not `Int`.

Missing primitives
- **No string operations** — no `.len`, `+`, split, trim, upper, lower,
  contains, slice, indexOf. Interpolation is the only string tool. Biggest
  single blocker (`std.str` cannot exist).
- **No function-typed parameters** — `fn(Int) Int` as a parameter type does not
  parse → no `map`/`filter`/`reduce`.
- **No maps** — `{ k: v }` literals do not parse (the `docs/features.md` example
  is aspirational); no `Map(K, V)` type.
- **No `while` loop** — only `for`.

Infrastructure
- **`import "std"`** special resolution is not implemented.
- **No generics** — no parameterized types / generic functions.

## Locked design decisions (2026-08-20)

1. **String ops = UFCS methods over Zig builtins.** Byte-level operations are
   implemented as builtins and surfaced as UFCS methods: `s.len`, `s.split ","`,
   `path.endsWith ".rn"`. Reuses the struct/UFCS machinery from 0.6.0.
2. **Pipeline ↔ param coercion.** A single-parameter, no-stdin function
   (`fn Void (s: String) String`) can be used as a pipeline stage
   (`… | trim`): in pipeline position, if the function's one parameter type
   matches the upstream stdout type, the incoming value binds to that parameter
   (stdin empty). This unifies UFCS/call/pipe — one helper works as `s.trim`,
   `trim s`, and `… | trim` — and makes the string methods pipeline-usable.
3. **`Int` becomes signed (`i64`).** Fixes `3 - 5 == -2` everywhere; matches
   scripting-language expectations. Foundation for all math.
4. **Composability: HOF + generics, via one comptime engine.** Generic functions
   are written with **implicit type variables** at the surface
   (`fn map(xs: []T, f: fn(T) U) []U`), which **desugar to comptime type
   parameters** internally, so the same engine later powers type-returning
   functions (generic containers). Monomorphized (a specialized copy per type
   combination). Generic containers are deferrable (arrays already cover the
   shallow stdlib). Fuller Zig-style comptime (comptime values, `@TypeOf`,
   type-level branching) is a later, separate discussion.

## Phases

Ordered by dependency. Correctness first (it unblocks everything and needs no
new syntax), then the primitives the stdlib is actually made of.

- [~] **Phase 0 — Correctness foundation.** No new syntax; fixes existing crashes.
  - [x] **Signed `Int` (`i64`) — DONE.** `Value.integer` is now `i64` (renamed
    from `uinteger: usize`); usize-consumption sites (lengths, indices, addr
    offsets, argv, heap len) cast explicitly; `parseInt`/literal parse read `i64`.
    Added **unary negation** (`-x` → `0 - x`, folds at comptime) with `UnaryOp.negate`
    (Int/Float). Tests: `signed_int_regression`; `call_in_arithmetic_regression`
    updated to a negative result.
  - [x] **Function-value call — DONE.** `const f = dbl` binds a function
    reference (a zero-arg reference to a parameter-taking function is a value, not
    a call); `f x` calls it. Handled in `compileCall` dispatch,
    `tryCompileTypedValueCapture`, and `callNeedsStdioCapture` via
    `InstructionSet.param_count`. Test: `function_value_regression`.
  - [x] **Typed array element access — DONE.** `BinaryExpr.resolveType` resolves
    `xs[i]` to the element type. Test: `typed_array_param_regression`.
  - [x] **Recursion returning a value — DONE.** Two fixes: (1) seed
    `closure_slot_count` with the parameter count at the *start* of `compileFnDecl`
    so a recursive fork emitted mid-body (from a value-capture wrapper, where
    `is_self_recursive` is false and the final count isn't set yet) allocates a
    closure large enough for the args — the previous `alloc 0` wrote the argument
    past a zero-size closure ("deref 0x11"); (2) give the self-visible function
    binding a `.function` type carrying its return type, so a recursive call is
    value-captured *by type* (`const r = f (n - 1)` keeps `r` typed) instead of
    byte-flattened to a string (which broke `n + f (n - 1)`). Now `sumTo`/`fact`/
    `fib` all work, including two recursive calls in one expression. Test:
    `recursion_value_regression`.

  **Phase 0 COMPLETE.** Full CI green (13/13, 42 diag, 8 examples, 113 smoke, 3 strict).

- [~] **Phase 1 — Strings.** The heart of the stdlib.
  - [x] **String builtins — DONE.** Byte-level ops via a new `str_op` IR
    instruction, surfaced as UFCS methods (`s.len`, `s.split ","`): `len`,
    `indexOf` (Int); `contains`/`startsWith`/`endsWith` (Bool);
    `upper`/`lower`/`trim`/`trimStart`/`trimEnd`/`slice`/`repeat`/`replace`
    (String); `split` (→ real `[]String` heap array) and `join` (`[]String` →
    String). They chain (`s.trim.upper`) and work on String params. Dispatch:
    no-arg in `compileMember`, with-args in `compileCall`, keyed by a
    `stringBuiltin(name)` table; `len` only intercepts a string receiver so
    arrays keep their own `.len`. Currently **byte**-oriented (not UTF-8-aware).
    Tests: `string_builtins_regression`, `string_split_join_regression`.
  - [x] **Pipeline↔param coercion (decision 2) — DONE.** A stage that references
    a single-parameter, Void-stdin function receives the upstream value in that
    parameter, so one helper works as `s.trim`, `trim s`, and `… | trim`. Type
    checker returns the parameter type as the stage's effective input;
    `tryCompilePipelineParamStage` desugars the stage to `collect_stdin` +
    bind-to-parameter + call (guarded to exactly one parameter; receiver stages
    only). Param-stages chain and mix with stdin-typed stages. Test:
    `pipeline_param_coercion_regression`. (Currently collects the input as a
    String; a typed scalar parameter — e.g. `Int` — would need typed transport,
    a follow-up.)
  - String `+` concatenation: not added; interpolation (`"${a}${b}"`) is the
    concat tool.

  **Phase 1 COMPLETE.** Full CI green (13/13, 42 diag, 8 examples, 116 smoke,
  3 strict).

- [ ] **Phase 2 — Higher-order functions + generics.** Composability.
  - Function values (overlaps Phase 0): a function bound to a value and called.
  - Function-typed parameters: parse + type-check `fn(T) U` parameter types.
  - Implicit type variables in signatures ⇒ desugar to comptime type params;
    monomorphization + call-site inference (decision 4).
  - Generic array helpers (in Runic, using the above): `map`, `filter`, `reduce`,
    `find`, `any`, `all`, `count`, `sort`. These double as the first real test of
    generics.

- [ ] **Phase 3 — The `std` module + shallow modules.**
  - Implement `import "std"` resolution: bundle std `.rn` modules (some
    builtin-backed) so `const std = import "std"` works and members resolve
    (`std.str`, `std.fs`, …).
  - Author the shallow modules: `std.str` (UFCS over Phase-1 builtins), `std.env`,
    `std.fs`, `std.path`, `std.process`, `std.testing` (assert helpers). Keep each
    compact per the stabilization checklist in `docs/standard-library.md`.

- [ ] **Phase 4 — Deferred / later discussion.**
  - Generic containers: comptime type-returning functions (`fn Box(comptime T:
    Type) Type`), reusing the Phase-2 engine.
  - Maps: `{ k: v }` literals + `Map(K, V)` type + access/keys/values.
  - `while` loop.
  - Fuller comptime (comptime values, `@TypeOf`, type-level computation) — its
    own design conversation.

## Open questions to resolve during implementation

- **UTF-8 vs bytes** for string ops (`len` counts what? `slice` on what units?).
- **Monomorphization strategy** — specialize per concrete type at each call; how
  to key/dedupe instantiations; error messages for un-inferrable type vars.
- **Function type syntax** — `fn(T) U` vs the existing `fn StdinType name(params)
  StdoutType`; how a function *type* (no name, no stdin) is written in a param
  position, and how it composes with the pipeline-coercion rule.
- **Comptime depth** — how far Stage-2/3 comptime goes; keep it minimal until a
  concrete stdlib need appears.

## Relationship to other docs

- `docs/standard-library.md` — the stdlib's modules, import model, conventions,
  and stabilization checklist (the *what*). This plan is the *language prerequisites*.
- `docs/plan.md` — roadmap Theme 3 (core language growth) lists generics, more
  operators, richer types; this plan is the concrete cut of that aimed at a stdlib.
