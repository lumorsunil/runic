# Comptime type construction — design note

Exploratory design for **building a type at compile time from computed parts** —
e.g. a struct whose fields are derived from another type, a comptime list, or a
computation. This is item #2 of the comptime "possible next directions" in
`docs/plan.md` (§3). Runic can already *return* a hand-written struct type from a
comptime type function; it cannot yet *build* one whose shape is decided at
compile time.

Status: **increment 1 landed** (`comptime-functions` branch) — `@insert "…"` in a
struct body re-parses a *static* string as a field list and grafts the fields
(parser re-entry). The generated field types may name the enclosing type's
parameters (`@insert "value: T"`), resolved by the existing generic substitution,
so no type-checker/compiler materialization was needed. **Increment 2 landed:**
dynamic `@insert "${…}"` and comptime `for (@fields(T)) |f| @insert "…"` inside a
struct body. The compiler materializes the recipe at instantiation — folding the
operand with `comptimeMessage`, resolving `@fields(T)` via `resolveComptimeType`,
re-parsing via a compiler-lifetime sub-parser — reached from both `resolveType­
Application` (member access, annotations) and struct construction (which pulls the
type args from the binding's annotation). **Tightening landed:** the type checker
now materializes a recipe at `resolveTypeApplication` too — it re-parses each
`@insert` operand into concrete fields (folding `${f.name}` to the literal name and
every type interpolation to a placeholder it substitutes back with the real
`TypeExpr`, via its own checker-lifetime sub-parser). So an *annotation* type
(`const p: Partial(Point)`) resolves to a struct with **real field types**: member
access is typed precisely (`p.a orelse …` sees `?Int`; `const s: String = p.a` is a
static error), not permissively. Recipes it can't fold statically — a dynamic field
*name*, or an unresolvable `@fields(…)` source — stay permissive for the compiler.
**Construction typos are now caught too:** a `Ctor{…}` literal validates against the
concrete struct its *annotation* materializes — `runStructLiteral` pulls the
expected type (`Partial(Rec(Int))`) off the expected-type stack, resolves it through
`resolveTypeApplication` (real fields), and checks the literal against that. So
`Partial{ .zzz = 1 }` reports the unknown field, the missing field, and a wrong
value type — resolving points 1 & 3 below for the annotation-driven case. This now
extends to a recipe-ctor literal passed as a **call argument** (`f Partial{ … }`):
the parameter's materialized type is pushed as the expected type, so the literal is
validated the same way (`expectedMaterializedStruct` also accepts an
already-resolved parameter struct, not only a written `Partial(…)` application).
The remaining gaps are a **bare literal with no annotation *and* no context**
(genuinely unresolvable — there are no type arguments to materialize from), and the
*runtime* construction of a bare recipe literal in argument position, which the IR
compiler still materializes only from a binding annotation (a separate, pre-existing
gap — the static check catches typos, but a valid recipe-literal argument should be
constructed via an annotated binding until the compiler materializes it in argument
context too).

**Increment 3 — reframed from `@code` to *comptime statement generation*.** The
original increment 3 was `@code` (AST as a first-class value). On review, the
higher-value move is to let comptime *generate implementations*, not only types —
Zig's `inline for` story. Runic already unrolls `for (@fields(T)) |f| { … }` in a
function body (`compileComptimeFieldsForLoop`), so the missing primitive was
value access by a comptime name. **Landed: `@field(value)(name)`** — the analog of
Zig's `@field(x, name)`. `name` folds at compile time (usually `f.name` from the
unrolled loop) and the call lowers to an ordinary member access `value.<name>`
(reusing `compileMember`), so a comptime loop can now touch each field's *value*
(serialize, compare, print), not just its metadata. **Lvalue also landed** —
`@field(value)(name) = v` writes the slot (obeying mutability), so a loop can *set*
each field. Two supporting fixes fell out: (a) `compileMember` now suppresses
unresolved member access in an *unspecialized generic template* (a `T`-typed value
has no layout until `T` binds), so reading/writing `@field` on a generic value works
outside a `for (@fields)` loop, not only inside one where the template unrolls to
nothing; (b) the struct field-assign path no longer panics when the lvalue doesn't
resolve. **Generic struct return — fixed.** Returning a whole struct *value* from a
`comptime T: type` function used to corrupt (even `fn f(comptime T,v:T) T { yield v }`):
the declared return was a bare type parameter (`T`), which the call site couldn't
classify as a by-value capture, so the struct went down the byte-capture path
(stdout text round-trip) and was misread. The typed-vs-byte decision now resolves
the declared return against the call's arguments (`callConcreteReturnType`, extended
to bind `comptime T: type` params, not only `|T|` captures) before deciding — so a
return that denotes a struct/application is captured by value. Generic field-wise
builders (`copy`, `doubled`, `reset`) now work end to end with `@field`.

## `@code` — deferred indefinitely (and why)

`@code`/`Code` (a first-class AST fragment, spliced by `@insert`) was the planned
increment 3. After building out the rest of the comptime surface and surveying
where it would actually be used, we **deferred it indefinitely**. The reasoning:

- **It's comptime-only** (like Jai's `#code`/`#insert`: an AST is grafted at
  compile time and gone before runtime), so there's no fields-vs-statements or
  comptime-vs-runtime barrier — splicing a `Code` block at a statement position is
  mechanically the same as grafting a field list into a struct. That part is fine.
- **But its value is now small.** `@field` + `for (@fields(T))` + comptime `if`/
  `match` pruning already generate *implementations* over values, and Runic
  evaluates comptime control flow **inline** (unroll/prune), so within a scope you
  rarely need an explicit quote+splice — you just write the control flow. `@code`'s
  remaining, genuinely-additive roles are hygienic/reusable `@insert` *fragments*
  and cross-scope block reuse.
- **A stdlib survey found no home for it.** Nothing in `std/` generates struct
  *shapes*; the codegen that *does* fit the stdlib is the `@field`/`@fields`
  implementation-generation we shipped (used in the new `std.meta`). `@insert`/
  `@code` field generation would only pay off with "mapped type" utilities
  (`Partial`/`Pick`/`Omit`), which read as a type-system library, not a scripting
  stdlib.
- **The reuse/control-flow cases it seemed to unlock are better served by
  *closures*.** A trailing block bound to a `fn () Void` parameter — a nullary
  closure capturing its environment — gives `withTimer { … }` / `retry { … }` with
  caller locals working, using machinery Runic already has, no metaprogramming.
  That is the direction we're taking instead.

So: `@code` is parked, not killed — revisit only if a concrete need for structured,
reusable AST fragments shows up. This note stays as the record of the approach (the
Jai/Mox "code is text/AST" model, deliberately *not* Zig's `@Type`) and why the
value collapsed once `@field`/`@fields` shipped.

## Found along the way (pre-existing bugs)

Building the stdlib codegen surfaced pre-existing compiler bugs in the "use a call
result as an operand/argument" area, independent of the comptime work — since
fixed:

- `h = h + (someCall)` (a reassignment whose RHS reads the target and has a call
  operand) crashed with "could not dereference value of type thread" — the call
  wasn't captured to its value. **Fixed** (the in-place `x = x <op> n` path now
  value-captures a call operand like every other arithmetic operand).
- `f (@field(v)(name))` — a `@field` (or call) result passed as a parenthesized
  argument — mis-derefed similarly.

A further, distinct blocker for *recursive* comptime codegen was that a generic
function recursing at a **different type per level** (a field-wise `hash`/`show`
that descends into struct-typed fields) hung forever: inside a specialization the
recursive callee resolved to the in-progress spec and never re-specialized for the
new type. **Fixed** — each recursive call is reconciled against the active
specializations by its argument-type key, so a different-type call re-routes
through the generic and specializes anew. Recursive `std.meta`/`std.map` struct-key
support is no longer blocked on the language.

## Increment 2 — attempted, and what it actually requires (findings)

I built 2a (the ordered-recipe AST + parser for `for (@fields(T)) |f| @insert "…"`)
and a working compiler-side materializer hooked into `resolveTypeApplication`
(binds the type params, resolves `@fields(T)` to the argument struct's fields via
the existing `resolveComptimeType`/`comptimeStructOf`, folds each `@insert`
operand with the existing `comptimeMessage`, re-parses via a compiler-lifetime
sub-parser — `Parser.reparseFieldString`, reachable because the compiler and
parser share the same `DocumentStore` type). That much compiled.

Then testing `Partial(Point(Int))` exposed that materialization at
`resolveTypeApplication` alone is **not enough** — the feature needs the concrete
fields at *four* points, and the two stages don't share them:

1. **Type-checker, construction** (`runStructLiteral`, ~:5099) — `Partial{ .x = 1 }`
   validates field names against the resolved struct, which is the *empty* recipe,
   so it errors "struct has no field 'x'" before the compiler ever runs.
2. **Type-checker, member access** — `p.x` needs `x` to exist on the type.
3. **Compiler, construction** (`compileStructLiteral`, ~:3980) — resolves the field
   layout from `user_struct_types[<bare name>]` (the raw recipe), *not* from the
   binding's annotation type, so it doesn't reach `resolveTypeApplication`'s
   materialization. A bare `Partial{…}` also carries no type args (they live on the
   annotation), so construction must consult the expected/annotation type.
4. **Compiler, member access** — `p.x` uses `p`'s annotated type, which *does* go
   through `resolveTypeApplication` (materialized), so this one likely works.

And the **type checker has no comptime string folder**: its `comptime_field_vars`
is a name *set* (it only type-checks `f.name`/`f.type`, doesn't fold them to
values), and its `@fields` loop doesn't unroll. So full static field-checking of a
generated struct needs either a folder there or a shared materializer both stages
call — plus caching so `Partial(Point(Int))` yields one identity across stages.

**Recommended shape for the real implementation:** a single shared materializer
(operating on `ast.TypeExpr` + a small stage context: resolve-type, type-name,
parse-fields, param-bindings), memoized per `(ctor, args)`, invoked at every point
that resolves a comptime-ctor application to its struct — annotation resolution,
construction, and member access — in both stages. That is the clean version; the
piecemeal `resolveTypeApplication`-only hook is not sufficient.

## Decisions (locked)

- **`@insert` is `String → field(s)`, nothing more.** No `emit` keyword, no `+`
  operator. All dynamism comes from ordinary comptime constructs feeding it a
  string. The end-state spelling is a comptime `for` *inside* the struct body,
  `@insert`-ing one field per iteration:
  ```runic
  fn Partial(comptime T: type) type {
    yield struct {
      for (@fields(T)) |f| @insert "${f.name}: ?${f.type}"
    }
  }
  ```
  This fits Runic's model better than accumulating a string before the `yield`:
  `comptimeTypeCtor` extracts the *yielded* struct type and ignores preceding
  statements, so putting the loop *inside* the struct keeps the generation logic
  where materialization already looks.

- **Comptime evaluation is a universal principle, not a struct feature.** Wherever
  the inputs are comptime-known, evaluate at compile time and emit IR only for the
  runtime remainder — everywhere, not just inside structs:
  1. a comptime-known `if`/`match` predicate keeps only the taken arm; the untaken
     arm emits *no* IR (this is already why an `@compileError` in a dead arm never
     fires);
  2. a loop over a comptime-known source (`@fields(T)`, a constant range, a
     comptime array) is unrolled with the loop variable bound per iteration;
  3. a taken/unrolled body that is itself pure-comptime is folded to a constant
     rather than lowered to IR.
  Rule 3 only erases *pure* work: a comptime-selected/unrolled body that still has
  runtime effects (`echo`, a command, a runtime `yield`) still lowers to IR — we
  prune the dead arm, unroll, and fold the pure parts, but real side effects
  remain. Runic already does each of these per-construct (comptime `if`/`match`
  pruning, `for (@fields)` unrolling, value/type folding); the goal is to make it
  uniform so `@insert` rides the same fold path a function body uses and works
  everywhere for free.

## The two models, and why we prefer the Jai/Mox one

**Zig (`@Type`) — build a structured value.** You assemble a `Type_Info`-shaped
record (a `fields` array of `{ name, type, alignment, … }`) and hand it to a
builtin. In recent Zig this split into a family of builtins (`@Type`, and
per-kind helpers). It is type-safe but verbose, and the split-into-many-builtins
is exactly what we want to avoid.

**Jai / Mox — generate the *source* (text or AST) and re-parse it.** There is one
mechanism regardless of what you generate — a type, a function, an expression —
so no `@Struct`/`@Union`/`@Enum` zoo. In Jai:

- `#insert "c := a + b;"` — a code string becomes source at that point.
- `#insert -> string { … String_Builder … return str; }` — a comptime function
  *computes* the code string. Placed inside a struct body, this is how you build
  a struct with a field per item:
  ```jai
  My_Struct :: struct {
    #insert -> string { /* build "x: int;\ny: int;\n…" */ };
  }
  ```
- `#code expr` — capture an AST as a first-class `Code` value (structured, not a
  string), inspectable via `compiler_get_nodes()`.

Mox is the same family, AST-leaning: `#format_temp(...)` builds a code string,
`__compiler_parse(str)` turns it into AST, `#land_ast(...)` splices it in
("ast and types are first class").

The essential move: **reuse the normal parser** on generated text instead of
describing the type as a data structure. Downsides are honest — it is
stringly-typed (mistakes surface in generated text; needs hygiene for generated
names) — and Jai's `#code`/Mox's AST path is the structured middle ground.

## Why Runic is well-positioned

Runic already has the pieces the Zig way lacks and the Jai way needs:

- comptime string interpolation — `${T}`, `${f.name}`, `${f.type}`;
- comptime iteration — `for (@fields(T)) |f| { … }`, nestable;
- comptime type functions that already `yield struct { … }`;
- a real parser (`src/frontend/parser.zig`) fed by a streaming lexer.

So generating the *text* of a struct body is nearly free with what exists; the
missing capability is narrow (below). This is a much smaller commitment than
arbitrary `#run` (a full comptime interpreter): no side effects, no I/O, no
sandbox — just "parse this comptime-produced string and graft the nodes in."

## Proposed surface (sketch, not final)

Primary form — an `#insert` inside a struct body that contributes fields, driven
by the existing comptime `for`:

```runic
// Make an "all fields optional" view of any struct T.
fn Partial(comptime T: type) type {
  yield struct {
    #insert (for (@fields(T)) |f| { emit "${f.name}: ?${f.type}\n" })
  }
}
```

Two spellings are worth weighing:

- **A. String `#insert`** (shown above): the operand is a comptime `String`; it
  is lexed+parsed as a struct-field list and merged into the enclosing struct.
  Most idiomatic given interpolation + `@fields` already produce comptime
  strings. Stringly-typed.
- **B. A `@parseType(str)` / type-from-string builtin**: parse a whole comptime
  string as a *type expression*, returning a type. Composes with the existing
  "a comptime type function returns a type" model without a special struct-body
  construct. `fn Partial(comptime T) type { yield @parseType(genBody(T)) }`.
- **C. AST-as-value (`#code`)** — a structured `Code` value built/inspected
  without strings. Strictly better ergonomics and hygiene, strictly more work;
  defer until A/B prove the demand.

Recommendation: prototype **A** (or B) first — string in, parser out — and only
add C if the stringly-typed friction bites.

## The one new mechanism: parser re-entry at comptime

Everything above reduces to: **run the parser on a comptime-produced string and
splice the resulting AST into the enclosing declaration**, then continue normal
type-checking/compilation. Concretely:

1. The comptime folder evaluates the generator (`for … emit …` or the
   `genBody(T)` call) to a `String` — this uses the *existing* comptime string
   machinery (interpolation, `@fields` iteration). No new evaluation power.
2. Lex + parse that string as the expected fragment — a struct-field list (form
   A) or a type expression (form B) — via the existing lexer/parser on a
   synthetic source buffer.
3. Graft the parsed nodes into the enclosing `struct { … }` AST (append the
   generated fields) *before* the struct type is resolved, so the rest of the
   pipeline (type-check, layout, member access, `${T}` serialization) sees a
   normal struct with concrete fields.

The integration point is where comptime type constructors are evaluated (the
type checker's `evalComptimeTypeCall` / `comptimeTypeCtor` path, mirrored by the
IR compiler's folder). The struct body must be *materialized* — generators run,
fragments parsed and grafted — at that point, before the type is first used.

This is bounded work: the lexer and parser already exist; we are calling them on
a comptime string and merging nodes, not building a new evaluator.

## Scope / non-goals (initial)

- **Types only, to start** — generating struct fields (and maybe a whole type
  expression, form B). Not arbitrary statement/function generation, and not the
  general `#insert "any code"` of Jai. Keeping the target to *type shape* keeps
  hygiene and error-reporting tractable.
- **No `#run`** — the generator is still the restricted, side-effect-free
  comptime surface (folding, `@fields`, interpolation). No I/O, no exec, no
  running arbitrary functions at compile time. That remains a separate, larger,
  probably-unwanted decision (`docs/plan.md` item #3).
- **No mutable-AST metaprogramming** — no compiler message loop, no
  `compiler_get_nodes`-style transforms of existing code. Generation only.

## Open questions

1. **String (A) vs type-from-string (B) vs AST value (C)** as the first cut. B is
   the smallest surface (one builtin, no new struct-body syntax); A reads best in
   examples; C is the endgame.
2. **Hygiene / name capture.** Generated field names come from `${f.name}` — do
   they collide with anything? For struct *fields* the risk is low (they live in
   the struct's own namespace), which is another reason to scope this to types
   first. General code `#insert` would need scope rules; we avoid it initially.
3. **Error reporting.** A parse/type error in generated text must point somewhere
   useful. Options: attach the `#insert` call-site span (Jai uses `#location()`),
   plus an offset into the generated buffer; surface the generated text in the
   diagnostic. Decide the span strategy before shipping.
4. **What can drive generation.** `@fields(T)` (have it), a comptime `[]String`
   of names, a `comptime n: Int` count. All already comptime-known — confirm the
   folder can produce the driving values in the type-constructor context.
5. **Caching / identity.** Two calls `Partial(Foo)` must yield the *same* type
   (structural identity + serialized name), consistent with how `Box(Int)`
   already caches. Ensure the grafted-struct type participates in the existing
   type cache keyed by the constructor + args.
6. **Interaction with monomorphization.** `Partial(T)` is a comptime type
   function; it already monomorphizes per `T`. The generated body must be
   materialized per specialization, once, and reused.

## Relationship to the existing comptime surface

This is additive and composes with what shipped on `comptime-functions`:

- reads types via the introspection builtins (`@fields`, `@kind`, `@fieldCount`,
  `@hasField`, `@elem`, `@child`);
- driven by comptime `for` and string interpolation;
- the result is an ordinary type usable with overloading, higher-kinded capture,
  `@hasMethod`/`@compileError`, and `${T}` serialization.

It is the natural next step that keeps Runic's comptime *restricted and
declarative* (generate shape, don't run arbitrary code) while removing the "can
return a type but not build one" limitation.

## References

- Jai metaprogramming: `#insert`, `#code`/`Code`, `String_Builder` codegen —
  <https://github.com/Jai-Community/Jai-Community-Library/wiki/Metaprogramming>,
  <https://github.com/Ivo-Balbaert/The_Way_to_Jai/blob/main/book/26A_Metaprogramming.md>
- Mox (`#format_temp`, `__compiler_parse`, `#land_ast`, `$T: __type_ptr`) —
  <https://github.com/Morglod/mox>
- Contrast: Zig `@Type`/compile-time function execution —
  <https://en.wikipedia.org/wiki/Compile-time_function_execution>
