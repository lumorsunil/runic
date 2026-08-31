# Maps — implementation plan

**Decision (2026-08-29):** no new special syntax — no `{ k: v }` literal, no
parser-baked `Map(K, V)`. Build maps up from existing primitives (generic
structs, arrays, generic functions) as a **`std/map` module written in Runic**,
with the types and generic dispatch handled at comptime (generic type
constructors + monomorphization) and the data operations at runtime.

## Design: association list

The simplest representation that needs no hashing — an array of key/value
entries, linear-scanned on lookup. O(n) per operation, which is fine for the
small maps typical in scripts; hashing is a later optimization (see below).

```rn
const Entry(K, V) = struct { key: K, value: V }
const Map(K, V)  = struct { entries: []Entry(K, V) }
```

Operations are generic functions in `std/map.rn`, monomorphized per `(K, V)`:

- `empty()` → `Map(K, V){ .entries = .{} }` (or a bare-construction helper)
- `set(m, key, value) Map(K, V)` — scan; replace the entry if the key exists,
  else append. Returns a **new** map (immutable, matching array `.push`).
- `get(m, key) ?V` — scan; yield the value or `null`.
- `has(m, key) Bool`
- `keys(m) []K`, `values(m) []V`, `len(m) Int` (`m.entries.len`)
- `remove(m, key) Map(K, V)` — filter out the entry.

Key equality uses `==`, which already works generically for `Int` and `String`
keys (verified). Struct/array keys need structural equality — deferred.

**Immutable style.** `set`/`remove` return a new map, so usage mirrors arrays:
`var m = map.empty; m = map.set m "a" 1`. (A mutable in-place style is possible —
struct fields are assignable through a `var` — but the immutable style is
consistent with `arr.push` and simpler.)

## Runtime / comptime split

- **Comptime:** the `Map(K, V)` / `Entry(K, V)` type resolution, generic dispatch,
  and per-`(K, V)` monomorphization of the operations. All type-level.
- **Runtime:** the `entries` array, the linear scan, the actual `set`/`get`. All
  value-level. Falls out naturally from generic constructors + monomorphization
  — no special machinery.

## Verified primitives (all present today)

- Generic type constructors (`const Box(T) = struct { … }`) and monomorphization.
- Generic `==` for key equality — works for `Int` and `String` (`sameKey 5 5`,
  `sameKey "x" "x"`).
- Array growth `arr.push x` (returns a new array), indexing, `.len`, `for`.
- Struct field mutation through a `var` (`h.items = h.items.push 3`).
- Optionals `?V` and `orelse`.

## Blockers to fix first (compiler / type-checker)

A prototype (`Entry`/`Map` + `set`/`get`) compiles down to one concrete gap:

1. **Generic struct return-type equality (blocker).** A generic function can't
   yet return a generic struct: `fn wrap(x: |T|) Box(T) { yield Box{ .value = x } }`
   fails with a "yield type mismatch" where the yielded and declared types print
   *identically* (`struct { value: T }`). The struct-type equality check doesn't
   treat two type-variable fields as equal (or one side is a `type_var` and the
   other a param identifier). Non-generic struct returns work fine. **This must
   be fixed before the map module can be written.** Likely in the type-checker's
   struct assignment/equality (`validateTypeAssignmentStruct` / the yield check):
   compare struct fields structurally, treating `type_var`/param identifiers
   permissively (as elsewhere).

2. **`Name(args){ … }` construction (nice-to-have).** Constructing a generic
   struct with explicit type args — `Entry(K, V){ .key = k, .value = v }` — does
   not parse; only the bare `Entry{ … }` form works (which is sufficient, the
   arguments don't affect the runtime layout). Add later for symmetry with the
   type position.

## Phasing

- [x] **Phase M0 — unblock. DONE (2026-08-30).** Generic structs are now
  returnable/reusable enough to build the map: fixed struct/application/type-var
  equality in `pipeTypesEqual`, typed generic struct literals by their field
  values (+ resolved nested `Entry(K, V)` fields), captured a `type_application`
  return by value (`isTypedCaptureReturn`), resolved the declared return in
  `runYield`, and made `var` reassignment value-capture a typed return. The
  full prototype (`set`/`get`/replace, String and Int keys) runs. Covered by
  `tests/features/generic_struct_return_regression.rn`.
- [x] **Phase M1 — the module. DONE (2026-08-30).** `std/map.rn` with `Entry`,
  `Map`, and `empty`/`set`/`get`/`has`/`keys`/`values`/`len`/`remove`. Registered
  in `std_modules.zig`, `build.zig`, and `std/std.rn`. Covered by
  `tests/features/std_map_regression.rn` (String→Int and Int→String, replace,
  remove) and a `docs/standard-library.md` entry. One compiler change was needed
  after all: an imported module's own type constructors weren't registered
  (only the main script's were), so `Map(K, V)` construction failed inside the
  module — fixed by extracting `registerTypeDecls` and calling it on module
  statements in `compileImportExpr`.
- **Phase M2 — ergonomics (optional).** `Name(args){ … }` construction: **DONE
  (2026-08-30)** — explicit-type-arg struct construction now parses (the type
  args don't affect layout, so it constructs by name and infers field types from
  the values); `std/map.rn` uses it. Fixing this also surfaced and fixed a
  pre-existing gap: member access on a *captured generic struct return*
  (`const e = wrap "x" 5` where `wrap` yields `Entry(K, V)`) — the member-access
  path didn't resolve a `type_application` object type to its struct layout.
  Covered by `tests/features/generic_struct_construction_regression.rn`.
  **Still open:** whether to also offer a mutable in-place API (see open q).
- **Phase M3 — performance (future):** hashing for O(1) lookup — a comptime
  hash/eq dispatched per key type (buckets of entries), or a builtin fast path,
  keeping the same `std/map` surface. **Blocked on foundational features**
  (2026-08-30 investigation): (1) a **string hash primitive** — there's no way
  to fold over a string's bytes in Runic today (`s.split ""` returns the whole
  string, no byte/char access), so a String hash needs a builtin or `s.bytes`;
  per-key-type dispatch itself already works (`if (key is String) …`, a runtime
  test on a type-param value). (2) **Array element-type propagation through
  indexing** — buckets must be indexed (`buckets[h]`), and indexing lost the
  element type. **DONE (2026-08-30):** `.push` now refines an unknown (`[]Void`)
  element type from the pushed value, and reassigning a mutable variable refines
  its tracked type; `arr[i].field`, `arr[i][j]`, and `const b = arr[i]` now
  resolve. Covered by `tests/features/array_element_typing_regression.rn`.

  **Byte access DONE (2026-08-31):** `s.bytes` yields a string's bytes as
  `[]Int`, so a hash can be folded from primitives (a polynomial rolling hash
  works — Runic has no bitwise `^`, so FNV-1a-style xor is out; use `*`/`+`/`%`).
  Covered by `tests/features/string_bytes_regression.rn`.

  **`is`-narrowing DONE (2026-08-31):** inside `if (key is String) { … }` the
  subject narrows to the tested type, so `key.bytes`/`key.upper` resolve and the
  hash can be folded **inline** on the narrowed key (no helper → sidesteps the
  call-boundary bug below). Covered by `tests/features/is_narrowing_regression.rn`.

  **Struct-field / call-index array typing DONE (2026-08-31):** `m.buckets[i]`
  and `arr[hash key]` now resolve (mutable reassignment refines `type_expr`; an
  inline-call index is captured). So the bucketed `Map`/`Bucket`/`Entry`
  representation constructs and reads with literal indices.

  **Integer modulo fix DONE (2026-08-31):** the real blocker turned out to be
  `int % int` returning **Float** (the slow arithmetic path forced float), so a
  hash bucket `h % n` couldn't index an array (`addr + Float`). `%` now yields an
  Int for integer operands (using `@mod`, so `x % n` ∈ `[0, n)` for positive `n`).
  Covered by `tests/features/integer_modulo_regression.rn`.

  **All M3 compiler/runtime blockers are now cleared.** The full bucketed
  prototype runs end-to-end (`Map`/`Bucket`/`Entry`, inline `is`-narrowed hashing,
  String and Int keys, replace-in-place). One pre-existing bug remains *noted but
  sidestepped*: array-returning string ops (`.bytes`/`.split`) on a string passed
  one hop as a function argument yield empty — inline hashing avoids it.

  **What's left for M3 is writing the module itself** (`std/map.rn` v2), plus one
  design decision — see Open questions: **iteration order.** The M1 association
  list preserves insertion order (`keys`/`values`); a pure bucketed map iterates
  in bucket/hash order. To keep the documented insertion-order contract, the
  hashed map would also carry an ordered entries list (buckets for O(1) lookup,
  list for iteration) — more bookkeeping in `set`/`remove`. Resize-on-load-factor
  is deferred (a fixed bucket count is fine for script-sized maps).

## Open questions

- **Key types beyond `Int`/`String`:** how far to push generic equality (structs,
  arrays). Start with what `==` supports; document the limit.
- **Immutable vs mutable API** (or both).
- **Iteration order:** insertion order (an association list preserves it for free).
