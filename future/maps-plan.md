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

- **Phase M0 — unblock:** fix generic struct return-type equality (#1). Small,
  self-contained type-checker change; re-run the prototype to confirm `set`/`get`
  type-check and run.
- **Phase M1 — the module:** write `std/map.rn` with `Entry`, `Map`, and
  `empty`/`set`/`get`/`has`/`keys`/`values`/`len`/`remove`. Feature tests +
  a `docs/standard-library.md` entry. Pure Runic — no compiler changes.
- **Phase M2 — ergonomics (optional):** `Name(args){ … }` construction (#2);
  decide whether to also offer a mutable in-place API.
- **Phase M3 — performance (future):** hashing for O(1) lookup — a comptime
  hash/eq dispatched per key type (buckets of entries), or a builtin fast path,
  keeping the same `std/map` surface.

## Open questions

- **Key types beyond `Int`/`String`:** how far to push generic equality (structs,
  arrays). Start with what `==` supports; document the limit.
- **Immutable vs mutable API** (or both).
- **Iteration order:** insertion order (an association list preserves it for free).
