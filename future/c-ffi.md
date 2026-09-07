# C FFI: importing dynamic libraries

## Introduction

Runic scripts should be able to load a C dynamic library (`.so`/`.dylib`/`.dll`)
and call functions in it. Zig's C interop makes the pieces available — runtime
dynamic loading via `std.DynLib`, and the C calling convention via
`callconv(.c)` — but there is one real obstacle that shapes the whole design,
covered next.

This document has moved past scoping: the surface-level design is decided (see
below) and the rest sketches the type model, the calling mechanism, the
integration points, and a phased rollout, flagging the questions that remain.

## Decisions

- **Syntax** — a dedicated `cimport "lib" { extern fn … }` block; its own AST
  node, distinct from Runic module `import` and the stream-typed `fn` form.
- **C types** — provided by a new `std.ffi` standard-library module, imported
  explicitly and referenced qualified (`c.Double`, `c.Ptr`, …), rather than a
  set of global `CInt`-style builtins. Keeps the C widths out of the global
  namespace and reuses the module system. **This depends on two language
  features Runic does not have yet** (verified 2026-09): a module exporting a
  *type* (`pub const X = struct {…}` currently fails to parse), and a *qualified
  type reference* (`module.Type` in annotation/expression position currently
  fails to parse). We have chosen to build those first — they are independently
  valuable — rather than fall back to a compiler-only C-type surface. See phase
  0 of the plan.
- **C-type set** — the full scalar set ships up front (signed + unsigned +
  narrow widths), not a minimal subset, so real libraries work on day one.
- **Calling mechanism** — statically linked, vendored `libffi` (below); the
  pure-Zig comptime trampolines are a documented fallback, not the default.
- **Safety** — `cimport` is available with no flag (Python `ctypes` / Lua `ffi`
  posture), documented as unsafe; a future `--safe` mode can disable it.

## Goals

- `import` a system or local C library and call scalar functions from it
  (`libm`'s `cos`/`pow`, a project's own `.so`, etc.).
- A declared, type-checked interface so calls are checked like any other Runic
  call and so the runtime knows how to marshal arguments.
- No *runtime* third-party dependency: the C-calling backend (`libffi`) is
  vendored and statically linked, so it ships inside the `runic` binary and
  end users install nothing.

## Non-goals (at least initially)

- `union`s and variadic functions. Passing and returning C structs *by value*
  is supported, including nested structs (see below).
- C callbacks (passing a Runic function as a C function pointer).
- Compile-time `@cImport` of headers. Runic is interpreted; there is no Zig
  compile step per script, so the C interface is described in the script, not
  parsed from a header.
- Automatic memory ownership across the boundary (see *Ownership & safety*).

## The core challenge

`dlsym` (via `std.DynLib.lookup`) returns an **address**. To *call* it, the
compiler must know the function's *type* (arity, argument classes, return
class) so it can emit the platform ABI (which registers/stack slots the
arguments go in). In Zig that type is required at **comptime**:

```zig
pub fn lookup(self: *DynLib, comptime T: type, name: [:0]const u8) ?T
```

But in an interpreted language the signature is only known at **runtime** (it
comes from the script). So we cannot simply cast the address to "the function
type the script declared" and call it — that type does not exist at Zig compile
time. Everything below is really about bridging that gap.

## Design

### 1. Loading a library

`std.DynLib.open(path)` / `.openZ(pathZ)` wraps `dlopen`; `.lookup` wraps
`dlsym`; `.close` wraps `dlclose`. Library handles are long-lived process
resources — they fit the existing `closeable` value/resource model
(`src/ir/value.zig`, `src/closeable.zig`) so they are closed at script exit.

Path resolution needs its own rule, distinct from `resolveModulePath`
(`src/frontend/document_store.zig`), because C libraries are found differently
from `.rn` modules:

- a bare soname (`"libm.so.6"`, `"libSDL2.so"`) → hand to `dlopen`, which uses
  the system loader search path (`LD_LIBRARY_PATH`, `/etc/ld.so.cache`, …);
- a relative/absolute path (`"./native/mylib.so"`) → resolve against the
  importing script's directory, like module imports.

### 2. Declaring the interface

A C library carries no Runic type information, so the script declares the
functions it will call. A dedicated `cimport` keyword with an `extern fn` block
keeps this separate from the Runic module import and from the stream-typed
`fn StdinType name(...) StdoutType` signature (C functions have no stdin/stdout
stream notion). The C types come from a standard-library `std.ffi` module,
imported explicitly and referenced qualified (`c.Double`), so the C widths stay
out of the global namespace and lean on Runic's existing module system rather
than a bag of new builtin type names:

```rn
const c = import "std/ffi.rn"

const m = cimport "libm.so.6" {
    extern fn cos(x: c.Double) c.Double
    extern fn pow(base: c.Double, exp: c.Double) c.Double
}

echo "${m.cos 0.0}"        // 1
echo "${m.pow 2.0 10.0}"   // 1024
```

The `cimport` result binds like a module value: `m.cos`, `m.pow` are members
resolved the same way module members are today (`compileMember` /
`resolveMemberFieldSpan`). Each `extern fn` records: the symbol name, the
ordered parameter C-types, and the return C-type.

The C types are named the C width directly (Runic `Int` is 64-bit, C `int` is
32-bit — conflating them is a real bug source). `std.ffi` exports the full set
up front so real libraries work day one, not just `libm`:

| `std.ffi` type | passed as                    | from / to Runic value      |
|----------------|------------------------------|----------------------------|
| `c.Int`        | 32-bit signed (GP)           | `Int` (i64, range-checked) |
| `c.UInt`       | 32-bit unsigned (GP)         | `Int`                      |
| `c.Long`       | 64-bit signed (GP)           | `Int`                      |
| `c.ULong`      | 64-bit unsigned (GP)         | `Int`                      |
| `c.Short`      | 16-bit signed (GP)           | `Int`                      |
| `c.UShort`     | 16-bit unsigned (GP)         | `Int`                      |
| `c.Char`       | 8-bit (GP)                   | `Int`                      |
| `c.SizeT`      | pointer-width unsigned (GP)  | `Int`                      |
| `c.Float`      | 32-bit float (SSE)           | `Float` (f64)              |
| `c.Double`     | 64-bit float (SSE)           | `Float`                    |
| `c.Bool`       | int 0/1 (GP)                 | `Bool`                     |
| `c.Str`        | `[*:0]const u8` (GP)         | `String`                   |
| `c.Ptr`        | pointer (GP)                 | opaque handle (`addr`)     |
| `c.Void`       | void return                  | `Void`                     |

The `c.X` names must be usable in the `extern fn` *type* position. That is the
same capability as an imported module exporting a named type used in an
annotation (structs already work this way today); the `std.ffi` members resolve
through that machinery, and the extern-block checker maps each to its ABI
class. Integer widths narrower than `Int` are range-checked at the call, like
`c.Int` against i64.

### 3. Marshalling

- **`c.Str` in:** Runic strings are `addr+len` byte slices in arena memory
  (`Value.Slice`), not null-terminated. Marshalling allocates a
  null-terminated copy for the call and frees it after (the callee must not
  retain it — document as a rule).
- **`c.Str` out:** a returned `char*` is copied into a fresh Runic `String`.
  Runic never `free`s it (C owns it); functions returning heap strings the
  caller must free are simply not safe to expose this way in the MVP.
- **`c.Ptr`:** modeled as an opaque `addr` value — passable back into other
  `extern fn`s but not dereferenceable from Runic. Enables handle-style APIs
  (`open` → handle → `use handle` → `close handle`).
- **Numbers/bools:** direct, with the narrower integer types (`c.Int`,
  `c.Short`, `c.Char`, `c.UInt`, …) range-checked against the Runic `Int` i64.
- **Structs by value (in):** a C struct passed by value is declared as an
  ordinary Runic struct whose fields are all scalar `c.X` types, e.g.

  ```runic
  const c = import "std/ffi"
  const Color = struct { r: c.Char, g: c.Char, b: c.Char, a: c.Char }
  const rl = cimport "libraylib.so" {
      extern fn ColorToInt(color: Color) c.Int
  }
  const packed = rl.ColorToInt Color{ .r = 255, .g = 0, .b = 0, .a = 255 }
  ```

  At the call the evaluator builds an `FFI_TYPE_STRUCT` from the field C-types,
  lets `libffi` compute the target-ABI field offsets
  (`ffi_get_struct_offsets`), and copies each Runic field into the argument
  buffer at its offset. The type checker admits such a struct in an `extern fn`
  signature and reports a field that is not a scalar `c.X` or a by-value struct.
- **Structs by value (out):** a struct *return* (`extern fn GetColor(hex:
  c.UInt) Color`) works the same way in reverse — `libffi` writes the struct
  into a return buffer, and the evaluator reads each field back at its computed
  offset into a fresh heap-backed struct value (the representation a struct
  literal compiles to), typed as the user struct so field access composes. A
  struct-returning extern call bypasses the stdio-capture path (it is a direct
  libffi dispatch, never a command), so its heap-backed result survives being
  bound.
- **Nested structs:** a field may itself be a by-value struct
  (`Camera2D { Vector2 offset; Vector2 target; … }`). Every Runic struct field
  is exactly one slot — a scalar holds its value, a nested struct holds the
  sub-struct's address (by reference) — so the evaluator builds a nested
  `FFI_TYPE_STRUCT`, and marshalling recurses: an argument field follows the
  address to marshal the sub-struct into the parent's buffer at its offset, and
  a return field rebuilds the sub-struct in its own heap block and stores its
  address in the parent slot. Field offsets always come from
  `ffi_get_struct_offsets`, so the target ABI's struct layout (padding,
  alignment) is respected at every level.

### 4. Calling — statically linked libffi

**Primary mechanism: `libffi`, vendored and statically linked.** `libffi`
builds a call interface (`ffi_cif`) from a *runtime* type list and performs the
call (`ffi_call`) — exactly the runtime-signature problem this whole document
is about, solved portably and battle-tested (it backs Python `ctypes`, Ruby
FFI, and others). It handles any arity, struct-by-value, and varargs.

By compiling `libffi` from vendored source and linking it into the `runic`
binary (Zig's build system compiles C directly), it becomes a **build/dev
dependency, not a runtime one** — end users install nothing, and there is no
`libffi.so` to find at script-run time. Licensing permits this: `libffi` is
under a permissive MIT-style license, so static linking and redistribution are
fine.

This gives one code path with the full feature set from the start, and — most
importantly — we never hand-write calling-convention code that could be subtly
wrong per ABI (the single biggest risk otherwise; see *Open questions*). At each
`extern fn` we build (and cache) an `ffi_cif` from the declared C-types once,
then per call marshal the Runic `Value`s into a scratch buffer, hand `ffi_call`
the address + the argument-pointer array, and convert the return slot back to a
`Value`.

The one real cost is the build: `libffi` is not pure C — it carries
per-architecture assembly (`unix64.S`, `sysv.S`, …) and configure-generated
headers (`fficonfig.h`, `ffitarget.h`) that differ per target. Vendoring it
means committing those pre-generated headers for each supported target and
selecting the right `.S` per arch in `build.zig` — a bounded but real
per-target matrix to maintain, and the thing that erodes Zig's otherwise
trivial cross-compilation. Existing `build.zig` ports of `libffi` do exactly
this; whether a maintained Zig package for it exists should be checked before
hand-vendoring (see *Open questions*).

**Optional pure-Zig fallback: comptime trampolines.** If the vendored-libffi
build friction (or a target libffi's matrix does not cover) ever justifies it,
the calling step can be swapped for a dependency-free pure-Zig path: reduce
every argument to its ABI *class* — on SysV AMD64 / AArch64 AAPCS,
integer/pointer/bool/string pass in general-purpose registers and float/double
in SSE/vector registers — so a signature collapses to an *ordered GP/SSE
sequence plus a return class*. Up to arity 6 that is only
`sum(2^n for n in 0..6) = 127` argument sequences × 3 return classes ≈ **~380**
distinct shims, which Zig generates at comptime; the runtime picks one by the
declared signature, `@ptrCast`s the `dlsym` address to it, and calls. It covers
all *scalar* signatures but **excludes** struct-by-value and varargs (their ABI
classification is not a simple GP/SSE split), so it is a fallback, not a
replacement. Because both mechanisms consume the same `extern fn` declarations
and the same marshalling layer, only the final dispatch step differs — the
choice can be revisited without touching the rest of the design.

### 5. Return values, errors, ownership, safety

- Calling C is inherently unsafe: a wrong signature, a bad pointer, or a
  library bug can corrupt or crash the process, and Runic cannot catch a
  segfault. The declared-interface + explicit-C-type approach narrows the
  footgun (no guessed signatures) but does not remove it. **Decision:** `cimport`
  is available with no flag or gate — the same posture as Python's `ctypes` and
  Lua's `ffi` — and documented as an unsafe escape hatch. The design keeps a
  future `--safe`/sandboxed mode able to turn it *off*, but nothing is gated now.
- A `dlopen`/`dlsym` failure (missing library or symbol) is a clean, catchable
  runtime error, distinct from a crash inside a call.
- Ownership across the boundary is manual and, in the MVP, deliberately
  restricted (no Runic-side `free` of C returns; strings copied in/out). Richer
  ownership (e.g. handing struct memory across) comes with struct support.

## Integration points

- **Lexer/parser** (`src/frontend/`): a `cimport` keyword + an `extern fn`
  declaration block (a new AST node — it is *not* a normal `FunctionDecl`, since
  there is no body and the parameter types are C types).
- **`std.ffi` module** (`std/ffi.rn`): a new standard-library module that
  exports the C-type set (`c.Int`, `c.Double`, `c.Ptr`, …) as named types. It is
  imported explicitly (`const c = import "std/ffi.rn"`) and its members are used
  in `extern fn` type positions. This is the one prerequisite that leans on the
  type checker resolving an imported module's exported type in annotation
  position — the same machinery a struct exported from a module already needs.
- **Type checker** (`src/semantic/type-checker.zig`): resolve `c.X` in an
  `extern fn` type position to its ABI class, and give the `cimport` value a
  module-like type whose members are the declared externs, so `m.cos x` type-
  checks (argument arity/types against the C-type list, result type from the
  return C-type). Reuses the module-member machinery.
- **IR** (`src/ir/instruction.zig`, `src/ir/compiler.zig`): a new instruction to
  (a) open a library and (b) call an extern by symbol with a marshalled argument
  set; `compileImportExpr` gets a sibling `compileCImport`.
- **Runtime** (`src/ir/evaluator.zig`, `src/ir/value.zig`, `src/closeable.zig`):
  a `dynlib` value/handle (closeable), the marshalling of `Value` ↔ C, the
  cached `ffi_cif` per extern, and the `ffi_call` itself.
- **Runtime shared** (`src/runtime/`): the C-type ↔ `ffi_type` mapping lives
  here so the compiler and evaluator agree.
- **Build** (`build.zig`, a vendored `libffi` tree): compile `libffi` from
  source and statically link it into `runic` and `runic-lsp`. This carries the
  per-target header/asm selection described in *Calling*.

## Binding generation

Writing an `extern fn` by hand for every export is fine for `libm` but
miserable for a library like SDL2 (hundreds of functions). A generator command
should produce the binding file, so the manual `cimport` block is only ever a
starting point or an override.

### Where the types come from

A callable binding needs each function's *signature* (argument and return
types), and that information only exists in a few places, which are very
unequal:

| Source                              | Names? | Types? | Notes |
|-------------------------------------|--------|--------|-------|
| Dynamic symbol table (`.dynsym`)    | yes    | no     | Plain C strips types from the ABI — you get `pow`, not its signature. |
| DWARF debug info (`-g` builds)      | yes    | yes    | Almost always stripped from distributed `.so`s; unreliable. |
| The C header                        | yes    | yes    | The real source of signatures — but headers are the hard part (preprocessor, macros, typedefs, nested includes, platform `#ifdef`s). |

So a symbol table alone gives a *name list without types* — not enough to call
anything. "Generate from a header" therefore means "parse C," which is the
crux of the tool.

### Dev-time codegen, not a runtime import

The generator is an **offline** command that emits a checked-in `.rn` binding
file — the same model as Rust's `bindgen`, Python's `ctypesgen`, and Zig's own
`@cImport`:

```
runic cbind ./vendor/SDL2/SDL.h --lib libSDL2.so -o sdl2.rn
```

The result is imported normally (`import "./sdl2.rn"`). Keeping it offline
matters:

- parsing C needs a C toolchain/parser present, which the *script runtime*
  should never depend on;
- the runtime FFI stays simple — it only ever consumes `extern fn`, and the
  generator is the only thing that touches C;
- bindings become reviewable, cacheable, and hand-editable (the ambiguous
  cases below *want* a human override).

### Parsing the header

Two realistic backends:

- **`zig translate-c` (recommended first cut).** Runic is already a Zig
  project, so `zig` is in the dev environment. `zig translate-c header.h` runs
  Clang internally and emits *Zig* with every typedef resolved to primitives
  (`pub extern fn SDL_CreateWindow(title: [*c]const u8, x: c_int, ...) ?*SDL_Window`).
  Parsing that regular, one-decl-per-line output and mapping `c_int → c.Int`,
  `f64 → c.Double`, `[*c]const u8 → c.Str`, `?*T → c.Ptr`, … is far easier than
  parsing C, and leans on Clang's correctness without embedding it.
- **libclang (robust version, later).** Link libclang, walk
  `CXCursor_FunctionDecl` cursors, read `clang_getResultType` /
  `clang_getArgType`, map `CXType` kinds to the C-types. This is what `bindgen`
  does — no fragile parsing of generated Zig, full control, but a real
  dependency and more code.

Rolling a bespoke C parser is the trap: headers are too gnarly. Don't.

### What the generator cannot fully do

Auto-generation covers most of a library, not all of it, and the tool should
**report what it skipped** rather than emit something that miscompiles or
crashes at the call:

- **`char*` ambiguity** — string (`c.Str`) or mutable byte buffer (`c.Ptr`)?
  The header does not say. Heuristic: `const char* → c.Str`, `char* → c.Ptr`,
  with a hand override when wrong.
- **struct-by-value** — a struct whose fields are all scalar C types or
  (recursively) such structs *is* generated: its `extern struct` becomes a Runic
  `const T = struct { … }` (a plain `const`, since the parser rejects
  `pub const X = struct {…}`), and its struct-arg / struct-return functions are
  kept. A nested struct field is emitted after the struct it references, in
  dependency order (`Vector2` before `Camera2D`). Only structs reached from a
  kept function are emitted. Because a generated struct is not `pub`, an importer
  that must *construct* one to pass in has to declare a matching struct locally
  (qualified construction `lib.Color{…}` is not yet supported); calling
  struct-*returning* functions and passing their results back needs no local
  declaration.
- **typedef chains** — the generator follows a typedef to whatever it names:
  `Texture2D → Texture → struct_Texture` (a struct), or `ModelAnimPose →
  [*c]Transform` (an opaque `c.Ptr`). This is what keeps a struct with pointer
  fields marshallable — the pointers become `c.Ptr` slots, preserved verbatim
  through a by-value copy (the usual way one gets such a struct is a `Load*`
  return, then passes it back).
- **fixed array fields** — a `float params[4]` becomes a synthesized
  `const Arr_4_f32 = struct { e0: c.Float, … e3: c.Float }` and the field is
  typed as it. A C array and a struct of that many identical fields share one
  layout, so the existing (nested) struct marshalling handles it unchanged —
  the array's elements are reached as `params.e0 … params.e3`.
- **varargs** — skipped (their ABI classification is not a fixed signature),
  reported with a count. These are the only functions a full header leaves out.
- **function pointers** (callbacks) — a callback typedef resolves to `c.Ptr`, so
  a function *taking* one is generated (pass a raw address, or null); *creating*
  a C callback from a Runic function is deferred to the callback phase.
- **`enum`s** — map to their underlying integer type (`c.Int` unless the header
  widens them); the scalar C-type set already covers `size_t`/unsigned widths.
- **compound-literal constants** — a `#define` whose value is a struct literal
  (raylib's named colors, `CLITERAL(Color){ … }`) *is* emitted: translate-c
  lowers it to `mem.zeroInit(CLITERAL(T), .{ … })`, which the generator pairs
  with `T`'s field names into a `pub const NAME = T{ .field = value, … }`. Since
  a struct-*literal* value (unlike a struct *type*) can be `pub`, an importer
  uses it directly (`rl.RAYWHITE`) with no local declaration. Only all-integer
  field lists over a known marshallable struct are converted; anything else is
  left out.

The C preprocessor runs as part of `zig translate-c` (Clang/Aro), so
`#define`d enums, macro-expanded declarations, and conditional `#include`s are
already resolved before the generator sees them — a separate preprocessor pass
adds nothing.

### Symbol-only fallback

`runic cbind --lib libfoo.so` with *no* header can still read `.dynsym` and
emit a stub `cimport` block with every symbol name filled in and `// TODO:`
types. Not callable as-is, but it saves typing the names and shows the surface
area when no header is available.

## Known limitation: direct access / wrappers need a closure-capture fix

A generated binding is used through the raw cimport value —
`rl.raylib.InitWindow 800 600 "…"` (import → cimport member → extern). Two nicer
forms are **blocked by the same underlying bug** and are not generated:

- **Aliases** — `pub const InitWindow = raylib.InitWindow` (bare access to a
  multi-arg extern is treated as a nullary call, and there is no first-class
  extern value to bind); and
- **Wrapper functions** — `pub fn Void InitWindow(…) { raylib.InitWindow … }`.

The root cause is in closure capture, not cbind. A cimport value is *typed* as a
struct (so `m.pow x` dispatches like struct-member access) but its *runtime
value* is a `.closeable` handle. When a function body references a top-level
cimport const, the closure captures it **by slot reference** — the captured
value is the address of the const's slot, not the handle. Struct *member access*
resolves that extra indirection (so `p.x` on a captured struct works), but
`cimport_call`'s library resolution does not, so the extern call gets a bogus
library (`CImportLoadFailed` / a bad dereference). Verified: `const lib2 = lib`
(top-level copy) works and captured scalars — even runtime-computed — work; only
a captured cimport handle breaks. Wrapping the handle in a heap slot did not
help: the capture still stored the slot reference rather than the value.

Fixing this (capturing a cimport/closeable by value, or making `cimport_call`
resolve the captured object the way struct member access does) would unblock both
wrappers and the alias form. It is a contained change but in the closure /
stack-vs-heap addressing model, so it wants a focused pass rather than a rushed
one.

## Phased plan

**Status (2026-09): the MVP is implemented and works end-to-end** — a `cimport`
block loads a C library via `std.DynLib`, resolves each `extern fn`, and a
member call (`m.pow 2.0 10.0`) marshals through a statically-linked `libffi`
and returns the value, in both bound and interpolated positions. Landed on the
`cffi` branch across phases 0a/0b/1 below; scalar args/returns (int widths,
float/double, bool, pointer) and `c.Str` *arguments* are supported. The
`runic cbind` generator (phase 3) is also implemented — it emits a `cimport`
block plus the header's enum values / `#define` constants from a C header via
`zig translate-c`. Remaining: `c.Str` *returns*, structs/varargs at the call
boundary, and the cross-compile vendoring — see below.

0a. **Language prerequisite — mostly already present (re-verified 2026-09).**
   The critical path for `c.Double` is a *qualified type reference in annotation
   position*, and that already works end-to-end: a module exporting a type
   (`const T = struct {…}` or an alias `const T = Int`; `TypeBindingDecl.is_pub`
   defaults to `true`, so no `pub` is needed to export), referenced qualified in
   a parameter/return annotation (`fn Void f(p: s.Point) …`, `n: s.MyInt`),
   parses, type-checks, and runs. The parser builds a multi-segment
   `.identifier` path and the type checker resolves it. What is *not* needed for
   FFI and can stay out of scope: qualified *construction* (`s.Point{…}` in
   expression position — member access currently rejects a type identifier) and
   the cosmetic `pub const X = struct {…}` (only the `struct`-literal RHS after
   `pub const` fails; the plain `const` form already exports). A separate
   pre-existing bug — `expected type Int, actual: Int` when a module fn
   constructs a struct from its own params — is noted but unrelated.
0b. **Vendor + link `libffi`** — get `libffi` compiling from vendored source and
   statically linked into `runic` via `build.zig` for the primary dev target,
   with a trivial `ffi_call` smoke test. The load-bearing prerequisite for the
   calling mechanism; can proceed in parallel with 0a.
1. **MVP** — `std.ffi` module with the C-type set, `cimport` + `extern fn`
   (types resolved from `c.X`), `std.DynLib` loading, `ffi_cif`/`ffi_call`
   dispatch, `c.Str` in/out and `c.Ptr` handles. Enough for `libm`, and for a
   project's own scalar `.so` API.
2. **Ergonomics** — clean load/symbol errors as catchable Runic errors; a
   documented ownership contract; a smoke example (`examples/`) calling `libm`;
   the per-target `libffi` header/asm matrix filled in for the release targets.
3. **`runic cbind` generator — DONE.** `runic cbind <header.h> --lib <lib.so>
   [-o out.rn] [--name binding]` runs `zig translate-c` and emits a `std.ffi`
   import, the header's enum values and integer/string `#define`s as `pub const`s,
   and one `pub`-bound `cimport` block of `extern fn`s (C types mapped to `c.X`).
   Compiler predefined macros are filtered out (via an empty-header baseline
   translate-c), `char*`→`c.Str` / other pointers→`c.Ptr`, and struct-by-value
   / variadic functions are skipped with a reported count. The transform lives
   in `src/ffi/cbind.zig` (pure, unit-tested); the CLI glue is
   `cmd/runic/cbind.zig`. Still open (phase 5): a `.dynsym`-only fallback when
   no header is available, and a libclang backend.
4. **Structs / varargs** — `libffi` already calls them; this phase is the
   marshalling side (struct-layout `ffi_type`s, Runic ↔ struct value mapping).
5. **Later / maybe** — a libclang backend for `cbind`; the pure-Zig trampoline
   fallback (if the `libffi` build matrix proves a burden); Runic → C callbacks;
   a `--safe` gate; typed pointer views over `c.Ptr` memory.

## Open questions / risks

- **Vendoring `libffi` into a Zig build.** `libffi` needs per-target
  configure-generated headers (`fficonfig.h`, `ffitarget.h`) and per-arch
  assembly; committing those for each release target is the main build cost and
  the thing that erodes Zig's otherwise trivial cross-compilation. **A
  `build.zig` port already exists** — a "friendly fork" of `libffi` whose source
  is identical to upstream, adding only a Zig build script and a `build.zig.zon`
  so it is consumable via `zig fetch --save` and handles the per-target header
  matrix itself. That would remove most of the phase-0 work, if it is current.
  *Caveat (verified 2026-09):* the two repo URLs surfaced by search
  (`alexrp/libffi`, `vezel-dev/libffi`) both 404 now, so the fork's current home
  and its minimum Zig version need confirming — check the Zig package index or
  `zig fetch` a candidate before committing to it; hand-vendoring stays the
  bounded fallback.
- **The pure-Zig trampoline fallback** (see *Calling*) stays a documented
  escape hatch if the `libffi` build matrix becomes a burden; its own risk is
  that the GP/SSE class split only holds on SysV AMD64 / AArch64 and would need
  a per-ABI implementation.
- **Module-exported types in annotation position — RESOLVED (re-verified
  2026-09): already works.** A module type export (`const T = struct`/alias,
  `is_pub` default `true`) referenced qualified in an annotation (`p: s.Point`,
  `n: s.MyInt`) parses, type-checks, and runs end-to-end. Only *construction*
  (`s.Point{…}`) and the `pub const X = struct` literal form remain unsupported,
  and neither is on the FFI path. So `std.ffi` does *not* need new type-system
  machinery for the `c.Double`-in-`extern-fn` surface.
- **How `std.ffi` represents the C types — RESOLVED (option a, virtualized).**
  A C type is not an ordinary Runic type, and four names (`Int`/`Float`/`Bool`/
  `Void`) collide with primitives and cannot be declared at all — so `std.ffi`
  declares *nothing*: it is a documented marker module, and the type checker
  recognizes a qualified `c.<Name>` in an `extern fn` type position, mapping the
  name to the Runic type it marshals from/to (`c.Double`→`Float`, integer widths
  →`Int`, `c.Str`→`String`, `c.Ptr`→`Int` as an opaque address for the MVP,
  `c.Void`→`Void`). Unknown C types and an un-imported namespace are reported.
  The original `c.X` names stay in the `ExternFn` AST for marshalling.
- **`cimport` member-call resolution — deferred to the IR step.** A `cimport`
  value is typed as a struct of its externs (function-typed fields), but a
  member *call* `m.pow 2.0 10.0` currently resolves through Runic's permissive
  UFCS/command path rather than strictly against those fields (module values get
  special member-call handling that a struct type does not; and Runic checks no
  argument types anywhere). Making `m.pow`'s return type flow — and rejecting an
  unknown extern — needs dedicated cimport member handling, which is entangled
  with the IR/eval stage and can only be verified end-to-end once calls run.
- **`c.SizeT` / pointer-width types.** These resolve per target; `std.ffi` must
  expose them with the target's actual width rather than a fixed one.
- **Threading/reentrancy** — Runic already runs pipeline stages on threads;
  calling non-thread-safe C from multiple stages is a caller hazard to document.
- **Windows** (`.dll`, stdcall vs cdecl) is out of scope for the MVP but the
  `DynLib` layer already abstracts loading.
