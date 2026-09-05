# C FFI: importing dynamic libraries

## Introduction

Runic scripts should be able to load a C dynamic library (`.so`/`.dylib`/`.dll`)
and call functions in it. Zig's C interop makes the pieces available — runtime
dynamic loading via `std.DynLib`, and the C calling convention via
`callconv(.c)` — but there is one real obstacle that shapes the whole design,
covered next.

This is a scoping document, not an implementation plan. It sketches the syntax,
the type model, the calling mechanism, the integration points, and a phased
rollout, and flags the open questions.

## Goals

- `import` a system or local C library and call scalar functions from it
  (`libm`'s `cos`/`pow`, a project's own `.so`, etc.).
- A declared, type-checked interface so calls are checked like any other Runic
  call and so the runtime knows how to marshal arguments.
- No *runtime* third-party dependency: the C-calling backend (`libffi`) is
  vendored and statically linked, so it ships inside the `runic` binary and
  end users install nothing.

## Non-goals (at least initially)

- Passing/returning C structs *by value*, `union`s, or variadic functions.
  `libffi` can *call* these, so the limit is the marshalling side (describing a
  struct's layout as an `ffi_type` and mapping it to/from a Runic value), which
  is deferred rather than blocked by the calling mechanism.
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
functions it will call. A dedicated form keeps this separate from the Runic
module import and from the stream-typed `fn StdinType name(...) StdoutType`
signature (C functions have no stdin/stdout stream notion):

```rn
const m = cimport "libm.so.6" {
    extern fn cos(x: CDouble) CDouble
    extern fn pow(base: CDouble, exp: CDouble) CDouble
}

echo "${m.cos 0.0}"        // 1
echo "${m.pow 2.0 10.0}"   // 1024
```

The result binds like a module value: `m.cos`, `m.pow` are members resolved the
same way module members are today (`compileMember` /
`resolveMemberFieldSpan`). Each `extern fn` records: the symbol name, the
ordered parameter C-types, and the return C-type.

The declared C types are a small, explicit set so the ABI is unambiguous
(Runic `Int` is 64-bit, C `int` is 32-bit — conflating them is a real bug
source, so the interface names the C width directly):

| C type    | passed as                | from / to Runic value        |
|-----------|--------------------------|------------------------------|
| `CInt`    | 32-bit int (GP)          | `Int` (i64, truncated/checked) |
| `CLong`   | 64-bit int (GP)          | `Int`                        |
| `CFloat`  | 32-bit float (SSE)       | `Float` (f64)                |
| `CDouble` | 64-bit float (SSE)       | `Float`                      |
| `CBool`   | int 0/1 (GP)             | `Bool`                       |
| `CStr`    | `[*:0]const u8` (GP)     | `String`                     |
| `CPtr`    | pointer (GP)             | opaque handle (`addr`)       |
| `CVoid`   | void return              | `Void`                       |

### 3. Marshalling

- **`CStr` in:** Runic strings are `addr+len` byte slices in arena memory
  (`Value.Slice`), not null-terminated. Marshalling allocates a
  null-terminated copy for the call and frees it after (the callee must not
  retain it — document as a rule).
- **`CStr` out:** a returned `char*` is copied into a fresh Runic `String`.
  Runic never `free`s it (C owns it); functions returning heap strings the
  caller must free are simply not safe to expose this way in the MVP.
- **`CPtr`:** modeled as an opaque `addr` value — passable back into other
  `extern fn`s but not dereferenceable from Runic. Enables handle-style APIs
  (`open` → handle → `use handle` → `close handle`).
- **Numbers/bools:** direct, with `CInt` range-checked against i64.

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
  footgun (no guessed signatures) but does not remove it. `cimport` should be
  understood as an unsafe escape hatch, and probably gated the way other
  privileged operations are (e.g. off in a future `--safe`/sandboxed mode).
- A `dlopen`/`dlsym` failure (missing library or symbol) is a clean, catchable
  runtime error, distinct from a crash inside a call.
- Ownership across the boundary is manual and, in the MVP, deliberately
  restricted (no Runic-side `free` of C returns; strings copied in/out). Richer
  ownership (e.g. handing struct memory across) comes with struct support.

## Integration points

- **Lexer/parser** (`src/frontend/`): a `cimport` keyword + an `extern fn`
  declaration block (a new AST node — it is *not* a normal `FunctionDecl`, since
  there is no body and the parameter types are C types).
- **Type checker** (`src/semantic/type-checker.zig`): give the `cimport` value a
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
  Parsing that regular, one-decl-per-line output and mapping `c_int → CInt`,
  `f64 → CDouble`, `[*c]const u8 → CStr`, `?*T → CPtr`, … is far easier than
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

- **`char*` ambiguity** — string (`CStr`) or mutable byte buffer (`CPtr`)? The
  header does not say. Heuristic: `const char* → CStr`, `char* → CPtr`, with a
  hand override when wrong.
- **struct-by-value / varargs** — not callable until the libffi phase, so they
  are skipped with a commented stub and a count (`// skipped 34: struct-by-value / variadic`).
- **function pointers** (callbacks) — deferred to the callback phase.
- **`size_t`, `enum`s, unsigned variants** — need the fuller C-type set
  (`CSizeT`, `CUInt`, …) noted in the open questions.

### Symbol-only fallback

`runic cbind --lib libfoo.so` with *no* header can still read `.dynsym` and
emit a stub `cimport` block with every symbol name filled in and `// TODO:`
types. Not callable as-is, but it saves typing the names and shows the surface
area when no header is available.

## Phased plan

0. **Vendor + link `libffi`** — get `libffi` compiling from vendored source and
   statically linked into `runic` via `build.zig` for the primary dev target,
   with a trivial `ffi_call` smoke test. This is the load-bearing prerequisite;
   the calling mechanism depends on it.
1. **MVP** — `cimport` + `extern fn`, `std.DynLib` loading, the scalar C-type
   set, `ffi_cif`/`ffi_call` dispatch, `CStr` in/out and `CPtr` handles. Enough
   for `libm`, and for a project's own scalar `.so` API.
2. **Ergonomics** — clean load/symbol errors as catchable Runic errors; a
   documented ownership contract; a smoke example (`examples/`) calling `libm`;
   the per-target `libffi` header/asm matrix filled in for the release targets.
3. **`runic cbind` generator** — a dev-time command that emits a `.rn` binding
   file from a C header (via `zig translate-c`), plus the `.dynsym`-only
   fallback. Removes the hand-written `extern fn` tedium for large libraries.
4. **Structs / varargs** — `libffi` already calls them; this phase is the
   marshalling side (struct-layout `ffi_type`s, Runic ↔ struct value mapping).
5. **Later / maybe** — a libclang backend for `cbind`; the pure-Zig trampoline
   fallback (if the `libffi` build matrix proves a burden); Runic → C callbacks;
   a `--safe` gate; typed pointer views over `CPtr` memory.

## Open questions / risks

- **Vendoring `libffi` into a Zig build.** `libffi` needs per-target
  configure-generated headers (`fficonfig.h`, `ffitarget.h`) and per-arch
  assembly; committing those for each release target is the main build cost and
  the thing that erodes Zig's otherwise trivial cross-compilation. **First check
  whether a maintained Zig package/`build.zig` port of `libffi` already exists**
  (e.g. on the Zig package index) before hand-vendoring — it may remove most of
  this work.
- **The pure-Zig trampoline fallback** (see *Calling*) stays a documented
  escape hatch if the `libffi` build matrix becomes a burden; its own risk is
  that the GP/SSE class split only holds on SysV AMD64 / AArch64 and would need
  a per-ABI implementation.
- **`CInt` vs `CLong` width** and signed/unsigned variants — the table above is
  a starting point; the full set (`CUInt`, `CShort`, `CSizeT`, …) needs deciding.
- **Threading/reentrancy** — Runic already runs pipeline stages on threads;
  calling non-thread-safe C from multiple stages is a caller hazard to document.
- **Windows** (`.dll`, stdcall vs cdecl) is out of scope for the MVP but the
  `DynLib` layer already abstracts loading.
- **Whether `cimport` is the right surface** vs. reusing `import` with a
  modifier, and whether the extern block should live inline or in a separate
  binding file (a "Runic header" for a C lib).
