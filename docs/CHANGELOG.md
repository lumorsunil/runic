# Changelog

All notable changes to Runic will be documented here.

Version numbers follow [Semantic Versioning](https://semver.org/): `MAJOR.MINOR.PATCH`

- **PATCH** — bug fixes and internal improvements; no breaking changes
- **MINOR** — new features or minor breaking changes (e.g. syntax adjustments, changed flag names)
- **MAJOR** — significant new features or breaking changes to the language, runtime, or CLI

---

## [Unreleased]

### Added

- **The empty literal `.{}` is an empty struct.** With no elements it has no
  element type, so it is a value you can pass around but not index, iterate, or
  `.push` — an appendable empty array must name its element type
  (`var xs: []Int = .{}`), and the empty struct then coerces to the empty array.
  An empty `.{}` still counts as an empty array in an array context — a `[]T`
  annotation, a struct field's declared array type, or a concat operand
  (`.{} + xs`). Indexing an unannotated empty literal reports a directed error
  pointing at the missing annotation. (Previously an unannotated `.{}` was a
  permissive "any" array.)
- **Tuples.** A `.{ … }` literal whose elements have different types is now a
  *tuple* — an ordered, per-position-typed collection — rather than a
  homogenized array; a literal whose elements share one type is still an array
  `[]T`. Tuples share the array runtime representation (index, `for`, `.len()`,
  `.push`, concatenation all work), so the distinction is static: a tuple keeps
  each element's type. That fixes heterogeneous **destructuring from a variable**
  — `var m = .{ A{…}, B{…} }; const x, y = m` binds `x` as an `A` and `y` as a
  `B`, where before both collapsed to one element type and a struct element lost
  its fields. Because an array is a single type, forcing a heterogeneous tuple
  into an array annotation (`const xs: []Int = .{ 1, "two" }`) is a compile
  error; a homogeneous literal coerces to `[]T` as before. A tuple type is
  written `struct { T0, T1, … }` (a struct body with positional types and no
  field names) and can annotate a binding, parameter, or return type; it is
  checked position by position, so `const t: struct { Int, String } = .{ 1, 2 }`
  errors. Indexing a tuple with a *constant*
  index has that position's type (`t[0]` is `Int`, `t[1]` is `String`), so a
  method/field on the result resolves; a runtime index stays permissive.
- **Inferred struct literals.** When the struct type is known from context, a
  struct value can be written `.{ .field = value, … }` without repeating the type
  name — `const v: Vector = .{ .x = 3, .y = 5 }` instead of `Vector{ … }`. The
  type is taken from the context: a binding's annotation; the matching parameter
  type of a call argument (`moveEntity e .{ .x = 3, .y = 5 }`, including UFCS
  method calls where the receiver fills the first parameter); a function's
  declared return type (`yield .{ … }`); a struct field's declared type in a
  construction (`Line{ .from = .{ … } }`); and an array's element type
  (`const path: []Vector = .{ .{ … }, .{ … } }`), which nests. The literal is
  validated against that type exactly as the named form is. An anonymous struct
  literal with no type to infer from is a compile error asking for an annotation;
  the array-literal form `.{ e0, e1 }` is unchanged.
- **Tuple and record destructuring in bindings.** A binding target can now be a
  tuple pattern — `const a, b = .{ 1, 2 }` binds the positional elements of an
  array/tuple — or a record pattern — `const { x, y } = point` binds a struct's
  fields to same-named locals, with `{ x: local }` to rebind a field to a
  different name and a subset of fields allowed. `_` discards an element;
  `const`/`var` sets mutability for all parts. A record field the struct doesn't
  have is a "struct has no field" error. Patterns nest arbitrarily — a record or
  tuple can appear as a tuple element or a record-field rebinding (a nested tuple
  is parenthesized, `(a, b)`, since a bare comma separates the outer elements),
  and a tuple over an array literal keeps each element's type, so a heterogeneous
  `const s, n, p = .{ "hi", 2, point }` destructures each part at its own type.
- **Array concatenation with `+`.** `a + b` on two arrays produces a new array
  holding `a`'s elements followed by `b`'s (a fresh copy; the operands are
  unchanged). Works for any element type, with empty operands, and composes with
  indexing and iteration. Scalar `+` is unaffected. Previously this errored with
  `UnsupportedBinaryExpression`; arrays could only grow via `.push`.
- **`break` and `continue`.** Loop control statements now exist: `break` exits
  the innermost enclosing loop and `continue` skips to its next iteration. They
  work in every loop form — a counted range `for`, an array/multi-source `for`, a
  `for (&0)` stream, and `while` — bind to the innermost loop when nested, and
  clean up body-local bindings on the jump. Using either outside a loop is a
  compile error. A range loop whose body uses `break`/`continue` runs on the
  regular (non-atomic) path; loops without them keep the fast `counted_loop`
  lowering.
- **String indexing `s[i]`.** A string can be indexed by a single position,
  yielding a one-character `String` (lowered to a one-char slice `s[i .. i+1]`),
  complementing string slicing. Out-of-range and negative indices clamp to the
  empty string like a slice. Previously `s[i]` failed with a bare
  `UnsupportedBinaryOperation`.
- **`c.Str` return values.** A `cimport` extern declared to return `c.Str` (a C
  `char*`) now yields a Runic `String` — the borrowed C string is copied into
  Runic-owned memory, so it composes like any string (`.len`, interpolation,
  builtins). A NULL return (e.g. `getenv` of an unset variable) becomes the
  empty string. This closes the last scalar-marshalling gap of the C FFI MVP.
- **Language server — deeper analysis and editing features.** `runic-lsp` gained
  a batch of new capabilities on top of the [0.8.0] surface (see `docs/lsp.md`):
  - _Completion & hover:_ error sets complete and hover with their variants
    (`Variant` / `Variant: PayloadType`). Hover and go-to-definition now follow
    a full member chain — a nested access like `a.b.c` resolves `c` against the
    type of `a.b`, descending named field types to their struct — and resolve
    struct-literal field names (the `.x` in `Vector{ .x = … }`, including nested
    literals), which are not `object.member` accesses.
  - _Navigation:_ call hierarchy for top-level functions — prepare, outgoing
    calls (same-file functions and imported-module `m.f`), and incoming calls
    including **cross-file** callers (a `m.f` access in an importing file,
    resolved through the workspace index).
  - _Symbols:_ the document outline now surfaces the names introduced by a
    destructuring binding (`const a, b = …`, `const { x, y } = …`), recursing
    through nested patterns.
  - _Editing:_ signature help (the callee's parameter list with the active
    argument highlighted); code actions — add an inferred type annotation, remove
    an unused binding (individually or all at once as a source action), wrap a
    bare undeclared uppercase type as `|T|`, and capitalize a lowercase type name
    (the last two driven off diagnostics); a document formatter that re-indents by
    structural nesting depth while preserving each line's interior (command-
    argument spacing is significant); and semantic tokens
    (`textDocument/semanticTokens/full`) classifying keywords, types, variables,
    numbers, strings, and operators, refined from the AST so function
    declarations and call sites are `function`, parameters are `parameter`, and
    declarations carry `declaration`/`readonly` modifiers.

### Changed

- **Generic type parameters must be introduced with `|T|`; a bare unknown
  uppercase type name is an error.** Previously an uppercase type name that
  wasn't declared was silently treated as an implicit generic type variable, so
  a typo like `Recangle` slipped through. Now a type variable is introduced only
  by an explicit `|T|` capture (`fn Void first(xs: []|T|) |T|`); a bare `T` after
  it references the same variable, and a bare uppercase name with no such capture
  is an undeclared-type error with a hint pointing at the `|T|` form. **Breaking**
  for signatures written with bare implicit generics — add `|…|` at each type
  variable's first occurrence (the standard library was migrated). User-defined
  generic type *constructors* (`const Box(T) = struct { value: T }`) are
  unchanged — their `(T)` parameters are now properly scoped when resolving the
  body (so a typo in a constructor body is also caught). *(Longer term, the
  `Box(T)` constructor form is expected to be superseded by comptime functions
  returning types, Zig-style — see `docs/plan.md`.)*
- **`;` after a binding is always a plain statement separator.** A binding whose
  initializer was a command/pipeline used to "absorb" a following `;`-separated
  statement into the bound value as a command sequence (`const b = cmd1; cmd2`
  captured both). That special-cased `;` — the one place it was not equivalent to
  a newline — and surprised the common `const r = cmd; echo "${r}"` by pulling
  `r` out of scope. Now `;` never folds the next statement into a binding: the
  initializer is exactly the expression right of `=`. Command *sequences* are
  still captured via `&&`/`||` (single expressions) or a `$( … )` subshell.
- **Clearer `for`-loop capture-count diagnostic.** A `for` loop with a capture
  count that doesn't match its sources (e.g. `for (items) |v, i|`) now reports a
  located error that points at the index idiom — `for (items, 0..) |item, i|` —
  instead of surfacing the bare `ForCapturesMustMatchSources` enum, and no longer
  emits a cascading second error for the loop body.

### Fixed

- **An optional struct field stays optional when given a non-null value.**
  `struct { x: ?Int }` constructed as `P{ .x = 5 }` kept the field optional only
  when the value was `null`; a bare non-null value re-typed the field to the
  value's concrete type (`Int`), so `p.x orelse …` failed at compile time ("left
  side of orelse must be an optional"). A struct-literal field now keeps an
  optional/promise declared type (the value is coerced into it); generic fields
  still specialize to their value's type.
- **An array literal as a function/command argument.** `f .{ 1, 2 }` now passes
  the array literal as an argument — previously a bare `.{` ended argument
  parsing, so it was misparsed as a nullary call `f` plus a separate dangling
  array (and failed outright inside string interpolation). Only the parenthesized
  form `f (.{ 1, 2 })` worked before. This also fixes calling stdlib
  higher-order helpers with a literal, e.g. `std.list.map .{ 1, 2, 3 } dbl`.
- **Member access, indexing, and slicing on a call result.** Reading a field
  (`(f x).field`, `recv.method.field`, a chain like `v.inc.inc.x`), indexing
  (`(mk)[1]`), or slicing (`(mk)[1..3]`) directly off a call result failed with
  "member access is only supported for struct types in IR" /
  "UnsupportedBinaryOperation". The receiver is now value-captured (not forked)
  in each case, so it sees the produced value instead of a thread handle. Plain
  struct-field chains (`b.a.n`), string builtins (`s.trim.upper`), and array/
  string variables are unaffected. Binding first already worked.
- **Iterating a call result.** `for (mk)` (a function returning an array) or
  `for (v.items)` (a UFCS method call) now iterates the produced array instead of
  erroring "for loops with source type 'call' not yet implemented" — the source
  is value-captured like other call-result positions. Binding first already
  worked.
- **A non-boolean `if`/`while` condition no longer crashes.** A condition that
  resolves to `void` — e.g. `if ((f) > 3)`, where the `>` binds as an output
  redirect of the command `(f)` rather than a comparison — panicked on a union
  access (`exit_code` while `void` active). A void/non-boolean condition is now
  treated as false. (To compare a function's result, bind it first:
  `const r = f; if (r > 3)`.)
- **Duplicate struct field names are rejected.** A struct type declaration with
  a repeated field (`struct { x: Int, x: Int }`) was silently accepted — struct
  types weren't validated at all. They now are: a duplicate field name is a
  located error, and each field's type is validated. (An inline struct type in a
  parameter position is still unchecked here.)
- **An un-annotated function parameter reports a clean diagnostic.** A parameter
  with no type annotation (`fn Void f(x) Void`) aborted the entire type-check
  run with an uncaught `error.TypeNotFound` ("Type checker failed to run"). It
  now produces a located diagnostic naming the parameter — one per bad param —
  and checking continues, so other errors are still reported.
- **A negative array index no longer crashes the interpreter.** Reading an
  index computed to a negative value (e.g. `a[0 - 1]`) panicked with "integer
  does not fit in destination type" (a `@intCast` of the negative pointer offset
  to `usize`). The offset arithmetic is now signed-aware and saturating, so a
  negative or otherwise out-of-range index degrades gracefully — an unspecified
  value or a caught failed dereference — the same as a positive out-of-bounds
  read. (Indexing is still not bounds-checked; that remains future work.)
- **`;` after a command-producing binding.** A binding whose initializer is a
  command/pipeline (`const n = echo "9" | parseInt`) no longer swallows a
  following statement that cannot be part of a command sequence — a `yield` or
  `exit`, a closing `}`, or a statement that uses the just-bound value. Such a
  `;` now separates statements (as a newline does): `const n = pipe; yield n`
  parses, and `const n = pipe; echo "${n}"` binds `n` before the `echo` reads
  it. Sequencing two commands under one binding (`const s = cmd1; cmd2`) still
  works.
- **Constructing a struct field from another struct's field.** A struct literal
  whose field value is a member access — `V{ .x = e.x }` — now type-checks. The
  member access surfaced the field's raw declared type (an unresolved identifier
  such as `Int`, or an alias like `c.Int`), which was compared unresolved
  against the declared field type and spuriously rejected with "expected type
  Int, actual: Int" (or "actual: c.Int"). The value's type is now resolved
  before the comparison.
- **Calling a C extern through a captured `cimport` value.** A function that
  references a top-level `cimport` const and calls its externs now works even
  when the function forks (a pipeline/loop consumer, a threaded body) — no local
  re-import of the library is needed. A cimport value is an immutable
  `.closeable` handle; it is now captured by value instead of by the slot
  reference used for aliasable structs, which `cimport_call` could not resolve
  as a library (`CImportLoadFailed`).
- **Language server resilience.** A batch of crashes found by fuzzing the server
  are fixed, so a document being edited can no longer take it down: an empty or
  non-ASCII document (an out-of-bounds read and a lexer error escaping a scan),
  and any single request handler that errors is now contained and logged instead
  of killing the server. The lexer itself no longer panics on a string that runs
  to end-of-input (now a clean unterminated-string diagnostic) or underflows its
  delimiter counters on a stray `)`/`]`/`}` — fixes that harden the compiler too.
  A memory leak in the diagnostics list was also closed.

## [0.10.1] - 2026-09-14

### Fixed

- **Forward-referenced consumer closures.** A top-level function used as a
  pipeline or `for`-loop consumer *before* its own declaration
  (`fn run() { items | handle }` with `handle` declared later) now captures its
  closed-over globals correctly. The fork site previously read the callee's
  closure captures before the callee's body was compiled, so the consumer ran
  with an uninitialized closure slot and dereferenced garbage at runtime
  (`Could not dereference address 0x…`, most visibly for a struct-valued
  global). The callee's body is now compiled on demand before its captures are
  read.
- **Function-call arguments to C externs.** Passing a Runic function call
  straight to a `cimport` extern — including a function that returns a by-value
  struct, e.g. `DrawRectangleRec (entityRec e) e.color` — now value-captures the
  returned value instead of forking the call and handing the FFI marshaller a
  thread handle (which failed with `CImportUnsupportedType`).

## [0.10.0] - 2026-09-13

Compute-heavy, in-process Runic is now dramatically faster. A synchrony (effect)
analysis identifies code that needs no concurrency and lowers it to a fork-free
"sync" execution path, so function calls, recursion, and tight loops run at flat
memory and near-interpreter-ceiling speed — while the plain interpreter stays
the zero-startup default and every script keeps its existing behavior. Several
latent correctness bugs surfaced and were fixed along the way, and a benchmark
regression guard now protects the fast paths. Native compilation remains
deferred.

All performance changes are transparent: output and semantics are unchanged; the
same program simply runs faster and with flat memory where it used to fork.

### Performance

- **Fork-free `sync` calls.** A new synchrony analysis (`src/semantic/effects.zig`)
  classifies each function as `sync` (pure compute, a single yield, no pipeline /
  command / generator / indirect call) or `threaded`. A `sync` call is lowered to
  a direct `call`/`ret` convention with a per-thread return stack instead of
  forking a green thread plus a pipe and a wait — eliminating the dominant
  per-call time and memory cost. Covers nullary and parameterized calls, calls in
  `yield`-value and command-argument positions, and **UFCS method calls**
  (`p.method args`), so a struct-parameter method no longer forks per call.
- **Atomic loops and recursion.** A counted loop (`for (0..n)`) runs its whole body
  in one native loop without yielding to the scheduler — now including `const`/
  computed-binding bodies and bodies with control flow (`if`/`else`/`match`), and
  following a `sync` call into its body and back. A recursive `sync` call runs its
  entire subtree atomically the same way, so recursion (e.g. `fib`) is fork-free
  *and* no longer pays a per-instruction scheduler round-trip.
- **Optional and error-union returns on the sync path.** A `sync` function
  returning `?T`, `E!T`, or an inferred `!T` stays fork-free: the value rides back
  in place with its discriminant intact, and `catch`, `try` propagation, `orelse`,
  and `match` read it directly.
- **In-place array growth.** A linear buffer grown with `xs = xs.push e` now grows
  in place (amortized O(1)) instead of copying the whole array each push
  (quadratic), at top level as well as inside functions.
- **Runtime memory and latency fixes.** Runtime pipes are created from a freeing
  allocator; the stdin stream-forwarding thread no longer busy-polls after EOF (it
  had dominated the syscall cost of compute loops); and the execution tracer no
  longer allocates on every stream forward when tracing is off.

Representative measurements (ReleaseFast): a scalar/struct function call in a
200k-iteration loop drops from ~0.6–9.9 s and 540–610 MB to ~0.3–0.4 s and ~5 MB;
recursive `fib 25` drops from ~27 s to ~0.24 s; a `const`-body loop at 4M
iterations runs in ~0.9 s at flat ~5 MB.

### Added

- **Benchmark regression guard.** `scripts/bench_guard.py` (new `bench_guard` CI
  stage) runs the `tests/benchmarks/` scripts against a ReleaseFast build and
  fails if an optimized fast path regresses — measuring peak RSS per process and
  wall time against deliberately loose, order-of-magnitude budgets, and checking
  output so a "fast but wrong" regression also fails. See `CLAUDE.md`.

### Fixed

- **Function-call operands were not captured in comparisons.** `a < b` where `a`
  and `b` are function calls compared raw thread handles instead of the yielded
  values, giving the wrong answer (`const c = a < b` for `a=3, b=5` returned
  false); comparison operands are now captured like arithmetic operands.
- **Function-call operands were not captured in logical `&&` / `||`.** Two
  Bool-returning calls (`yes && yes`) concatenated their outputs into the capture
  pipe instead of combining their boolean values; Bool-valued operands now lower
  through the value path.
- **A forking value expression passed as a command argument** (`echo (build)`
  where `build` forks) raced the producer and echoed nothing; such arguments are
  now value-captured.
- **A self-recursive UFCS method returned empty output** on the fork path; it now
  lowers fork-free and returns correctly.
- Compound assignment to a struct field; two LSP crashes (a use-after-free freeing
  completion matches, and a crash on go-to-definition of a virtual/embedded span);
  and type-checker fixes for function-parameter and module-qualified type
  resolution.

## [0.9.0] - 2026-09-06

A C foreign-function interface: call C functions in a shared library directly,
pass and return C structs by value, and generate bindings from a header.

### Added

- **C FFI via `cimport`** — a `cimport "libfoo.so" { extern fn … }` block loads
  a shared library through `libffi` (linked into the interpreter) and declares
  the C functions to call; the bound value is module-like, its members the
  externs (`m.pow 2.0 10.0`). C types come from the `std/ffi.rn` marker module,
  written qualified so they never clash with Runic's primitives: `c.Int`,
  `c.UInt`, `c.Long`, `c.ULong`, `c.Short`, `c.UShort`, `c.Char`, `c.SizeT`,
  `c.Float`, `c.Double`, `c.Bool`, `c.Str`, `c.Ptr`, and `c.Void`. Narrow
  integers are range-mapped to `Int`, a `c.Str` argument is marshalled to a
  null-terminated copy, and a `c.Ptr` is an opaque address. Always available,
  no flag; the library is `dlclose`d at script exit. See `docs/features.md` and
  `future/c-ffi.md`.
- **C structs by value** — a C struct passed or returned by value is declared as
  an ordinary Runic struct whose fields are all `c.X` types, or, recursively,
  other such structs (nested structs like raylib's `Camera2D`). The evaluator
  builds the struct's `libffi` type and marshals each field at the ABI's
  computed offset, in both directions; a returned struct composes normally
  (field access, passing it back).
- **`runic cbind`** — generates a Runic binding from a C header
  (`runic cbind <header.h> --lib <lib> --name <binding>`, via `zig translate-c`):
  the `cimport` block with every callable `extern fn`, the by-value struct types
  they use (emitted in dependency order, following typedef chains), enum values
  and integer/string `#define`s as `const`s, and struct-valued `#define`s (such
  as raylib's named colours) as struct-literal constants. Fixed C arrays inside
  a struct become a struct of that many fields; function pointers become
  `c.Ptr`. Only variadic functions are left out.
- **LSP support for `cimport`** — member completion lists a cimport's externs
  with their C signatures, hover shows an extern's signature, go-to-definition
  jumps to the `extern fn` declaration, and the document outline shows a cimport
  as a module with its externs nested. `c.` completes the `std.ffi` C types, and
  the `cimport`/`extern` keywords complete with snippets.
- **Qualified struct construction** — a struct type from an imported module can
  be constructed directly with `m.Vector3{ .x = 0, … }`, including nested inside
  another struct literal. Struct types can't be declared `pub` (the parser
  rejects `pub const X = struct {…}`), so a module's struct type is reachable
  without a visibility marker; `m.Vector3` also resolves as a type and as a
  type-name reference in value position.
- **Module-qualified type annotations** — a qualified type like `m.Vector3` used
  as a field, parameter, or return type now resolves to the module's actual
  type rather than to the module `m`, so a function `fn f(pos: m.Vector3) …`
  type-checks and passes the value through correctly.

### Fixed

- **Unresolved function-parameter types** — a function parameter's declared type
  was stored unresolved, so a primitive annotation like `Int` (parsed as a bare
  identifier) surfaced as an `.identifier` rather than the resolved primitive.
  Using a parameter where its type is checked — a struct-literal field value
  (`Vector{ .x = x }`), an assignment — spuriously failed with
  `expected type Int, actual: Int`. Parameter types are now resolved at
  declaration, like the stdin and function types already were.
- **Injected globals leaked into module members** — a module value's type (and
  so member completion on an imported module, `m.<TAB>`) included the builtins
  and primitive types the type checker injects into every module scope
  (`parseInt`, `parseFloat`, `Int`, …), because they are declared into the same
  scope as the file's own exports. Such globals are now marked and excluded from
  a module value's members.
- **LSP crash on go-to-definition** — jumping to the definition of a member
  whose declaration lives in an embedded module (a virtual path like `:std/str`
  with no on-disk file), or in a file not resolvable from the server's working
  directory, propagated a `FileNotFound` out of the request handler and crashed
  the server. Such a request now returns an empty result.
- **LSP crash on member completion** — completing a member (`m.<partial>`),
  especially on a large imported module, could segfault the language server: an
  owned completion match wrote to its heap slot *after* freeing it
  (`ptr.* = undefined` following `destroy`), a use-after-free that crashed once
  that page was reused or unmapped.
- **Compound assignment to a struct field** — `a.x += 1` (and `a.x = a.x + 1`)
  crashed the type checker with `UnresolvedTypeLiteral` (or reported a spurious
  `expected type Int, actual: Int`). A field's declared type surfaces through
  member access and arithmetic as a bare type name; the assignment check now
  resolves it, so compound assignment to a struct field — including a nested
  one (`b.inner.n += 1`) — type-checks. A genuine type mismatch is still caught.

## [0.8.1] - 2026-09-05

Bug fixes and stabilization after the 0.8.0 language-server build-out. The
focus is LSP protocol correctness and resilience.

### Fixed

- **Rename / code-action edits crashed clients** — a `WorkspaceEdit`'s
  `documentChanges` entries were serialized as `{"textDocumentEdit": {…}}`
  instead of the bare `TextDocumentEdit` the protocol requires, so applying a
  rename or the "add type annotation" quick fix failed in editors (Neovim
  reported `attempt to index local 'text_document' (a nil value)`).
- **More LSP protocol encodings** — `InsertTextMode` and `CompletionItemTag`
  now serialize as their numeric codes, and `ProgressToken` as a bare scalar,
  completing the numeric-code pass started in 0.8.0. Responses are now emitted
  in JSON-RPC result-XOR-error form (no stray `"error": null` beside a result),
  and the server's debug log now matches the exact bytes sent on the wire.
- **Crash on an out-of-bounds position** — a hover/definition/rename/… request
  at a line past end-of-file, or a character past the end of a line, ran the
  position scan off the end of the buffer and crashed the server. Such a
  position now resolves to nothing and the request returns an empty result.
- **A malformed request killed the session** — an unparseable request body
  propagated an error out of the main loop and terminated the server, dropping
  every request after it. A malformed message is now logged and dropped, and
  the session continues.

### Changed

- **Workspace indexing is lazy** — the full `.rn` workspace scan now runs on the
  first workspace-wide request (symbol search, references, rename, cross-file
  definition) for a client-provided root, never at `initialize`. A slow or
  malformed file can no longer block startup or the per-file features (hover,
  completion, diagnostics).
- **Tree-sitter grammar refreshed** for the current syntax (`comptime`,
  `yield`, `is`, hex/octal/binary integer literals, `||=`/`&&=`), and the
  parser is generated at ABI 14 for editor compatibility (e.g. Neovim 0.10).

### Internal

- The LSP protocol test suite was expanded substantially: response wire-shape
  assertions (numeric enum codes, unwrapped `documentChanges`, result-XOR-error
  envelopes), exact 0↔1-indexed coordinate encoding for rename/references/
  prepare-rename, robustness cases (out-of-bounds positions, unknown methods,
  duplicate `initialize`, malformed bodies, never-opened documents), and the
  diagnostics-clear-on-fix lifecycle. `zig build test` is now 126 tests.

## [0.8.0] - 2026-09-04

### Added

- **Exponent and bit shifts** — `**`, `<<`, `>>`. Integer operands stay `Int`
  (`**` saturates on overflow; shifting by 64+ clears; `>>` of a negative is
  arithmetic); a `Float` operand or negative exponent widens `**` to `Float`.
  `>>` is overloaded with append-redirect — a command on the left appends, a
  value shifts (resolved in the IR, like `>`).
- **Bitwise operations as `Int` methods** — `a.band b`, `a.bor b`, `a.bxor b`,
  `a.bnot` (the `&`/`|`/`^` symbols are taken by background/fd, pipe, and the
  promise prefix). Method calls work on integer literals too (`6.band 3`).
- **`||=` and `&&=`** compound assignment, alongside `+=`/`-=`/`*=`/`/=`/`%=`.
- **Array/string slicing** — `x[a..b]` (half-open), `x[a..]`, `x[..b]`, `x[..]`;
  bounds are clamped so an out-of-range or inverted range is empty.
- **Hex / octal / binary integer literals** — `0x1f`, `0o17`, `0b1010`. A
  leading zero is not octal (`042` is decimal).
- **`parseBool`** pipeline builtin (`fn String parseBool() ParseError!Bool`) —
  parses `"true"`/`"false"` (case-insensitive), mapping per value like
  `parseInt`/`parseFloat`.
- **`$(a; b)` subshell statement sequences** — a `$(...)` body is a statement
  sequence like `(...)`, not just a single expression.
- **`&0 | cmd`** — a bare `&0` used as a pipeline stage forwards the function's
  stdin into the pipeline (distinct from reading `&0` as a value).
- **Language server — major expansion.** `runic-lsp` gained a broad, tested
  feature surface (see `docs/lsp.md`):
  - _Completion:_ snippets for `const`/`var`/`fn`/`import` (gated on the
    client's snippet capability); member access, including chained `a.b.c` and
    recovery of a scope after a bare trailing dot (`obj.`); function-signature
    and struct-field-type detail text; executables found on `$PATH`; symlinked
    module paths; and `completionItem/resolve` filling documentation on focus.
  - _Navigation:_ go-to-definition for locals, parameters, the nearest
    shadowing binding, function calls, struct fields, and cross-file symbols;
    references and rename that are **binding-aware** (scope/binding identity,
    not name matching) and workspace-wide, including cross-file module members
    (`m.foo`) even in files that were never opened.
  - _Symbols & structure:_ a document outline nesting struct fields and
    function parameters; document highlight; document links for import paths;
    workspace symbol search; and folding ranges.
  - _Hints & actions:_ inlay hints for inferred binding types and for call
    parameter names (in nested positions and imported-module calls);
    prepare-rename; and an "add type annotation" code action.
  - _Workspace index:_ on initialize with a client-provided root, every `.rn`
    file is indexed, which is what makes cross-file navigation and search work.

### Changed

- A unary prefix is now accepted directly after a binary operator (`2 ** -2`,
  `3 - -2`), matching the leading-unary behavior.
- `.` before a letter on an integer literal is member access, not a decimal
  point, so `6.band 3` works (`6.5` is still a float).

### Fixed

- **Parser error recovery** — a failed top-level statement now resynchronizes
  and parsing continues, so multiple independent errors are reported per run.
- **Unterminated string / block comment** now produce a clear diagnostic
  pointing at the source, instead of exiting silently.
- **Missing executable** reports `command not found: '<name>'` with the call's
  source location, instead of a bare `error.FileNotFound`.
- **Lexer infinite loop on a lone `$` in a string** — a `$` inside a string
  that does not begin a `${…}` interpolation (e.g. `"$r"`, `"cost: $5"`) is now
  literal text; it previously spun forever because the character was never
  consumed. This hung both the CLI and the LSP (whose workspace scan parses
  every file at startup).
- **LSP stability** — the workspace type checker's analysis memory is now reset
  each edit (it previously grew unboundedly); the per-edit re-check is bounded
  to the open documents rather than every module ever touched; and a
  use-after-free when closing a document (stale cached scopes referencing the
  freed AST) was fixed.
- **LSP protocol encodings** — `SymbolKind`, `DiagnosticSeverity`,
  `DiagnosticTag`, `TextDocumentSyncKind`, and `InsertTextFormat` now serialize
  as their numeric codes; they were emitted as tag-name strings, which clients
  reject.

### Internal

- A shared **builtin registry** and a single **binary-operator classification
  table** replace logic that was duplicated across the compiler and type
  checker.
- The unit-test suite was **resurrected and expanded**: `zig build test` went
  from 13 to 70 tests after the runtime module's in-file tests (lexer, parser,
  type checker, IR compiler, evaluator) were wired to run and brought up to
  date, plus a new type-checker test harness. It now stands at 112 tests, the
  growth being an end-to-end `runic-lsp` protocol suite (49 tests) covering the
  completion, navigation, symbol, hint, and action features above.

## [0.7.0] — 2026-08-31

The **stdlib-foundations** release: the language features that let the standard
library be written in Runic itself (generics, comptime, `while`), plus the first
generic collection — a hashed `std.map` — and the performance and correctness
fixes discovered while building it.

### Added

- **Generic type constructors and monomorphization.** Declare generic types with
  `const Box(T) = struct { value: T }` and generic functions with `|T|` type
  captures (`fn wrap(x: |T|) Box(T) { … }`). Calls are **monomorphized** per
  concrete type — direct, multi-parameter, nested applications (`Box(Pair(K,V))`),
  and recursion. Construct with explicit type args, `Box(Int){ .value = 1 }`, or
  bare `Box{ … }`. A type in value position serializes to its name
  (`"${Box(Int)}"` → `Box(Int)`).
- **`std.map` — a generic hashed key/value map.** O(1)-average lookup via hashed
  buckets, with insertion-order iteration. `empty`/`set`/`get`/`has`/`remove`/
  `keys`/`values`/`len`, plus mutable in-place variants `setIn`/`removeIn`. `Int`
  and `String` keys (a String is hashed over its bytes; an interpolated key
  hashes identically to the same literal).
- **`comptime` value evaluation** — `comptime <expr>` interprets pure user
  functions at compile time.
- **`while` loop**, including `while (opt) |v| { … }` optional-unwrap capture.
- **`is`-type narrowing.** Inside `if (x is String) { … }`, `x` is treated as the
  tested type, so type-specific operations (`x.upper`, `x.bytes`) resolve there.
- **`s.bytes`** — a string's bytes as `[]Int`, enabling byte-level work (hashing,
  checksums) in pure Runic.
- **`arr.with i v`** — a new array with element `i` replaced (immutable element
  update, the analog of `arr[i] = v`).
- **Float math builtins** — `sqrt`/`floor`/`ceil`/`round`/`trunc` and `powF`,
  surfaced through `std.math`.
- **`setenv name value`** builtin (backs `std.env.set`) for dynamically-named
  environment writes.
- Standard-library additions: `std.math` Float surface, `std.env.set`, and
  completed `std.list` (`find`/`contains`/`sort`) and `std.path` (`normalize`).

### Changed

- **Integer remainder yields `Int`.** `int % int` was a `Float`; it is now an
  `Int` (like `+`/`-`/`*`), so it can index arrays and feed hashes. A float
  operand still widens the result.
- **Interpolated strings are real `String` values.** A built string (`"k${i}"`,
  concatenations) is now a contiguous `[]Byte` — recognized by `is String`,
  readable by `.bytes`, and consistent under `==` and hashing — instead of an
  internal segment rope.
- **Array indexing propagates the element type**, so `arr[i].field`, `arr[i][j]`,
  nested `[][]T`, and `const b = arr[i]` resolve the element's layout.

### Fixed

- Generic struct return-type equality and value-transport, so a generic function
  can return and reuse a generic struct.
- A `Void` function's nested call (including in an `if` branch) is awaited before
  the function returns, instead of racing the caller.
- Nullary module-member functions are auto-called in value position
  (`std.env.cwd`, not a function reference).
- Multiple interpolated command arguments keep their own values; an interpolated
  command argument combined with a file redirect no longer deadlocks.
- A function's / block's file redirects (`myFn > "out"`) are drained to the file.

### Performance

- **In-place linear array buffers.** A `var x = .{ }` grown only via
  `x = x.push e` / `x = x.with i e`, read only as `x[i]`/`for(x)`/`x.len`, and
  escaping only in a final `yield`, is uniquely owned and grown **in place**
  (amortized O(1)) instead of copied each push — turning O(n²) build loops into
  O(n) (building 3000 elements: ~5.6s → ~1.0s). Gated by a conservative analysis
  that leaves anything it can't prove unique untouched.

## [0.6.1] — 2026-08-20

### Fixed

- A function call (or UFCS method call) used directly as an **arithmetic
  operand** — e.g. `${five - 1}` or `${p.magSq - q.magSq}` — no longer crashes
  with "Could not dereference value of type thread". Such operands are now
  captured so they yield their value instead of a fork/thread handle, and two
  call operands in one expression are stabilized so they don't clobber each
  other.

## [0.6.0] — 2026-08-19

### Added

- **User-defined structs.** Declare a struct type with
  `const Point = struct { x: Int, y: Int }`, construct values with
  `Point{ .x = 3, .y = 4 }`, and read fields with `p.x`. Construction checks every
  field (unknown / missing / duplicate field and per-field type are compile
  errors). Structs nest, and can be passed to and returned from functions by
  value.
- **Struct methods via UFCS.** A method is a free function whose first parameter
  is the receiver; `recv.method args…` is shorthand for `method(recv, args…)`. A
  field of the same name takes precedence over a method.
- **Struct field mutation.** A field of a `var` struct can be reassigned with
  `p.field = value` (nested fields too); the field's declared type is enforced,
  and mutating a field of a `const` struct is a compile error.

### Fixed

- Struct field reads no longer alias when two are used in one expression (e.g.
  `p.x + q.x`, or two struct parameters read in a function body) — each read is
  now stable rather than sharing a scratch register.
- A typed binding whose initializer is a function-call result no longer
  false-positives (`const r: Int = someIntFn` previously reported "expected Int,
  actual: Int"); the assignment validators now normalize an alias-wrapped return
  type. Also fixed a latent stack-corruption crash when passing a struct value as
  a function argument through the value-capture path.

## [0.5.0] — 2026-07-01

### Added

- **Strict mode (`--strict` / `-e`).** A `set -e`-style opt-in that also requires
  handling of command failures (`ExecutableError`), which are exempt by default.
  Off by default, so existing scripts are unaffected; when on, a bare command
  whose failure isn't `catch`/`||`'d is a compile error.

## [0.4.0] — 2026-06-30

This release adds two major language features — structured **error handling** and
**sum types** — and removes the `return` keyword.

### Added

#### Structured error handling

A complete, typed error system replacing ad-hoc exit codes and `set -e`
conventions. See `docs/features.md`.

- **Error sets & unions.** `const E = error { Bad, WithPayload: String }`;
  functions return an error union `E!T`, or `!T` to infer the set from the body.
  Construct values with `E.Variant` / `E{ .Variant = payload }`.
- **Handling: `catch`, `||`, `try`, `match`.** `catch` unwraps-or-handles (`|err|`
  binds the error), `||` discards to a fallback, `try` propagates out of the
  enclosing function, and `match` dispatches on variants with payload capture and
  exhaustiveness checking.
- **Errors as values across the in-process boundary.** A function/pipeline result
  preserves the real error value (set/variant/payload) via typed in-process
  capture, so `catch`/`match`/`try`/`if`/`||`/`&&` operate on the structured
  error, not its flattened text. Also covers optional-returning functions.
- **Mandatory explicit handling.** An unhandled error is a **compile error** — it
  must be `catch`/`||`'d or propagated with `try`, and a `try`'s error must be
  covered by the enclosing function's declared set (a top-level `try` is
  rejected). Commands keep the exit-code model: `ExecutableError` is exempt.
- **Commands & pipelines as catchable errors.** A command's value view is
  `ExecutableError!String`; `parseInt`/`parseFloat` are `ParseError!Int`/`!Float`.
  Pipelines are **`pipefail`-style**: any stage yielding an error makes the whole
  pipeline evaluate to that error for a trailing handler to catch.
- **Inferred error sets (`!T`)** collect the body's error variants (including
  cross-function propagation and a `try`'d command's `ExecutableError`), so
  `match` exhaustiveness is enforced and callers see a concrete set.
- **Error-set merge.** `A || B` between two error sets builds a merged set (the
  union of their variants, with payload-conflict checking and dedup).

#### Sum types: `A || B`

A structural sum type — a value that is *one of* several member types — written
with `||` (any number of members: `A || B || C`). See `docs/features.md` and the
runnable `examples/sum_types.rn`.

- **Declaration & widening.** `const IntOrString = Int || String` (or inline as an
  annotation, parameter, or return type). Sums are unordered sets and normalized:
  `Int || String` equals `String || Int`, and nested/duplicate members flatten. A
  value of a member type widens into the sum implicitly.
- **Must narrow before use.** A bare (un-narrowed) sum cannot be used as one
  specific member — arithmetic and string interpolation on it are compile errors.
  You narrow it first.
- **Narrowing with `is`.** The new `x is T` operator is a runtime type test
  (works on any value) evaluating to `Bool`; in an `if` it refines the binding
  per branch (then: `T`; else: the remaining members, collapsing a two-member sum
  to the survivor).
- **Narrowing with comparisons.** `==`/`!=` and the relational operators narrow a
  sum-typed binding; comparing a sum to a value it can never equal (a non-member)
  is rejected as a likely mistake.
- **Narrowing with `match`.** `match x { Int => …, String => … }` dispatches on the
  member type, narrows the subject in each case body, enforces exhaustiveness
  (unless `_`), and a case may bind the narrowed value with `|name|`.
- **`var` flow narrowing.** A mutable sum binding narrows in branch conditions,
  and a reassignment refines its type from that point on — while the declared type
  still governs what may be assigned.
- **Functions** can take and return sums; the concrete member value survives the
  call boundary, so the caller can narrow the result.

(Note: `||` between two error *sets* builds an error-set merge instead of a sum —
see the error-handling section above.)

#### Tooling

- `examples/error_handling.rn` and `examples/sum_types.rn` showcases, and
  `tests/cli_examples.sh` which exit-checks every `examples/*.rn` in CI
  (previously no example was CI-verified).

### Changed

- **Error/value propagation is value/yield-based.** A function produces its
  result (including an error) via `yield`; there is no `return`. `try` propagates
  by re-yielding the error.
- Numeric literal typing is now spelled out: a literal with no decimal point (or
  exponent) is `Int`; one with a decimal point (even `0.0`) or an exponent is
  `Float`.

### Removed

- **The `return` keyword.** Functions output via `yield` only. To stop a function
  early, `yield` the result and place no further statements after it (or use
  `exit` to halt the stage).

### Fixed

- **Multi-digit literal crash**: any numeric literal with three or more digits
  (e.g. `100`) panicked the lexer (a fixed-size digit probe buffer was sliced out
  of bounds). Fixed.
- **`;` statement separator after a value binding**: `const z = y; echo "hi"`
  (and arithmetic-RHS bindings) silently swallowed the following statement as a
  command argument; only command-producing initializers now sequence with `;`.
- **`if`-branch stack drift**: a branch body that bound a value (e.g.
  `const n: Int = x`) leaked a runtime stack slot, corrupting a later statement;
  the branch now balances its stack.
- Type diagnostics render the string type as `String` instead of `[]Byte`.

## [0.3.0] — 2026-06-14

### Changed

#### Zig 0.16.0 migration
- **Minimum Zig version is now 0.16.0** (`build.zig.zon` `minimum_zig_version`).
  The migration is internal — no language, syntax, or CLI behavior changes — but
  it touches the entire I/O core.
- **`std.Io` threading**: Zig 0.16 moved the filesystem (`std.fs.File`/`Dir` →
  `std.Io.File`/`Dir`), process, and reader/writer APIs under `std.Io`, and every
  side effect (opening/reading/writing/closing files, spawning and waiting on
  processes, `realpath`, `isTty`, terminal mode) now requires an `std.Io`
  instance. An `io` value obtained from `std.process.Init` is threaded from the
  CLI/LSP entry points down through the runner, evaluator, IR context, process
  layer (`FileSink`, `PipeReader`/`PipeWriter`, `ProcessCloseable`,
  `CloseableProcessIo`), and the LSP server/workspace.
- **Process spawning rewritten**: `std.process.Child.init`/`spawn`/`waitForSpawn`
  and the `argv`/`env_map`/`cwd`/`term` fields were removed. Command execution now
  uses `std.process.spawn(io, .{ ... })` with `SpawnOptions` (the `Child.Cwd`
  union, `environ_map`, and the lowercase `.pipe`/`.inherit` `StdIo` variants) and
  reaps via `Child.wait(io)` with the new lowercase `Term` (`.exited`/`.signal`).
- **Environment map**: `std.process.EnvMap` became `std.process.Environ.Map`, now
  carried by value (no longer optional) on each subshell context.
- **Assorted std renames**: `std.mem.trimRight`/`trimLeft` → `trimEnd`/`trimStart`,
  `File.Reader`/`File.Writer`-based seeking and reading, and `std.posix.SIG` /
  `Sigaction` handlers becoming enum-typed.

### Fixed
- **Double-wait panic**: `Child.wait` is now single-shot and asserts the process
  has not already been reaped, so the thread-cleanup wait and a process's
  `ProcessCloseable` could no longer both reap the same child. Both paths now skip
  the wait when the process has already exited.
- **LSP message framing**: header lines are read with `takeDelimiterInclusive` so
  the trailing newline is consumed; the previous exclusive read left the delimiter
  in the stream, misaligning the request body and breaking every LSP request.
- **`realpath` allocation sizes**: results of `realPathFileAlloc` (sentinel-
  terminated `[:0]u8`) are re-duped into plain slices before being stored/freed,
  fixing allocator free-size mismatches in the LSP server, document store, and
  module-path resolution.
- **Diagnostic file paths**: corrected the `std.fs.path.relative` argument order so
  source locations again render relative to the working directory instead of
  `../../..`.

## [0.2.0] — 2026-06-04

> The **typed pipes** work below is summarized narratively in
> [typed-pipes-update.md](./typed-pipes-update.md).

### Added

#### Typed pipeline boundaries
- **`parseFloat` builtin**: `parseFloat` (`fn String parseFloat() Float`) is the
  `Float` counterpart of `parseInt` — it maps each input value to a `Float`, so
  `Float` pipelines run end-to-end (`{ echo "1.5"; echo "2.5" } | lines |
  parseFloat | square` → `2.256.25`). Non-numeric input fails with the same
  single, source-located diagnostic style (`cannot parse "x" as Float`).
- **`lines` builtin + per-value `parseInt`**: `lines` (`fn String lines() String`)
  reads its whole byte stdin, splits on `\n`, and emits each non-empty line as a
  separate framed value onto its (typed-queue) stdout — turning a newline-
  delimited byte stream into a multi-value stream. `parseInt` now *maps* over its
  input (one `Int` per input value) instead of reading a single value, so it
  composes with a framed stream. Combined with a `for (&0)` filter, a whole
  stream flows through the pipeline: `{ for (0..5) |i| echo i } | lines |
  parseInt | square` (where `square` is `fn Int square() Int { for (&0) |in| {
  yield in * in } }`) emits `0 1 4 9 16`. Custom per-value filters use the
  `for (&0) |v| { ... }` form.
- **File-descriptor stream syntax (`&0`/`&1`/`&2`)**: the three standard streams
  are referenced with `&0` (stdin), `&1` (stdout), `&2` (stderr). `&0` is a value
  expression that reads stdin (replacing the previous `@stdin`); `&1`/`&2` are
  write streams. `&` followed by a digit lexes as a file-descriptor token.
- **`yield` keyword for explicit output**: functions and pipeline stages push
  values with `yield expr` (to stdout, `&1`) or `yield &2 expr` (to stderr). A
  function's `return`/body value is no longer auto-pushed to stdout, so a stage
  that consumes its input without yielding produces no output (e.g. a function
  that only runs a side-effecting `echo`). `return` now serves control flow / the
  function's exit value, and the declared stdout type constrains what may be
  `yield`ed to `&1` (`yield &2` carries untyped diagnostics).
- **Type checking at every `|`**: the type checker now validates that the
  upstream stdout type matches the downstream stdin type at each pipeline
  boundary. Mismatches are caught before execution with a clear diagnostic
  naming both sides.
- **Arbitrary typed pipe values + `parseInt`**: non-`String` typed values now
  flow across pipe boundaries. A function with an `Int` stdin receives `&0`
  already parsed into an `Int` (so `&0 * 2` works), and a stage returning an
  `Int`/`Float` serializes it as canonical decimal text. The new `parseInt`
  builtin (`fn String parseInt() Int`) bridges a `String` stage to an `Int`
  stage, e.g. `echo "10" | parseInt | doubler | inc` → `21`. `Int → String`
  remains a compile-time mismatch.
- **In-process typed transport**: an exact boundary carrying a by-value scalar
  (`Int`/`Float`, no executable on either side) now passes the value in-process
  instead of serializing it to text and re-parsing. The inter-stage pipe is
  marked `typed`; `yield` stores the value in a side-channel keyed by the pipe
  handle (writing no bytes) and `&0` reads it back directly. `String`/executable
  boundaries keep the byte path.
- **Function body contracts**: function bodies are checked against their
  declared `StdinType` and `StdoutType`. Calling a function whose stdin type
  is incompatible with the enclosing function's declared stdin produces a
  diagnostic.
- **`&0` access**: the built-in `&0` expression reads the function's stdin pipe
  as a typed value. Available in any function with a non-Void stdin type.
  Implemented via the `collect_stdin` IR instruction.
- **Consuming `&0` reads**: each `&0` read takes (consumes) the next value off
  the input stream; once the producer has closed, reading `&0` again yields EOF
  (`.null`) and `yield`ing an EOF value emits nothing. So a stage can read once
  per value and `yield` multiple times over its lifetime; to reuse a value, bind
  it (`const n = &0`).
- **Multi-value live streaming with `for (&0) |v|`**: a producer that `yield`s
  many values is drained by the downstream stage with a `for` loop over `&0`.
  Each iteration reads one value off the live stream (blocking until it arrives
  or the producer closes), so a consumer transforms an unbounded number of
  values as they arrive — `produce | double_each` where `double_each` is
  `for (&0) |v| { yield v * 2 }` emits each doubled value with the producer's
  timing. EOF ends the loop. Per-value iteration covers in-process typed
  (`Int`/`Float`) streams; a `String`/byte stream (no message framing) reads as
  a single value. Yield type-checking moved onto a stdout-type stack so a
  `yield` inside the loop validates the capture in its own scope.
- **Mixed exec/typed pipelines**: executable stages and typed Runic functions
  can be freely combined. The type checker enforces that an executable followed
  by a typed function must have `String` stdin (since executables output bytes).
- **Multi-stage typed pipelines**: three-or-more-stage pipelines with any
  combination of executable and typed-function stages are fully supported.
- **`T→?T` pipeline coercion**: a stage producing `T` can feed a downstream
  stage whose stdin is `?T`. The value flows through unchanged and `&0` is
  typed as `?T`, so `&0 orelse "default"` type-checks and runs. `T→E!T` is
  accepted by the type checker as well (runtime exercise pending error-union
  stdin type parsing). Genuinely incompatible boundaries (e.g. `String→Int`,
  `Void→String`, `String→Void`) still produce a clear mismatch diagnostic.
- **`Pipeline.resolveType`** now correctly unwraps function return types so
  pipeline expressions report the right value type in assignment contexts.

### Changed
- A `for` loop and `if`/`else` body may now be a bare statement without `{ }` —
  a single `yield`/`return`/`exit` (in addition to a bare expression, which
  already worked) is allowed directly: `for (&0) |in| yield in * in`,
  `if (cond) yield a else yield b`. A single such statement is desugared into a
  one-statement block. (`while` is not yet parsed at all, so it is unaffected.)
- `parseInt` (and an `Int`-typed `&0`) on non-numeric input now fails with a
  single, source-located diagnostic naming the offending value —
  `[error]: <file>:<line>:<col>: cannot parse "abc" as Int` — instead of dumping
  a raw `Error evaluating … error.InvalidInt` plus a generic CLI footer. The
  redundant generic lines are suppressed for this case.
- Bound command expressions now preserve execution-result data more consistently across `&&`, `||`, and `;`, so `.stdout`, `.stderr`, and `.exit_code` remain available after sequencing command-producing expressions.
- `scripts/run_ci.sh` is now the preferred CI entrypoint. It wraps `scripts/run_ci.rn`, checks for expected progress output, and falls back to the direct shell stages if the Runic-driven CI path regresses.

### Fixed
- `lines` followed by an external command (or any byte consumer) — e.g.
  `printf "a\nb\n" | lines | cat`, `… | lines | grep` — no longer silently
  produces empty output. `lines`/`emit_lines` only wrote to the in-process typed
  queue, so a downstream that reads bytes saw nothing. It now mirrors `yield`:
  on a `typed` boundary it enqueues framed values (so `lines | parseInt` keeps
  per-line framing), and on a byte boundary it writes each line back as `line\n`.
- `>` is now overloaded by operand rather than always being parsed as an output
  redirect. Previously `if (n > 2) { ... }` (and any `>` between values) was
  parsed as "redirect `n` to a file named `2`", so the condition was the value
  `n` and the runtime panicked (`access of union field 'exit_code' while field
  'uinteger' is active`). The parser now leaves `>` as an unresolved
  `.binary{.greater}` and the IR compiler decides from the **left operand**: a
  command (an external executable call, a Runic function call, a block, or a
  subshell) makes it an output redirect; a value or an in-scope value binding
  makes it the greater-than comparison. So `echo "x" > "f"` and `myFn > "file"`
  redirect, while `n > 2` and `count > limit` compare, in any context
  (condition, binding RHS, `yield`/`return` value, …). `>>` and `>&` are
  unaffected (they have no comparison meaning and always redirect). To compare a
  function's return value instead of redirecting it, bind it first
  (`const r = myFn; if (r > 2) ...`).
- A producer block whose `yield` is nested inside a loop/`if`/`match`
  (`{ for (0..5) |i| { yield i } } | square`) is now correctly recognized as a
  scalar stage and gets framed (per-value) typed transport. Previously the
  stdout-type inference only looked at top-level `yield`s, so the boundary fell
  back to the byte path: the producer's values were concatenated into one blob
  and the consumer saw a single value (e.g. `0 1 2 3 4` became `01234` → parsed
  as `1234`, so `square` returned `1522756` instead of `0 1 4 9 16`). Inference
  now recurses into nested bodies and resolves loop captures (and the bare
  `yield i` value, which parses as a zero-arg call).
- `yield <binding>` (e.g. `for (&0) |v| { yield v }`, or any `yield v` where `v`
  is a loop capture or local) no longer crashes the compiler with an
  `integer overflow` panic. `yield` previously popped its value unconditionally
  when it looked like a stack location, but a bare binding yields a *borrowed*
  reference that must not be popped; doing so corrupted the frame and underflowed
  the loop compilers' per-iteration ref accounting. `yield` now pops only the
  temporaries that compiling its value actually pushed. Affected all three
  for-loop forms (live `for (&0)`, counted array, and range).
- Block pipeline stages carrying a scalar (`{ yield 1; ... } | { yield &0 }`)
  now use in-process typed transport with per-value framing, like named typed
  functions do. Previously a block stage was always classified as a byte stream,
  so the boundary buffered every `yield` into one text blob read after the
  producer closed — breaking live streaming (`{ yield 1; sleep "1"; yield 2 } |
  { yield &0; yield &0 }` waited a second and emitted `12` together instead of
  `1` immediately then `2`) and per-value framing (a single `&0` read returned
  the whole buffer instead of one value). The compiler now infers a block
  stage's stdout type from its first `yield &1` so the boundary is recognized as
  `Int`/`Float` typed transport.
- `&0` can now be referenced from block expressions inside a function body
  (including nested blocks and bindings like `const n = &0` used later). The
  type checker previously resolved a function's stdin/stdout types in a scope
  that did not contain the body's bindings, producing a spurious stdout-type
  mismatch once `&0` carried a non-`String` type.
- A block used directly as a pipeline stage now infers its `&0` type from
  the upstream stage, so `echo "3" | parseInt | { yield &0 * &0 }`
  evaluates `&0` as an `Int` (→ `9`) instead of failing on `String * String`.
- An explicit passthrough stage `{ yield &0 }` re-emits its input unchanged,
  preserving its type (`echo "5" | parseInt | { yield &0 } | doubler` →
  `10`). A bare `&0` used as a stage consumes-and-discards (it does not
  yield), so a type-incompatible chain like `parseInt | &0 | doubler` is
  rejected at compile time.
- `yield` of a multi-segment string (produced by interpolation like `"${x}!"`)
  serializes correctly — the segments are concatenated rather than space-joined.
- Chained fd redirects now preserve left-to-right shell semantics, so forms like `echo "hello" 1>&2 2>"/dev/null"` keep writing to the original stderr stream before the later redirect replaces fd `2`.
- Direct top-level executable calls now preserve TTY-aware stdout/stderr behavior when Runic itself is attached to a terminal, so scripts can keep color/ANSI output without breaking redirected or captured output paths.

## [0.1.0] — 2026-03-22

Initial versioned release. Establishes a baseline for tracking changes going forward.

### Added
- Versioning via `--version` / `-V` flag on the `runic` CLI
- `--version` flag on `runic-lsp` now reports the shared project version
- This changelog

### Language
- Typed variables: `const` (immutable) and `var` (mutable) bindings
- Primitive types: `String`, `Int`, `Float`, `Bool`, `Void`
- Array literals with Zig-style anonymous syntax: `.{ "a", "b" }`
- Structured pipelines and command execution
- String interpolation
- Control flow: `if`/`else`, `for`, `while`, `match`
- Functions with typed signatures: `fn StdinType name(params) StdoutType { ... }`
- Closures
- Pattern matching with predicate match cases
- Optional types, promise types (`^T`), and error sets
- Background process execution with `^` operator
- Module system via `.rn.module.json` manifests
- Bash interop by invoking `bash` as a command (`bash "-c" "…"`)
- LSP support (completions, diagnostics, hover)
