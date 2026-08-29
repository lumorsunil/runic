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
    `pipeline_param_coercion_regression`. **Typed transport works:** a typed
    scalar parameter (`Int`/`Float`) receives the value typed (the boundary is
    classified by the parameter type, so the inter-stage pipe is typed and `&0`
    dequeues the typed value); the collected input is bound as the parameter's
    type. `parseInt | double` and `parseFloat | halve` work. (A param-stage
    processes one value per invocation — the same single-value behavior as any
    stdin-typed function stage.)
  - String `+` concatenation: not added; interpolation (`"${a}${b}"`) is the
    concat tool.

  **Phase 1 COMPLETE.** Full CI green (13/13, 42 diag, 8 examples, 116 smoke,
  3 strict).

- [x] **Phase 2 — Higher-order functions + generics. DONE.** Composability.
  - [x] Function values (Phase 0): a function bound to a value and called.
  - [x] **Function-typed parameters**: `fn(ParamType, …) ReturnType` parses in
    type position (implicit Void stdin). A `.function`-typed location callee with
    args compiles to a **runtime indirect call** — `Fork.dest_from` reads the
    target instr_set from the fn_ref at runtime; closure size = arg count
    (capture-free top-level functions, which is what a HOF receives).
  - [x] **Implicit type variables**: an uppercase name in a signature that isn't
    a declared type is a `type_var`, permissive in type comparisons (both
    directions). No monomorphization needed — the runtime is dynamic, so a generic
    function compiles once. (Simpler than the planned comptime-desugaring;
    revisit if comptime type *construction* is needed for Phase 4 containers.)
  - [x] **Generic array helpers**, written in Runic: `map`, `filter`, `reduce`
    (and `count`/`first`), using type vars + a higher-order function + array
    building (`.{ }` empty literal + `arr.push value` → new array). Also fixed
    comparison result typing (`==`/… → Bool). `find`/`any`/`all`/`sort` are
    trivial follow-ups on the same primitives.
  - Tests: `higher_order_function_regression`, `generics_regression`,
    `map_filter_regression`.

  **Phase 2 COMPLETE.** Full CI green (13/13, 42 diag, 8 examples, 119 smoke,
  3 strict).

- [x] **Adjacent — Executables outside `PATH`. DONE (2026-08-24).** Not a stdlib
  blocker, but resolved alongside it: a program at a subpath couldn't be named as
  a bare command word (`scripts/start` parsed as division). Two unambiguous forms,
  both documented in `docs/features.md`:
  - **Path sigil** — a command word starting with `./`, `../`, or `/` parses whole
    as one executable path. Unambiguous because those sigils can't begin an
    expression, so division (`a / b`) and member access (`a.b`) are untouched.
    Parser: `tryParsePathExpression`/`isPathToken`, hooked into the `.expr` state
    of the component binary parser; the path flows through the existing
    direct-executable machinery as an identifier whose name is the full path.
  - **`run` builtin** — `run <path> args…` executes a runtime-computed path (var,
    interpolation, spaced) with arg[0] as the executable. Reuses
    `compileExecutableCall` (generalized to compile a dynamic executable
    expression); `materializeExecArgv` already accepted a dynamic executable.
    Same `ExecutableError!String` value view as any command; shadowable.
  - Tests: `path_executable_regression`, `run_builtin_regression`. `~/` (home)
    deferred — needs a tilde lexer token + expansion; covered today by
    `run "${HOME}/…"`-style interpolation.

- [x] **Phase 3 — The `std` module + shallow modules. COMPLETE (2026-08-28).**
  Concrete API spec (every signature) is in `docs/standard-library.md` → *Module
  Reference*. Agreed scope (2026-08-23): all eight modules; `std.str` is
  **composites only** (the built-in string methods stay canonical); **add Float
  math builtins now**. Full CI green (13/13, 42 diag, 8 examples, 136 smoke, 3
  strict). All seven shipped modules, every foundational IR fix, and the entire
  bug list below are done (`std.process` dropped by decision).
  - [x] **`import "std"` resolution — DONE (2026-08-25).** The reserved `std`
    namespace resolves to modules **embedded in the binary** (`@embedFile` via
    anonymous imports in `build.zig` → `src/frontend/std_modules.zig`), registered
    under virtual document paths (`:std`, `:std/list`). `resolveModulePath` maps
    `import "std"` → `:std` and `import "std/<name>"` → `:std/<name>` (importer-
    independent, trailing `.rn` trimmed); `requestDocument` loads `:std…` from the
    embed table. `std.rn` re-exports each module as a public member.
  - [x] **Foundational IR fixes unblocking typed stdlib** (all committed):
    module-member calls value-capture by return type; array `.len` stabilized;
    for-loop capture binds the *element* type; `String`/`[]String`/`""` type
    consistently.
  - [x] **`std.list` — DONE.** map/filter/reduce/count/any/all/find/contains/
    reverse/concat/range/sort (full spec surface).
  - [x] **`std.str` — DONE.** words/capitalize/isBlank/padLeft/padRight (composites).
  - [x] **`std.path` — DONE.** join/basename/dirname/ext/stem/isAbsolute/normalize
    (full spec surface; normalize added 2026-08-27).
  - [x] **Re-export offset bug FIXED (2026-08-26).** Was THE blocker: a module
    re-exporting sub-modules via `pub const x = import "…"` (the `std` root
    pattern) only exposed its first two members — the 3rd+ read a wrong offset.
    The module result struct stores each field in one slot (an address), but
    `fieldLayout` summed each field's `slotSize` and a module-value field is a
    `struct_type` with slotSize > 1. Fixed with `StructType.by_reference_fields`
    (one slot per field) on the dynamically-built result structs. Now `std` can
    hold any number of modules.
  - [x] **String-builtin/module-member collision FIXED (2026-08-26).** `X.join`/
    `X.split`/… now check `memberIsStructField` first, so `std.path.join` calls
    the module member, not the built-in `[]String.join`.
  - [x] **`std.math` — DONE (2026-08-27).** Int: abs/min/max/clamp/sign/pow.
    Float: absF/sqrt/floor/ceil/round/trunc/powF (backed by the Float builtins).
  - [x] **`std.fs` — DONE (2026-08-26).** exists/isDir/isFile/readText/writeText/
    appendText/listDir/mkdirp/remove/cwd (portable-command wrappers).
  - [x] **`std.env` — DONE (`set` added 2026-08-27).** get/getOr/has/home/path,
    plus **set** — a `setenv name value` builtin (dynamic-name counterpart of
    `$NAME = value`; `SetEnv.name_source`) wrapped by `std.env.set`. Full surface.
    Test: setenv_regression.
  - [x] **Float math builtins — DONE (2026-08-27).** `float_op` IR instruction
    (mirrors `str_op`): UFCS `x.sqrt`/`x.floor`/`x.ceil`/`x.round`/`x.trunc` and
    `x.powF y`. `std.math`'s Float surface wraps them.
  - [x] **`std.testing` — DONE (2026-08-27).** assert/assertEq/assertContains/fail,
    aborting via `exit 1`. Unblocked by the process-exit fix below.
  - [x] **Process-exit fix — DONE (2026-08-27).** `exit` in a function now
    terminates the whole program (a new `process_exit` instruction/step result);
    previously it only closed that function's thread. Test: function_exit_regression.
  - [x] **`std.process` — DROPPED (DECISION 2026-08-27).** The struct view
    (`r.exit_code`/`r.stdout`) and the error-union view (`r catch {…}`) can't
    coexist on one value, and the module would be thin sugar over idioms that
    already work directly: `const r = cmd; r.exit_code` / `r.stdout` (struct view)
    and `cmd catch {…}` / `try cmd` (error view). So `std` ships **7 modules**, not
    8; `std.process` is intentionally omitted. (The broader result-model insight —
    a call yields a *stream* of values *and* a terminal error — is recorded
    separately as a future language-design direction, independent of this drop.)
  - **Compiler bugs found while authoring:**
    - [x] **Nullary String module fn in direct interpolation — FIXED (2026-08-27).**
      `"${std.fs.cwd}"` now calls it. `compileMember` auto-calls a nullary
      `fn_ref_type` member (using the instruction set's authoritative param count),
      and `analyzeExpressionEffects` marks it for capture. Test:
      nullary_member_call_regression.
    - [x] **Nested statement-position call isn't awaited — FIXED (2026-08-27).**
      A Void function now waits on its body's thread-handle result, and
      `compileIfElse` keeps a branch's thread handle typed as a thread so the wait
      catches an if-branch call. A delegated `exit` now aborts correctly (std.testing
      could de-inline, though it still inlines). Test: nested_call_await_regression.
    - [x] **Interpolated arg + file redirect deadlock — FIXED (2026-08-27).**
      `echo "${x}" > file` (and `>>`) hung. Traced to a timing bug: building the
      interpolated, multi-segment string arg inside the exec closure delays the
      command's spawn, so the stdout→file drain forwards the pipe *before* the
      command connects its stdout; with the pipe's default `keep_open=false` the
      drain sees no source, closes the file immediately, and exits, so the
      command's output has nowhere to go and its `wait` hangs. Fix: create the
      redirect pipe with `keep_open=true` (drain spins on `no_source` until the
      command connects), then clear `keep_open` from inside the exec closure right
      after the command's own wait (it has connected + finished by then), so the
      drain drains the EOF source and closes the file. Verified truncate/append,
      multi-arg, text+interp, 5000-byte output, and inside a Void fn. Test:
      interpolated_redirect_regression. `fs.writeText` can now interpolate (still
      uses the unquoted form; either works).
    - [x] **Multiple interpolated args in one command — FIXED (2026-08-27).**
      `echo "${x}" "${y}"` printed `AA AA` (every arg took the first's value; with
      three, off-by-one). Cause: each interpolated (multi-segment) string arg
      leaves its `string_literal` ref on the exec-closure stack, and the old loop
      pushed each value immediately, interleaving the refs with the pushes so
      `exec` popped the wrong slots. Fix: compile all args into stable value refs
      first, then push contiguously. Test: multi_interpolated_args_regression.
    - [x] **Function-call / block file redirect — FIXED (2026-08-28).**
      `myFn > "file"`, `{ … } > "file"` (and `>>`) now work, including when the
      body runs external commands (e.g. `echo`) whose real stdout must reach the
      file. Previously hung: the function/block redirect path set up the redirect
      pipe + file sink but spawned **no drain**, so the inner command's stdout was
      never read → never hit EOF → its `wait` never returned. **The fix** (all in
      `compiler.zig`), mirroring the command path's proven structure:
      - `compileRedirectStreams` now sets `keep_open=true` on each file-redirect
        pipe and records its location on `RedirectStreams.{stdout,stderr}_file_pipe`.
      - New `compileFileRedirectDrains(writer_handle, streams)`: forks a
        **concurrent** `stdoutStreamSet` drain per file pipe (drives pipe→file
        forwarding while the writer runs), waits the writer thread, clears
        `keep_open` so the drain flushes+closes the file, then waits the drains.
      - `compileFunctionCall` (non-pub path, `stdout_override == null`) and
        `compileBlockCallWithRedirects` call it and return `void`.
      - Two subtle bugs fixed along the way: (1) the fn path's `consume(closure)`
        pop aliased the redirect-pipe stack slot — resolved by capturing the fn
        handle into a fresh top-of-stack ref and **not** popping the closure here;
        (2) `compileBlockCallWithRedirects` never called
        `compileClosureInitialization`, so the block thread was never actually
        spawned (create-closure jump landed in an empty set) — now called before
        `scopes.pop()`. Covered by `tests/features/subprocess_redirect_regression.rn`
        (subprocess in fn + block, `>` and `>>`); features.md updated.
    - `std.list` **piping into a module-member stage** (`… | std.list.count`) isn't
      coerced (the pipeline-param coercion keys on local bindings). Call directly.

- [~] **Phase 4 — Later language growth. IN PROGRESS.**
  - [x] **`while` loop — DONE (2026-08-28).** `while (condition) { body }`:
    re-evaluates a truthy condition (Bool / comparison / command exit status)
    each iteration and runs a brace block until falsy; body bindings are
    loop-local; loops nest. The `kw_while` token + `WhileStmt` AST node already
    existed; added the parser production (`parseWhileStatement`, wired into
    `parseStatement`), the type-check case (`runWhile` — condition in the
    enclosing scope, body via `runBlockInNewScope`), and IR lowering
    (`compileWhileLoop`: condition stashed in a ref outside the loop so its
    transient stack refs pop back to a fixed base before the exit branch, keeping
    the continue/exit stacks aligned). Covered by
    `tests/features/while_loop_regression.rn`; features.md documents it.
  - [x] **`while (opt) |v|` optional-unwrap capture — DONE (2026-08-28).** Loops
    while the optional condition is present, binding the unwrapped value each
    pass — the optional analogue of `if (opt) |v|`, reusing the same
    `IfCaptureBinding` machinery. Parser now reads the optional capture clause
    (`parseOptionalCaptureClause`); `runWhile` declares the unwrapped binding in
    the body scope (via `resolveConditionType` → `.optional.child`, erroring on a
    non-optional condition); `compileWhileLoop` computes `present = cond != null`
    into a ref outside the loop, uses it as the exit test, and binds
    `cond` typed as the child in the body. Covered by
    `tests/features/while_capture_regression.rn`. CI green (13/13, 42 diag, 8
    examples, 138 smoke, 3 strict).
  - [x] **`comptime` value evaluation — DONE (2026-08-28).** `comptime <expr>`
    forces compile-time evaluation and folds to a constant, extending the
    existing constant-folder to **interpret pure user functions** (recursion,
    params, local `const`, `if`/`match`). New `kw_comptime` token + `ComptimeExpr`
    AST node; parser prefix (`parseComptimeExpression`); type-check delegates to
    the operand. In the compiler: a `comptime_forcing` flag gates call-folding
    (so ordinary calls stay runtime), `comptime_fn_decls` maps a function's
    instr_set → its AST (recorded in `compileFnDecl`), and `evalComptimeCall` /
    `evalComptimeBody` / `evalComptimeStatement` interpret the body with a
    `ComptimeFlow` (yield=return / fall-through / not-foldable) signal. A
    `comptime_max_depth` (128) recursion cap turns non-terminating comptime into
    a compile error instead of a native stack overflow. A non-foldable operand
    (reads a `var`, impure/unknown call) is a compile error. Covered by
    `tests/features/comptime_regression.rn` +
    `tests/diagnostics/comptime_not_evaluable.rn`; features.md documents it. CI
    green (13/13, 44 diag, 8 examples, 139 smoke, 3 strict).
    - Follow-ups: comptime over loops (`for`/`while`); larger recursion via an
      explicit interpreter stack.
  - [x] **Type captures `|T|` — DONE (2026-08-28).** `|T|` in a type position
    binds `T` to the type there (a Zig-proposal-style capture); `T` is usable
    anywhere a type is. In a binding it captures the initializer's concrete type;
    in a signature it is a permissive generic type variable. Nested under the
    built-in generic constructors it destructures: `[]|T|` binds the element
    type, `?|T|` the child. **Replaces `@TypeOf`** (removed). Compile-time only.
    New `type_capture: TypeCapture` `TypeExpr` variant; parsed via `.pipe` in
    type position (`parseTypeCaptureTypeExpr`; `.pipe` made a non-terminator).
    `collectTypeVars` registers the name; `resolveTypeExpr` resolves it to the
    scoped concrete type or a `type_var`. Concrete binding via `bindTypeCaptures`
    (recursive unification through `[]`/`?`/promise) in both the type-checker's
    `runBindingDecl` and the compiler's `compileIdentifierBinding`
    (`type_captures` map + `lookupTypeCapture`, consulted from
    `normalizeStringTypes`). Unmatched captures (`?|T| = null`) stay type vars;
    mismatches against a captured type are still caught. Covered by
    `tests/features/type_capture_regression.rn` +
    `tests/diagnostics/type_capture_mismatch.rn`; features.md documents it. CI
    green (13/13, 45 diag, 8 examples, 140 smoke, 3 strict).
    - Follow-ups: enforcing that repeated `|T|`/`T` uses in one signature agree
      (currently permissive).
  - [x] **Generic type constructors — DONE (2026-08-29).** `const Box(T) =
    struct { value: T }` declares a type parameterized by `T`; `Box(Int)` applies
    it (substitutes the arg) and `Box(|T|)` destructures an application to
    capture the arg — so `fn unwrap(box: Box(|T|)) T { yield box.value }` serves
    every instantiation. Multi-param (`Pair(A, B)`), composition (`[]Box(Int)`),
    and capture-in-binding (`const c: Box(|E|) = b`) all work. No monomorphization
    (dynamic runtime — `Box(Int)`/`Box(String)` share a layout); type args are
    compile-time-only. New `type_application` `TypeExpr` + `params` on
    `TypeBindingDecl`; parser handles `const Name(P…) = …` and `Name(args…)` in
    type position. Type-checker: a `generic_type_ctors` registry,
    `resolveTypeApplication` (resolve args → `substituteTypeParams` → resolve),
    struct-field capture unification in `bindTypeCaptures`, and generic-aware
    `runStructLiteral`. Compiler resolves an application to the registered
    `user_struct_types` layout (args don't affect it). Covered by
    `tests/features/generic_type_regression.rn` +
    `tests/diagnostics/generic_type_undeclared.rn`; features.md documents it. CI
    green (13/13, 46 diag, 8 examples, 141 smoke, 3 strict).
    - Follow-ups: the comptime-function model (`fn Box(comptime T: type) type`)
      as the underlying form (needs a `type` value); inferred construction
      `.{ .value = 5 }` typed by result location, and inferred args (`Box(_)` /
      bare `Box`); capturing a generic's type param for downstream member access
      on the compiler side.
  - [x] **Type identifiers → strings — DONE (2026-08-29).** A type identifier
    used where a string is expected (string interpolation or a bare command
    argument) serializes to the type's name: primitives (`${Int}` → "Int"),
    named structs / generic constructors / error sets (`${Point}`), and a bound
    `|T|` capture (`const n: |T| = 42; ${T}` → "Int"). Anonymous structs expand
    structurally. Compile-time only (no runtime type info). Compiler:
    `writeTypeName` + `typeIdentifierString`, hooked into `compileIdentifier`
    before the executable fallback; a `valueTypeExpr` seeds a capture from a
    literal's value tag; `registerParamTypeVars` records `|T|` captures (in
    params and unmatched binding captures) as permissive type vars so they
    serialize to their name. Type-checker: an `is_type` binding flag
    (`scope.declareType`) + `isTypeIdentifierExpr`, so the interpolation and
    command-argument guards accept a type identifier instead of rejecting it as a
    struct. Covered by `tests/features/type_serialization_regression.rn`.
    - Follow-ups: **monomorphization** so a generic function's `|T|` serializes
      the concrete per-call type (`describe 5` → "Int") instead of the variable
      name "T"; concrete derivation for a capture nested in a generic application
      (`const b: Box(|T|) = Box{ .value = 5 }` — `${T}` is best-effort "T", wants
      "Int"; needs compiler-side generic substitution + struct-literal field
      typing); `${Box(Int)}` giving "Box(Int)" (currently "Box"); array-literal
      element inference so `|A|` on `.{1,2,3}` is `[]Int` not `[]Void`.
  - Maps: `{ k: v }` literals + `Map(K, V)` type + access/keys/values.

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
