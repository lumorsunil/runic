# Embedding Runic as a scripting runtime

Status: **Phases 1–2 landed; Phases 3+ not started.** `runic.embed.run` (in
`src/embed.zig`) runs a script from a source string and returns a structured
`Outcome` (captured stdout/stderr, an exit code or null on compile error, and a
flattened diagnostic list). A host can register functions (`options.host_functions`,
`runic.embed.HostFn`) that the script calls **like commands** (`greet "world"`):
when the evaluator is about to spawn a command whose name is registered, it calls
the host callback instead, passing the string arguments and writing the returned
string to the command's stdout. Feeding the script's *stdin* from the provided
bytes is deferred within Phase 1. The roadmap entry is Theme 7 in `docs/plan.md`.

The realized Phase 2 deliberately chose the **command-interception** model over
the `import "host"` module sketched below: it reuses the entire external-command
pipeline (type-checked as an executable, captured by binding, piped) and needed
**no type-checker or IR changes** — only a dispatch hook at the two spawn sites
in the evaluator. The trade-offs, now deferred to later phases: everything is
string-marshalled (the command model), host functions are ambient command names
rather than a namespaced module, and there are no typed signatures yet.

## Goal

Let a host program embed Runic the way a game or app embeds Lua: the host owns
the process, exposes a slice of its own functionality to a script as callable
functions, hands Runic a script, and runs it. When the script calls one of the
exposed functions, control returns to the host, which does the work and passes a
value back. The script is *driven by* the host and *extends* it — the inverse of
the C FFI (`cimport`), where a Runic script reaches *out* to a C library.

Minimal shape the host wants:

```zig
var rt = try runic.Embed.init(allocator, .{ .sandbox = .strict });
defer rt.deinit();

// Expose a host function with a declared Runic signature.
try rt.register("greet", .{ .params = &.{.string}, .returns = .string }, greet, &app);

const result = try rt.run(script_source, .{ .args = args });
// result carries the script's exit code / captured output / a diagnostic list.
```

```zig
fn greet(call: *runic.Embed.Call) runic.Embed.Error!runic.Embed.Value {
    const app: *App = @ptrCast(@alignCast(call.ctx));
    const name = try call.arg(0).asString();
    return call.string("Hello, {s}, from {s}", .{ name, app.title });
}
```

And in the script:

```rn
import "host"            // the module of host-registered functions
const msg = host.greet "world"
echo "${msg}"
```

## How it fits the existing pipeline

Runic already runs as `Source → Lexer → Parser → Type Checker → IR Compiler →
IR Runner/Evaluator`, driven by `ir.runner.runIR(allocator, document_store,
script, config)` with `stdin`/`stdout`/`stderr` streams (see
`cmd/runic/run_script.zig`). `src/root.zig` already exposes the pieces as a
library. Embedding adds four things on top of that existing surface:

1. **A facade** (`src/embed.zig`, exported as `runic.Embed`) that owns a
   `DocumentStore`, a host-function registry, a capability config, and the
   stdin/stdout/stderr wiring — so a host runs a script without re-deriving the
   CLI's orchestration.
2. **A host-function registry**: a dynamic table of `{ name, signature,
   callback, ctx }`, distinct from the static builtin registry in
   `src/builtins.zig` (which is a comptime array).
3. **Type-checker integration**: each registered function is declared into the
   entry module's scope as an ordinary function binding whose body is "native",
   so calls type-check against the declared signature exactly like a Runic
   function.
4. **A native-call dispatch** in the evaluator: a call whose target is a
   registered host function marshals the arguments to host values, invokes the
   Zig callback synchronously on the evaluator thread, and marshals the result
   back.

Points 3–4 parallel the C FFI path (`cimport` → `CImportCloseable` /
`ResolvedExtern` → a `libffi` call). The difference is the ABI: a host callback
is a plain Zig function taking/returning Runic values, so no `libffi` and no
C-type coercion — the two can share the value-marshalling layer but not the call
mechanism.

## Design pieces

### 1. Embedding facade (`runic.Embed`)
Wraps what `run_script.zig` does today: init a `DocumentStore`, build the
stdin/stdout/stderr streams (a host may supply its own writers or capture
output to a buffer), hold the env map, run through `runIR`, and surface the
`RunResult` (success exit code, or the diagnostic list rendered the way the CLI
renders it). `run` returns a structured result rather than writing to a real
terminal, so a GUI/server host can consume it.

### 2. Host-function registration & signatures
`register(name, signature, callback, ctx)` adds a row to the registry. The
`signature` gives the Runic stdin/param/return types (reusing `ast.TypeExpr` or
a small host-facing enum that lowers to it) so the type checker can validate
calls and so return values coerce correctly. `ctx` is an opaque `*anyopaque`
the host threads through to its callback (its app state). Registration happens
before `run`; a later phase may allow registering constants/values too.

### 3. Exposing the API to the script
Decision pending (see open questions). The leading option is a **reserved host
module** imported by a well-known name — `import "host"` — whose members are the
registered functions (`host.greet`). This reuses the existing module-member
resolution (a module that exports functions) rather than inventing new syntax,
and keeps host names namespaced so they can't shadow builtins or user bindings.
Alternatives: inject the functions as ambient builtins (no import; simpler calls
`greet "x"` but pollutes the global namespace), or a host-chosen module name.

### 4. Native-call dispatch
At type-check time a call to a host function resolves to its registered
signature. At IR-compile time the call lowers to a new instruction (say
`host_call { registry_index, args }`) — or reuses the FFI call instruction with
a native-callback variant. At eval time the evaluator reads the argument values
off the stack, wraps them in a `Call` view, invokes the callback, and places the
returned `Value` in the result register. The call is **synchronous** on the
evaluator thread; host callbacks must not block the scheduler (documented
constraint). Reentrancy (a host callback that itself asks Runic to run a snippet)
is deferred.

### 5. Value marshalling
A host-facing `Value` layer converts between the runtime's `ir.value.Value` and
host Zig values. Phase in by complexity:
- **Scalars first**: `Int`, `Float`, `Bool`, `String` (both directions).
- **Collections**: arrays and maps.
- **Structured**: structs (by field), optionals, and error unions.
The host-facing representation should be a stable, documented shape (an accessor
API like `call.arg(i).asInt()` / constructors like `call.string(...)`), not the
raw internal `Value` union, so the embedding API stays stable as the runtime
evolves. Marshalling is shared with the C FFI layer where the shapes line up.

### 6. Sandboxing / capabilities
Embedded scripts are often untrusted, so `Embed.init` takes a capability config
that gates the side-effecting surface: process execution (external commands),
filesystem access, environment access, and `cimport`/FFI. Default for embedded
mode should be **deny-by-default** for exec/FS/FFI (a script computes and calls
host functions; it does not spawn `rm`). The CLI keeps today's permissive
behavior. This likely needs a capability check at IR-compile or eval time for
the gated builtins/exec paths.

### 7. Errors
A host callback can fail. Its error maps to a Runic error value in a host error
set so the script can `catch`/`try`/`match` it (building on the shipped error
model). A diagnostic from compilation, and an uncaught runtime error, come back
to the host in the `RunResult` rather than being printed.

## Phasing

1. **Facade + run-a-string** — **landed.** `runic.embed.run(gpa, io, source,
   options)` returns an `Outcome { exit_code, stdout, stderr, diagnostics }`,
   capturing output into buffers and flattening parser/type-checker/runtime
   diagnostics into a host-friendly `{ severity, message, file, line, column }`.
   Repackages the `run_script.zig` orchestration (parse → resolve imports →
   type-check → run) minus the CLI-only concerns. Stdin delivery from
   `options.stdin` is deferred (the fixed-reader → stream wiring didn't deliver
   to a top-level `&0`; revisit with the real-stdin pipe semantics). Import
   resolution currently walks top-level `import`/`const … = import` statements —
   the common positions; a later pass can share the CLI's full AST walk.
2. **Host functions callable from a script** — **landed**, via the
   command-interception model (not the `import "host"` module originally
   sketched). `options.host_functions` registers `{ name, callback, ctx }`; a
   call to that name (`greet "world"`) dispatches to the Zig callback with the
   string arguments, and the callback's returned string becomes the command's
   stdout — captured by a binding (`const m = greet "x"`), piped, or printed,
   with an exit code. Implemented as a dispatch hook at the evaluator's two spawn
   sites (`spawnSimpleExec` and the `.exec` handler, the latter producing a
   synthetic already-closed closeable so `wait` works unchanged); shared types in
   `src/host.zig` avoid an import cycle. No type-checker or IR changes.
   Deferred to later phases: **typed signatures** and non-string marshalling
   (`Int`/`Float`/`Bool` arrive as strings today), a namespaced **`import "host"`
   module** instead of ambient command names, and interception of host calls on
   the *other* exec paths if any surface.
3. **Sandbox config** — deny-by-default exec/FS/FFI for embedded mode, gated by
   the capability config.
4. **Richer marshalling** — arrays, maps, structs, optionals, and bidirectional
   error mapping; optional host-provided constants/values.
5. **C ABI facade (deferred, maybe out of scope)** — a stable C header so
   non-Zig hosts can embed, mirroring Lua's C API. Large; only if there is real
   demand. Zig hosts are the near-term target.

## Open design questions

- **Exposure syntax**: reserved `import "host"` module vs ambient builtins vs a
  host-named module. (Leaning `import "host"`.)
- **Signature representation**: reuse `ast.TypeExpr` directly, or a small
  host-facing descriptor that lowers to it?
- **Instruction reuse**: a dedicated `host_call` IR op, or a native-callback
  variant of the existing FFI call op?
- **Value ABI**: accessor/constructor API over an opaque handle (stable) vs
  exposing `ir.value.Value` (simpler, leakier).
- **Sandbox granularity**: per-capability booleans vs a policy object with
  allow-lists (e.g. only these executables).
- **Reentrancy & threading**: host callbacks are synchronous and single-threaded
  for v1; a callback re-entering the runtime is deferred.
- **Lifecycle**: one-shot `run` vs a persistent state the host calls into
  repeatedly (Lua keeps a `lua_State`); start one-shot, consider persistence
  later.

## Relation to existing work

- **Inverse of the C FFI** (`future/c-ffi.md`): FFI is Runic → C; embedding is
  host → Runic (+ host callbacks). They share value marshalling, not the call
  mechanism.
- Builds on the **library API** already exported from `src/root.zig` and the
  `runIR` entry point.
- Leans on the shipped **error model** for host-callback failures and the
  **module system** for the `host` namespace.

## Non-goals (for now)

- A C ABI for non-Zig hosts (phase 5, deferred).
- Hot-reloading or incremental script state across runs.
- Letting a host callback re-enter the runtime recursively.
