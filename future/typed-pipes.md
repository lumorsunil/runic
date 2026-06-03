# typed pipes

## introduction

Function signatures carry explicit stdin and stdout types using the form
`fn StdinType name(params) StdoutType`. The stdin type names what the function
reads from its process stdin; the stdout type names what the function writes to
its process stdout. When a type is `Void` on either side, that stream is
immediately closed.

Any side effects using stdin or stdout in the function body must match the type
given in the function signature. For example, this function declaration produces
a type error because `greetings` declares `String` stdin but calls `hello` which
expects `Void` stdin:

```rn
fn Void hello() String echo "hello"
fn String greetings() String hello
```

## implemented semantics

The following rules are enforced as of the current implementation:

- **Exact type match**: The upstream stdout type must equal the downstream stdin
  type. `String | fn String ...` is accepted; `String | fn Int ...` is rejected.
- **Void rejection**: A `Void` upstream stdout to a non-`Void` downstream stdin
  is rejected. A non-`Void` upstream stdout to a `Void` downstream stdin is
  rejected.
- **Function body stdin contract**: The type checker walks the function body and
  rejects calls to functions whose declared stdin type is incompatible with the
  enclosing function's declared stdin.
- **`yield` stdout contract**: The type checker verifies that every `yield`ed
  value matches the declared stdout type.

## standard streams: &0, &1, &2

The three standard streams use file-descriptor syntax: `&0` (stdin), `&1`
(stdout), `&2` (stderr). `&0` is a readable value expression; `&1`/`&2` are write
streams written to with `yield`.

Lexer: `&` followed by a digit lexes as an `.fd` token (the redirect `>&` is
handled separately, so this never collides with redirects). AST: `FdExpr`.

## yield — pushing values to a stream

Output is explicit. `yield expr` writes `expr` to stdout (`&1`); `yield &2 expr`
writes to stderr. The function's `return`/body value is **not** auto-pushed. A
stage that consumes its input without `yield`ing produces no stdout output
(subprocess writes such as `echo` are independent). `return` is for control flow
and the function's exit value.

```rn
fn Int square() Int {
    yield &0 * &0
}
```

Implementation: `yield` compiles to a `pipe_write` to the target stream's pipe
(stack slot 1 for stdout, 2 for stderr; `materializeString`, then flush). The
evaluator's `exit_with` no longer writes the return value to stdout — it only
sets the exit code (non-coercible values exit with success). The pipeline
compiler no longer auto-pushes a stage's value. Only `yield &1` is constrained
by the declared stdout type; `yield &2` carries untyped diagnostics.

## &0 — reading typed stdin as a value

`&0` reads the function's stdin pipe and returns it as a value (a `String`, or a
parsed `Int` for an `Int` stdin). It is meaningful inside a function with a
non-Void stdin type, or in a block/expression pipeline stage (whose stdin type
is inferred from the upstream).

```rn
fn String transform() String {
    const received = &0
    yield "${received}!"
}
```

Implementation: `collect_stdin` IR instruction — marks the stdin pipe as
`has_been_connected=true` (letting the upstream stream thread exit) then
reads `buffer_writer.written()` (or the in-process typed value) once
`keep_open=false` is set.

### consuming reads

`&0` is a consuming read: each read takes the next value off the stream. The
context tracks consumed pipe handles (`consumed_pipes`); after the first read,
a subsequent `collect_stdin` on a closed pipe returns EOF (`.null`) instead of
re-reading. `parse_int` passes `.null` through, and `yield` of a `.null` value
emits nothing. So `&0` read once per value; reuse a value with `const n = &0`.
(`&0 * &0` reads two values — the second is EOF for a single-value producer.)

### consuming a live stream — `for (&0) |v|`

A producer that `yield`s N values over its lifetime is drained by the consumer
with `for (&0) |v| body`. Each iteration reads one value (the green thread
blocks until a value arrives or the producer closes); EOF (`.null`) ends the
loop. There is no precomputed iteration count, unlike the counted array/range
for-loop.

```rn
fn Void produce() Int { yield 1; yield 2; yield 3 }
fn Int double_each() Int { for (&0) |v| { yield v * 2 } }
produce | double_each   // 246
```

Implementation: `compileForLoop` detects a single `&0` source and delegates to
`compileFdForLoop`, which emits a read-test-bind loop — `collect_stdin`
(`+ parse_int` for an `Int` stdin) into a reused ref, `cmp .equal … null` to
detect EOF and `jmp` past the body, bind the capture, compile the body, and
`jmp` back. The capture's element type comes from `IRCompiler.stdin_type_stack`
(the enclosing function's stdin type).

Type-checking: `runForExpr` types the capture from the for-source. Yield
validation no longer walks the body separately — `runYield` validates each
`yield &1` against `TypeChecker.stdout_type_stack` (pushed per function body),
so a `yield` inside the loop resolves the capture `v` in the loop's child scope
rather than the function scope.

Per-value iteration applies to in-process typed streams (`Int`/`Float`). A
`String`/byte stream has no message framing, so `collect_stdin` returns the
whole accumulated buffer on the first read — `for (&0)` over a byte stream runs
a single iteration with the entire input. The `lines` builtin (below) frames a
byte stream into per-line values for explicit per-value processing.

## `lines` — framing a byte stream into per-line values

`lines` (`fn String lines() String`) reads its whole byte stdin, splits on `\n`,
and emits each non-empty line as a separate value onto its stdout, so a
downstream `for (&0)` / mapping stage processes one line at a time. The
`lines → downstream` boundary is forced to the typed (queue) path even though it
carries `String`s (`boundaryUsesTypedTransport` special-cases `lines`), so each
line is a distinct framed value rather than re-concatenated bytes.

Implementation: `lines` compiles to `collect_stdin` (read the whole byte blob)
followed by the `emit_lines` IR instruction, which splits `%r` on `\n` and
`enqueueTypedPipeValue`s each non-empty line (trimming a trailing `\r`) onto the
stdout pipe. No compiler-side loop is needed — the split + multi-enqueue happens
in the one instruction.

`parseInt` now **maps** over its input rather than reading a single value: it
compiles to an EOF-terminated loop (`collect_stdin`; break on `.null`;
`parse_int`; `yield`), so it works on a single value (`echo "10" | parseInt`)
*and* a framed stream (`… | lines | parseInt`, one `Int` per line). Custom
per-value filters use `for (&0) |v| { ... }`.

So `{ for (0..5) |i| echo i } | lines | parseInt | square` (with
`square = fn Int square() Int { for (&0) |in| { yield in * in } }`) emits
`0 1 4 9 16`.

Still future: streaming `lines` (it currently buffers all input before
splitting, since it waits for the byte stream to close) and other framers /
delimiters (e.g. split on a custom separator).

## executable boundary

Runic cannot infer each executable's real signature. The catch-all type used at
boundaries is:

```
fn String @(...String) ExecutionResult
```

For pipe boundary validation, `ExecutionResult` is mapped to `String` so that
a typed function following an executable must declare `String` stdin.

## coercions

- `T → ?T` (passing a value where an optional is expected): **implemented**. The
  value flows through the boundary unchanged; the downstream `&0` is typed
  as `?T`, so `&0 orelse default` type-checks and runs. The compiler tracks
  the enclosing function's declared stdin type via `IRCompiler.stdin_type_stack`
  to give `&0` the correct optional type.
- `T → E!T` (passing a value where an error union is expected): **accepted by
  the type checker**. The runtime path mirrors `T → ?T`, but cannot be exercised
  end-to-end yet because error-union stdin types (`fn E!String ...` /
  `fn !String ...`) do not parse in the current grammar. This will work once
  error declarations and error-union type expressions are wired into the parser.

Genuinely incompatible boundaries — differing concrete types (`String → Int`),
and Void mismatches (`Void → String`, `String → Void`) — are rejected with a
clear diagnostic naming both sides.

## non-string typed values and `parseInt`

Pipelines carry arbitrary typed values, not just strings. The wire format is
canonical text (decimal for `Int`/`Float`); each boundary's *type* tells the
downstream stage how to interpret the bytes:

- A stage whose stdin type is `Int` has its `&0` parsed into an `Int`
  value (compiled as `collect_stdin` followed by a `parse_int` instruction), so
  arithmetic like `&0 * 2` works.
- A stage that returns an `Int`/`Float` serializes it to decimal text on its
  stdout pipe (`exit_with` handles `.uinteger`/`.float`). `Bool` is represented
  as `.exit_code` and stays exit-code based, so `match` predicates are unaffected.
- The `parseInt` builtin (`fn String parseInt() Int`) bridges a `String` stage
  to an `Int` stage. A user-defined `parseInt` in scope takes precedence.

Example: `echo "10" | parseInt | doubler | inc` evaluates to `21`.

Failure modes: `parseInt` / `Int`-typed `&0` on non-numeric input fails at
runtime with a single, source-located diagnostic naming the offending value
(`[error]: <file>:<line>:<col>: cannot parse "abc" as Int`) and a non-zero exit;
the generic runtime/CLI error footers are suppressed for this case so the user
sees one clean message. `parseInt` returns `Int` (not `?Int` / `E!Int`) for now,
so the failure aborts rather than producing a recoverable optional/error value;
optional/error-union return types depend on error-declaration parsing.

## runtime transport

Boundaries that touch an executable (or carry a `String`) use the byte-pipe
path: values travel as canonical text through the pipe's `buffer_writer`, and
`&0`/`collect_stdin` reads them after the upstream signals completion via
`keep_open=false`. A non-waitable typed stage (such as `parseInt`) writes via
`pipe_write`, which flushes so the bytes are visible to the downstream
`collect_stdin`.

### in-process typed transport

An exact boundary that carries a by-value scalar (`Int`/`Float`, no executable on
either side) skips serialization entirely. `compilePipeline` marks the
inter-stage pipe `typed` (`pipe_opt ... typed`). At runtime:

- `yield` to a `typed` pipe stores the value in `context.typed_pipe_values`
  (keyed by the pipe handle) and writes **no** bytes.
- `&0`/`collect_stdin` on a `typed` pipe returns that value directly (no parse).
  The value stays in the map, so `&0` is re-readable (e.g. `&0 * &0`).
- `parse_int` passes a value through unchanged when it is already an `Int`, so
  an `Int`-stdin function works whether it received bytes (from `parseInt`/an
  executable) or an in-process `Int`.

So `echo "10" | parseInt | doubler | inc` byte-transports only `echo → parseInt`;
the `parseInt → doubler → inc` boundaries pass `Int`s in-process.

`String` boundaries keep the byte path (a `String` is already bytes). Extending
in-process transport to structured values (arrays, structs) is future work.

`parseFloat` (`fn String parseFloat() Float`) is the `Float` counterpart of
`parseInt` — same per-value mapping (it shares `compileParseMapStage`, emitting
`collect_stdin` / EOF-break / `parse_float` / `yield`), so `Float` pipelines run
end-to-end: `{ echo "1.5"; echo "2.5" } | lines | parseFloat | square` (with a
`for (&0)` filter) emits `2.256.25`. Bad input fails with the same single,
source-located diagnostic as `parseInt` (`cannot parse "x" as Float`,
`Error.InvalidFloat`). Whole-number results serialize without a decimal point
(`3.0` → `3`), matching Runic's existing `{}` float formatting.

#### block stages and transport classification

A boundary is classified by `classifyBoundary` → `classifyStageOutputKind` /
`classifyStageInputKind`. Named functions report their kind from the declared
signature; a **block** stage (`{ ... }`) has no signature, so the output kind is
inferred from what it `yield`s to `&1` (`inferBlockStdoutType`) and the input
kind is treated as permissive (a block adapts its `&0` to the upstream). So
`{ yield 1; ... } | { yield &0 }` is recognized as an `Int` boundary and gets
the typed queue path (per-value framing + live reads), the same as
`producer | consumer` between named typed functions. An executable on either
side still forces the byte path. When a block's yield type cannot be inferred,
the boundary falls back to the byte path.

The inference **recurses** into nested `for`/`while`/`if`/`match`/block bodies
(via `findStdoutYieldType`), so a `yield` inside a loop still determines the
block's output type — `{ for (0..5) |i| { yield i } }` is an `Int` stage. It
also tracks the in-scope loop captures (`InferCapture`): a range source iterates
`Int`, so `yield i` (which parses as a zero-arg call to `i`) and arithmetic on it
(`yield i * i`) resolve to `Int`. Captures over non-range sources are left
unknown (byte fallback). `inferYieldValueType` handles literals, numeric
arithmetic, `&0`, identifiers/zero-arg calls (captures and bindings); anything
else is unknown.
