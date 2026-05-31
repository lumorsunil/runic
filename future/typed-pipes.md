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

## yield — pushing values to stdout

Output is explicit. `yield expr` writes `expr` to the function/stage's stdout
stream; the function's `return`/body value is **not** auto-pushed. A stage that
consumes its input without `yield`ing produces no stdout output (subprocess
writes such as `echo` are independent and still go to stdout). `return` is for
control flow and the function's exit value.

```rn
fn Int square() Int {
    yield @stdin * @stdin
}
```

Implementation: `yield` compiles to a `pipe_write` to the thread's stdout pipe
(`materializeString`, then flush). The evaluator's `exit_with` no longer writes
the return value to stdout — it only sets the exit code (non-coercible values
exit with success). The pipeline compiler no longer auto-pushes a stage's value.

## @stdin — reading typed stdin as a value

The `@stdin` built-in expression reads all bytes from the function's stdin pipe
and returns them as a `String` value (or a parsed `Int` for an `Int` stdin). It
is available only inside functions that declare a non-Void stdin type.

```rn
fn String transform() String {
    const received = @stdin
    yield "${received}!"
}
```

Implementation: `collect_stdin` IR instruction — marks the stdin pipe as
`has_been_connected=true` (letting the upstream stream thread exit) then
reads `buffer_writer.written()` once `keep_open=false` is set.

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
  value flows through the boundary unchanged; the downstream `@stdin` is typed
  as `?T`, so `@stdin orelse default` type-checks and runs. The compiler tracks
  the enclosing function's declared stdin type via `IRCompiler.stdin_type_stack`
  to give `@stdin` the correct optional type.
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

- A stage whose stdin type is `Int` has its `@stdin` parsed into an `Int`
  value (compiled as `collect_stdin` followed by a `parse_int` instruction), so
  arithmetic like `@stdin * 2` works.
- A stage that returns an `Int`/`Float` serializes it to decimal text on its
  stdout pipe (`exit_with` handles `.uinteger`/`.float`). `Bool` is represented
  as `.exit_code` and stays exit-code based, so `match` predicates are unaffected.
- The `parseInt` builtin (`fn String parseInt() Int`) bridges a `String` stage
  to an `Int` stage. A user-defined `parseInt` in scope takes precedence.

Example: `echo "10" | parseInt | doubler | inc` evaluates to `21`.

Failure modes: `parseInt` / `Int`-typed `@stdin` on non-numeric input raises a
runtime `InvalidInt` error. `parseInt` returns `Int` (not `?Int` / `E!Int`) for
now; optional/error-union return types depend on error-declaration parsing.

## runtime transport

All pipeline boundaries use the byte-pipe path: values travel as canonical text
through the pipe's `buffer_writer`, and `@stdin`/`collect_stdin` reads them after
the upstream signals completion via `keep_open=false`. A non-waitable typed
stage (such as `parseInt`) writes via `pipe_write`, which flushes so the bytes
are visible to the downstream `collect_stdin`.

A "skip serialization" optimization (passing in-process `Value`s directly
without text conversion) remains future work; it would mainly benefit large or
structured values where decimal/utf-8 round-tripping is wasteful.
