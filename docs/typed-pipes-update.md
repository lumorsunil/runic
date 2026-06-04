# Typed pipes — update summary

This document is a narrative overview of the **typed pipes** work. It complements
the per-change list in [`CHANGELOG.md`](./CHANGELOG.md), the language-surface
reference in [`features.md`](./features.md), and the design/implementation notes
in [`../future/typed-pipes.md`](../future/typed-pipes.md). For a runnable tour,
see [`../examples/typed_pipelines.rn`](../examples/typed_pipelines.rn).

## What changed, in one paragraph

Pipelines are now **typed and explicit**. A function names what it reads and
writes (`fn StdinType name() StdoutType`); the three standard streams use
file-descriptor syntax (`&0`/`&1`/`&2`); output is written explicitly with
`yield`; and the type checker validates that the upstream stdout type matches the
downstream stdin type at every `|`. On top of that, values (not just bytes) flow
across boundaries, producers can stream many values that consumers drain one at a
time, and a few ergonomics and operator-overloading rough edges were resolved.

## Explicit streams and output

- **File-descriptor streams `&0` / `&1` / `&2`.** `&0` is a readable value
  expression (the stage's stdin); `&1`/`&2` are write streams. This replaced the
  earlier `@stdin`.
- **`yield` for output.** `yield expr` writes to stdout (`&1`); `yield &2 expr`
  writes to stderr. A function's `return`/body value is **no longer** auto-pushed
  to stdout, so a stage that only consumes (or only runs a side-effecting
  command) produces no stdout. Only `yield &1` is constrained by the declared
  stdout type; `yield &2` carries untyped diagnostics.

```rn
fn Int square() Int {
    const n = &0
    yield n * n
}
echo "6" | parseInt | square   // 36
```

## Typed boundaries and value transport

- **Type-checked at every `|`.** The upstream stdout type must match the
  downstream stdin type; mismatches are compile-time errors that name both sides.
- **Arbitrary typed values, not just strings.** Non-`String` values flow across
  boundaries. An exact scalar boundary (`Int`/`Float`, no executable on either
  side) uses **in-process typed transport** — the value is passed by-value
  through a side channel instead of being serialized to text and re-parsed.
  `String`/executable boundaries keep the byte path.
- **`parseInt` / `parseFloat` bridges.** `parseInt` (`fn String parseInt() Int`)
  and `parseFloat` (`fn String parseFloat() Float`) turn a byte/`String` stream
  into typed values, so `echo "10" | parseInt | doubler` works. Both *map* over
  their input, one result per input value, so they compose with multi-value
  streams. Bad input fails with a single, source-located diagnostic
  (`cannot parse "abc" as Int`) instead of a raw error dump.
- **Coercions.** `T → ?T` is implemented end-to-end; `T → E!T` is accepted by the
  type checker (runtime pending error-union stdin parsing).

## Multi-value streaming

- **Producers yield many values; consumers drain with `for (&0)`.** A stage can
  `yield` repeatedly over its lifetime; the downstream reads one value per
  iteration with `for (&0) |v| { ... }`, blocking until a value arrives or the
  producer closes (EOF ends the loop). Delivery is live — values arrive as the
  producer emits them.

```rn
fn Void produce() Int { yield 1; yield 2; yield 3 }
fn Int double_each() Int { for (&0) |v| yield v * 2 }
produce | double_each   // 246, as they arrive
```

- **`&0` is a consuming read.** Each read takes the next value; after the
  producer closes, a further read is EOF (`.null`). Reuse a value by binding it
  (`const n = &0`).
- **`lines` frames a byte stream.** An executable's output is unframed, so a
  single `&0` read consumes the whole buffer. `lines` (`fn String lines() String`)
  splits its stdin on `\n` and emits each non-empty line as a separate value, so
  `{ … } | lines | parseInt | filter` processes one line at a time. `lines`
  followed by a byte consumer (`lines | cat`, `lines | grep`) writes the lines
  back as a newline-delimited byte stream.

## Operator and syntax refinements

- **`>` is overloaded by operand.** Because `>` is both the redirect and the
  greater-than operator, the parser leaves it unresolved and the compiler decides
  from the **left operand**: a command (an external executable call, a Runic
  function call, a block, or a subshell) makes it an output redirect; a value
  makes it the comparison. So `echo "x" > "f"` and `myFn > "f"` redirect, while
  `n > 2` and `count > limit` compare. `>>`/`>&` are always redirects. (This
  fixed a crash where `if (n > 2)` was parsed as "redirect `n` to a file named
  `2`".)
- **Function-call redirects.** A Runic function call's stdout can be redirected
  like any command — `myFn > "file"` (truncate), `myFn >> "file"` (append).
- **Bare control-flow bodies.** A `for` or `if`/`else` body may be a bare
  statement (a single `yield`/`return`/`exit`, or a bare expression) without
  `{ }`: `for (&0) |v| yield v * 2`, `if (cond) yield a else yield b`.

## Notable fixes

- A producer block whose `yield` is nested in a loop/`if`/`match`
  (`{ for (0..5) |i| { yield i } } | square`) is now recognized as a scalar
  stage and framed per value, instead of falling back to the byte path and
  concatenating into one value.
- `yield <binding>` (e.g. `for (&0) |v| { yield v }`) no longer crashes the
  compiler — `yield` only cleans up temporaries it actually pushed, not borrowed
  references.
- `lines | <executable>` no longer silently drops all data.

## Known limitations / follow-ups

- Redirecting a **function** call's stdout uses `>`, so to compare a function's
  return value you must bind it first (`const r = myFn; if (r > 2) ...`).
- `lines` buffers its whole input before splitting (not yet line-by-line live).
- Per-value framing for `String` streams is opt-in via `lines`; raw byte streams
  read whole.
- Out of scope and pre-existing: `while` is not yet parsed; untyped function
  parameters error in the type checker.
