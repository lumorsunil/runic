# error handling

> **Status: implemented and shipped in 0.4.0.** This was the original proposal;
> it has been updated to reflect what was actually built. The canonical
> user-facing reference is the "Errors as first-class types" section of
> `docs/features.md`; the detailed implementation record is
> `error-handling-plan.md` (and `pipeline-errors-plan.md` for the pipeline
> cluster). Runnable showcase: `examples/error_handling.rn`.

## introduction

Zig-like error handling, with both values and types. Define an error set, then
use it in a binding or as a function's return type. A function produces its
result — including an error — via `yield` (Runic has no `return`):

```runic
const ParseIntError = error { ExpectedNumber }

fn String parseLevel() ParseIntError!Int {
  yield ParseIntError.ExpectedNumber
}
```

An error set may also be left to inference with a leading `!T`, which collects
the variants the body actually produces:

```runic
fn Void mayFail() !String { ... }   // error set inferred from the body
```

## `catch`

`const level = echo "1234" | parseInt catch 0`

`catch` takes an error-union value: if it holds an error it evaluates to the
handler (`0` above), otherwise to the unwrapped ok value. So
`ParseIntError.ExpectedNumber catch "hello"` is `"hello"`, while
`"success" catch "hello"` is `"success"`. The handler can bind the error with
`|err|`. Applying `catch` to a non-error value is a type error.

## `||`

`||` is shorthand for "catch and discard the error", yielding the fallback:
`const name = lookupName || "anonymous"`.

## `try`

`try` propagates an error out of the enclosing function (re-yielding it), else
evaluates to the ok value:

```runic
fn Void run() ParseError!Int {
  const level = try (echo "1234" | parseInt)
  yield level * 2
}
```

The enclosing function must **cover** every error `try` propagates — its return
type has to be an error union whose set includes those variants (or an inferred
`!T`). A `try` whose error isn't covered, or a top-level `try` (nothing to
propagate to), is a compile error.

## mandatory handling

An error that is produced but neither handled (`catch`/`||`) nor propagated
(`try`) is a **compile error**, so failures are never silently dropped. At the
top level there is nothing to propagate to, so the error must be caught. Commands
keep the implicit exit-code model — their `ExecutableError` is exempt, so a bare
`ls` or `echo "x" | grep "y"` doesn't require a `catch`.

## executable calls

A command's value view is an error union with `ExecutableError` as the set
(`NonZeroExit` / `Signalled` / `SpawnFailed`):

```runic
const result: ExecutableError!String = echo "this should succeed"
```

The builtins `parseInt` / `parseFloat` return `ParseError!Int` / `ParseError!Float`.

## pipelines

A pipeline is `pipefail`-style: if any stage yields an error, the whole pipeline
evaluates to that error for a trailing `catch`/`||`/`match`/`try` to handle. As in
bash the stages still run concurrently — a stage erroring doesn't forcibly stop
the others; the error simply becomes the pipeline's value.

## errors with values

An error variant may carry a typed payload:

```runic
const MyError = error {
    UnknownError,
    ErrorWithMessage: String,
}
```

`MyError` behaves like a Zig `union(enum)`. Construct a payload-less variant with
`MyError.UnknownError`, and a payloaded one with `MyError{ .ErrorWithMessage = "..." }`:

```runic
fn Void givesError() MyError!Void {
    yield MyError{ .ErrorWithMessage = "This is the error message." }
}
```

`match` dispatches on the variant and binds the payload. Matching an error set is
exhaustive (every variant, or a `_` case), and the structured error value is
preserved across the in-process call boundary so `match`/`catch`/`if`/`||` see the
real variant, not its flattened text:

```runic
fn Void program() Void {
    givesError catch |err| match err {
        MyError.UnknownError => echo "Error: Unknown error" >&2
        MyError.ErrorWithMessage => |message| echo "Error: ${message}" >&2
    }
}
```

## error-set merge

`A || B` between two error sets builds a merged set whose variants are the union
of both (chains as `A || B || C`; a shared variant name must carry a compatible
payload). The same `||` spelling between non-error types is a general
[sum type](./sum-types-plan.md).
