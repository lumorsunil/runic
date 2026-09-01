# Runic Language Features

This document captures the current surface area of the Runic language and mirrors the guiding principles outlined in the README. Each feature includes a short example to illustrate the syntax and the expected outcome so experimenters can quickly understand how scripts behave.

## Command-first execution

Shell-friendly commands remain the primary abstraction. Every bare word starts a command, and pipes behave like bash but surface structured results.

```rn
echo "hello world" | tr "a-z" "A-Z"
```

**Result:** Prints `HELLO WORLD` to STDOUT while capturing exit codes for each pipeline stage so they can be inspected programmatically.

## Strong, predictable data semantics

Variables are typed, immutable by default, and only change through explicit `var` declarations. Array literals use Zig-style anonymous syntax that sidesteps legacy word-splitting quirks. (There is no map literal; use `std.map` — see the standard-library docs.)

```rn
const greeting = "hello"
var count = 2
const items = .{ "apples", "oranges" }
```

**Result:** `greeting` cannot be reassigned, `count` can be incremented intentionally, and the literal preserves its structure when passed between commands or functions.

Type annotations are available when you need to nail down a binding’s shape. Append `: Type` after the variable name so the compiler can enforce conversions up front. Type names always begin with a capital letter (`String`, `Int`, `Float`, `Bool`, `Void`).

```rn
const greeting: String = "hello"
var retries: Int = 2
const scores: []Int = .{ 90, 80 }
```

**Result:** Each declaration advertises its type at the point of definition, catching mismatches such as assigning a string to `retries` before the script ever runs.

### Numeric operators

Runic has the usual arithmetic (`+`, `-`, `*`, `/`, `%`), plus exponent `**` and the bit shifts `<<` / `>>`.

```rn
const area = width ** 2      // exponent (2 ** 10 == 1024)
const kib = size << 10       // shift-left  (multiply by 1024)
const half = total >> 1      // shift-right (arithmetic, sign-extending)
echo "${9 ** 0.5}"           // 3 — a fractional/negative exponent widens to Float
```

**Result:** `**` and the shifts keep integer operands `Int` — exponent saturates at the `Int` bounds on overflow, and shifting by 64 or more clears the value; a `Float` operand or a negative exponent widens `**` to `Float`. `**` binds tighter than `*`, and the shifts sit just below `+`/`-`; both shifts share one tier, so a run of same-tier operators is left-associative (`2 ** 3 ** 2` is `(2 ** 3) ** 2 == 64`, and `1024 >> 3 << 1 == 256`). Note `>>` is overloaded with append-redirect — see [*File descriptor redirects*](#file-descriptor-redirects) — but a value on the left always makes it the shift.

### Environment variables

Environment variables are explicit. Use `$NAME` to read an environment entry as `?String`, and `$NAME = ...` to update the current subshell context so later child processes inherit the new value. Bare identifiers like `HOME` are normal Runic bindings and are distinct from `$HOME`.

```rn
const HOME = "binding-home"
echo "${HOME}"
echo "${$HOME orelse "missing"}"

$HOME = "/tmp/runic-home"
printenv "HOME"

const nested = $({
  $HOME = "/tmp/runic-nested-home"
  printenv "HOME"
})

printenv "HOME"
```

**Result:** `HOME` prints `binding-home`, `$HOME` reads from the process environment, the outer `printenv` sees `/tmp/runic-home`, the nested subshell sees `/tmp/runic-nested-home`, and once the subshell exits the parent context still reports `/tmp/runic-home`.

### Builtin `cd`

`cd` is a builtin rather than an external process. It updates the working directory stored on the current subshell context, which means later commands in that same context inherit the new directory without mutating the host process running Runic.

```rn
cd "/tmp"
pwd

const nested = $({
  cd "/"
  pwd
})

pwd
echo "${nested}"
```

**Result:** The first `pwd` prints `/tmp`, the nested subshell prints `/`, and once the subshell exits the parent context still reports `/tmp`. Calling `cd` with no argument uses the current subshell's `HOME` value.

### Executables outside `PATH`

A bare command word (`git`, `ls`) is looked up on `PATH` or in the current
directory, so a program at a *sub*path — `scripts/start`, `tools/build/gen` —
can't be named as a bare word (`scripts/start` would parse as the division
`scripts / start`). Runic offers two unambiguous ways to run one:

**Path sigil.** A command word that *starts* with a path sigil — `./`, `../`,
or `/` — is parsed whole as a single executable path, arguments follow as usual:

```rn
./scripts/start "arg0" "arg1"     // relative to the current directory
../tools/build/gen "out"          // parent-relative
/usr/bin/env                      // absolute
```

The sigil requirement is what keeps it unambiguous: `/`, `./`, and `../` can't
begin an ordinary expression, so `a / b` (division) and `a.b` (member access)
are unaffected — only a leading sigil selects the path form. A bare relative
subpath (`scripts/start`, no sigil) still parses as division; write `./scripts/start`
or use `run`.

**`run` builtin.** When the path is computed at runtime — a variable, an
interpolated string, or anything a static command word can't express — `run`
takes the executable as its first argument and the rest as its arguments:

```rn
run "scripts/start" "arg0"        // literal path (no sigil needed)
const bin = "tools/build/gen"
run bin "out"                     // path from a variable
const version = run bin "--version"   // captures output like any command
```

`run` produces the same `ExecutableError!String` value view as any command, so
its result can be captured, piped, and error-handled identically. (A local
binding named `run` shadows the builtin.)

### Errors as first-class types

Runic treats `error` like Zig: an error set is a type, declared with `const`, whose variants may be opaque or carry a typed payload.

```rn
const NetworkError = error { Timeout, ConnectionLost }
const FileError = error {
  NotFound,
  PermissionDenied: String,   // a variant may carry a payload
}
```

A function that can fail returns an **error union** `E!T` (an error from set `E`, or a value of type `T`). The error set may also be left to inference with a leading `!T`:

```rn
fn Void parseLevel() FileError!Int { ... }   // FileError or Int
fn Void mayFail() !String { ... }            // error set inferred from the body
```

**Constructing error values.** Use `Set.Variant` for a payload-less variant, and `Set{ .Variant = payload }` when it carries one:

```rn
const e1 = FileError.NotFound
const e2 = FileError{ .PermissionDenied = "alice" }
```

**Merging error sets.** Combine sets with `||` (as in Zig) to get a set whose variants are the union of both — useful for a function that can fail in several ways, or for merging the builtin `ExecutableError`:

```rn
const IoError = NetworkError || FileError              // Timeout, ConnectionLost, NotFound, PermissionDenied
const Merged  = ExecutableError || FileError           // command failures *and* file errors
fn Void fetch() IoError!String { ... }
```

Merges chain (`A || B || C`) and dedup a shared variant name (its payload type must match). A merged set that carries any non-command error is *not* exempt from handling — only a set whose variants are all `ExecutableError`'s keeps the implicit exit-code model. (Merging arbitrary non-error types — general sum types like `Int || String` — is not implemented yet.)

**Result:** Error sets are inspectable data. Authors define terse opaque variants or attach a payload when extra context matters, and callers branch on failures structurally instead of comparing strings or shell exit codes.

#### Handling errors: `catch`, `||`, and `try`

`catch` consumes an error union: if it holds an error, the result is the handler; otherwise it is the unwrapped ok value. The handler may bind the error with `|err|`.

```rn
const level = parseLevel catch 0                    // default on error
const level2 = parseLevel catch |err| { echo "bad: ${err}"; exit 1 }
```

`||` is shorthand for "catch and discard the error", yielding the fallback:

```rn
const name = lookupName || "anonymous"
```

`try` propagates the error out of the enclosing function, otherwise evaluates to the ok value:

```rn
fn Void run() FileError!Int {
  const cfg = try parseLevel    // on error, run returns it; otherwise cfg is the Int
  yield cfg * 2
}
```

The enclosing function **must cover** every error `try` propagates — its return type has to be an error union whose set includes those variants (or an inferred `!T`, which collects them). Propagating an error a function does not declare is a compile error: a `try` in a function whose return type is not an error union, or whose set is missing the variant, is rejected. A top-level `try` has no enclosing function to propagate to, so it must be `catch`'d instead. (`ExecutableError` from a command is exempt — commands keep the exit-code model.)

#### Mandatory explicit handling

An error that is produced but neither handled (`catch`/`||`) nor propagated (`try`/yielded from an error-returning function) is a compile error, so failures are never silently dropped. This applies wherever the error escapes as a statement's value — a bare error value, a call to an error-returning function, or a pipeline whose final stage yields an error union. At the top level there is nothing to propagate to, so the error must be caught. (Commands keep the exit-code model: their `ExecutableError` is exempt, so a bare `ls`/`echo "x" | grep "y"` does not require a `catch`.)

```rn
const E = error { Bad }
E.Bad           // error: error is not handled; use catch (or || to discard) or propagate it
E.Bad catch 0   // ok
```

**Strict mode (`--strict`, or `-e`).** By default a command's `ExecutableError`
is exempt (bash-like: a bare `ls` runs and its failure is ignored). Passing
`--strict` removes that exemption — a command failure must be handled too, just
like a user error. It's a `set -e`-style opt-in, off by default, so existing
scripts are unaffected.

```rn
ls "/missing"                     // default: runs, failure ignored
                                  // --strict: error — must handle it
const out = ls "/missing" catch "" // ok under --strict
```

#### Inspecting variants with `match`

`match` dispatches on an error's variant and can capture the payload:

```rn
const FileError = error { NotFound, PermissionDenied: String }

opError catch |err| match (err) {
  FileError.NotFound => { echo "missing" },
  FileError.PermissionDenied => |user| { echo "denied for ${user}" },
  _ => { echo "other failure" },
}
```

#### Commands are error unions

Every executable call has the value view `ExecutableError!String`: a zero exit code yields the captured output, any other exit yields an `ExecutableError`. This makes command failures catchable like any other error (while `ExecutionResult` — `.exit_code`, `.wait`, etc. — remains available as the explicit handle):

```rn
const ExecutableError = error { NonZeroExit: Int, Signalled: Int, SpawnFailed }

const matched = grep "needle" "haystack.txt" catch "no match"
const out = (build | tee "build.log") catch "build failed"
```

**Result:** Commands, user functions, and pipelines all surface failures through the same `catch`/`try`/`||`/`match` mechanics, instead of `set -e` and exit-code conventions.

### Exact-value `match`

Runic also supports `match` as an expression for exact-value dispatch. In the current implementation, patterns are limited to literals and `_`, and the first matching case wins.

```rn
const label = match ("runic") {
  "zig" => { "compiler" }
  "runic" => { "shell" }
  _ => { "unknown" }
}

echo "${label}"
```

**Result:** `label` evaluates to `shell`. Cases are tested in order, `_` acts as the fallback, and each case body is a block expression whose final value becomes the match result. Capture clauses and richer matcher forms are planned, but not available yet.

### Optional data that behaves like Zig

Any type can be wrapped in an optional using Zig-style `?T` syntax, and `null` denotes the absence of a value. Use `orelse` to provide a fallback value when an optional is empty.

```rn
const maybe_name: ?String = null
echo "${maybe_name orelse "fallback"}"

var maybe_count: ?Int = 7
echo "${maybe_count orelse 9}"

maybe_count = null
echo "${maybe_count orelse 9}"
```

**Result:** `?String` and `?Int` hold either a concrete value or `null`. `orelse` evaluates to the left-hand value when present and to the right-hand expression when the left side is `null`.

When you want a strict unwrap, use postfix `.?` to extract the payload from an optional value.

```rn
const maybe_name: ?String = "runic"
echo "${maybe_name.?}"
```

**Result:** `maybe_name.?` evaluates to the inner `String`. Applying `.?` to a non-optional value is rejected during checking.

Optionals also integrate with `if` capture clauses. When the condition is an optional, `if (value) |inner| ...` enters the then branch only when the optional is non-`null`, and `inner` is bound to the unwrapped payload inside that branch.

```rn
const maybe_name: ?String = "runic"

if (maybe_name) |name| {
  echo "name=${name}"
} else {
  echo "missing"
}
```

**Result:** The then branch runs only when `maybe_name` contains a string, and `name` is available only inside that branch as the inner `String`.

### Sum types: `A || B`

A **sum type** `A || B` (any number of members: `A || B || C`) describes a value that is *one of* the member types. Declare one with `const` and use it as an annotation, a parameter, or a return type:

```rn
const IntOrString = Int || String

const a: IntOrString = 42      // an Int widens in
const b: Int || String = "hi"  // so does a String (inline form)
```

Sums are **unordered sets** and are normalized: `Int || String` is the same type as `String || Int`, and nested/duplicate members are flattened (`(Int || String) || Int` is just `Int || String`).

**Widening is implicit; narrowing is explicit.** A value of a member type flows *into* a sum automatically, but you cannot use a bare sum as if it were one specific member — you must **narrow** it first. Operations that assume a concrete type (arithmetic, string interpolation, …) are rejected on an un-narrowed sum:

```rn
const x: Int || String = 5
echo "${x}"        // error: cannot interpolate a sum directly; narrow it first
echo "${x + 1}"    // error: cannot use a sum in arithmetic; narrow it first
```

**Narrowing with `is`.** The `x is T` operator is a runtime type test (it works on any value, not just sums) that evaluates to `Bool`. In an `if`, it refines the binding's type per branch — the then branch sees `T`, and the else branch sees the remaining members (a two-member sum collapses to the survivor):

```rn
const x: Int || String = 5
if (x is Int) {
  echo "doubled=${x + x}"   // x is Int here
} else {
  echo "string: ${x}"        // x is String here
}
```

**Narrowing with comparisons.** `==`/`!=` and the relational operators (`<`, `>`, `<=`, `>=`) also narrow. `x == v` narrows the then branch to the members `v` could be; relational operators narrow to the numeric members. Comparing a sum against a value it can never equal (a non-member) is rejected as a likely mistake.

```rn
const x: Int || String = 0
if (x == 0) { echo "int=${x + 1}" } else { echo "other" }  // then branch: x is Int
```

**Narrowing with `match`.** A `match` on a sum dispatches on the member type; each case narrows the subject inside its body, and the match must be exhaustive over the members unless a `_` case is present:

```rn
const x: Int || String || Float = 6
match x {
  Int   => echo "int=${x + 1}"
  Float => echo "float=${x}"
  _     => echo "other"
}
```

A case may bind the narrowed value with a `|name|` capture — handy when the subject isn't a plain binding (e.g. a direct call):

```rn
match readValue() {
  Int    => |n| echo "int ${n}"
  String => |s| echo "string ${s}"
}
```

**`var` bindings narrow too.** A mutable sum binding narrows in branch conditions just like a `const`, and a reassignment refines its type from that point on — while the *declared* type still governs what may be assigned (so you can reassign across members):

```rn
var v: Int || String = 1
v = "text"            // v is String from here
echo "str=${v}"
v = 99                // v is Int again
echo "doubled=${v + v}"
```

**Functions** can take and return sums; the member's concrete value is preserved across the call, so the caller can narrow the result:

```rn
fn Void pick(b: Bool) Int || String {
  if (b is Bool) { yield 7 } else { yield "x" }
}
const r = pick true
if (r is Int) { echo "got int ${r}" } else { echo "got string ${r}" }
```

**Result:** A sum type is a single value slot that may hold any of its members; the compiler tracks which member it currently is (its *flow type*) and forces you to narrow — with `is`, a comparison, or `match` — before performing any member-specific operation, so a sum value is never silently mistaken for one branch.

**Precedence with `?`, `[]`, and `!`.** A type constructor captures a following `||` into its operand, so `?Int || String` parses as `?(Int || String)` (an optional sum) and `[]Int || String` as `[]( Int || String )` (an array of sum elements). A bare top-level `A || B` folds into a sum. Use parentheses for any other grouping — `(?Int) || String` is a sum whose members are `?Int` and `String`. (Narrowing with `is`/`match` recognizes the primitive members `Int`, `Float`, `Bool`, and `String`; a member that is itself an optional or array isn't narrowable by type test.)

> Note: `A || B` between two **error sets** instead builds a merged error set (the union of their variants) — see *Errors as first-class types*. General sum types and error-set merges share the `||` spelling but only error-set operands merge into a set; any other operand produces a sum.

### Background commands and `.wait`

Runic supports bash-style background execution with a trailing `&`. In statement position the command continues in the background while the script moves on immediately. When you bind a background execution, the command's stdout/stderr are captured in memory just like any other bound execution result, and `.wait` blocks until the background work finishes.

```rn
(sleep 0.05; echo "warmup") &
echo "continued"

const job = (sleep 0.05; echo "hello from job" &)
echo "before wait"
job.wait
printf "%s" "${job.stdout}"
```

**Result:** The first background block runs without blocking the next statement. The bound `job` stays silent while it is running, `job.wait` waits for completion, and `job.stdout` exposes the buffered output afterward.

## Structured flow control

Blocks rely on indentation or braces with keyword-driven control structures so scripts read like modern languages instead of `then/fi` pairs.

```rn
fn Void describe(count: Int) Void {
  if count > 1 {
    echo "plural"
  } else {
    echo "singular"
  }
}
```

**Result:** Functions and conditional branches behave predictably, returning explicit values and avoiding bash’s implicit status codes.

## Native iteration constructs

`for` and `while` statements consume any iterator the runtime exposes, so streaming APIs and collections share the same loop syntax. Loops use Zig-style capture clauses to bind each yielded value (and optional index) to a local name.

```rn
const fruits = .{ "apple", "banana", "pear" }

for (fruits, 0..) |fruit, idx| {
  echo "${idx}: ${fruit}"
}
```

**Result:** Iteration works uniformly across arrays and ranges without manual indexing, and the capture clause makes loop variables explicit without leaking bindings outside the block.

A `for` or `if`/`else` body does not have to be a block. It may be a bare
expression or a single `yield`/`exit` statement, which avoids `{ }` for
one-liners:

```rn
for (0..3) |i| echo "${i}"           // bare command
{ for (0..5) |i| yield i } | square  // bare yield as a pipeline producer
if (ready) yield value else yield 0  // bare branches
```

### Condition loops with `while`

`while (condition) { … }` re-evaluates the condition at the top of each
iteration and runs the block until it is falsy. The condition is any truthy
expression — a `Bool`, a comparison, or a command's exit status — and the body
is a brace block whose bindings are loop-local (re-declared each pass). A
condition that starts false skips the body entirely; loops nest freely.

```rn
var i = 0
while (i < 5) {
  echo "i=${i}"
  i = i + 1
}
```

**Result:** `while` covers the open-ended loops `for` can't express — repeat
until a flag flips or a value crosses a threshold. (The body must be a brace
block.)

An optional condition can be unwrapped with a capture: `while (opt) |v| { … }`
loops while `opt` is non-null, binding the unwrapped value to `v` for that
iteration — the optional analogue of `if (opt) |v|`. Reassigning the optional in
the body drives the loop:

```rn
var cur: ?Int = 3
while (cur) |v| {
  echo "v=${v}"
  if (v <= 1) { cur = null } else { cur = v - 1 }
}
```

## Compile-time evaluation with `comptime`

`comptime <expr>` forces an expression to be evaluated at compile time and
folds it to a constant. It covers arithmetic and logic, and — crucially — can
interpret **pure user functions**: recursion, parameters, local `const`
bindings, and `if`/`match` control flow all run at compile time.

```rn
const size = comptime 4 * 1024        // 4096

fn Int fib(n: Int) Int {
  if (n < 2) { yield n }
  yield (fib (n - 1)) + (fib (n - 2))
}
const tenth = comptime fib 10         // folded to 55 during compilation
```

The result is an ordinary value, usable anywhere a constant is — including
inside larger expressions (`(comptime fib 7) * 2`).

**Result:** work that only depends on compile-time-known inputs is done once,
during compilation, instead of on every run. If the operand can't be reduced —
it reads a `var`, calls an impure/unknown function, or recurses past the
interpreter's depth limit — the program fails to compile with a clear error
rather than silently falling back to a runtime computation. (Without the
keyword, `fib 10` is an ordinary runtime call.)

### Type captures with `|T|`

A type capture `|T|` binds `T` to the type occupying that position, and `T` is
then usable anywhere a type is. In a binding it captures the initializer's
concrete type; in a function signature it acts as a generic type variable (one
definition, any argument type):

```rn
const seed: |T| = 7         // T is bound to Int
const n: T = 5              // reused as a type

fn Void say(greeting: |G|) Void {   // generic over the argument's type
  echo "${greeting}"
}
```

Nested under a built-in generic, `|T|` **destructures** — it matches the shape
and binds the inner type:

```rn
fn Void firstOf(xs: []|E|) E { yield xs[0] }   // E = the element type
var maybe: ?|M| = null                          // M = the child type
```

**Result:** a value's type can be named and propagated without spelling it out,
generic functions are written once, and mismatches are still caught
(`const bad: T = "x"` where `T` was captured as `Int` is a compile error). A
captured type is a purely compile-time entity — it never exists at runtime.
(This subsumes the earlier `@TypeOf`, which has been removed.)

### Generic type constructors

A type binding can take type parameters, defining a generic type constructor:

```rn
const Box(T) = struct { value: T }

const b: Box(Int) = Box{ .value = 5 }   // apply with Int
echo "${b.value}"
```

`Box(Int)` applies the constructor by substituting the argument. Combined with a
`|T|` capture, a signature **destructures** an application to recover its type
argument — so one function serves every instantiation:

```rn
fn Void unwrap(box: Box(|T|)) T { yield box.value }   // T = the element type
unwrap b   // → 5
```

Multiple parameters (`const Pair(A, B) = struct { first: A, second: B }`) and
composition (`[]Box(Int)`) work too.

**Result:** reusable container and wrapper types without repetition. Because the
runtime is dynamically typed, `Box(Int)` and `Box(String)` share a single layout
— there is no monomorphization; the type arguments exist only for compile-time
checking and capture. (Construction is currently the explicit `Box{ … }` form;
an inferred `.{ … }` literal typed by its target is a planned addition.)

### Serializing type identifiers

A type identifier used where a string is expected — in string interpolation or
as a bare command argument — serializes to the type's name. Named types give
their name; a bound `|T|` capture gives the captured type's name:

```rn
echo "${Int}"                     // Int
const Point = struct { x: Int }
echo "${Point}"                   // Point

const n: |T| = 42
echo "the type is ${T}"           // the type is Int
```

**Result:** since types are compile-time only, this is a compile-time string
constant — no runtime type information is involved. Inside a *generic function*,
a parameter's `|T|` reflects the concrete per-call type: a direct call is
**monomorphized** (a specialization is compiled with the type variables bound to
the argument types), so the same function reports the actual type at each call:

```rn
fn Void describe(x: |T|) Void { echo "${x} is a ${T}" }
describe 5       // 5 is a Int
describe "hi"    // hi is a String
```

Each distinct type argument gets its own specialization; the same type argument
reuses one. (Monomorphization currently applies to direct, non-recursive calls
whose `|T|` captures bind to a concrete argument type; other calls fall back to a
single generic compilation, where a bare `|T|` serializes to the variable name.)

## Command vs. expression separation

Runic distinguishes between invoking external commands and evaluating expressions, reducing quoting issues by making intent explicit.

```rn
const files = ls "./src"
```

**Result:** `ls` executes as a command; downstream helpers operate on typed lists so you can transform data without juggling quoting rules.

## Processes as first-class values

Starting a program returns a structured execution value. Binding a command captures stdout/stderr plus exit metadata, and appending `&` starts the work in the background while still producing a capturable execution value.

```rn
const sync_proc = git "status" "--short"
echo "${sync_proc.stdout}" // already finished, STDOUT buffered
echo "${sync_proc.stderr}"
echo "${sync_proc.exit_code}"

const combined = printf "hello\n" && printf "warning\n" >&2
echo "${combined.stdout}"
echo "${combined.stderr}"

const sequenced = printf "hello\n"; printf "warning\n" >&2
echo "${sequenced.stdout}"
echo "${sequenced.stderr}"

const async_proc = (sleep 0.05; echo "done" &)
async_proc.wait
echo "${async_proc.stdout}"
```

**Result:** Binding `const proc = <command ...>` executes the program synchronously and returns its buffered output plus exit metadata, read as `proc.stdout`, `proc.stderr`, and `proc.exit_code`. When command-producing expressions are chained with `&&`, `||`, or `;`, the resulting bound value still exposes the buffered `stdout`, `stderr`, and exit metadata from the evaluated expression. Appending `&` runs the work in the background; when you bind that value, its output is still buffered rather than printed immediately, and `.wait` blocks until it finishes.

### File descriptor redirects

Runic supports explicit stdout/stderr redirects for commands and preserves shell-style left-to-right redirect ordering when fd duplication is involved.

```rn
echo "out" > "out.log"      // truncate stdout to a file
echo "more" >> "out.log"    // append stdout to a file
echo "saved error" 2>"err.log"

echo "hello" 1>&2 2>"/dev/null"
```

**Result:** `>` / `>>` redirect stdout (truncate / append), `2>...` redirects stderr, and `1>&2` duplicates stdout onto the current stderr target. Because redirects are applied left to right, `echo "hello" 1>&2 2>"/dev/null"` still writes `hello` to the original stderr stream instead of discarding it. Redirecting a *function call* or a *block*'s output to a file — `myFn > "file"`, `{ … } > "file"` — is also supported, including when the body runs external commands (their real stdout is drained to the file), and both `>` and `>>` apply.

**`>` and `>>` are overloaded.** Since `>` is also the greater-than operator and `>>` the shift-right operator, Runic resolves each by its left operand: a **command** (an external executable call, a function call, a block, or a subshell) makes it an output redirect, while a **value** makes it the operator. So `echo "x" > "f"` / `echo "x" >> "f"` and `myFn > "f"` redirect (truncate / append), but `n > 2` compares and `bits >> 2` shifts. `>&` and the fd-prefixed forms (`1>`, `2>>`, …) are always redirects. To use a function's return value as an operand instead of redirecting it, bind it first: `const r = myFn; if (r > 2) ...`.

## Error-aware pipelines

A pipeline's result is an error union, so a trailing `catch`/`||`/`match`/`try` handles failure without relying on `set -e`. Error propagation is **`pipefail`-style**: if *any* stage yields an error (a failing command's `ExecutableError`, or e.g. `parseInt` producing a `ParseError`), the whole pipeline evaluates to that error — not just the final stage's status. As in bash, the stages run concurrently and a stage erroring doesn't forcibly halt the others (data already in flight is still processed); the error simply becomes the pipeline's value for a surrounding handler to catch:

```rn
const log = build | tee "build.log" catch "build failed"
if (build | tee "build.log") |output| {
  echo "ok: ${output}"
} else {
  echo "build failed"
  exit 1
}
```

**Result:** Pipeline failures are caught at the boundary and handled deterministically rather than silently ignored. (`catch` binds looser than `|`, so it applies to the whole pipeline.)

## Module system and reuse

Libraries live alongside your scripts (or inside shared module directories) and can be imported with clear syntax, enabling teams to package shared utilities across scripts.

```rn
const http = import "net/http"

const response = http.get("https://example.com/status")
echo "${response.code}"
```

**Result:** Modules encapsulate reusable functionality and expose their `pub` declarations on the imported value.

To define your own module, add a `.rn` file relative to the script that will import it. A spec of `util/math.rn` resolves to `<script_dir>/util/math.rn`:

```rn
// <script_dir>/util/math.rn
fn Void @() Void

pub fn Void add(lhs: Int, rhs: Int) Int {
  yield lhs + rhs
}

pub const pi = 3.14159
```

Imported modules must not declare parameters in their `@(...)` signature. Use
`pub` functions instead of module parameters when you need reusable behavior.

The current implementation executes the imported module, caches it by resolved
path, rejects circular imports, and returns a merged value that includes:

- execution-result fields such as `stdout`, `stderr`, `exit_code`, and `wait`
- every `pub` top-level declaration from the module

Example:

```rn
// <script_dir>/util/math.rn
fn Void @() Void

pub fn Void add(lhs: Int, rhs: Int) Int {
  yield lhs + rhs
}

pub const pi = 3.14159
```

```rn
// importer
const math = import "util/math.rn"
echo "${math.pi}"
echo "${math.exit_code}"
```

**Result:** Importing a module runs it, exposes its `pub` declarations, and also leaves the module execution result available for inspection.

## Typed pipeline boundaries

> See [`examples/typed_pipelines.rn`](../examples/typed_pipelines.rn) for a
> runnable tour of this section (`zig build run -- examples/typed_pipelines.rn`).

Function signatures carry explicit stdin and stdout types using the form
`fn StdinType name(params) StdoutType`. The pipe operator `|` enforces that the
upstream stdout type matches the downstream stdin type at every boundary. A
mismatch is a compile-time error.

```rn
fn Void produce() String echo "typed output"
fn String passthrough() String cat

produce | passthrough
```

**Result:** `produce` outputs `String` to stdout; `passthrough` accepts `String`
on stdin and forwards it. The type checker validates that these types align
before the script runs.

### Standard streams: `&0`, `&1`, `&2`

The three standard streams are referenced with file-descriptor syntax: `&0`
(stdin), `&1` (stdout), `&2` (stderr).

- `&0` is a value expression that reads the function's (or stage's) stdin.
- `&1` / `&2` are write streams; you write to them with `yield` (see below).

### `yield` — pushing values to a stream

Output is explicit. `yield expr` writes a value to stdout (`&1`); `yield &2 expr`
writes to stderr. A function's body value is **not** automatically
written to stdout, so a stage that consumes its input without `yield`ing
produces no stdout output.

```rn
fn Int square() Int {
    yield &0 * &0
}

echo "4" | parseInt | square   // prints 16
```

The declared stdout type constrains what may be `yield`ed to `&1` — `yield "text"`
in an `Int`-stdout function is a compile-time error. (`yield &2` carries untyped
diagnostic output and is not constrained.) A function may `yield` zero or more
times; there is no `return` — output is carried solely by `yield`, and a
function halts when it runs out of statements (use `exit` to halt early):

```rn
fn Int consume() Void {
    const n = &0
    // no yield: this stage produces no stdout output
}

echo "3" | parseInt | consume   // prints nothing
```

```rn
fn Int tee() Int {
    const n = &0
    yield &2 "log: received ${n}"   // diagnostic, goes to stderr
    yield n * 10                    // result, goes to stdout
}
```

A stage that `yield`s more than once emits each value as it happens (streamed,
not buffered to the end), so one input value can produce several outputs:

```rn
// one input (6) -> two outputs: 36 now, 12 after the sleep
echo "6" | parseInt | {
    const in = &0
    yield in * in
    sleep "2"
    yield in + in
}
```

Commands inside a function body (like `echo`) still write to stdout directly —
that is independent of `yield`:

```rn
fn Void greet(name: String) String {
    echo "Hello, ${name}!"   // echo writes to stdout itself
}
```

### `&0` — reading typed stdin as a value

Inside a function body with a typed stdin, `&0` reads the function's stdin pipe
and returns it as a value. This lets you process pipeline input with pure Runic
expressions instead of relying on executables like `cat`.

```rn
fn Void produce() String {
    yield "hello from pipe"
}

fn String transform() String {
    const received = &0
    yield "${received}!"
}

produce | transform
```

**Result:** Prints `hello from pipe!`. `produce` yields the string directly
(no process involved), `transform` collects it via `&0` and yields it with
`"!"` appended.

The `&0` value has the type declared in the function's stdin position. Using
it in a function that has `Void` stdin would be a type error.

`&0` is a **consuming** read: each read takes the next value off the input
stream. To use a value more than once, bind it with `const` — `&0 * &0` would
read *two* values, but `const n = &0; n * n` reads one and reuses it. Once the
producer has closed, reading `&0` again yields EOF, and `yield`ing an EOF value
emits nothing:

```rn
fn Int consume_once() Int {
    yield &0    // emits the value
    yield &0    // EOF: the producer is closed, so this emits nothing
}

echo "7" | parseInt | consume_once   // prints 7
```

#### Consuming a live stream with `for (&0)`

When the upstream stage `yield`s many values over its lifetime, the downstream
stage drains them with a `for` loop over `&0`. Each iteration reads the next
value off the live stream (blocking the stage until a value arrives or the
producer closes), so a consumer transforms an unbounded number of values
without knowing the count ahead of time. The producer closing its stdout is
reported as EOF, which ends the loop:

```rn
fn Void produce() Int {
    yield 1
    yield 2
    yield 3
}

fn Int double_each() Int {
    for (&0) |v| {
        yield v * 2
    }
}

produce | double_each   // prints 246 (2, 4, 6 as they arrive)
```

Each value is delivered as the producer emits it — if `produce` slept between
yields, `double_each` would emit each result with the same spacing rather than
all at once. Per-value iteration applies to in-process typed streams
(`Int`/`Float`); a `String`/byte stream has no message framing, so `for (&0)`
over one reads the whole accumulated input as a single value (one iteration).

#### Framing a byte stream with `lines`

Executable (and other byte) output is unframed — `&0` over it reads the entire
buffer at once. The `lines` builtin turns a newline-delimited byte stream into a
multi-value stream: it reads its whole stdin, splits on `\n`, and emits each
non-empty line as a separate value. A downstream `for (&0)` filter (or the
`parseInt` builtin, which maps each input value to an `Int`) then processes one
line at a time:

```rn
fn Int square() Int {
    for (&0) |in| {
        yield in * in
    }
}

// echo prints 0..4 on their own lines; lines frames them; parseInt maps each
// to an Int; square squares each.
{ for (0..5) |i| echo i } | lines | parseInt | square   // prints 014916
```

`parseInt` maps per value, so it works on a single value (`echo "10" | parseInt`)
or a framed stream (`… | lines | parseInt`). `parseFloat`
(`fn String parseFloat() Float`) is the `Float` counterpart and behaves the same
way. A custom stage that should process every value uses the `for (&0) |v| {
... }` form — a stage that reads `&0` once (e.g. `const n = &0`) consumes only
the first value.

### Mixed executable and typed-function pipelines

Executable stages and typed Runic functions can be freely mixed. An executable
that precedes a typed function must match the function's declared stdin type
(which must be `String`, since executables always output bytes):

```rn
fn String process() String {
    const input = &0
    yield "${input}!"
}

// executable output → typed function via &0
echo "exec input" | process
```

Multi-stage pipelines work in all combinations:

```rn
fn Void source() String { yield "pipeline" }
fn String middle() String { const s = &0; yield "typed ${s}" }

// typed fn → typed fn → executable
source | middle | cat
```

### Non-string typed values and `parseInt`

Pipelines are not limited to strings. A function whose stdin type is `Int`
receives `&0` already parsed into an `Int`, so it can do arithmetic on it
directly. The `parseInt` builtin bridges a `String` stage to an `Int` stage,
making the type transition explicit:

```rn
fn Int doubler() Int {
    yield &0 * 2
}

fn Int inc() Int {
    yield &0 + 1
}

// String → parseInt → Int → Int : prints 21
echo "10" | parseInt | doubler | inc
```

**Result:** `echo "10"` produces the text `10`; `parseInt` asserts it as an
`Int`; `doubler` reads `&0` as `10` and yields `20`; `inc` yields `21`.
Numeric values travel between stages as their canonical decimal text, and each
stage's declared type drives how the bytes are interpreted. `parseInt` has the
type `fn String parseInt() Int`, so a following stage must declare `Int` stdin —
feeding its output into a `String` stage is a compile-time mismatch.

### Optional coercion at pipe boundaries

A stage that outputs `T` can feed a downstream stage whose stdin is `?T`. The
value flows through unchanged and the downstream sees it as the non-null case,
so `&0 orelse "default"` reads the piped value when present:

```rn
fn Void produce() String {
    yield "coerced value"
}

fn ?String consume() String {
    const received = &0 orelse "fallback"
    yield "${received}"
}

produce | consume
```

**Result:** Prints `coerced value`. The `?String` stdin of `consume` accepts the
`String` produced upstream (a `T → ?T` coercion), and `&0` is typed as
`?String` so the `orelse` fallback type-checks.

### Void boundaries and rejected mismatches

A `Void` stdin type means the function does not read from the pipeline; a `Void`
stdout type means nothing is written. Connecting a stage with `Void` output to a
stage that expects a non-`Void` input is rejected:

```rn
fn Void make_void() Void {}
fn String need_string() String cat

// rejected: upstream stdout is Void, downstream stdin expects String
make_void | need_string
```

The type checker also enforces that calls made inside a function body are
compatible with the enclosing function's declared stdin type. Calling a function
with a different stdin contract from within another function is an error:

```rn
fn Void hello() String echo "hello"
// rejected: greetings declares String stdin but calls hello which expects Void stdin
fn String greetings() String hello
```

External executables use the catch-all boundary `fn String @(...String) ExecutionResult`,
so a typed function that follows an executable stage must declare `String` stdin.

## Structs

A struct is a named collection of typed fields. Declare a struct type with
`const`, using `name: Type` fields separated by commas and/or newlines:

```rn
const Point = struct { x: Int, y: Int }

const Result = struct {
    value: Float
    ok: Bool
}
```

**Construction & field access.** Build a value with `Name{ .field = value, … }`
(all fields required), and read a field with `.field`:

```rn
const p: Point = Point{ .x = 3, .y = 4 }
echo "x=${p.x} y=${p.y}"
```

Constructing a struct checks every field: an unknown field, a missing field, a
duplicate field, or a value whose type doesn't match the field's type is a
compile error.

**Nesting.** A field may itself be a struct; construction and access nest:

```rn
const Line = struct { from: Point, to: Point }
const l: Line = Line{ .from = Point{ .x = 0, .y = 0 }, .to = Point{ .x = 3, .y = 4 } }
echo "${l.to.x}"
```

**Functions.** Structs can be passed to and returned from functions; the value
survives the call boundary intact, so a caller can read the returned struct's
fields:

```rn
fn Void midpointX(a: Point, b: Point) Int { yield (a.x + b.x) / 2 }
fn Void origin() Point { yield Point{ .x = 0, .y = 0 } }

const o = origin
echo "mid=${midpointX o p}"
```

**Methods (UFCS).** A method is just a free function whose first parameter is the
receiver; `recv.method args…` is shorthand for `method(recv, args…)`. A field of
the same name takes precedence over a method:

```rn
fn Void mag(self: Point) Int { yield self.x + self.y }
echo "mag=${p.mag}"          // == mag(p)

fn Void scaled(self: Point, k: Int) Int { yield (self.x + self.y) * k }
echo "${p.scaled 10}"        // == scaled(p, 10)
```

**Mutation.** A field of a `var` struct can be reassigned with `p.field = value`
(nested fields too). The field's declared type is enforced, and mutating a field
of a `const` struct is a compile error:

```rn
var p: Point = Point{ .x = 3, .y = 4 }
p.x = 10
echo "${p.x}"                // 10
```

**Result:** `Point`/`Result` are struct types you can annotate bindings,
parameters, and returns with. A struct value holds its fields together and is
passed by value. Interpolate an individual field (`${p.x}`) — interpolating a
whole struct is rejected, since it has no single string form. (Default field
values and generic/parameterized structs are not yet supported.)

## Files are structs

Just like in zig, all files in runic are implicitly structs. All functions or declarations in a file will exist on the struct type of the file.

```rn
// lib.rn

fn Void add(x: Float, y: Float) Float {
  yield x + y
}
```

```rn
// main.rn

const lib = import("lib")

echo "${lib.add 3 5}"
```

**Result:** `lib.rn` will become a struct type with a function `add` declared on it. `main.rn` is importing `lib.rn` and binding it to the identifier `lib`. `lib` is of the type `struct { fn add(x: Float, y: Float) }`.
