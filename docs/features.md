# Runic Language Features

This document captures the current surface area of the Runic language and mirrors the guiding principles outlined in the README. Each feature includes a short example to illustrate the syntax and the expected outcome so experimenters can quickly understand how scripts behave.

## Command-first execution

Shell-friendly commands remain the primary abstraction. Every bare word starts a command, and pipes behave like bash but surface structured results.

```rn
echo "hello world" | upper
```

**Result:** Prints `HELLO WORLD` to STDOUT while capturing exit codes for each pipeline stage so they can be inspected programmatically.

## Strong, predictable data semantics

Variables are typed, immutable by default, and only change through explicit `var` declarations. Arrays and maps use literal syntax that sidesteps legacy word-splitting quirks.

```rn
const greeting = "hello"
var count = 2
const items = ["apples", "oranges"]
const vars = { greeting: greeting, total: count }
```

**Result:** `greeting` cannot be reassigned, `count` can be incremented intentionally, and the literals preserve their structure when passed between commands or functions.

Type annotations are available when you need to nail down a binding’s shape. Append `: Type` after the variable name so the compiler can enforce conversions up front.

```rn
const greeting: Str = "hello"
var retries: Int = 2
const config: Map(Str, Int) = { port: 8080 }
```

**Result:** Each declaration advertises its type at the point of definition, catching mismatches such as assigning a string to `retries` before the script ever runs.

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

### Errors as first-class types

Runic treats `error` the same way Zig does: it is a special type that can be declared as a simple enum when all variants are opaque or as a tagged union when each variant should carry structured data. Functions that can fail return `!T` (an error or a `T`), and callers use `try`/`catch` mechanics to branch on failure paths explicitly.

```rn
error NetworkError = enum { Timeout, ConnectionLost }
error FileError = union {
  NotFound: { path: Str },
  PermissionDenied: { user: Str },
}

fn load_config() !Config {
  return error.FileError.NotFound { path: "/etc/app.conf" }
}

const config = load_config() catch |err| {
  echo "load failed: ${err}"
}
```

**Result:** Error sets become inspectable data. Authors define lightweight enums for terse cases or unions when extra context matters, and callers can catch failures in a structured way instead of relying on string comparisons or shell exit-code conventions.

#### Mandatory explicit handling

Every emitted error must be handled explicitly—either propagated with `try` or resolved locally with `catch`—so the compiler guarantees nothing is silently dropped. The syntax mirrors Zig exactly, preserving muscle memory for developers already familiar with its error discipline.

```rn
fn init() !Void {
  try bootstrap_network(); // bubbles failures to the caller

  read_config("/etc/runic.conf") catch |err| {
    if err == error.FileError.NotFound {
      return err; // or transform before rethrowing
    }
    echo "Recovered from ${err}";
  }
}
```

**Result:** Callers must either continue propagating errors through `try` or consume them via `catch |err| { ... }`, making failure paths visible in every function signature and block just as they are in Zig.

#### Restricting function error sets

If a function can only surface a subset of errors, constrain its return type so callers know exactly which failures to handle.

```rn
fn read_config(path: Str) FileError!Config {
  if !file.exists(path) {
    return error.FileError.NotFound { path }
  }
  try file.ensure_access(path) catch |err| {
    return error.FileError.PermissionDenied { user: err.user }
  }
  return parse_config(path)
}
```

**Result:** `read_config` advertises that it can only fail with the specific file-related variants, so callers that already handle `NetworkError` or other families know they never have to match on them when invoking this function.

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
echo "${sync_proc.status.exit_code}"

const { stdout: sync_stdout, stderr: sync_stderr, exit_code: sync_exit_code } = git "status" "--short"
git "status" "--short" 1>$status_stdout
git "status" "--short" 2>$status_stderr

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

**Result:** Binding `const proc = <command ...>` executes the program synchronously and returns its buffered output plus exit metadata. You can destructure the execution value to grab only `stdout`, `stderr`, or `exit_code`, or append the `1>var`/`2>var` shorthand to redirect a single stream directly into a variable while still receiving the same execution value for status checks. When command-producing expressions are chained with `&&`, `||`, or `;`, the resulting bound value still exposes the buffered `stdout`, `stderr`, and exit metadata from the evaluated expression. Appending `&` runs the work in the background; when you bind that value, its output is still buffered rather than printed immediately, and `.wait` blocks until it finishes.

### File descriptor redirects

Runic supports explicit stdout/stderr redirects for commands and preserves shell-style left-to-right redirect ordering when fd duplication is involved.

```rn
echo "saved output" 1>"out.log"
echo "saved error" 2>"err.log"

echo "hello" 1>&2 2>"/dev/null"
```

**Result:** `1>...` redirects stdout, `2>...` redirects stderr, and `1>&2` duplicates stdout onto the current stderr target. Because redirects are applied left to right, `echo "hello" 1>&2 2>"/dev/null"` still writes `hello` to the original stderr stream instead of discarding it.

## Error-aware pipelines

Pipelines expose per-stage exit codes, enabling guarded chaining without relying on `set -e`.

```rn
const status = (build | tee "build.log").status
if !status.ok {
  echo "Build failed at step ${status.failed_stage}"
  exit 1
}
```

**Result:** The script reports which stage of the pipeline failed and aborts deterministically rather than silently ignoring intermediate errors.

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
  return lhs + rhs
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
  return lhs + rhs
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
writes to stderr. A function's `return`/body value is **not** automatically
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
times; `return` is for control flow / the function's exit value and no longer
carries output:

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

Structs are a collection of fields, each with their own type.

```rn
const Result = struct {
    value: ?Float,
    ok: bool,
}
```

**Result:** This binds an identifier Result to a type which is a struct. This way you can also alias types using let bindings and also create struct types on the fly as type expressions.

## Files are structs

Just like in zig, all files in runic are implicitly structs. All functions or declarations in a file will exist on the struct type of the file.

```rn
// lib.rn

fn Void add(x: Float, y: Float) Float {
  return x + y
}
```

```rn
// main.rn

const lib = import("lib")

echo "${lib.add 3 5}"
```

**Result:** `lib.rn` will become a struct type with a function `add` declared on it. `main.rn` is importing `lib.rn` and binding it to the identifier `lib`. `lib` is of the type `struct { fn add(x: Float, y: Float) }`.
