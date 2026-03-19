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

### Promises as native asynchronous values

Promises are first-class types that use the dedicated `^T` syntax, mirroring the shorthand used for optionals (`?T`) and error unions (`!T`). Any function or expression marked `async` returns a `^T`, and `await` unwraps the eventual value while preserving the same capture ergonomics used elsewhere in the language.

```rn
error ReleaseError = enum { Timeout, Offline }

fn fetch_release(tag: Str) ^ReleaseError!Release {
  async {
    let resp = try http.get("https://api.example.com/releases/${tag}")
    return try parse_release(resp.body)
  }
}

const latest = fetch_release("stable")

await (latest) |release| {
  echo "Shipping ${release.version}"
} catch |err| {
  echo "Release lookup failed: ${err}"
}
```

**Result:** `^ReleaseError!Release` advertises that `fetch_release` completes in the future and may fail with a typed error set. `await (latest) |release| { ... }` uses the same capture clause syntax as optional-aware `if` statements to bind the resolved value, and `catch |err| { ... }` mirrors error handling so scripts can branch on successful resolutions or failures without bespoke glue code.

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

Starting a program returns a structured process handle whether you run it synchronously or fire it off in the background. Assigning a bare command to a `let` binding now yields that handle directly, eliminating the need for a wrapper function. Handles keep track of the PID, running state, exit code, and captured IO channels so scripts can reason about subprocesses without juggling implicit `$?` globals.

```rn
const sync_proc = git "status" "--short"
echo "${sync_proc.stdout}" // already finished, STDOUT buffered
echo "${sync_proc.status.exit_code}"

const { stdout: sync_stdout, stderr: sync_stderr, exit_code: sync_exit_code } = git "status" "--short"
git "status" "--short" 1>$status_stdout
git "status" "--short" 2>$status_stderr

const async_proc = tail "-f" "/var/log/app.log" &
// later...
const finished = await async_proc
if finished.status.ok {
  echo "${finished.stdout}"
}

// or using an if directly on a Promise will automatically await the proc as well as allow capturing the outputs
if async_proc |stdout, stderr, status| {
    echo "${stdout}"
}
```

**Result:** Binding `let var = <command ...>` executes the program synchronously and returns its full output plus exit metadata. You can destructure the process handle to grab only `stdout`, `stderr`, or `exit_code`, or append the `1>var`/`2>var` shorthand to redirect a single stream directly into a variable while still receiving the same handle for status checks. Appending `&` to a command runs it asynchronously and yields a handle that can be waited on later. All handles surface stdout, stderr, PID, start/stop timestamps, and the final status code so you never have to poll global shell state.

### Capturing pipelines and multiplexed IO

Process handles expose helpers to splice commands together programmatically and capture each stream independently. You can tee outputs into variables, files, or callbacks while streaming live data to the console, and the new binding sugar works seamlessly inside larger pipelines.

```rn
make "all" | tee "build.log" 1>build_stdout
if build_stdout.status.exit_code != 0 {
  echo "Build logs:"
  echo "${build_stdout.stdout}"
}

const server = python "api.py" capture = { stdout: :stream, stderr: :buffer } &
server.stdout.on_line(fn (line) => echo "[srv] ${line}")
const status = server.wait(timeout = 5s)
if status.timed_out {
  server.stop()
  echo "${server.stderr}" // buffered for post-mortem
}
```

**Result:** Capture policies declare whether each stream should be buffered, inherited, or streamed through a callback. Pipelines preserve per-stage exit codes inside a parent process object, and the streamlined binding syntax makes it easy to grab stdout or stderr without destructuring the whole handle. Waiting returns a final snapshot containing stdout, stderr, and the exit metadata so scripts can log failures deterministically.

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

**Result:** Modules encapsulate functionality, provide typed APIs, and return predictable structures such as HTTP response objects.

To define your own module, add a `.rn` file relative to the script that will import it. A spec of `util/math` resolves to `<script_dir>/util/math.rn`:

```rn
// <script_dir>/util/math.rn
fn Void add(lhs: Int, rhs: Int) Int {
  return lhs + rhs
}

const pi = 3.14159
```

Each module also carries a manifest named `<file>.module.json` that documents which functions or values are exported and their types. The module loader reads this metadata to surface strongly-typed APIs to consumers:

```json
// <script_dir>/util/math.rn.module.json
{
  "exports": [
    {
      "kind": "function",
      "name": "add",
      "params": [
        { "name": "lhs", "type": { "kind": "primitive", "name": "int" } },
        { "name": "rhs", "type": { "kind": "primitive", "name": "int" } }
      ],
      "return_type": { "kind": "primitive", "name": "int" }
    },
    {
      "kind": "value",
      "name": "pi",
      "type": { "kind": "primitive", "name": "float" }
    }
  ]
}
```

The manifest uses a concise schema (`primitive`, `array`, `map`, `optional`, `promise`, etc.) so downstream scripts know what they are importing before the runtime executes the module. See `docs/module_authoring.md` for the full list of descriptors and publishing conventions.

## Legacy compatibility escape hatch

When you need to embed classic bash, Runic offers compatibility blocks so you can adopt the language incrementally.

```rn
bash {
  for f in *.txt; do
    echo "legacy: $f"
  done
}
```

**Result:** The block executes verbatim in bash, allowing existing snippets to run untouched while the surrounding script benefits from Runic’s safer semantics.

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

echo "${lib.add(3, 5)}"
```

**Result:** `lib.rn` will become a struct type with a function `add` declared on it. `main.rn` is importing `lib.rn` and binding it to the identifier `lib`. `lib` is of the type `struct { fn add(x: Float, y: Float) }`.
