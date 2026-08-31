# Runic Standard Library

The Runic standard library is the shared foundation for scripts that need more
than direct command execution. Its job is to collect common scripting utilities
behind stable Runic modules so authors can reuse behavior without copying helper
functions between projects or depending on host-specific shell snippets.

Runic is still experimental, so the standard library should start small and
grow only around behavior that is already common, portable, and well tested.
External commands remain the language's primary integration model; the standard
library exists to make frequent script logic predictable, typed, and discoverable.

## Purpose

The initial standard library should provide:

- portable helpers for common file, path, environment, process, string, and data
  tasks
- typed wrappers around recurring command-result patterns, such as checking
  status, reading captured output, and normalizing failures
- reusable functions that are easier to test than inline pipelines
- one documented place for core conventions, including naming, error handling,
  and module layout

The standard library should not hide normal command execution. A Runic script
should still be able to call `git`, `find`, `grep`, `jq`, or any other external
program directly when that is the clearest tool for the job.

## Import Model

The intended standard-library entry point is a reserved module import:

```rn
const std = import "std"
```

After that import, standard modules are accessed as public members on `std`:

```rn
const cwd = std.fs.cwd()
const home = std.env.get("HOME") orelse "/tmp"
const trimmed = std.str.trim("  runic  ")
```

This special `std` import is planned but not yet implemented. Until it exists,
standard-library development should follow the same rules as ordinary Runic
modules:

1. Put reusable code in parameterless `.rn` files.
2. Export public functions and values with `pub`.
3. Import modules with `const name = import "path/to/module.rn"`.
4. Run importers through the CLI with any needed `--module-path` entries.

For example, an early local equivalent might look like:

```rn
const fs = import "std/fs.rn"

const here = fs.cwd()
echo "working directory: ${here}"
```

## Module Reference

The first standard library is eight narrow modules. Each exposes a compact set of
functions before adding broader coverage — prefer a few stable names over large
surfaces that will churn.

| Module        | Purpose                                                                    |
| ------------- | -------------------------------------------------------------------------- |
| `std.list`    | Generic array helpers (`map`/`filter`/`reduce`, …) over `[]T`.             |
| `std.str`     | String helpers *composed from* the built-in methods (the builtins stay canonical). |
| `std.path`    | Path manipulation — join, basename/dirname, extension, normalize.         |
| `std.env`     | Explicit environment lookup, fallback, and scoped updates around `$NAME`. |
| `std.fs`      | File and directory helpers (wrapping `test`/`cat`/`ls`/… commands).        |
| `std.process` | Command execution results — status, captured output, require-ok.          |
| `std.math`    | Numeric helpers (`abs`/`min`/`max`/`clamp` + Float `sqrt`/`floor`/…).      |
| `std.testing` | Assertions for `.rn` module and CLI smoke tests.                          |

**Conventions.** Functions use the space-call form (`std.list.map xs f`), camelCase
names, and explicit input/output types. A fallible operation returns an **error
union** (`fs.readText : ExecutableError!String`); an operation whose result may be
absent returns an **optional** (`env.get : ?String`). Generic helpers use implicit
type variables (`T`/`U`) and function-typed parameters (`fn(T) U`).

### `std.list`

Generic helpers over arrays, built on type variables + higher-order functions +
`arr.push`.

| Signature | Result |
| --- | --- |
| `map(xs: []T, f: fn(T) U) []U` | each element mapped through `f` |
| `filter(xs: []T, keep: fn(T) Bool) []T` | elements for which `keep` is true |
| `reduce(xs: []T, f: fn(U, T) U, init: U) U` | left fold |
| `find(xs: []T, pred: fn(T) Bool) ?T` | first matching element, or absent |
| `any(xs: []T, pred: fn(T) Bool) Bool` | true if any matches |
| `all(xs: []T, pred: fn(T) Bool) Bool` | true if all match |
| `count(xs: []T) Int` | number of elements (`xs.len` is the builtin) |
| `contains(xs: []T, x: T) Bool` | membership (scalar equality) |
| `reverse(xs: []T) []T` | reversed copy |
| `concat(a: []T, b: []T) []T` | `a` followed by `b` |
| `range(start: Int, end: Int) []Int` | `[start, …, end - 1]` |
| `sort(xs: []T, less: fn(T, T) Bool) []T` | sorted copy by `less` |

### `std.str`

Only helpers **not** covered by the built-in UFCS methods (`s.len`, `s.split`,
`s.trim`, `s.upper`/`lower`, `s.contains`/`startsWith`/`endsWith`, `s.indexOf`,
`s.slice`, `s.replace`, `s.repeat`, `s.split`/`join`). No re-export.

| Signature | Result |
| --- | --- |
| `words(s: String) []String` | split on runs of whitespace, no empties |
| `capitalize(s: String) String` | first byte upper, rest unchanged |
| `padLeft(s: String, width: Int, fill: String) String` | left-pad to `width` |
| `padRight(s: String, width: Int, fill: String) String` | right-pad to `width` |
| `isBlank(s: String) Bool` | empty or all whitespace |

### `std.path`

Pure string manipulation over `/`-separated paths.

| Signature | Result |
| --- | --- |
| `join(parts: []String) String` | join with `/`, collapsing repeats |
| `basename(p: String) String` | last component |
| `dirname(p: String) String` | everything but the last component |
| `ext(p: String) String` | extension incl. dot (`.rn`), or empty |
| `stem(p: String) String` | basename without extension |
| `isAbsolute(p: String) Bool` | starts with `/` |
| `normalize(p: String) String` | collapse `//`, resolve `.`/`..` lexically |

### `std.env`

Over `$NAME` reads and the subshell-context updates.

| Signature | Result |
| --- | --- |
| `get(name: String) ?String` | value or absent |
| `getOr(name: String, default: String) String` | value or `default` |
| `set(name: String, value: String) Void` | update the current context |
| `has(name: String) Bool` | whether the variable is set |
| `home() String` | `$HOME` (or a sensible fallback) |
| `path() []String` | `$PATH` split on `:` |

### `std.fs`

Thin wrappers over portable commands; each documents its command. Fallible ops
return `ExecutableError!…` so callers use `catch`/`try`.

| Signature | Wraps | Result |
| --- | --- | --- |
| `exists(p: String) Bool` | `test -e` | path exists |
| `isDir(p: String) Bool` | `test -d` | is a directory |
| `isFile(p: String) Bool` | `test -f` | is a regular file |
| `readText(p: String) ExecutableError!String` | `cat` | file contents |
| `writeText(p: String, content: String) ExecutableError!Void` | redirect | write (truncate) |
| `appendText(p: String, content: String) ExecutableError!Void` | redirect | append |
| `listDir(p: String) []String` | `ls` | entry names |
| `mkdirp(p: String) ExecutableError!Void` | `mkdir -p` | create dirs |
| `remove(p: String) ExecutableError!Void` | `rm -rf` | delete |
| `cwd() String` | builtin `pwd` | current directory |

### `std.process`

Helpers around a command's `ExecutableError!String` value view.

| Signature | Result |
| --- | --- |
| `output(result: ExecutableError!String) String` | captured stdout, or "" on failure |
| `status(result: ExecutableError!String) Int` | exit code (0 on success) |
| `succeeds(result: ExecutableError!String) Bool` | exited 0 |
| `requireOk(result: ExecutableError!String, msg: String) String` | output, or abort with `msg` |

### `std.math`

Integer/Float numeric helpers. `abs`/`min`/`max`/`clamp`/`sign`/`pow` are pure
Runic; the Float functions are backed by **new builtins** (see prerequisites).

| Signature | Result |
| --- | --- |
| `abs(n: Int) Int`, `absF(x: Float) Float` | absolute value |
| `min(a: Int, b: Int) Int`, `max(a: Int, b: Int) Int` | extremum |
| `clamp(n: Int, lo: Int, hi: Int) Int` | constrain to `[lo, hi]` |
| `sign(n: Int) Int` | -1 / 0 / 1 |
| `pow(base: Int, exp: Int) Int` | integer power (`exp ≥ 0`) |
| `sqrt(x: Float) Float` | square root — *builtin* |
| `floor(x: Float) Float`, `ceil(x: Float) Float` | round toward −∞ / +∞ — *builtin* |
| `round(x: Float) Float`, `trunc(x: Float) Float` | nearest / toward zero — *builtin* |

### `std.testing`

| Signature | Behavior |
| --- | --- |
| `assert(cond: Bool, msg: String) Void` | abort (nonzero exit) with `msg` if false |
| `assertEq(a: T, b: T, msg: String) Void` | abort if `a != b` |
| `assertContains(s: String, sub: String) Void` | abort if `s` lacks `sub` |
| `fail(msg: String) Void` | unconditional abort |

### `std.map`

A generic key/value map, built entirely in Runic. Keys are hashed into a fixed
number of buckets (each a small association list scanned on collision), so
lookups are O(1) on average; a parallel ordered list preserves **insertion
order** for `keys`/`values` (and `set` on an existing key keeps its position).
Types resolve at comptime (generic constructors + monomorphization); the data
operations run at runtime. Keys are compared with `==` and hashed by value, so
`Int` and `String` keys work — a `String` is hashed over its bytes, so an
interpolated key hashes identically to the same literal. The API is
**immutable**, mirroring `arr.push`: `set`/`remove` return a new map. (The bucket
count is fixed for now; resize-on-load-factor is a future refinement.)

| Signature | Result |
| --- | --- |
| `empty() Map(K, V)` | an empty map |
| `set(m: Map(K, V), key: K, value: V) Map(K, V)` | new map with `key` → `value` (replaces or appends) |
| `get(m: Map(K, V), key: K) ?V` | the value for `key`, or absent |
| `has(m: Map(K, V), key: K) Bool` | whether `key` is present |
| `remove(m: Map(K, V), key: K) Map(K, V)` | new map without `key` |
| `keys(m: Map(K, V)) []K`, `values(m: Map(K, V)) []V` | entries in insertion order |
| `len(m: Map(K, V)) Int` | number of entries |

```rn
var m = std.map.empty
m = std.map.set m "a" 1
m = std.map.set m "b" 2
const v = std.map.get m "a" orelse 0   // 1
```

## Language prerequisites for Phase 3

Most modules are writable in Runic today. The remaining language work:

- **The `import "std"` mechanism** — resolve the reserved `std` import to the
  bundled modules so `const std = import "std"` and `std.list.map …` work. (Until
  then, modules are plain `.rn` files imported by relative path — see below.)
- **Float math builtins** — `sqrt`, `floor`, `ceil`, `round`, `trunc` (and a Float
  `pow`), exposed as UFCS methods or `std.math` functions, for `std.math`'s Float
  surface. Everything else in `std.math` is pure Runic.

Nothing else is blocked: `std.list`/`str`/`path`/`env` are pure Runic;
`std.fs`/`process` are command wrappers; `std.testing` needs only assert + abort.

## Usage Patterns

Use standard-library helpers when they make script intent clearer than raw
pipeline mechanics:

```rn
const std = import "std"

const config_path = .{ ".runic", "config" } | std.path.join
if (std.fs.exists config_path) {
  const config = std.fs.read_text config_path
  echo "loaded ${std.str.trim config}"
} else {
  echo "missing config"
}
```

Keep external commands direct when the command is already the clearest
interface:

```rn
git "status" "--short"
```

Combine the two when a library helper handles the reusable policy and the
external command does the domain-specific work:

```rn
const result = git "status" "--short"
std.process.require_ok(result, "git status failed")
```

## Design Rules

Standard-library modules should follow the same conventions as user-authored
Runic modules:

- modules are parameterless and expose reusable behavior through `pub`
  declarations
- functions use explicit input and output types
- failures are represented with typed errors or structured command statuses
- helpers should be deterministic unless their name clearly implies process,
  filesystem, time, or environment access
- examples should run through `zig build run -- path/to/script.rn` once the
  corresponding functionality exists

When a helper wraps an external command, document the portability assumption and
avoid pretending the behavior is built into the language runtime.

## Stabilization Checklist

Before a standard-library function is treated as stable, it should have:

- a documented signature and one short example
- success and failure coverage in regression tests
- behavior that works from both direct scripts and imported modules
- clear error behavior for missing files, absent environment values, failed
  commands, and invalid inputs

The standard library should evolve from real script needs. If a helper is only
useful to one narrow script, keep it as a project module until the pattern
appears repeatedly.
