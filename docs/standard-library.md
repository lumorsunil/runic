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

## Initial Module Set

The first standard-library modules should be narrow and practical:

| Module        | Purpose                                                                                         |
| ------------- | ----------------------------------------------------------------------------------------------- |
| `std.fs`      | File and directory helpers such as `exists`, `read_text`, `write_text`, `cwd`, and path checks. |
| `std.path`    | Path joining, basename/dirname extraction, extension checks, and normalization helpers.         |
| `std.env`     | Explicit environment lookup, fallback handling, and scoped updates around `$NAME`.              |
| `std.process` | Small helpers for command execution results, status checks, and captured output.                |
| `std.str`     | String trimming, splitting, prefix/suffix checks, and simple conversions.                       |
| `std.testing` | Assertions and fixture helpers for `.rn` module and CLI smoke tests.                            |

Each module should expose a compact set of functions before adding broader
coverage. Prefer a few stable names over large surfaces that will churn.

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
