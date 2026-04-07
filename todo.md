# todo

## modules

- [x] import modules
- [x] bind module to identifier
- [x] execute statements in module when imported
- [x] use output of module execution
- [x] member access on modules
- [x] pub keyword for accessible bindings
- [x] define a function signature for imported runic files

## executables

- [ ] explicit command syntax
  - `!echo "hello" // runs echo with argument "hello"`
  - `const echo = &!echo // binds echo to echo command`
  - `const !echo // shorthand syntax for above statement`
- [ ] trying to call a command as an identifier that is not bound will result in unknown identifier error
- [ ] all executables can be thought of as runic functions with the signature:
      `fn Stream(String) <exec-name>(...[]String) Stream(String)`

## functions

- [x] closures
- [x] lexer
- [x] ast
- [x] executor
- [x] bug: cannot call functions more than one (thanks ai)
- [x] calling a function using member access from imported module results in FileNotFound
- [x] function signature
  - [x] stdin type
  - [x] parameters
- [ ] stdin special bindings
  - `@stdin`
  - `@stdin | cat`
- [ ] typed pipes
- [x] exit code
- [ ] remaining function/runtime gaps
  - stdin semantics
  - stdout/stderr capturing and piping
  - typed pipes

## expressions

- [x] (re-)assignment
- [x] arithmetic
- [x] assignment modifiers (+=, -=, \*=, /=, %=)
- [ ] more assignment modifiers (||=, &&=)
- [ ] more binary operators
  - [ ] \*\* (exponentation)
  - [ ] << (bit shift left)
  - [ ] \>> (bit shift right)
  - [ ] & (bitwise and)
  - [ ] | (bitwise or)
  - [ ] ^ (bitwise xor)
- [ ] unary operators
  - [ ] ~ (bitwise negation)
- [x] boolean algebra
- [x] comparisons (numeric)
- [x] string concatenation
  - [ ] maybe ban?
- [x] array literals
- [ ] slicing
- [x] array element accessor
- [x] if
- [x] for loops
  - [x] ranges
  - [x] arrays
  - [x] multiple sources
- [ ] pipeline_or, pipeline_and
- [x] recursive functions (add function def to closure?)
- [x] exit/return statement
- [ ] blocks as anonymous functions?
- [ ] value references `const my_function = &module.some_function`
  - [ ] partial applications
    - `fn Void add(x: Int, y: Int) Int { return x + y }`
    - `const add5 = &add 5`
    - `const add5_2 = &add 5 2`
- [ ] pattern matching expansion
  - [ ] regex patterns
  - [ ] glob patterns?
  - [x] basic pattern matching expression
    - ```runic
        const number_pattern = /[0-9]+/
        const file_pattern = glob "*/*.*"
        const input_string = "451"

        if (input_string | number_pattern) {
            // input_string matched number_pattern
        }

        match input_string {
            "451" => {},
            glob "*/*.*" => {},
            file_pattern => {},
            number_pattern => {},
            /[_a-z][_a-z0-9]*/i => {},
        }
      ```

- [ ] encoding/decoding runic values
      `1.rn: echo "5" | std.parseInt | std.encode | std.writeFile "data"`
      `2.rn: cat "data" | std.decode | (x) => printf "%s\n" "${x}"`

## pipelines

- [x] find out why we get FileNotFound when trying to interpolate a ProcessHandle, it works when the expression itself is a pipeline, but not when you have bound it to a variable first and then try to interpolate the variable: `echo "${echo "hello"}" // Works`, `const a = echo "hello"; echo "${a}" // Doesn't work`
- [x] streaming input and output
  - [x] streaming output (only for one-stage pipelines)
  - [x] streaming input
  - [x] streaming output between pipeline stages
    - [x] maybe add a stream construct that can push data
    - [x] able to forward output into outer context from inside the executor/evaluator
    - [x] create a new context when binding identifiers
    - [x] explore idea of creating a context stack that we can push/pop same as the scope stack
      - [x] OR we could modify scope to have the evaluator context in it
  - [ ] block streaming into a pipeline (only needed if we want to pipe functions or blocks into other things)
- [ ] expressions as pipeline stages
  - [x] auto-coerce non-strings into strings? (or force interpolation? lsp hint?)
- [x] exit code in execution result
- [x] bug: when binding to a pipeline, runic hangs
- [x] figure out what to do with stderr

## lsp

- [x] completions for member access
- [ ] add more information to completion results (symbol type, etc)
- [ ] implement basic snippets (const/var/fn)
- [x] hover
  - [x] basic hover implementation, identifier lookup
- [x] go to definition
- [ ] workspace-wide go to definition for symbols not present in currently tracked documents
- [x] completions for keywords
  - [x] bug: markdown syntax highlighting does not work for runic
- [x] restart causes leaks and crashes
- [x] completions for import module names
- [x] document change does not reflect new symbols
- [x] didChange support, document diagnostics
- [ ] add protocol regressions for incremental `didChange` edits
- [x] crashes (investigate that we do similar setup as run_script)
- [x] type checker integration
  - [x] type checking imports
- [x] references
- [ ] workspace-wide references beyond currently tracked/open documents
- [x] rename
- [ ] workspace-wide rename
- [ ] improve semantic definition/rename/reference resolution for module members so it does not rely on conservative symbol fallbacks
- [x] bug: takes 100% cpu after a while
- [x] bug: stops working after a while, may be related to bug above
- [ ] add support for document links?

## imports

- [x] imports
  - [x] struct types
    - [x] f (ast) -> struct type
  - [x] parse imported file
  - [x] ast cache
  - [x] during execution, when importing a file, it should be immediately executed
    - [x] move script executor to the interpreter lib code
    - [x] create one script executor for each import that is to be executed
      - [x] pass env map (env map is global, change we get to "sub-shells")
    - [x] cache execution result (module ScopeStack)
    - [x] wire up scope to the identifier binding (evaluate an import expression to be the ScopeStack of that module)

## std lib

- [ ] std lib special import module name "std"
- [ ] stored on file system so that you can access the source code through the lsp

## repo / tooling

- [x] add a shell safety wrapper for `scripts/run_ci.rn`
  - `scripts/run_ci.sh` should validate that the Runic-driven CI path emits expected output and fall back to the direct shell stages if the Runic path regresses
- [ ] replace remaining `.sh` scripts in the repo with `.rn` equivalents where practical
  - long term goal: Runic should be able to drive its own contributor/test/tooling workflows instead of depending on shell wrappers
- [ ] add first-class regression coverage for `scripts/run_ci.rn`
- [ ] move Neovim tree-sitter/parser registration out of `editor/neovim/syntax/runic.vim` into a more appropriate plugin/setup file

## parser

- [ ] parse error bail and continue
  - when parsing fails entirely in a statement or expression, look for the next expression terminator and continue parsing from there, storing the error for later diagnostics
  - still produce a valid ast for the type checker to use
  - [x] in progress
- [x] type expression parser

## type checker

- [x] basic scope structure and semantic checker in place
- [x] imports
- [ ] do we even need lazy types? how will it work?
- [x] gather diagnostics foundation
- [x] scope lookup based on location
  - [x] finds outermost scope
  - [x] when finding scope by location, properly return the innermost scope found
- [x] bug: when reassigning a variable, the type gets overwritten to the assignment type
- [x] resolve types for pipelines that are actually just identifiers

## error handling

- [ ] call stack when executing functions
- [ ] review and generalize diagnostic/error reporting
  - normalize which errors are reported on stdout vs stderr
  - make compiler/runtime diagnostic output behavior consistent across failure kinds

## ir / runtime

### misc

- [x] support ctrl+d for closing stdin (not mvp)
- [x] coercion of execution result to a string needs to have both stdout and stderr merged (at least when binding or coercing directly in a string interpolation)
- [x] add expected results check to tests
- [x] main thread arguments from process startup
- [x] builtin env variables support
  - [x] env vars are populated into global scope
  - [ ] way to check if identifier exists (orelse except for this specific purpose) ?
- [ ] do we want to strip newlines at the end of an echo or not?
- [x] preserve ANSI/TTY-aware output when a Runic script runs subprocesses
  - direct top-level executable calls now inherit stdout/stderr only when Runic itself is attached to a TTY, so scripts like `scripts/run_ci.rn` can preserve color/ANSI output without breaking redirected or captured output

### data

- [x] void
- [x] exit code
- [x] unsigned integer
- [x] string (as slice)
- [x] stream
- [x] addr
- [x] struct
- [x] closeable (handles) (processes/etc)
- [x] pipe (handles)
  - [x] generic sources/destinations
    - [x] executable (file handle)
    - [x] blocks
    - [x] function calls
    - [ ] values (not mvp)
- [x] blocks
- [x] blocks as pipes
- [x] arrays

### instructions

- [x] instruction addr
- [x] labels
- [x] stack push
- [x] stack pop
- [x] arithmetic
- [x] compare
- [x] logical
- [x] new ref
- [x] set addr to value
- [x] unconditional jump
- [x] conditional jump if true
- [x] conditional jump if false
- [x] call executable
- [x] wait
- [x] fork
- [x] new pipe
- [x] set pipe option
- [x] forward pipe
- [x] exit

### compiler

- [x] sharing bindings between scopes/threads
  - [x] each thread has it's own closure
  - [x] closure-relative addresses
  - [x] shared binding for deeply nested closures
  - [x] mutable variables
- [x] mutable variables
- [x] binding to executable calls
  - [x] executable call result struct
  - [x] create a context when binding to blocks/calls that is stored in memory
- [x] address mapping
- [x] basic executable calls
- [x] if else
- [x] labels have the wrong address if we have closures (since closures alter addresses after the fact by inserting instructions)
- [x] for loops
  - [x] array iterator
  - [x] range iterator
- [x] read-only data
- [x] instructions
- [x] labels
- [x] refs
- [x] structs
  - [ ] user-land structs (not mvp)
- [x] struct types
  - [ ] user defined struct types (not mvp)
- [x] arithmetic
- [x] boolean algebra
- [x] pipelines
  - [x] multiple sources in streams ReaderWriterStream (stderr usually connects many sources)
  - [ ] error handling? (not mvp)
  - [x] && / ||
  - [x] boolean negation (!)
  - [x] redirections
    - [x] redirect stdout separately (not mvp)
    - [x] redirect stderr separately (not mvp)
    - [x] redirect to file
      - [x] replace mode
      - [x] append mode
    - [ ] "redirect" from file to stdin (not mvp)
- [x] function declarations
  - [x] arguments
- [x] functions calls
  - [x] arguments
  - [x] fix ref bug (calling same function multiple times to trigger)
  - [x] closures
- [x] bindings
- [x] compound assignment operators
- [x] member access
  - [x] execution result
- [x] arrays
  - [x] arrays that do not require stdio context
  - [x] arrays that do require stdio context
  - [x] element access

### execution context

- [x] (virtual) threads
- [ ] document one execution step
  - main thread sleeps/wakes, executes instructions, and publishes the program exit code on exit
  - background threads sleep/wakes and continue instruction execution
  - pipe threads forward available data and terminate when streams end
- [ ] document shared between threads
  - read-only data
  - instructions
  - struct types
  - labels
- [ ] document per thread state
  - refs
  - stack and stack counter
  - instruction counter

### evaluator

- [x] instruction step
- [x] new ref
- [x] push
- [x] pop
- [x] set
- [x] jump
- [x] executables
  - [x] execute
  - [x] wait for executable
- [x] exit
- [x] pipes
  - [x] new pipe
  - [x] stream pipe (blocking)
  - [x] set pipe option
  - [x] forward pipe
- [x] threads
  - [x] fork
  - [x] wait for thread
- [x] materialize string
  - [x] stream
  - [x] slice
  - [x] unsigned integer
  - [x] dereference addr
  - [x] exit code
  - [x] execution result
- [x] arithmetic
- [x] logical
- [x] compare

## debugger

- [x] commands
  - [x] step
  - [x] quit
  - [x] threads
  - [x] instructions
  - [x] pipes
  - [x] breakpoint
    - [x] add
    - [x] remove
    - [x] list
- [x] command history per session
- [x] stdin processing
  - [x] up/down arrow -> move command history cursor
  - [x] autocomplete
    - [x] main commands (only first part of the command is autocompleted right now)
    - [ ] sub-commands
    - [ ] arguments
  - [x] undo word/line
- [ ] memory view
- [ ] IR instructions view
- [ ] source code view
- [ ] output view (stdout/stderr/combined)
- [ ] tui lib?
- [ ] mark thread as skip

## bugs

- [x] grep is not outputting anything, see ir-blocks.rn
- [x] cannot call function multiple times
- [x] debugger: cannot paste to stdin
- [x] arrays will executable calls as elements cannot be accessed with element access operator
- [x] pipelines are broken again
- [x] escaped double quotes print as \"
- [ ] `$(sleep 0.5; echo "after")` produces a parsing error
- [x] syntax highlighting for match
- [ ] `true "hello` is parsed as a call with `true` as the callee
- [x] scripts/run_ci.rn fails with: `Type checker failed to run: error.UnsupportedStatement`
- [x] scripts/run_ci.rn fails to detect Zig when repo-root paths are built from env-backed strings
- [x] assigning a bound string or execution result into an env var does not populate the env var correctly
