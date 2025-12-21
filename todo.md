# todo

## modules

- [x] import modules
- [x] bind module to identifier
- [x] execute statements in module when imported
- [ ] use output of module execution
- [x] member access on modules
- [ ] pub keyword for accessible bindings (maybe?)
- [ ] define a function signature for the runic file `fn String @() String`

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
- [ ] exit code
- [ ] a lot of things missing (stdin, stdout/stderr capturing and piping, return statements, etc.)

## expressions

- [x] (re-)assignment
- [x] arithmetic
- [ ] assignment modifiers (+=, -=, \*=, /=)
- [x] boolean algebra
- [x] comparisons (numeric)
- [x] string concatenation
  - [ ] maybe ban?
- [x] array literals
- [ ] slicing
- [ ] array element accessor
- [x] if
- [x] for loops
  - [x] ranges
  - [x] arrays
  - [x] multiple sources
- [ ] pipeline_or, pipeline_and
- [x] recursive functions (add function def to closure?)
- [ ] exit/return statement
- [ ] blocks as anonymous functions?
- [ ] value references `const my_function = &module.some_function`
  - [ ] partial applications
    - `fn Void add(x: Int, y: Int) Int { return x + y }`
    - `const add5 = &add 5`
    - `const add5_2 = &add 5 2`
- [ ] pattern matching
  - [ ] regex patterns
  - [ ] glob patterns?
  - [ ] pattern matching expression
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
- [ ] exit code in execution result
- [ ] bug: when binding to a pipeline, runic hangs
- [ ] figure out what to do with stderr

## lsp

- [ ] completions for member access (need some sort of scope machinery around collecting symbols and stuff, and it would help a ton with semantic analysis)
- [ ] add more information to completion results (symbol type, etc)
- [ ] implement basic snippets (const/var/fn)
- [ ] hover
  - [x] basic hover implementation, identifier lookup
- [ ] go to definition
- [x] completions for keywords
  - [ ] bug: markdown syntax highlighting does not work for runic
- [x] restart causes leaks and crashes
- [x] completions for import module names
- [x] document change does not reflect new symbols
- [x] didChange support, document diagnostics
- [x] crashes (investigate that we do similar setup as run_script)
- [x] type checker integration
  - [x] type checking imports
- [ ] bug: takes 100% cpu after a while
- [ ] bug: stops working after a while, may be related to bug above
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

## parser

- [ ] parse error bail and continue
  - when parsing fails entirely in a statement or expression, look for the next expression terminator and continue parsing from there, storing the error for later diagnostics
  - still produce a valid ast for the type checker to use
  - [x] in progress
- [ ] type expression parser
  - [x] in progress

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
