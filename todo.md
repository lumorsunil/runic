# todo

## functions

- [ ] closures
- [x] lexer
- [x] ast
- [x] executor
- [x] bug: cannot call functions more than one (thanks ai)
- [x] calling a function using member access from imported module results in FileNotFound
- [ ] a lot of things missing (stdin, stdout/stderr capturing and piping, return statements, etc.)

## pipelines

- [x] find out why we get FileNotFound when trying to interpolate a ProcessHandle, it works when the expression itself is a pipeline, but not when you have bound it to a variable first and then try to interpolate the variable: `echo "${echo "hello"}" // Works`, `const a = echo "hello"; echo "${a}" // Doesn't work`

## lsp

- [ ] completions for member access
- [ ] add more information to completion results (symbol type, etc)
- [ ] restart causes leaks and crashes
- [ ] completions for keywords
- [x] completions for import module names
- [x] document change does not reflect new symbols
- [x] didChange support, document diagnostics
- [x] crashes (investigate that we do similar setup as run_script)

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
