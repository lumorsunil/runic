# todo

- [x] lsp didChange support, document diagnostics
- [ ] lsp now crashes (investigate that we do similar setup as run_script)
- [ ] imports
  - [x] struct types
    - [x] f (ast) -> struct type
  - [x] parse imported file
  - [x] ast cache
  - [x] during execution, when importing a file, it should be immediately executed
    - [x] move script executor to the interpreter lib code
    - [x] create one script executor for each import that is to be executed
      - [x] pass env map (env map is global, change we get to "sub-shells")
    - [x] cache execution result (module ScopeStack)
    - [ ] wire up scope to the identifier binding (evaluate an import expression to be the ScopeStack of that module)
