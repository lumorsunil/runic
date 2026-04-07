# Runic Contribution Progress

This document summarizes the current state of the active contribution branch.
It is not a roadmap and it is not a design proposal. Its job is to explain what
this branch actually changed, what has been verified, and what remains
intentionally out of scope.

## Branch Focus

The branch is now primarily about language-server and contributor-surface
improvements.

Completed areas:

- LSP request handling for navigation/editing features that were already being
  advertised by the server
- LSP regression coverage through a protocol-level JSON-RPC harness
- contributor documentation
- Neovim syntax setup cleanup so the plugin no longer depends on a clone living
  at one specific local path

Explicitly not part of the branch anymore:

- lambda / anonymous-function implementation work
- speculative language-syntax expansion
- broad roadmap redefinition

## Implemented Changes

### 1. LSP request handling

The following LSP methods are now wired into request parsing and server
dispatch:

- `textDocument/definition`
- `textDocument/references`
- `textDocument/documentSymbol`
- `textDocument/rename`
- `textDocument/formatting`

Before this pass, several of these handlers existed but were either incomplete
or not reachable through the request payload union.

### 2. LSP behavior fixes

The branch now fixes several concrete LSP correctness issues:

- rename returns concrete edits instead of a zero-length insertion at the
  cursor
- formatting preserves `#` comments instead of deleting them
- document symbols return real ranges rather than `0:0 -> 0:0`
- definition returns the binding's actual source file URI when that information
  is available
- references search across currently opened/tracked documents
- references respect `includeDeclaration = false`
- imported-module member definition can fall back to known document symbols when
  direct semantic lookup at the cursor is not available

### 3. LSP ownership and leak fixes

While adding the regressions, the branch also fixed several ownership bugs that
the new tests exposed:

- heap-allocated workspace/document-store objects are destroyed correctly
- stored `Document` allocations are destroyed correctly on replacement and store
  shutdown
- temporary lexer instances in identifier extraction are deinitialized
- document-symbol responses no longer duplicate response fields unnecessarily
- normal diagnostics logging now goes through the existing opt-in LSP logger
  instead of unconditional stderr logging that broke tests

### 4. LSP regression harness

The branch adds a protocol-level test harness in
`tests/lsp_protocol.zig`, wired into `zig build test`.

Covered regressions:

- formatting preserves comments
- document symbols include real ranges
- rename returns concrete same-file edits
- rename ignores plain strings and comments
- cross-file definition for imported module members
- references across opened documents
- references excluding declarations
- diagnostics publication for invalid source

### 5. Contributor documentation

The branch adds a top-level `CONTRIBUTING.md` draft and links it from the
README contributor-facing flow.

The draft currently covers:

- where contributors should look first
- the current Zig/build/test workflow
- expectations around regressions and documentation updates
- PR/commit guidance
- good first contribution areas

### 6. Neovim syntax cleanup

The Neovim syntax file no longer hardcodes a local clone path for the tree-sitter
grammar directory. It now derives the grammar location relative to the plugin
file itself, so it works from arbitrary clone locations.

## Verification

The branch has been verified with:

```bash
zig build
zig build test
```

The LSP protocol regressions are part of `zig build test`, so they are also
covered when the project CI script reaches the unit-test stage.

## Current Limitations

This branch does not claim that LSP support is complete. Remaining limitations
include:

- rename is still same-document only
- references are limited to documents currently tracked by the LSP store
- definition fallback for symbol-only resolution is intentionally conservative
  and returns `null` instead of guessing when multiple candidate definitions
  exist

These limitations are acceptable for the current branch because they are now
explicit, tested, and materially better than the prior behavior.

## Files Touched Most Heavily

- `src/lsp/server.zig`
- `src/lsp/document.zig`
- `src/lsp/types.zig`
- `src/lsp/symbols.zig`
- `src/lsp/workspace.zig`
- `src/lsp/completion.zig`
- `tests/lsp_protocol.zig`
- `build.zig`
- `CONTRIBUTING.md`
- `editor/neovim/syntax/runic.vim`

## Notes For Review

When reviewing this branch, evaluate it as:

- an LSP correctness and regression-coverage branch
- a contributor-surface improvement branch

Do not review it as a language-feature branch for lambdas or anonymous
functions. That is no longer the scope of the work represented here.
