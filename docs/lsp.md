# Runic LSP Status and Roadmap

This document describes the current state of the Runic language server and the
main future work planned for it.

It replaces the older MVP-spec framing that assumed the server did not yet
exist.

## Current State

The `runic-lsp` binary already exists under `cmd/runic-lsp/`.

Today it provides:

- stdio-based LSP transport
- document open/change/close handling
- workspace awareness
- diagnostics integration
- basic hover support
- keyword and module-name completion
- symbol-based completion from the current workspace
- document symbols (outline) for top-level bindings, functions, and structs —
  nested: functions expose their parameters and structs expose their fields as
  child symbols
- document highlight (occurrences of the identifier under the cursor)
- document links (import paths link to the module file)
- workspace symbol search across all indexed files
- a workspace index: on initialize with a client-provided root, every `.rn`
  file is loaded into the document store, making navigation work across files
  that are not open

There is also a placeholder `--tcp <port>` flag in the CLI surface, but that
transport is still reserved rather than being part of the supported workflow.

## Current Priorities

### 1. Better completions

The main usability gap is completion quality.

Near-term improvements:

- member-access completions — chained access (`a.b.c`) resolves each segment's
  type (struct fields, including named-type fields, and imported module
  members), not just the immediate object. Completion triggered on a bare
  trailing dot (`obj.`), which is a syntax error that leaves the document
  unparseable, is handled by recovering a scope: a repaired copy (with a
  placeholder member) is type-checked as a throwaway scratch document in the
  same directory — keeping relative imports resolvable — and closed again once
  the scope has been read.
- ~~richer completion metadata such as symbol kind and signature/detail text~~
  **done:** function completions show their call signature (`(name: String,
  times: Int) Void`) and struct-field completions show the field's type
- ~~snippets for common constructs like `const`, `var`, and `fn`~~ **done:**
  the `import`/`const`/`var`/`fn` keyword completions now insert snippets with
  tab stops (e.g. `const ${1:name} = ${2:value}`), gated on the client's
  `completionItem.snippetSupport` capability so clients without snippet support
  still get a plain insert
- continued improvement of import/module-path completions

### 2. Navigation features

The next major capability after completions is navigation.

Current planned work:

- go-to-definition — **working for the common cases:** local bindings,
  function parameters, the nearest shadowing binding, function-call → function
  declaration, and struct field/decl member access (`p.x` → the field
  declaration, which takes precedence over an unrelated same-named binding).
  Regression-tested in `tests/lsp_protocol.zig`.
- ~~more reliable identifier-to-symbol resolution~~ / workspace-wide
  navigation — **done for definition:** the workspace is indexed on
  initialize (client-provided roots only, never the cwd fallback), so
  go-to-definition and references resolve to declarations in files that were
  never opened; workspace/symbol search is served from the same index
- ~~possibly document links where they clearly help navigation~~ **done:**
  `import "./x.rn"` paths are links to the module file (embedded `std`
  imports are skipped, having no file to open)

### 3. Stability and performance

There are known long-running reliability issues that are more important than
adding broad new feature surface.

Current concerns:

- occasional high CPU usage after extended sessions — **partly addressed:**
  each edit re-type-checked *every* document in the store, which includes
  modules pulled in transitively by imports and never removed, so per-keystroke
  work grew without bound as a session touched more files. The per-edit recheck
  is now limited to the open (client-managed) documents; imported modules are
  still validated on demand when an importing open document is checked.
  Regression-tested in `tests/lsp_protocol.zig`.
- LSP degradation or failure after running for a while — **partly addressed:**
  the workspace type checker reused a single arena freed only at shutdown, so
  analysis memory grew per edit; it is now reset (and open documents re-checked)
  on each change, bounding memory to one pass over the open set.
- continued memory/leak/crash hardening during restarts and document churn —
  **in progress:** closing a document freed its AST but left the type checker's
  cached scopes referencing it (both the closed document's own scope and the
  scopes of any open document that imported it), so a request issued after a
  close but before the next edit read freed memory. `close` now re-checks the
  remaining open set so those references are dropped and rebuilt. Regression
  tests in `tests/lsp_protocol.zig` cover close-then-request on an importer and
  repeated open/change/close/reopen churn.

### 4. Better alignment with the language pipeline

The LSP should keep converging on the same parser and semantic rules used by
the CLI.

That means:

- reusing the current parser/type-checking behavior whenever possible
- avoiding editor-only semantics that drift from script execution behavior
- keeping diagnostics, completions, and hover grounded in the same source of
  truth

## Non-Goals Right Now

These may happen later, but they are not the immediate focus:

- formatting support
- semantic tokens beyond basic highlighting support outside the server
- a wide LSP feature matrix for every editor capability
- TCP-first workflows for normal development

## Relationship to Other Docs

- `README.md` explains how to build and run `runic-lsp`
- `docs/plan.md` is the top-level project roadmap
- `todo.md` contains additional lower-level LSP backlog items alongside other
  engineering backlog notes
