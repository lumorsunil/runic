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
- hover with type information
- completion — keywords (with `const`/`var`/`fn`/`import` snippets), workspace
  symbols, member access (chained `a.b.c` and bare-trailing-dot recovery),
  import module paths, and `$PATH` executables; items carry signature/type
  detail and resolve documentation on focus
- go-to-definition (locals, parameters, shadowing, functions, struct fields,
  cross-file)
- references and rename — binding-aware (not name-based) and workspace-wide,
  including cross-file module members
- document symbols (outline) for top-level bindings, functions, and structs —
  nested: functions expose their parameters and structs expose their fields as
  child symbols
- document highlight (occurrences of the identifier under the cursor)
- document links (import paths link to the module file)
- inlay hints (inferred types after un-annotated bindings; parameter names
  before call arguments, including calls in nested positions and to imported
  module functions)
- folding ranges (multi-line statements: functions, structs, control flow)
- prepare-rename (validates the rename target and pre-fills the identifier)
- code actions (quick fix: add an inferred type annotation to a binding)
- workspace symbol search across all indexed files
- a workspace index: the first workspace-wide request (symbol search,
  references, rename, cross-file definition) with a client-provided root loads
  every `.rn` file into the document store, making navigation work across files
  that are not open. The scan is lazy — it never runs at initialize — so a slow
  or malformed file can only ever affect that first request, not startup or the
  per-file features (hover, completion, diagnostics)

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
- executables on `$PATH` are indexed at initialize and offered as command
  completions, with the resolved path as detail (argument-level completion
  like a shell's `git <TAB>` is out of scope for now)

### 2. Navigation features

The next major capability after completions is navigation.

Current planned work:

- go-to-definition — **working for the common cases:** local bindings,
  function parameters, the nearest shadowing binding, function-call → function
  declaration, and struct field/decl member access (`p.x` → the field
  declaration, which takes precedence over an unrelated same-named binding).
  Regression-tested in `tests/lsp_protocol.zig`.
- semantic (binding-aware) references and rename: when the symbol under the
  cursor resolves to a binding — or, for a member access `m.foo`, to a struct
  field or an imported module's `pub` declaration — only occurrences resolving
  to that *same* declaration (plus the declaration site itself) are
  renamed/listed; distinct same-named symbols in other scopes/files are left
  alone. Cross-file member accesses resolve even in workspace files that were
  never opened (type-checked on demand for the request); unresolvable symbols
  (commands, unbound names) fall back to a lexical name match
- ~~more reliable identifier-to-symbol resolution~~ / workspace-wide
  navigation — **done:** the workspace is indexed lazily on the first
  workspace-wide request (client-provided roots only, never the cwd fallback),
  so go-to-definition, references, and rename resolve to declarations in files
  that were never opened; workspace/symbol search is served from the same index
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
- request-level crash resilience — **addressed (0.8.1):** a position past
  end-of-file (a line beyond the document, or a character past a line's end)
  ran the position→offset scan off the buffer and crashed the server; such a
  position now resolves to nothing. A malformed request body previously
  propagated an error out of the main loop and ended the session; it is now
  logged and dropped, and the session continues. Both are regression-tested,
  alongside unknown-method (`-32601`) and duplicate-`initialize` (`-32600`)
  handling and navigation requests against never-opened documents.
- protocol wire-shape correctness — **addressed (0.8.1):** outgoing enums
  serialize as their numeric codes and single-variant unions
  (`documentChanges`, capability `Either`s) unwrap to bare objects rather than
  tag-wrapped envelopes — a rename/code-action edit that clients rejected is
  fixed, and the remaining latent cases (`InsertTextMode`, `CompletionItemTag`,
  `ProgressToken`) are guarded. Responses use JSON-RPC result-XOR-error form.
  Covered by unit tests in `src/lsp/types.zig` and wire-shape assertions in
  `tests/lsp_protocol.zig`.

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
