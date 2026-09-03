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

There is also a placeholder `--tcp <port>` flag in the CLI surface, but that
transport is still reserved rather than being part of the supported workflow.

## Current Priorities

### 1. Better completions

The main usability gap is completion quality.

Near-term improvements:

- member-access completions
- richer completion metadata such as symbol kind and signature/detail text
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
- more reliable identifier-to-symbol resolution
- possibly document links where they clearly help navigation

### 3. Stability and performance

There are known long-running reliability issues that are more important than
adding broad new feature surface.

Current concerns:

- occasional high CPU usage after extended sessions
- LSP degradation or failure after running for a while — **partly addressed:**
  the workspace type checker reused a single arena freed only at shutdown, so
  analysis memory grew per edit; it is now reset (and open documents re-checked)
  on each change, bounding memory to one pass over the open set.
- continued memory/leak/crash hardening during restarts and document churn

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
