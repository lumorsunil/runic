# Runic Language Server: Minimal Viable Requirements

This document captures the minimum bar for launching a functional Language
Server Protocol (LSP) implementation for Runic. The intent is to stand up a
server that editors can connect to, maintain workspace awareness, and answer
basic autocompletion queries backed by workspace symbols. Subsequent phases
can build on this foundation for diagnostics, formatting, and navigation.

## Scope & Goals
- Ship a standalone binary under `cmd/runic-lsp/` that speaks LSP over stdin /
  stdout so it can be registered by Neovim, VS Code, and other editors.
- Implement only the protocol methods required for initialization, lifecycle,
  text synchronization, and `textDocument/completion`.
- Index files inside the workspace to extract top-level Runic symbols (modules,
  functions, exported variables) and expose them as completion entries.
- Avoid speculative features (diagnostics, hover, go-to-definition, semantic
  tokens) until the interpreter pipeline matures; the initial LSP is strictly a
  connectivity + workspace completion milestone.

## Project Layout & Tooling
- **Binary location:** add a Zig entry point at `cmd/runic-lsp/main.zig` that
  wires into shared runtime helpers inside `src/lsp/`. Keep protocol decoding,
  workspace management, and feature handlers as separate modules under
  `src/lsp/` to preserve single-responsibility boundaries.
- **Build integration:** extend `build.zig` with a target named `runic-lsp`.
  Document the formatter → build → test workflow in `README.md` once the server
  exists (`zig fmt ., zig build runic-lsp`, etc.).
- **Testing:** introduce table-driven unit tests alongside the new modules
  (e.g., `src/lsp/workspace_test.zig`) plus a thin integration smoke test under
  `tests/` that spawns the server, sends `initialize`, and requests completion.

## Protocol Lifecycle Requirements
- **Initialization:** implement `initialize`, `initialized`, and
  `shutdown`/`exit` handlers. Advertise the following capabilities only:
  `textDocumentSync` (incremental) and `completionProvider` (trigger characters
  include `.`, `:`, and alphanumeric fallthrough for manual requests).
- **Text synchronization:** support `textDocument/didOpen`,
  `textDocument/didChange`, and `textDocument/didClose`. Maintain an in-memory
  map of open documents so completion handlers can combine unsaved buffer state
  with on-disk indexing results.
- **Configuration fallback:** if clients send `workspace/didChangeConfiguration`
  or `workspace/didChangeWatchedFiles`, acknowledge the request but no-op, since
  the MVP does not support settings or file watchers beyond manual refresh.

## Workspace Symbol Indexing
- **Source discovery:** walk the workspace root provided in `initialize` plus
  any `workspaceFolders` to locate `.rn` files under `src/`, `examples/`, and
  `tests/`. Respect `.gitignore` defaults and never traverse outside the
  workspace boundaries.
- **Parsing strategy:** reuse the upcoming front-end parser when available; until
  then, implement a lightweight lexer that identifies `let`, `mut`, and `fn`
  declarations with optional `pub` or module qualifiers. Extract symbol names,
  the file path, and byte offsets for quick lookup.
- **Index maintenance:** trigger a full scan at startup, then re-index files
  when corresponding documents are saved or when an open document changes.
  Debounce updates to avoid re-scanning on every keystroke and keep the index in
  a thread-safe structure accessible by completion providers.
- **Symbol metadata:** record at least the symbol kind (function vs. variable),
  containing module (if derivable from the file path), and optional signature
  snippet so completions can present useful detail text.

## Completion Behavior
- **Triggering:** respond to `textDocument/completion` by merging the current
  buffer context with the workspace index. For MVP, ignore syntactic context and
  always return the full workspace symbol list filtered by the typed prefix.
- **Filtering:** perform case-sensitive prefix filtering that respects ASCII and
  Unicode symbol characters; fallback to substring matching when no prefix hits
  exist so users still see candidates.
- **Completion items:** each entry should include:
  - `label`: the symbol name.
  - `kind`: map Runic symbols to LSP kinds (`Function`, `Variable`, `Module`).
  - `detail`: module path or file-relative path plus signature, if known.
  - `documentation`: optional short snippet (first comment line or docstring).
  - `sortText`: normalized label (e.g., lowercase) to maintain stable ordering.
- **Performance expectations:** handle completion requests within 50 ms for
  median projects (<200 `.rn` files) by keeping the symbol index in memory and
  avoiding blocking file IO during the completion handler.

## Observability & Development Ergonomics
- Emit structured logs behind a `RUNIC_LSP_LOG` env flag to aid local debugging
  without polluting LSP stdout; logs should go to stderr.
- Provide a `--stdio` flag (default) plus a hidden `--tcp <port>` option so
  developers can attach protocol inspectors without editor mediation.
- Document a quickstart snippet in `docs/` or the main `README.md` showing how
  to register the server with Neovim or VS Code once the binary lands.
