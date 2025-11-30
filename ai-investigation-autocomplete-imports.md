• Current LSP completion flow (src/lsp/server.zig:305-337) always filters doc/workspace symbols
via extractPrefix and never inspects import expressions, so module specs in string literals are
ignored and /-separated prefixes are truncated (extractPrefix only accepts identifier chars in
symbols.isIdentifierChar). Here’s a concrete way to add import-module autocomplete:

- Detect import context before generic symbol lookup: use the parsed AST already stored on
  the document (Document.ast built in Document.rebuildSymbols in src/lsp/document.zig). Add a
  helper to walk the script and find an ast.ImportExpr whose module_name.span contains the LSP
  position (convert to offset with types.Position.findIndex). If parsing failed, fall back to a
  lightweight textual check for import " on the current line.
- Index modules alongside symbols during workspace scans (src/lsp/workspace.zig): when
  walkDir hits a .rn, record a module candidate with spec = path without the .rn suffix and
  detail = relative path from the workspace root/importer dir. Optionally skip files missing
  a .module.json sibling so we only suggest valid modules. Store these in a new Workspace.modules array for fast lookup.
- When the position is inside an import string, build completion items from the module index
  instead of symbol matches: emit SymbolKind.module / CompletionItemKind.module with label =
  spec, detail = describePath(...), and maybe a documentation flag if a manifest is present. Keep existing symbol completions for non-import contexts.
- Adjust prefix/trigger handling for module specs: add a string-aware prefix extractor that
  accepts / and . but stops at quotes, and wire "// into completionProvider.triggerCharacters in
  sendInitializeResult so editors request completions as soon as a module path is typed.
- Add coverage: a small unit/integration test that opens a doc with import "net/ cursor, seeds
  the workspace with net/http.rn, and asserts handleCompletion returns net/http as a module item
  (plus a failure-path test when the AST is invalid but the textual fallback still works).
