# Neovim Syntax Highlighter Plan

1. **Capture requirements**
   - Audit existing Runic syntax features (keywords, literals, operators, comments, interpolation, etc.) in `docs/` and `src/`.
   - Decide whether highlighting must support Tree-sitter queries, traditional Vim regex matches, or both (prefer Tree-sitter if grammar work already exists).
2. **Settle on extension scaffolding**
   - Pick plugin structure (Lua-based runtime files under `ftdetect`/`ftplugin`/`syntax` plus standalone `lua/runic_highlight` folder).
   - Define minimum Neovim version and dependencies (Tree-sitter optional?).
3. **Design highlight groups**
   - Enumerate semantic tokens (keywords, type names, builtins, pipelines, rune literals, comments, docstrings).
   - Map each token to standard `@` capture or `Runic*` highlight groups and document fallback colors.
4. **Build grammar or queries**
   - If leveraging Tree-sitter: extend/author `tree-sitter-runic` grammar or in-repo `queries/highlights.scm`.
   - If using regex: craft syntax files (`syntax/runic.vim`) with regions and matches for comments, strings, numbers, heredocs.
5. **Implement runtime files**
   - Create plugin entrypoint that registers filetype detection (`ftdetect/runic.vim`) and sets `setlocal` options.
   - Add highlight definitions (`after/syntax/runic.vim` or `queries/highlights.scm`) and fold/indent helpers if necessary.
6. **Integrate with Runic repo tooling**
   - Wire `cmd/` CLI or `scripts/` to install/update the Neovim plugin for contributors (optional helper bash function).
   - Add `examples/*.rn` as sample buffers for quick validation.
7. **Testing & validation**
   - Create automated tests using `plenary.nvim` or `busted` scripts verifying highlight captures against fixture buffers.
   - Smoke-test manually in Neovim with `:TSHighlightCapturesUnderCursor` (Tree-sitter) or `:syntax list Runic`.
8. **Documentation**
   - Update `README.md` with installation instructions (packer/lazy/vim-plug), configuration, and screenshots.
   - Add troubleshooting tips for missing Tree-sitter parsers or fallback behavior.
9. **Distribution plan**
   - Decide hosting (dedicated repo vs subdir inside Runic); set up versioning and release tagging.
   - Prepare `LICENSE`, `CHANGELOG`, and `:help runic-highlighter` doc if sharing publicly.
10. **Maintenance workflow**
    - Define process for syncing syntax tokens whenever language features change (e.g., checklist in PR template).
    - Set up CI job to run highlight tests on every change.
