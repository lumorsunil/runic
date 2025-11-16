# Neovim Extension Plan Progress

## Task 1 – Capture requirements _(completed 2024-05-07)_
- Reviewed the lexical inventory in `src/frontend/token.zig` plus the explainer in `docs/tokens.md` to enumerate every keyword, literal, and operator token that needs a dedicated highlight capture (commands/identifiers, declaration keywords, error/async/control keywords, literal booleans/null, punctuation such as `|`, `||`, `&&`, `..`, `...`, `=>`, and grouping tokens).
- Audited the lexer implementation (`src/frontend/lexer.zig`) to confirm comment syntax: `#` shell-style, `//` line comments, and nested `/* */` block comments must all be recognized so that highlight regions can end correctly even without a Tree-sitter grammar. Strings support typical escape sequences and interpolation via `${}` from the feature examples, so clusters inside `"` need a contained region rule.
- Cross-referenced `features.md` to capture behavioral constructs that require semantic highlight groups: typed `let/mut` bindings, `fn` signatures with `: Type`, error declarations (`error`, `enum`, `union`, `try`, `catch`), optional/promise shorthands (`?T`, `^T`, `await`), pipeline/process syntax (pipes, `&`, background processes, redirection like `1>`), module imports (`import`/`from`), `match`/`=>` arms, looping captures (`for (...) |...|`), and `bash { ... }` compatibility blocks.
- Highlight scope must therefore cover: keywords listed above, literals (ints/floats/strings plus `true/false/null`), punctuation/operators (assignment, comparison, range, pipeline/logic, capture delimiters), string interpolation placeholders (`${expr}`), capture clauses `|name, idx|`, and the triad of comment styles.
- Decision: no Tree-sitter grammar or `queries/` directory exists yet, so the first iteration will ship regex-based Vim syntax files (`ftdetect`, `syntax`, `after/syntax`) while structuring highlight groups so Tree-sitter queries can reuse the same naming once a grammar lands. This lets us deliver immediate value without blocking on a full parser.

## Task 2 – Settle on extension scaffolding _(completed 2025-11-16)_
- Created `docs/neovim-extension-scaffolding.md` to lock in the plugin architecture: an `editor/neovim/` root with Lua modules (`lua/runic_highlight/`), runtime files (`ftdetect`, `ftplugin`, `syntax`, `after/syntax`), optional Tree-sitter queries (`queries/runic/highlights.scm`), tests, and helper scripts.
- Added `editor/neovim/README.md` so the directory exists now and documents the decisions contributors should follow when populating the runtime files.
- Formalized the baseline requirements: Neovim 0.9.5+ is the supported target so we can rely on Lua `ftplugin`s and stable Tree-sitter APIs; Tree-sitter plus `nvim-treesitter` is optional, regex syntax is always available, and developer tests pull in `plenary.nvim`.

## Task 3 – Design highlight groups _(completed 2025-11-16)_
- Authored `docs/neovim-highlight-groups.md`, enumerating the `Runic*` highlight groups, the matching Tree-sitter capture names (e.g. `@keyword.declaration.runic`, `@operator.pipeline.runic`), and the Runic tokens that feed each group alongside their fallback links to Neovim’s built-in highlight names.
- Split the inventory into categories (keywords & flow, literals/builtins, operators & punctuation, comments/docstrings, structural helpers) so both regex syntax rules and Tree-sitter queries can iterate over a single canonical table defined later in `lua/runic_highlight/highlights.lua`.
- Captured implementation guidance showing how the Lua helper will emit `syn keyword`/`syn match` directives, link Tree-sitter captures to `Runic*` groups, and require future lexer changes to update the document first, keeping regex and parser-driven highlighting synchronized.

## Task 4 – Build grammar or queries _(completed 2025-11-16)_
- Populated `editor/neovim/lua/runic_highlight/highlights.lua` with the canonical `Runic*` definition table (group name, capture, fallback link, and the concrete `syn keyword`/`syn match`/`syn region` clauses for each token family) and wired a Lua helper `editor/neovim/lua/runic_highlight/syntax.lua` that iterates over that table to emit the Vim syntax rules plus `:hi def link` statements.
- Added `editor/neovim/syntax/runic.vim`, which simply requires the Lua helper so buffers pick up the regex-based syntax when `:set filetype=runic` fires; interpolation delimiters, capture clauses, keyword lists, punctuation, and nested comments are now recognized out of the box.
- Ported the capture list to `editor/neovim/queries/runic/highlights.scm` so a future `tree-sitter-runic` grammar can reuse the same `@keyword.*.runic` names; the query file documents the placeholder node names we expect to expose and keeps Tree-sitter users in sync with the regex groups.
- Noted a couple of heuristics we will revisit when AST data exists: pipeline operators only single-match `|`/`&` while `&&`/`||` stay in the logical group, process operators currently match fd-prefixed redirects plus `<<`/`>>`, and type-name detection watches identifiers after `:`/`->`; the metadata file makes these compromises explicit so we can tighten them later without chasing scattered `syn` directives.

## Task 5 – Implement runtime files _(completed 2025-11-16)_
- Added the Lua entrypoint `editor/neovim/lua/runic_highlight/init.lua` plus `editor/neovim/plugin/runic_highlight.lua` so `require("runic_highlight")` exposes `setup()`/`apply_buffer_settings()` and registers `.rn` filetype detection via `vim.filetype.add` while warning when Neovim < 0.9.5.
- Created the runtime files promised in the scaffolding doc: `ftdetect/runic.vim` for `*.rn`/`Runicfile` autocommands, `ftplugin/runic.lua` that calls the Lua helper to set buffer-local defaults (two-space indent, `commentstring`, tracked `b:undo_ftplugin`), and `after/syntax/runic.vim` which links every Tree-sitter capture to the canonical `Runic*` group.
- Added `editor/neovim/scripts/open_example.sh`, an executable helper that prepends the plugin directory to Neovim’s runtimepath and opens either the default `examples/data_and_flow.rn` or a caller-specified Runic source file for quick smoke tests.
- Documented the new runtime pieces and helper script inside `editor/neovim/README.md` so contributors know how to load the plugin via runtimepath and validate highlighting manually.

**Next task:** Task 6 – _Integrate with Runic repo tooling_. Wire helper commands/scripts so contributors can install or update the plugin easily.

## Task 6 – Integrate with Runic repo tooling _(completed 2025-11-16)_
- Added `scripts/neovim_plugin.sh`, a repo-level helper that symlinks the plugin into a user’s `nvim/site/pack/runic/start` directory (customizable via `--pack-root`, `--slot`, or `--target`), removes the link, reports status, or proxies to the `open_example.sh` launcher so contributors can pop open `examples/*.rn` buffers with the highlight rules already on `runtimepath`.
- Documented the script inside both `editor/neovim/README.md` (new “Helper Scripts” section) and the root `README.md` (new “Neovim syntax plugin” section) so the workflow is visible alongside the rest of the tooling guidance.
- The new helper checks for existing directories, supports `--force` replacement, and prints clear instructions for loading the plugin, giving contributors a one-command setup path that keeps the plugin synced with their working tree.

**Next task:** Task 7 – _Testing & validation_. Build automated highlight tests and smoke the plugin through Neovim.
