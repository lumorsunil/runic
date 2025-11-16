# Neovim Syntax Extension Scaffolding

This document locks in the structure, runtime requirements, and optional
dependencies for the upcoming Neovim syntax plugin so subsequent tasks can fill
in implementation details without re-litigating the basics.

## Layout & Runtime Files

All plugin code will live under `editor/neovim/` to keep editor integrations
separated from the interpreter toolchain. The directory tree will look like:

```
editor/neovim/
├── README.md                   # quickstart, installation, roadmap
├── lua/runic_highlight/
│   ├── init.lua                # public setup() and configuration handling
│   ├── highlights.lua          # shared helpers for queries + regex
│   └── dev.lua                 # helper utilities for tests and example buffers
├── plugin/runic_highlight.lua  # auto-calls setup() and wires augroups
├── ftdetect/runic.vim          # filetype detection via vim.filetype.add fallback
├── ftplugin/runic.lua          # buffer-local options (commentstring, indent)
├── syntax/runic.vim            # regex matcher for baseline highlighting
├── after/syntax/runic.vim      # highlight group links + overrides
├── queries/runic/
│   └── highlights.scm          # Tree-sitter captures (loaded when TS parser exists)
├── tests/
│   ├── highlighting_spec.lua   # Plenary/Busted tests for highlight groups
│   └── fixtures/*.rn           # copied from examples/ for deterministic buffers
└── scripts/
    └── open_example.sh         # helper to launch Neovim with sample files
```

Key points:
- Runtime directories live at the root so `:set runtimepath+=...` immediately
  exposes filetype detection and syntax rules. Lua helpers remain separate to
  keep logic testable.
- Regex-based highlighting in `syntax/runic.vim` ships first and is always
  loaded; Tree-sitter queries in `queries/runic/highlights.scm` are optional
  and only activated when the user installs a `tree-sitter-runic` parser.
- The Lua `highlights.lua` module centralizes token → highlight group mappings
  so both regex matches and Tree-sitter captures stay in sync.
- Tests and scripts live alongside the plugin to avoid mixing editor tooling
  into the core Runic interpreter tree.

## Minimum Supported Neovim Version

- **Neovim 0.9.5+** is required. This unlocks `vim.filetype.add`, Lua-based
  `ftplugin` files, `vim.iter` helpers used in table-driven highlight specs, and
  Tree-sitter APIs that remain stable in 0.9+. Earlier releases would need VimL
  fallbacks we do not intend to maintain.
- Users on 0.8 can technically use `syntax/runic.vim`, but we will not test or
  document that workflow; the plugin prints a warning if `vim.fn.has("nvim-0.9")`
  fails.

## Dependencies

- **Core runtime:** no hard dependencies beyond Neovim itself; the plugin will
  ship as a standard runtimepath package so it works with `packadd`, Packer,
  Lazy, or manual `rtp` modifications.
- **Optional Tree-sitter:** when `nvim-treesitter` manages a `tree-sitter-runic`
  parser, the plugin automatically loads `queries/runic/highlights.scm`. Until
  that grammar exists, the regex syntax file provides complete coverage.
- **Testing:** developer-only dependency on `plenary.nvim` (for busted test
  helpers) plus `neovim --headless` to run CI jobs located under
  `editor/neovim/tests/`.
- **Examples:** tests and manual validation reuse Runic source files from
  `examples/*.rn`; no additional fixtures are needed.

## Integration Hooks

- `editor/neovim/README.md` will document installation snippets for Lazy, Packer,
  and Vim-Plug plus developer instructions for running tests and syncing the
  plugin with the Runic repo.
- CI or helper scripts (future task) can call `nvim --headless -c
  "lua require('runic_highlight.dev').smoke()"` to ensure highlight groups load.
- A `scripts/open_example.sh` helper will bootstrap Neovim with the plugin on
  the runtimepath and open a chosen `examples/*.rn` file for quick manual
  inspection.
