# Runic Neovim Plugin

This directory hosts the Neovim runtime files that ship Runic syntax highlighting
and related helpers. The structure chosen for the plugin (Lua modules plus the
standard `ftdetect`/`ftplugin`/`syntax` directories) is described in detail in
`docs/neovim-extension-scaffolding.md`.

## Current Decisions

- **Layout:** Lua entrypoint under `lua/runic_highlight/`, Vim runtime files at
  the top level (`ftdetect`, `ftplugin`, `syntax`, `after/syntax`), Tree-sitter
  queries under `queries/runic/`, and automated tests in `tests/`.
- **Minimum Neovim version:** 0.9.5+. Earlier releases may load the syntax file,
  but we do not guarantee support.
- **Dependencies:** no hard dependencies besides Neovim; optional integration
  with `nvim-treesitter` when a `tree-sitter-runic` grammar exists and developer
  tests leverage `plenary.nvim`.

Future tasks in the Neovim extension plan will populate this directory with the
actual runtime files, queries, and scripts.

## Runtime Components

- `ftdetect/runic.vim` wires `.rn` files (plus `Runicfile`) to the
  `runic` filetype so buffers pick up the syntax rules automatically.
- `ftplugin/runic.lua` calls into `lua/runic_highlight/init.lua` to set
  buffer-local defaults such as `commentstring` and two-space indentation.
- `after/syntax/runic.vim` links each Tree-sitter capture
  (`@keyword.declaration.runic`, etc.) to the canonical `Runic*` highlight group
  so regex and parser-driven highlighting share the same palette.

## Installation

- **lazy.nvim:** add the plugin directory as a spec so Lazy can source the
  `ftdetect` rules ahead of time and lazily load the rest of the runtime when a
  Runic buffer opens.

  ```lua
  {
    dir = "/path/to/runic/editor/neovim",
    name = "runic-highlighter",
    ft = "runic",
    opts = {},
    config = function(_, opts)
      require("runic_highlight").setup(opts)
    end,
  }
  ```

- **Manual packpath install:** run `./scripts/neovim_plugin.sh install` from the
  repo root to symlink the plugin into `nvim/site/pack/<name>/start`. Use
  `./scripts/neovim_plugin.sh open-example` to smoke-test the plugin against
  `examples/*.rn`.

## Helper Scripts

- `editor/neovim/scripts/open_example.sh` launches Neovim with this directory on
  the runtimepath and opens a Runic example (`examples/data_and_flow.rn` by
  default). Pass a different path or `--` plus extra flags to forward them to
  Neovim for quick manual smoke tests.
- `./scripts/neovim_plugin.sh install` symlinks the plugin into your local
  `nvim/site/pack/runic/start` so Neovim picks it up without extra config. The
  same tool exposes `status`, `uninstall`, and `open-example` subcommands; the
  latter delegates to the helper above so contributors can pop open any file
  from `examples/*.rn` with the plugin preloaded.
