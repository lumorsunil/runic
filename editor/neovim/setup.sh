#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
repo_root="$(cd -- "${script_dir}/../.." >/dev/null 2>&1 && pwd)"
plugin_root="${repo_root}/editor/neovim"
lsp_binary="${repo_root}/zig-out/bin/runic-lsp"

check_neovim() {
  if ! command -v nvim &>/dev/null; then
    echo "Error: Neovim is not installed" >&2
    return 1
  fi

  local version
  version="$(nvim --version | head -1 | grep -oP 'v?\K\d+\.\d+\.\d+' || true)"
  if [[ -z "${version}" ]]; then
    echo "Error: Could not determine Neovim version" >&2
    return 1
  fi

  local major minor
  major="$(echo "${version}" | cut -d. -f1)"
  minor="$(echo "${version}" | cut -d. -f2)"

  if [[ "${major}" -eq 0 && "${minor}" -lt 9 ]]; then
    echo "Error: Neovim 0.9+ required, found ${version}" >&2
    return 1
  fi

  echo "Neovim ${version} detected"
  return 0
}

build_lsp() {
  if [[ -x "${lsp_binary}" ]]; then
    echo "LSP already built at ${lsp_binary}"
    return 0
  fi

  echo "Building Runic LSP..."
  local lsp_dir="${repo_root}/cmd/runic-lsp"
  if [[ ! -d "${lsp_dir}" ]]; then
    echo "Error: LSP source not found at ${lsp_dir}" >&2
    return 1
  fi

  if ! command -v zig &>/dev/null; then
    echo "Error: Zig is not installed" >&2
    return 1
  fi

  (cd "${repo_root}" && zig build runic-lsp)

  if [[ ! -x "${lsp_binary}" ]]; then
    echo "Error: LSP binary not found at ${lsp_binary}" >&2
    return 1
  fi

  echo "LSP built successfully at ${lsp_binary}"
}

install_plugin() {
  echo "Installing Runic Neovim plugin..."
  local plugin_script="${repo_root}/scripts/neovim_plugin.sh"

  if [[ -x "${plugin_script}" ]]; then
    "${plugin_script}" install
  else
    local xdg_data="${XDG_DATA_HOME:-${HOME}/.local/share}"
    local pack_dir="${xdg_data}/nvim/site/pack/runic/start/runic_highlight"
    mkdir -p "$(dirname "${pack_dir}")"
    ln -sfn "${plugin_root}" "${pack_dir}"
    echo "Linked plugin to ${pack_dir}"
  fi
}

generate_lsp_config() {
  local config_dir="${HOME}/.config/nvim/lua"
  local config_file="${config_dir}/runic_lsp_config.lua"

  mkdir -p "${config_dir}"

  cat >"${config_file}" <<'EOF'
-- Runic LSP configuration for nvim-lspconfig
-- Add to your init.lua: require("runic_lsp_config")

local lspconfig_ok, lspconfig = pcall(require, "lspconfig")
if not lspconfig_ok then
  return nil
end

local configs_ok, configs = pcall(require, "lspconfig.configs")
if not configs_ok then
  lspconfig.configs.runic_lsp = {
    default_config = {
      cmd = { "PATH_TO_RUNIC_LSP", "--stdio" },
      filetypes = { "runic" },
      root_dir = function(fname)
        return lspconfig.util.find_git_ancestor(fname) or vim.loop.cwd()
      end,
      settings = {},
    },
    handlers = {
      ["textDocument/hover"] = lsphandlers.hover,
      ["textDocument/signatureHelp"] = lsphandlers.signature_help,
    },
  }
else
  if not configs.runic_lsp then
    configs.runic_lsp = {
      default_config = {
        cmd = { "PATH_TO_RUNIC_LSP", "--stdio" },
        filetypes = { "runic" },
        root_dir = function(fname)
          return lspconfig.util.find_git_ancestor(fname) or vim.loop.cwd()
        end,
        settings = {},
      },
    }
  end
  lspconfig.runic_lsp.setup({})
end
EOF

  sed -i "s|PATH_TO_RUNIC_LSP|${lsp_binary}|g" "${config_file}"

  echo "LSP config written to ${config_file}"
  echo ""
  echo "Add to your init.lua:"
  echo "  -- If using nvim-lspconfig:"
  echo "  require('runic_lsp_config')"
  echo ""
  echo "  -- Or manually:"
  echo "  require('lspconfig').runic_lsp.setup{"
  echo "    cmd = {'${lsp_binary}', '--stdio'},"
  echo "    filetypes = {'runic'},"
  echo "  }"
}

print_summary() {
  echo ""
  echo "=== Setup Complete ==="
  echo ""
  echo "Runic editor support is ready:"
  echo "  - Syntax highlighting: Installed"
  echo "  - LSP: ${lsp_binary}"
  echo ""
  echo "To test, run:"
  echo "  nvim examples/data_and_flow.rn"
  echo ""
  echo "For LSP support, ensure nvim-lspconfig is installed and add:"
  echo "  require('lspconfig').runic_lsp.setup{}"
}

main() {
  check_neovim
  build_lsp
  install_plugin
  generate_lsp_config
  print_summary
}

main "$@"
