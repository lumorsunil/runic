#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
plugin_root="$(cd -- "${script_dir}/.." >/dev/null 2>&1 && pwd)"
repo_root="$(cd -- "${plugin_root}/../.." >/dev/null 2>&1 && pwd)"

if [[ $# -gt 0 && ( "$1" == "-h" || "$1" == "--help" ) ]]; then
  cat <<'EOF'
Usage: open_example.sh [relative-or-absolute-path] [-- additional nvim args]

Launches Neovim with the Runic syntax plugin on the runtimepath and opens the
requested Runic source file. Defaults to examples/data_and_flow.rn when no path
is provided. Extra arguments after `--` are forwarded to nvim.
EOF
  exit 0
fi

example="${repo_root}/examples/data_and_flow.rn"
target=""
if [[ $# -gt 0 && "$1" != "--" ]]; then
  target="$1"
  shift
fi

if [[ -z "${target}" ]]; then
  target="${example}"
elif [[ "${target}" != /* ]]; then
  target="${repo_root}/${target#./}"
fi

if [[ ! -f "${target}" ]]; then
  echo "File not found: ${target}" >&2
  exit 1
fi

if [[ $# -gt 0 && "$1" == "--" ]]; then
  shift
fi
nvim_args=("$@")

rtp_cmd="lua vim.opt.runtimepath:prepend(vim.fn.fnamemodify([[${plugin_root}]], ':p'))"
exec nvim --cmd "${rtp_cmd}" "${target}" "${nvim_args[@]}"
