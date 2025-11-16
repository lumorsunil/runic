#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: neovim_plugin.sh <command> [options]

Commands:
  install [options]       Symlink the Runic Neovim plugin into your packpath.
  uninstall [options]     Remove the symlink created by install.
  status [options]        Show whether the plugin is linked into your packpath.
  open-example [args]     Launch Neovim with the plugin and open an example buffer.
  help                    Show this message.

Common options (install/uninstall/status):
  --pack-root PATH        Override the base pack directory (default: $XDG_DATA_HOME/nvim/site/pack or ~/.local/share/...).
  --pack-name NAME        Choose the pack name under pack-root (default: runic).
  --slot start|opt        Choose the pack slot (default: start).
  --target PATH           Override the final target directory directly.
  --force                 Replace an existing directory/symlink at --target (install only).

open-example forwards its arguments to editor/neovim/scripts/open_example.sh, so you
can pass either a relative path under examples/ or an absolute file followed by "--"
plus extra Neovim flags.
USAGE
}

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
repo_root="$(cd -- "${script_dir}/.." >/dev/null 2>&1 && pwd)"
plugin_root="${repo_root}/editor/neovim"
open_example_script="${plugin_root}/scripts/open_example.sh"
if [[ ! -d "${plugin_root}" ]]; then
  echo "Runic Neovim plugin directory not found at ${plugin_root}" >&2
  exit 1
fi

xdg_data="${XDG_DATA_HOME:-${HOME}/.local/share}"
default_pack_root="${xdg_data}/nvim/site/pack"

declare pack_root
declare pack_name
declare pack_slot
declare explicit_target
declare force_replace

reset_defaults() {
  pack_root="${RUNIC_NEOVIM_PACK_ROOT:-${default_pack_root}}"
  pack_name="${RUNIC_NEOVIM_PACK_NAME:-runic}"
  pack_slot="${RUNIC_NEOVIM_PACK_SLOT:-start}"
  explicit_target="${RUNIC_NEOVIM_PLUGIN_TARGET:-}"
  force_replace=0
}

resolve_target() {
  if [[ -n "${explicit_target}" ]]; then
    printf '%s\n' "${explicit_target}"
    return
  fi
  case "${pack_slot}" in
    start|opt) ;;
    *)
      echo "Invalid pack slot: ${pack_slot} (expected start or opt)" >&2
      exit 1
      ;;
  esac
  printf '%s\n' "${pack_root%/}/${pack_name}/${pack_slot}/runic_highlight"
}

parse_common_flags() {
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --pack-root)
        if [[ $# -lt 2 ]]; then
          echo "--pack-root requires a value" >&2
          exit 1
        fi
        pack_root="$2"
        shift 2
        ;;
      --pack-name)
        if [[ $# -lt 2 ]]; then
          echo "--pack-name requires a value" >&2
          exit 1
        fi
        pack_name="$2"
        shift 2
        ;;
      --slot)
        if [[ $# -lt 2 ]]; then
          echo "--slot requires a value" >&2
          exit 1
        fi
        pack_slot="$2"
        shift 2
        ;;
      --target)
        if [[ $# -lt 2 ]]; then
          echo "--target requires a value" >&2
          exit 1
        fi
        explicit_target="$2"
        shift 2
        ;;
      --force)
        force_replace=1
        shift
        ;;
      -h|--help)
        usage
        exit 0
        ;;
      *)
        echo "Unknown option: $1" >&2
        exit 1
        ;;
    esac
  done
}

install_plugin() {
  reset_defaults
  parse_common_flags "$@"
  local target
  target="$(resolve_target)"
  local parent
  parent="$(dirname -- "${target}")"
  mkdir -p -- "${parent}"
  if [[ -e "${target}" && ! -L "${target}" ]]; then
    if [[ "${force_replace}" -ne 1 ]]; then
      echo "Target exists and is not a symlink: ${target}. Use --force to replace it." >&2
      exit 1
    fi
    rm -rf -- "${target}"
  fi
  if [[ -L "${target}" && "${force_replace}" -eq 0 ]]; then
    local existing
    existing="$(readlink -- "${target}")"
    if [[ "${existing}" == "${plugin_root}" ]]; then
      echo "Runic plugin already linked at ${target}"
      return
    fi
  fi
  ln -sfn -- "${plugin_root}" "${target}"
  echo "Linked ${target} -> ${plugin_root}"
  echo "Add 'packadd runic_highlight' or rely on the start directory to load automatically."
}

uninstall_plugin() {
  reset_defaults
  parse_common_flags "$@"
  local target
  target="$(resolve_target)"
  if [[ -L "${target}" || -e "${target}" ]]; then
    rm -rf -- "${target}"
    echo "Removed ${target}"
  else
    echo "No Runic plugin found at ${target}"
  fi
}

status_plugin() {
  reset_defaults
  parse_common_flags "$@"
  local target
  target="$(resolve_target)"
  if [[ -L "${target}" ]]; then
    local dest
    dest="$(readlink -- "${target}")"
    echo "runic_highlight linked: ${target} -> ${dest}"
  elif [[ -e "${target}" ]]; then
    echo "${target} exists but is not a symlink (run with --force to replace)."
  else
    echo "runic_highlight not installed at ${target}"
  fi
}

open_example() {
  if [[ ! -x "${open_example_script}" ]]; then
    echo "Example launcher missing at ${open_example_script}" >&2
    exit 1
  fi
  exec "${open_example_script}" "$@"
}

main() {
  if [[ $# -eq 0 ]]; then
    usage >&2
    exit 1
  fi
  local cmd="$1"
  shift
  case "${cmd}" in
    install)
      install_plugin "$@"
      ;;
    uninstall|remove)
      uninstall_plugin "$@"
      ;;
    status)
      status_plugin "$@"
      ;;
    open-example)
      open_example "$@"
      ;;
    help|-h|--help)
      usage
      ;;
    *)
      echo "Unknown command: ${cmd}" >&2
      usage >&2
      exit 1
      ;;
  esac
}

main "$@"
