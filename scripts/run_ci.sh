#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
stages_dir="$repo_root/scripts/stages"

detect_language() {
  if [[ -n "${RUNIC_LANG:-}" ]]; then
    echo "$RUNIC_LANG"
    return
  fi

  if [[ -f "$repo_root/Cargo.toml" ]]; then
    echo "rust"
    return
  fi

  if [[ -f "$repo_root/go.mod" ]]; then
    echo "go"
    return
  fi

  if [[ -f "$repo_root/build.zig" ]]; then
    echo "zig"
    return
  fi

  echo "unknown"
}

run_stage() {
  local stage_name="$1"
  local script_path="$stages_dir/$stage_name.sh"

  if [[ ! -x "$script_path" ]]; then
    echo "Missing stage script: $script_path" >&2
    exit 1
  fi

  echo ""
  echo "==> $stage_name"
  RUNIC_LANG="$language" RUNIC_REPO_ROOT="$repo_root" "$script_path"
}

language="$(detect_language)"

echo "Runic CI pipeline (formatter → linter → unit tests → CLI smoke tests)"
echo "Detected toolchain: $language"

mkdir -p "$stages_dir"

run_stage "formatter"
run_stage "linter"
run_stage "unit_tests"
run_stage "cli_smoke"
