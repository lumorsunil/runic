#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${RUNIC_REPO_ROOT:-$(cd "$script_dir/.." && pwd)}"

if ! command -v zig >/dev/null 2>&1; then
  echo "zig is required for CLI smoke tests but was not found on PATH." >&2
  exit 1
fi

declare -a runic_scripts

if [[ $# -gt 0 ]]; then
  for target in "$@"; do
    if [[ -f "$target" ]]; then
      runic_scripts+=("$(cd "$repo_root" && realpath --relative-to="$repo_root" "$target" 2>/dev/null || printf '%s' "$target")")
    elif [[ -f "$repo_root/$target" ]]; then
      runic_scripts+=("$target")
    else
      echo "Unable to locate Runic script: $target" >&2
      exit 1
    fi
  done
else
  mapfile -t runic_scripts < <(
    cd "$repo_root" >/dev/null
    find tests -type f -name '*.rn' -print | LC_ALL=C sort
  )
fi

if [[ ${#runic_scripts[@]} -eq 0 ]]; then
  echo "CLI smoke tests skipped: no Runic scripts found under tests/."
  exit 0
fi

count=0
for script in "${runic_scripts[@]}"; do
  abs_path="$script"
  if [[ -f "$repo_root/$script" ]]; then
    abs_path="$repo_root/$script"
  fi

  rel_path="${abs_path#"$repo_root"/}"
  [[ "$rel_path" == "$abs_path" ]] && rel_path="$abs_path"

  echo "-- running ${rel_path}"
  (cd "$repo_root" && zig build run -- "$rel_path")
  ((count += 1))
done

echo "CLI smoke tests passed (${count} scripts)."
