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

  base_path="${abs_path%.rn}"
  stdin_fixture="${base_path}.stdin"
  stdout_fixture="${base_path}.stdout"
  stderr_fixture="${base_path}.stderr"
  status_fixture="${base_path}.status"

  expected_status=0
  if [[ -f "$status_fixture" ]]; then
    expected_status="$(tr -d '[:space:]' < "$status_fixture")"
  fi

  stdout_tmp="$(mktemp)"
  stderr_tmp="$(mktemp)"
  echo "-- running ${rel_path}"

  set +e
  if [[ -f "$stdin_fixture" ]]; then
    (cd "$repo_root" && zig build run -- "$rel_path" < "$stdin_fixture") >"$stdout_tmp" 2>"$stderr_tmp"
  else
    (cd "$repo_root" && zig build run -- "$rel_path") >"$stdout_tmp" 2>"$stderr_tmp"
  fi
  status=$?
  set -e

  if [[ "$status" != "$expected_status" ]]; then
    echo "Unexpected exit status for ${rel_path}: got ${status}, expected ${expected_status}" >&2
    cat "$stderr_tmp" >&2
    exit 1
  fi

  if [[ -f "$stdout_fixture" ]]; then
    if ! diff -u "$stdout_fixture" "$stdout_tmp"; then
      echo "Stdout mismatch for ${rel_path}" >&2
      exit 1
    fi
  fi

  if [[ -f "$stderr_fixture" ]]; then
    if ! diff -u "$stderr_fixture" "$stderr_tmp"; then
      echo "Stderr mismatch for ${rel_path}" >&2
      exit 1
    fi
  elif [[ -s "$stderr_tmp" ]]; then
    echo "Unexpected stderr for ${rel_path}" >&2
    cat "$stderr_tmp" >&2
    exit 1
  fi

  rm -f "$stdout_tmp" "$stderr_tmp"
  ((count += 1))
done

echo "CLI smoke tests passed (${count} scripts)."
