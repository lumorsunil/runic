#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${RUNIC_REPO_ROOT:-$(cd "$script_dir/.." && pwd)}"
repo_root="$(cd "$repo_root" && pwd)"
pwd_marker="__RUNIC_PWD__"

if ! command -v zig >/dev/null 2>&1; then
  echo "zig is required for CLI diagnostic tests but was not found on PATH." >&2
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
      echo "Unable to locate Runic diagnostic script: $target" >&2
      exit 1
    fi
  done
else
  mapfile -t runic_scripts < <(
    cd "$repo_root" >/dev/null
    find tests/diagnostics -type f -name '*.rn' -print | LC_ALL=C sort
  )
fi

if [[ ${#runic_scripts[@]} -eq 0 ]]; then
  echo "CLI diagnostic tests skipped: no Runic scripts found under tests/diagnostics."
  exit 0
fi

strip_ansi() {
  perl -pe 's/\e\[[0-9;]*m//g'
}

normalize_output() {
  RUNIC_NORMALIZE_PWD="$repo_root" RUNIC_PWD_MARKER="$pwd_marker" perl -pe '
    BEGIN {
      $pwd = $ENV{RUNIC_NORMALIZE_PWD} // "";
      $marker = $ENV{RUNIC_PWD_MARKER} // "__RUNIC_PWD__";
    }
    s/\Q$pwd\E/$marker/g if length $pwd;
  '
}

(
  cd "$repo_root"
  zig build >/dev/null
)

runic_bin="$repo_root/zig-out/bin/runic"
if [[ ! -x "$runic_bin" ]]; then
  echo "runic binary was not built at $runic_bin" >&2
  exit 1
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

  expected_status=1
  if [[ -f "$status_fixture" ]]; then
    expected_status="$(tr -d '[:space:]' < "$status_fixture")"
  fi

  stdout_tmp="$(mktemp)"
  stderr_tmp="$(mktemp)"
  stdout_norm_tmp="$(mktemp)"
  stderr_norm_tmp="$(mktemp)"
  stdout_fixture_norm_tmp="$(mktemp)"
  stderr_fixture_norm_tmp="$(mktemp)"
  echo "-- running ${rel_path}"

  set +e
  if [[ -f "$stdin_fixture" ]]; then
    (cd "$repo_root" && "$runic_bin" "$rel_path" < "$stdin_fixture") >"$stdout_tmp" 2>"$stderr_tmp"
  else
    (cd "$repo_root" && "$runic_bin" "$rel_path") >"$stdout_tmp" 2>"$stderr_tmp"
  fi
  status=$?
  set -e

  strip_ansi <"$stdout_tmp" | normalize_output >"$stdout_norm_tmp"
  strip_ansi <"$stderr_tmp" | normalize_output >"$stderr_norm_tmp"

  if [[ "$status" != "$expected_status" ]]; then
    echo "Unexpected exit status for ${rel_path}: got ${status}, expected ${expected_status}" >&2
    cat "$stdout_norm_tmp" >&2
    cat "$stderr_norm_tmp" >&2
    exit 1
  fi

  if [[ -f "$stdout_fixture" ]]; then
    strip_ansi <"$stdout_fixture" | normalize_output >"$stdout_fixture_norm_tmp"
    if ! diff -u "$stdout_fixture_norm_tmp" "$stdout_norm_tmp"; then
      echo "Stdout mismatch for ${rel_path}" >&2
      exit 1
    fi
  elif [[ -s "$stdout_norm_tmp" ]]; then
    echo "Unexpected stdout for ${rel_path}" >&2
    cat "$stdout_norm_tmp" >&2
    exit 1
  fi

  if [[ -f "$stderr_fixture" ]]; then
    strip_ansi <"$stderr_fixture" | normalize_output >"$stderr_fixture_norm_tmp"
    if ! diff -u "$stderr_fixture_norm_tmp" "$stderr_norm_tmp"; then
      echo "Stderr mismatch for ${rel_path}" >&2
      exit 1
    fi
  elif [[ -s "$stderr_norm_tmp" ]]; then
    echo "Unexpected stderr for ${rel_path}" >&2
    cat "$stderr_norm_tmp" >&2
    exit 1
  fi

  rm -f \
    "$stdout_tmp" \
    "$stderr_tmp" \
    "$stdout_norm_tmp" \
    "$stderr_norm_tmp" \
    "$stdout_fixture_norm_tmp" \
    "$stderr_fixture_norm_tmp"
  ((count += 1))
done

echo "CLI diagnostic tests passed (${count} scripts)."
