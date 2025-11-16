#!/usr/bin/env bash

set -euo pipefail

lang="${RUNIC_LANG:-unknown}"
repo_root="${RUNIC_REPO_ROOT:?RUNIC_REPO_ROOT must be set}"

run_zig_fmt_check() {
  local -a zig_dirs=()

  for dir in src cmd tests; do
    if [[ -d "$repo_root/$dir" ]]; then
      zig_dirs+=("$dir")
    fi
  done

  if [[ ${#zig_dirs[@]} -eq 0 ]]; then
    echo "No Zig source directories found to lint; skipping."
    return 0
  fi

  (cd "$repo_root" && zig fmt --check "${zig_dirs[@]}")
}

case "$lang" in
  rust)
    if ! command -v cargo >/dev/null 2>&1; then
      echo "cargo not found in PATH. Install Rust to run Clippy." >&2
      exit 1
    fi
    (cd "$repo_root" && cargo clippy --all-targets --all-features -- -D warnings)
    ;;
  go)
    if ! command -v go >/dev/null 2>&1; then
      echo "go command not found in PATH. Install Go to run go vet." >&2
      exit 1
    fi
    (cd "$repo_root" && go vet ./...)
    ;;
  zig)
    if ! command -v zig >/dev/null 2>&1; then
      echo "zig command not found in PATH. Install Zig to run the linter." >&2
      exit 1
    fi
    run_zig_fmt_check
    ;;
  *)
    echo "Linter stage skipped: no recognized toolchain (set RUNIC_LANG or add Cargo.toml/go.mod/build.zig)."
    ;;
esac
