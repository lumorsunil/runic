#!/usr/bin/env bash

set -euo pipefail

repo_root="${RUNIC_REPO_ROOT:-${1:-}}"
lang="${RUNIC_LANG:-${2:-unknown}}"

if [[ -z "$repo_root" ]]; then
  echo "RUNIC_REPO_ROOT must be set (or passed as the first argument)." >&2
  exit 1
fi

case "$lang" in
  rust)
    if ! command -v cargo >/dev/null 2>&1; then
      echo "cargo not found in PATH. Install Rust to run tests." >&2
      exit 1
    fi
    (cd "$repo_root" && cargo test --lib --bins --tests)
    ;;
  go)
    if ! command -v go >/dev/null 2>&1; then
      echo "go command not found in PATH. Install Go to run tests." >&2
      exit 1
    fi
    (cd "$repo_root" && go test ./...)
    ;;
  zig)
    if ! command -v zig >/dev/null 2>&1; then
      echo "zig command not found in PATH. Install Zig to run tests." >&2
      exit 1
    fi
    (cd "$repo_root" && zig build test)
    ;;
  *)
    echo "Unit test stage skipped: no recognized toolchain (set RUNIC_LANG or add Cargo.toml/go.mod/build.zig)."
    ;;
esac
