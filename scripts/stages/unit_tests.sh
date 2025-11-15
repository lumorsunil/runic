#!/usr/bin/env bash

set -euo pipefail

lang="${RUNIC_LANG:-unknown}"
repo_root="${RUNIC_REPO_ROOT:?RUNIC_REPO_ROOT must be set}"

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
