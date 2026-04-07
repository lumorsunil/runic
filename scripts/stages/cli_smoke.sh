#!/usr/bin/env bash

set -euo pipefail

repo_root="${RUNIC_REPO_ROOT:-${1:-}}"

if [[ -z "$repo_root" ]]; then
  echo "RUNIC_REPO_ROOT must be set (or passed as the first argument)." >&2
  exit 1
fi

mapfile -t cli_scripts < <(
  cd "$repo_root"
  find tests -maxdepth 1 -type f -name 'cli_*.sh' 2>/dev/null | sort || true
)

if [[ ${#cli_scripts[@]} -eq 0 ]]; then
  echo "CLI regression stage skipped: add shell scripts matching tests/cli_*.sh."
  exit 0
fi

for script in "${cli_scripts[@]}"; do
  echo "-- running ${script}"
  (cd "$repo_root" && "$script")
done
