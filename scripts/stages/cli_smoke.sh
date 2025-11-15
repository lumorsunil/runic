#!/usr/bin/env bash

set -euo pipefail

repo_root="${RUNIC_REPO_ROOT:?RUNIC_REPO_ROOT must be set}"

mapfile -t cli_scripts < <(
  cd "$repo_root"
  find tests -maxdepth 1 -type f -name 'cli_*.sh' 2>/dev/null | sort || true
)

if [[ ${#cli_scripts[@]} -eq 0 ]]; then
  echo "CLI smoke stage skipped: add shell scripts matching tests/cli_*.sh."
  exit 0
fi

for script in "${cli_scripts[@]}"; do
  echo "-- running ${script}"
  (cd "$repo_root" && "$script")
done
