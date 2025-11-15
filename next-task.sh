#!/usr/bin/env bash

set -euo pipefail

PLAN_FILE="docs/plan.md"
PROGRESS_FILE="docs/progress.md"

if [[ ! -f "$PLAN_FILE" ]]; then
  echo "Plan file $PLAN_FILE not found." >&2
  exit 1
fi

mapfile -t tasks < <(grep -E '^[0-9]+\.' "$PLAN_FILE")
if [[ ${#tasks[@]} -eq 0 ]]; then
  echo "No numbered tasks found in $PLAN_FILE." >&2
  exit 1
fi

if [[ ! -f "$PROGRESS_FILE" ]]; then
  cat <<'EOF' >"$PROGRESS_FILE"
# Runic Plan Progress

## Completed Tasks
EOF
fi

completed=$(grep -E '^[0-9]+\.' "$PROGRESS_FILE" | wc -l || true)
total=${#tasks[@]}

if (( completed >= total )); then
  echo "All tasks from $PLAN_FILE have already been executed."
  exit 0
fi

next_task="${tasks[$completed]}"
timestamp=$(date -Iseconds)

cat <<EOF
Executing next task:
$next_task
EOF

repo_root=$(pwd)
prompt=$(cat <<EOF
You are working in the Runic repository at $repo_root.
Use docs/plan.md for the ordered list of tasks, features.md for the language behavior details, and README.md for tooling/build instructions. Completed work should be documented in docs/progress.md after execution.
Your next task from docs/plan.md is:
$next_task

Carry out the changes necessary to finish this task inside the repository.
EOF
)

codex exec "$prompt"

{
  printf '\n'
  printf '%s (completed %s)\n' "$next_task" "$timestamp"
} >>"$PROGRESS_FILE"

echo "Recorded progress in $PROGRESS_FILE."
