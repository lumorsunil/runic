#!/usr/bin/env bash
set -euo pipefail

total=0
for ((i = 0; i < 20000; i++)); do
  total=$((total + i))
done
echo "$total"
