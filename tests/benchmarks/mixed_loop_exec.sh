#!/usr/bin/env bash
set -euo pipefail

total=0
for ((i = 0; i < 2000; i++)); do
  total=$((total + i))
  echo "hello"
done
echo "$total"
