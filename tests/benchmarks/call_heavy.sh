#!/usr/bin/env bash
set -euo pipefail

inc() { echo $(($1 + 1)); }

total=0
for ((i = 0; i < 20000; i++)); do
  x=$(inc "$i")
  total=$((total + x))
done
echo "$total"
