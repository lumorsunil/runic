expr="$1"
printf 'echo "%s: ${%s}"' "$expr" "$expr" > tmp.rn
runic tmp.rn
rm tmp.rn
