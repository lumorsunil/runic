echo "hello" && echo "command succeeded"
echo "$?"

grep --invalid-flag 2>/dev/null || echo "command failed"
echo "$?"

grep --invalid-flag 2>/dev/null && echo "command succeeded"
echo "$?"

echo "echo test"
echo "hello" && echo "command succeeded"
echo "hello" || echo "command failed"

echo "grep test"
grep --invalid-flag 2>/dev/null && echo "command succeeded"
grep --invalid-flag 2>/dev/null || echo "command failed"
