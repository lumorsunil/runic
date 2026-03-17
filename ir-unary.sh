echo "Check a file:"
file=$(head "-n" "1")

if [ -f "$file" ]; then
  echo "$file exists"
else
  echo "$file does not exist"
fi
