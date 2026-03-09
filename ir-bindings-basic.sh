s="hello, world!"
echo "$s"

x=$(if false; then echo "this is from then"; else "nothing is happening here, this is just a literal"; fi)
echo "$x"
