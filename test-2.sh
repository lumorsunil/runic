hello="hello"
value="$hello, world!"
b=true
p="$(printf "hello, world!\nno\nyes" | grep "yes")"

echo "$value"
echo "$b"
echo "$p"
echo "\n"

grep "-no"
