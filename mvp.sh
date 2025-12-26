# basic echo and pipeline
echo "hello, world!"
printf "without newline..."
echo "hello cat!" | cat

# if blocks
if true; then
  echo "true"
else
  echo "false"
fi

# if inline
if true; then echo "true"; else echo "false"; fi

# for loop
for i in {0..9}; do
  echo "$i: for loop"
done

# for loop dynamic range
n=4
for i in $(seq 0 $n); do echo "$i: variable range end"; done

# functions
hello() {
  echo "Hello! $1"
}
hello "I'm runic."

# bindings
declare -r string_ex="this is an example string"
# shellcheck disable=SC2155
declare -r echo_result="$(printf "%s" "$string_ex")"
echo "$echo_result"

# integers
my_integer_a=4
my_integer_b=3
echo "my_integer_a + my_integer_b = $((my_integer_a + my_integer_b))"

# floats
my_integer=3
my_float=2.5
echo "my_integer = $my_integer"
echo "my_float = $my_float"
echo "my_integer + my_float = $(echo "$my_integer + $my_float" | bc)"

# compound assignment
my_var_int=1
echo "my_var_int = $my_var_int"
((my_var_int+=3))
echo "my_var_int += 3 = $my_var_int"
((my_var_int-=1))
echo "my_var_int -= 1 = $my_var_int"
((my_var_int*=4))
echo "my_var_int *= 4 = $my_var_int"
((my_var_int/=2))
echo "my_var_int /= 2 = $my_var_int"
((my_var_int%=2))
echo "my_var_int %= 2 = $my_var_int"

# booleans
printf "\nbooleans\n\n"

and_table=$(cat << EOF
and table | true | false
true | $(true && true; echo $?) | $(true && false; echo $?)
false | $(false && true; echo $?) | $(false && false; echo $?)
EOF
)
echo "$and_table" | column "-t" "-s" "|"
echo ""
or_table=$(cat << EOF
or table | true | false
true | $(true || true; echo $?) | $(true || false; echo $?)
false | $(false || true; echo $?) | $(false || false; echo $?)
EOF
)
echo "${or_table}" | column "-t" "-s" "|"
echo ""

# arrays
my_array=("one" "two" "three")
for item in "${my_array[@]}"; do
    echo "$item"
done

for i in "${!my_array[@]}"; do
    echo "$i: ${my_array[$i]}"
done

my_array_2=("four" "five" "six")
for i in "${!my_array[@]}"; do
    echo "$i: ${my_array[$i]},${my_array_2[$i]}"
done

# functions with pipelines
# hello "I'm runic with a pipeline." | cat
