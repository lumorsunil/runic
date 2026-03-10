$(echo "asdf")
array=("one" "two" $(echo "three"))
echo "${array[@]}"
