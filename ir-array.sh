rm -f ir-array.log
# in bash, this evaluates in sequence
array=($(printf "one " | tee -a ir-array.log) $(sleep 2; printf "two " | tee -a ir-array.log) $(printf "three " | tee -a ir-array.log))
echo "array = ${array[*]}"
echo "log = $(cat ir-array.log)"
rm ir-array.log
