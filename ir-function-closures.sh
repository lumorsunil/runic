x=3

function hello() {
    echo "$1: $x"
}

x=5

function hello2() {
    echo "$1: $x"
}

hello "hello1"
hello2 "hello2"

x=7

hello "hello1"
hello2 "hello2"
