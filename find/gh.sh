#!/bin/bash

#TODO https://github.com/issues?utf8=%E2%9C%93&q=+is%3Arepo+language%3Aerlang+

outf=seed.gh

# Extract URLs of Erlang projects to serve as seed.

source function.we.sh

function REST() {
    local p=$1 # 1..100
    local cat=$2 # stars forks updated
    local ordering=$3 # desc asc
    echo 'https://github.com/search?o='$ordering'&p='$p'&q=language%3Aerlang&ref=searchresults&s='$cat'&type=Repositories&utf8=%E2%9C%93'
}

function find() {
    for found in $(we -O - "$(REST $*)" | grep -Eoa '<a href="/([^"]+)">\1</a>' | cut -d '"' -f 2 | tr '[:upper:]' '[:lower:]'); do
        proj=github.com$found
        echo $proj
        echo $proj >>$outf
    done
}

function iter() {
    local category=$1
    local ordering=$2
    for ((p=1; p<=100; p++)); do
        echo $p $category $ordering
        find $p $category $ordering
        sleep 5
    done
}


# for ordering in asc desc; do
#     for category in stars forks updated; do
#        iter $category $ordering
#     done
# done

iter stars asc
sleep 30
iter forks asc
sleep 30
iter updated asc
sleep 30
iter stars desc
sleep 30
iter forks desc
sleep 30
iter updated desc
