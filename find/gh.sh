#!/bin/bash

[[ $# -ne 1 ]] && echo "$0  ‹path to other.erldocs.com dir›" && exit 1
osite="$1"

# Extract URLs of Erlang projects to serve as seed.

root='https://erlangcentral.org/erlang-projects'
outf=gh.seed
touch $outf

function REST() {
    local p=$1 # 1..100
    local cat=$2 # stars forks updated
    local ordering=$3 # desc asc
    echo 'https://github.com/search?o='$ordering'&p='$p'&q=language%3Aerlang&ref=searchresults&s='$cat'&type=Repositories&utf8=%E2%9C%93'
}

function find() {
    for found in $(we -O - "$(REST $*)" | grep -Eoa '<a href="/([^"]+)">\1</a>' | cut -d '"' -f 2); do
        proj=github.com$found
        [[ -f $osite/$proj/meta.txt ]] && continue
        echo $proj
        echo $proj >>$outf
    done
}

for ordering in asc desc; do
    for category in stars forks updated; do
        for ((p=1; p<=100; p++)); do
            echo $p $category $ordering
            find $p $category $ordering
        done
    done
done
