#!/bin/bash

[[ $# -ne 1 ]] && echo "$0  ‹path to other.erldocs.com dir›" && exit 1
osite="$1"

# Extract URLs of Erlang projects to serve as seed.

root='https://erlangcentral.org/erlang-projects'
outf=seed.sc
touch $outf

source we.sh

function REST() {
    local p=$1 # 0..49
    echo 'https://searchcode.com/?q=e%20lang%3Aerlang&p='$p'&loc=0&loc2=10000'
}

function find() {
    for proj in $(we -O - "$(REST $*)" | grep repo: | cut -d '>' -f 2 | cut -d '<' -f 1 | sed 's%//% %;s/\.git/ /' | awk '{print $2}' | tr '[:upper:]' '[:lower:]' | sort -u); do
        [[ -f $osite/$proj/meta.txt ]] && continue
        echo $proj
        echo $proj >>$outf
    done
}

for ((p=0; p<=49; p++)); do
    echo $p
    find $p
done
