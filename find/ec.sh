#!/bin/bash

[[ $# -ne 1 ]] && echo "$0  ‹path to other.erldocs.com dir›" && exit 1
osite="$1"
outf=seed.ec

# Extract URLs of Erlang projects to serve as seed.

source we.sh

root='https://erlangcentral.org/erlang-projects'

function REST() {
    local p=$1
    echo $root/$p
}

function find() {
    for url in $(we -O - "$(REST $*)" | grep -P '"[^\s][^"]+"\s+target=[^,]+$' | cut -d '"' -f 4); do
	proj=$(echo $url | sed 's%https://%%;s%http://%%' | tr '[:upper:]' '[:lower:]')
	[[ -f $osite/$proj/meta.txt ]] && continue
	echo $proj
	echo $proj >>$outf
    done
}


max=$(we -O - $root | grep -Po 'href="\d+">\d+</a><a' | cut -d '"' -f 2 | sort -r | head -n 1)
[[ "$max" = '' ]] && exit 2

for ((i=1; i<=$max; i++)); do
    find $i
done
