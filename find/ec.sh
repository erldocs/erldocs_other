#!/bin/bash

[[ $# -ne 1 ]] && echo "$0  ‹path to other.erldocs.com dir›" && exit 1
osite="$1"

# Extract URLs of Erlang projects to serve as seed.

root='https://erlangcentral.org/erlang-projects'
outf=seed.ec
touch $outf

# Temp dir
tmp=repos
mkdir $tmp || exit 1

# Max number of pages
tmp_root=$tmp/root
curl -fsSLo $tmp_root $root
max=$(grep -Po 'href="\d+">\d+</a><a' $tmp_root | cut -d '"' -f 2 | sort -r | head -n 1)
rm $tmp_root
[[ "$max" = '' ]] && exit 2

for i in $(seq 1 "$max"); do
    echo $root/$i
    # Fetch HTML page
    curl -fsSLo $tmp/$i $root/$i

    # Extract projects' path
    for url in $(grep -P '"[^\s][^"]+"\s+target=[^,]+$' $tmp/$i | cut -d '"' -f 4); do
	proj=$(echo $url | sed 's%https://%%;s%http://%%' | tr '[:upper:]' '[:lower:]')
	[[ -f $osite/$proj/meta.txt ]] && continue
	echo $proj
	echo $proj >>$outf
    done
done
[[ $? -eq 0 ]] || exit 42

rm -rf $tmp/
cat $outf | tr '[:upper:]' '[:lower:]' | sed 's%^https\?://%%' | sed 's%/$%%' >rs && rm $outf
sort -u rs >s
find . -name meta.txt | cut -c3- | sed 's/.........$//' >__
sort -u __ >_
comm -23 s _ >the_repos
rm _ __ s rs
