#!/bin/bash

[[ "$(basename "$(dirname "$0")")" != 'find' ]] && echo cd into find/ && exit 2

[[ $# -ne 1 ]] && echo "Usage: $0  website dir" && exit 1
ofile=$RANDOM
touch $ofile

finders="$(ls -1 ??.sh)"
P=$(echo "$finders" | wc -l)
echo "$finders" | xargs -P $P -n 1 -t

sort -u seed.* > $ofile
wc -l seed.* $ofile
rm seed.*
mv $ofile ../seeds
