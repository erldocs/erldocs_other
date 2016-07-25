#!/bin/bash

[[ "$(basename "$PWD")" != 'find' ]] && echo cd into find/ && exit 2

ofile=$RANDOM
touch $ofile

finders="$(ls -1 ??.sh)"
P=$(echo "$finders" | wc -l)
echo "$finders" | xargs -P $P -n 1 -t -- bash

sort -u seed.* > $ofile
wc -l seed.* $ofile
rm seed.*
mv $ofile ../seeds
