#!/bin/bash

# Also this BigQuery
#  SELECT repo_name FROM [bigquery-public-data:github_repos.languages] WHERE language.name = 'Erlang'
# then cat results-*.csv | tail -n +2 | sed 's%^%github.com/%' | tr '[:upper:]' '[:lower:]' >seed.bq

[[ "$(basename "$(dirname "$0")")" != 'find' ]] && echo cd into find/ && exit 2

[[ $# -ne 1 ]] && echo "Usage: $0  website dir" && exit 1
osite="$1"
ofile=tmp.$RANDOM
touch $ofile

for finder in ec gh sc; do
	echo ./$finder.sh "$osite"
done \
	| xargs -P 3 -n 2 -t -- bash -c '$* _' _

sort -u seed.* > $ofile
wc -l seed.* $ofile
rm seed.*
mv $ofile seeds
