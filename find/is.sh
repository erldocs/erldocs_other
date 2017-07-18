#!/bin/bash -ex

outf=seed.issues

# Find links to Erlang projects when they are posted as the title of a GitHub issue.

find() {
    local url='https://api.github.com/repos/erldocs/erldocs_other/issues?page='$1
    echo Fetching "$url"
    curl "$url" -s | python -m json.tool | grep '"title"' | cut -d'"' -f4 | grep -F / >>$outf
}

touch $outf
i=1
count_before=$(cat $outf | wc -l)
find $i
while true; do
    count_after=$(cat $outf | wc -l)
    if [[ $count_after -eq $count_before ]]; then
        break
    fi
    count_before=$count_after
    sleep 10
    i=$(( $i + 1 ))
    find $i || true
done
