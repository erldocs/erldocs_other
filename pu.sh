#!/bin/bash

[[ $# -ne 2 ]] && echo "Usage: $0  ‹git dir› ‹commit message›"
dir="$1"
msg="$2"

cd "$dir" \
    && find . -name meta.txt | cut -c3- | sed 's/.........$//' > index.html \
    && git pull origin gh-pages \
    && git add -A . \
    && git commit -am "$msg" \
    && git push origin gh-pages
cd -
