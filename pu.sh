#!/bin/bash

[[ $# -ne 2 ]] && echo "Usage: $0  ‹git dir› ‹commit message›"
dir="$1"
msg="$2"

cd "$dir" \
    && git pull origin gh-pages \
    && git add -A . \
    && git commit -am "$msg" \
    && git push origin gh-pages
cd -
