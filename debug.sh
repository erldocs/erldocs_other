#!/bin/bash

# 

[[ $# -ne 1 ]] && exit 1

r=$RANDOM
R=~/Workspace/erldocs/erldocs_other.git
T=~/wefwefwef/docs

echo $r

PAs="-pa $R/ebin"
for dep in $R/deps/*/ebin; do
    PAs="$PAs -pa $dep"
done

erl -noshell $PAs \
    -eval 'io:format("~p\n~p\n", ['$r',eo_core:gen([{website_dir,"'$T/site'"},{dest,"'$T/wef/$r'"},{url,"'$1'"}])]).' \
    -s init stop
