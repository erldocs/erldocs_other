#!/bin/bash

# Generate content for other.erldocs.com

[[ $# -eq 0 ]] && echo "Usage: $0  ‹website dir› ‹repo URL›" && exit 1

odir="$1"
[[ ! -d "$odir" ]] && echo "$odir is not a directory" && exit 2
url="$2"

generator='./erldocs_other'
[[ ! -x $generator ]] && [[ ! -L $generator ]] && echo "$generator not executable" && exit 2

kf (){
    local key="$1"
    local metafile="$2"
    erl -noshell -eval '{ok, Terms} = file:consult("'"$metafile"'"), {_, Value} = lists:keyfind('$key', 1, Terms), io:format("~s\n", [Value]).' -s init stop
}


tmp=/tmp/other
mkdir -p $tmp
rm -rf $tmp/*

$generator     \
    "$url"     \
    -o $tmp    \
    --base '/' \
    2>&1 | tee $tmp/_

err_code=${PIPESTATUS[0]}
[[ $err_code -ne 0 ]] && echo "$generator failed" && exit 3

url=$(kf url $tmp/meta.terms)
target_path=$(kf target_path $tmp/meta.terms)
dest="$odir"/$target_path
mkdir -pv "$dest"
rm -rf    "$dest" # Instead of `rm -rf "$dest"/*` => Can `stat` "$dest" for info!
mkdir -pv "$dest"

mv -v $tmp/_ "$dest"/
mv -v $tmp/meta.terms "$dest"/
mv -v $tmp/repo/repo.css "$odir"/
for decor in 'erldocs.css' 'erldocs.js' 'jquery.js'; do
    path=$(find $tmp/repo -name $decor | head -n 1)
    [[ '' != "$path" ]] && mv -v "$path" "$odir"/
    find $tmp/repo -name $decor -delete
done
find $tmp/repo -type d -name '.xml' -exec rm -r "{}" \;  2>/dev/null
mv -v $tmp/repo/* "$dest"/

cd "$odir"
find . -name meta.terms | cut -c3- | sed 's/...........$//' > index.html
cd -

echo    "Just gen'd $url over at $dest"
echo -e "\t"http://other.erldocs.com/$target_path

if [[ -d "$odir"/.git ]]; then
    cd "$odir" \
        && git pull origin gh-pages \
        && git add -A $target_path \
        && git commit -am "Generated docs for $url" \
        && git push origin gh-pages
    cd -
fi

rm -rf $tmp
