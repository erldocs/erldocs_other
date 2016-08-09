#!/bin/bash

S=$(date)

. $HOME/.bin/builds/otp/18.3/activate

osite=~/www/dev.erldocs.com
wef=~/wefwefwef/docs
code=$wef/erldocs_other.git
urls=$wef/gen
urls_log=${urls}.log
tmp_urls=$RANDOM
seeds=$code/seeds
NPROCS=7

pushd $osite
# git checkout gh-pages
# git pull origin gh-pages

find ./*/ -name meta.txt | cut -c3- | sed 's/.........$//' >$urls
pushd $code
./meta_discover.escript $osite | \grep -vF ' -> ' | sed 's%https://%%' >>$urls
popd
[[ ! -f $seeds ]] && touch $seeds
sort -u $seeds $urls >$tmp_urls
mv $tmp_urls $urls

pushd $code
echo >$urls_log
#./gen.escript $osite $wef/other/ $urls 2>&1 | tee --append $wef/gen_log
count=$(cat $urls | wc -l)
echo $count
#cat $urls | pv -pet -i 0.5 -l -s $count | (xargs -P $NPROCS -n 1 -t -- ./gen.escript $osite $wef/other/ >>$urls_log)
##xargs -a $urls -P $NPROCS -I{} -t -- ./gen.escript $osite $wef/other/ "'"{}"'" >>$urls_log
xargs -a $urls -P $NPROCS -n 1 -t -- ./gen.escript $osite $wef/other/ >>$urls_log
# ./pu.sh ~/wefwefwef/docs/osite.git/ 'daily update'
popd

apps=apps.js
echo 'apps = [' >$apps
find . -name meta.txt | cut -c3- | sed 's/.........$/",/' | sed 's/^/"/' | tr -d '\n' >>$apps
echo '];' >>$apps

popd

echo
echo $S
echo $(date)
echo DONE
