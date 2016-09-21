#!/bin/bash

S=$(date)

. $HOME/.bin/builds/otp/18.3/activate

osite=~/www/dev.erldocs.com
apps=$osite/apps.js
wef=~/wefwefwef/docs
code=$wef/erldocs_other.git
urls=$wef/gen
log=${urls}.log
log_stats=${urls}.tsv
tmp_urls=$RANDOM
seeds=$code/seeds
NPROCS=7

pushd $osite
find ./*/ -name meta.txt | cut -c3- | sed 's/.........$//' >$urls

pushd $code
./meta_discover.escript $osite | \grep -vF ' -> ' | sed 's%https://%%' >>$urls
popd
[[ ! -f $seeds ]] && touch $seeds
sort -u $seeds $urls >$tmp_urls
mv $tmp_urls $urls

pushd $code
echo >$log
#./gen.escript $osite $wef/other/ $urls 2>&1 | tee --append $wef/gen_log
count=$(cat $urls | wc -l)
echo $count
#cat $urls | pv -pet -i 0.5 -l -s $count | (xargs -P $NPROCS -n 1 -t -- ./gen.escript $osite $wef/other/ >>$log)
##xargs -a $urls -P $NPROCS -I{} -t -- ./gen.escript $osite $wef/other/ "'"{}"'" >>$log
xargs -a $urls -P $NPROCS -n 1 -t -- ./gen.escript $osite $wef/other/ >>$log
# ./pu.sh ~/wefwefwef/docs/osite.git/ 'daily update'
popd

echo 'apps = [' >$apps
find ./*/ -name meta.txt | cut -c3- | sed 's/.........$/",/' | sed 's/^/"/' | tr -d '\n' >>$apps
echo '];' >>$apps
popd

E=$(date)
echo
echo $S >>$log
echo $E >>$log

count=$(grep -oF , $apps | wc -l)
pushd $code
Mcount="$(./meta_count.escript $osite 2>/dev/null)"
Mforks="$(./meta_forks.escript $osite 2>/dev/null)"
popd
echo -e "$S\t$E\t$count\t$Mcount\t$Mforks" >>$log_stats

echo DONE
