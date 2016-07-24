#!/bin/bash -ex

. $HOME/.bin/builds/otp/18.3/activate

osite=~/www/dev.erldocs.com
wef=~/wefwefwef/docs
code=$wef/erldocs_other.git
urls=$wef/gen
tmp_urls=$RANDOM
seeds=$code/seeds
NPROCS=7

pushd $osite
# git checkout gh-pages
# git pull origin gh-pages

#find ./*/ -name meta.txt | cut -c3- | sed 's/.........$//' >$urls
#pushd $code
#./meta_discover.escript $osite | \grep -vF ' -> ' | sed 's%https://%%' >>$urls
#popd
#[[ ! -f $seeds ]] && touch $seeds
#sort -u $seeds $urls >$tmp_urls
#mv $tmp_urls $urls

pushd $code
#./gen.escript $osite $wef/other/ $urls 2>&1 | tee --append $wef/gen_log
cat $urls | xargs -P $NPROCS -n 1 -t -- ./gen.escript $osite $wef/other/
# ./pu.sh ~/wefwefwef/docs/osite.git/ 'daily update'
popd

popd
echo DONE
