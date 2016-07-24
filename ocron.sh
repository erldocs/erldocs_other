#!/bin/bash -ex

. $HOME/.bin/builds/otp/19.0/activate

osite=~/www/dev.erldocs.com
wef=~/wefwefwef/docs
NPROCS=7

pushd $osite
# git checkout gh-pages
# git pull origin gh-pages

#find ./*/ -name meta.txt | cut -c3- | sed 's/.........$//' >$wef/gen
#./meta_discover.escript $osite | \grep -vF ' -> ' | sed 's%https://%%' >>$wef/gen
#sort -u $wef/gen >$wef/gen_
#mv $wef/gen_ $wef/gen

pushd $wef/erldocs_other.git
#./gen.escript $osite $wef/other/ $wef/gen 2>&1 | tee --append $wef/gen_log
cat $wef/gen | xargs -P $NPROCS -n 1 -t -- ./gen.escript $osite $wef/other/
# ./pu.sh ~/wefwefwef/docs/osite.git/ 'daily update'
popd

popd
echo DONE
