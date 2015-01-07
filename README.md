#erldocs_other • [GitHub](//github.com/erldocs/erldocs_other)

## Requirements
* Git ≥ 1.7.4.4
* [rebar](https://github.com/rebar/rebar)
* Basic UNIX command line tools

clear && m -j && rand=$RANDOM && echo $rand && erl -pz ebin/ -pz deps/erldocs/ebin/ -pz deps/erlydtl/ebin/ -pz deps/eunit_formatters/ebin/ -pz deps/merl/ebin/ -noshell -eval 'eo_core:gen([{url,"https://github.com/nebularis/memoize"}, {dest,"/Users/pete/wefwefwef/docs/wef"}, {website_dir,"/Users/pete/wefwefwef/docs/site"}, {random,"'$rand'"}, {base,"/"}, {ga,"UA-54292016-1"}]).' -s init stop
