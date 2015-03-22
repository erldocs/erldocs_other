#!/usr/bin/env escript
%%! -pz ebin/ -pz deps/erldocs/ebin/ -pz deps/erlydtl/ebin/ -pz deps/eunit_formatters/ebin/ -pz deps/merl/ebin/
%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% -*- coding: utf-8 -*-

%% gen.escript: gen.sh script revisited for performance
%% ./$0 ‹…›  2>&1 | tee ~/log

-mode(compile).

-define(PAR_MAX, 3).

%% API

main ([SiteDir, TmpDir, ListFile]) ->
    List = read_URLs(ListFile),
    random:seed(now()),
    par_gen(fabs(SiteDir), fabs(TmpDir), List);

main (_) ->
    usage().

%% Internals

usage () ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  ‹site dir› ‹tmp dir› ‹file with one URL per line›\n",
              [filename:basename(Arg0)]),
    halt(1).


fabs (Fn) ->
    filename:absname(Fn).

read_URLs (File) ->
    {ok, Raw} = file:read_file(File),
    Bins = binary:split(Raw, <<"\n">>, [global]),
    [binary_to_list(Bin) || Bin <- Bins, Bin =/= <<>>].

par_gen (SiteDir, TmpDir, URLs) ->
    case take_some(URLs) of
        {[], _} -> ok;
        {SomeURLs, Rest} ->
            Args = prep_args(SiteDir, TmpDir, SomeURLs),
            io:format("Args ~p\nm:~p\n", [Args,html_dtl:module_info()]),
            Res = rpc:pmap({eo_core,gen}, [], Args),
            io:format("~p\n", [lists:zip(SomeURLs,Res)]),
            par_gen(SiteDir, TmpDir, Rest)
    end.

prep_args (_SiteDir, _TmpDir, []) -> [];
prep_args (SiteDir, TmpDir, [URL|URLs]) ->
    Arg = [ {website_dir, SiteDir}
          , {dest, filename:join(TmpDir,random_str())}
          , {url, URL} ],
    [Arg | prep_args(SiteDir, TmpDir, URLs)].

random_str () ->
    integer_to_list(
      random:uniform(9999999999)).

take_some (List) ->
    Some = lists:sublist(List, ?PAR_MAX),
    {Some, dropN(length(Some), List)}.

dropN (0, List) -> List;
dropN (N, List) -> dropN(N-1, tl(List)).

%% End of Module.
