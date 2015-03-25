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
    seq_gen(fabs(SiteDir), fabs(TmpDir), List);

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

seq_gen (_SiteDir, _TmpDir, []) -> ok;
seq_gen (SiteDir, TmpDir, [URL|Rest]) ->
    Arg = [ {website_dir, SiteDir}
          , {dest, filename:join(TmpDir,random_str())}
          , {base, "/"}
          , {url, URL} ],
    io:format("~p Arg ~10000p\n", [now(),Arg]),
    Res = (catch (eo_core:gen(Arg))),
    io:format("Res ~10000p\n", [Res]),
    seq_gen(SiteDir, TmpDir, Rest).

random_str () ->
    integer_to_list(
      random:uniform(9999999999)).

%% End of Module.
