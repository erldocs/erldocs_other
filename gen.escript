#!/usr/bin/env escript
%%! -sname oe -Wall -pz ebin/ -pz deps/erldocs/ebin/ -pz deps/erlydtl/ebin/ -pz deps/eunit_formatters/ebin/ -pz deps/merl/ebin/
%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% -*- coding: utf-8 -*-

%% gen.escript: gen.sh script revisited for performance
%% ./$0 ‹…›  2>&1 | tee ~/log

-mode(compile).

-define(PAR_MAX, 3).

%% API

main ([SiteDir, TmpDir, ListFile]) ->
    {ok,_} = application:ensure_all_started(ssl),
    {ok,_} = application:ensure_all_started(inets),
    BlackList = read_URLs(maybe_HTTP_fetch(eo_core:remote_path_blacklist())),
    io:format("~p URLs blacklisted\n", [length(BlackList)]),
    {ok, Raw} = file:read_file(ListFile),
    WhiteList = read_URLs(Raw),
    seq_gen(fabs(SiteDir), fabs(TmpDir), WhiteList, length(WhiteList), BlackList);

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

read_URLs (Raw) ->
    case binary:split(Raw, <<"\r\n">>, [global]) of
        [Raw] -> Bins = binary:split(Raw, <<"\n">>, [global]);
        Else ->  Bins = Else
    end,
    URLs = [binary_to_list(Bin) || Bin <- Bins, Bin =/= <<>>],
    lists:filtermap(fun eo_scm:url/1, URLs).

seq_gen (_SiteDir, _TmpDir, [], _, _) -> ok;
seq_gen (SiteDir, TmpDir, [Url|Rest], N, BlackListed) ->
    case lists:member(Url, BlackListed) of
        true ->
            io:format("~p Blacklisted: ~p\n", [N,Url]);
        false ->
            Arg = [ {website_dir, SiteDir}
                  , {dest, filename:join(TmpDir, eo_scm:uuid(Url))}
                  , {base, "/"}
                  , {url, Url}
                  , {update_only, true}
                  ],
            io:format("~p ~p Arg ~10000p\n", [N,now(),Arg]),
            Res = (catch (eo_core:gen(Arg))),
            io:format("~p Res ~10000p\n", [N,Res])
    end,
    _ = erldocs_core:maybe_delete_xmerl_table(),
    seq_gen(SiteDir, TmpDir, Rest, N-1, BlackListed).

maybe_HTTP_fetch (Url) ->
    case httpc:request(get, {Url,[]}, [], [{body_format,binary}]) of
        {ok, {_,_,Body}} -> Body;
        _ -> <<>>
    end.

%% End of Module.
