#!/usr/bin/env escript
%%! -Wall -pz _build/default/lib/erldocs_other/ebin/ -pz _build/default/lib/erldocs/ebin/ -pz _build/default/lib/erlydtl/ebin/
%% -*- coding: utf-8 -*-
%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›

%% gen.escript: gen.sh script revisited for performance
%% ./$0 ‹…›  2>&1 | tee ./log

-mode(compile).

%% API

main ([SiteDir, TmpDir, ListFile]) ->
    URLs = case filelib:is_regular(ListFile)
               orelse eo_scm:url(ListFile)
           of
               true ->
                   {ok, Raw} = file:read_file(ListFile),
                   read_URLs(Raw);
               {true, URL} -> %% Not a file: maybe it's a URL then?
                   [URL];
               false ->
                   io:format("Bad input '~s', skipping\n", [ListFile]),
                   []
           end,
    seq_gen(fabs(SiteDir), fabs(TmpDir), URLs, length(URLs));

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

seq_gen (_SiteDir, _TmpDir, [], _) -> ok;
seq_gen (SiteDir, TmpDir, URLs, N) ->
    {ok,_} = application:ensure_all_started(ssl),
    {ok,_} = application:ensure_all_started(inets),
    BlackList = read_URLs(maybe_HTTP_fetch(SiteDir, eo_core:remote_path_blacklist())),
    io:format("~p URLs blacklisted\n", [length(BlackList)]),
    seq_gen(SiteDir, TmpDir, URLs, N, BlackList).

seq_gen (_SiteDir, _TmpDir, [], _, _) -> ok;
seq_gen (SiteDir, TmpDir, [Url|Rest], N, BlackListed) ->
    case lists:member(Url, BlackListed) of
        true ->
            io:format("~p Blacklisted: ~p\n", [N,Url]);
        false ->
            Arg = [{website_dir, SiteDir}
                  ,{dest, filename:join(TmpDir, eo_scm:uuid(Url))}
                  ,{base, "/"}
                  ,{url, Url}
                  ,{update_only, true}
                  ],
            io:format("~p ~p Arg ~10000p\n", [N,erlang:timestamp(),Arg]),
            Res = (catch (eo_core:gen(Arg))),
            io:format("~p Res ~10000p\n", [N,Res])
    end,
    _ = erldocs_core:maybe_delete_xmerl_table(),
    seq_gen(SiteDir, TmpDir, Rest, N-1, BlackListed).

maybe_HTTP_fetch (SiteDir, Url) ->
    case file:read_file(filename:join(SiteDir, "blacklist.txt")) of
        {ok, Bin} -> Bin;
        {error, _R} ->
            io:format("no local blacklist: ~p\n", [_R]),
            case httpc:request(get, {Url,[]}, [], [{body_format,binary}]) of
                {ok, {_,_,Body}} -> Body;
                _ -> <<>>
            end
    end.

%% End of Module.
