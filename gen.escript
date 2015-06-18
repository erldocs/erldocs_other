#!/usr/bin/env escript
%%! -pz ebin/ -pz deps/erldocs/ebin/ -pz deps/erlydtl/ebin/ -pz deps/eunit_formatters/ebin/ -pz deps/merl/ebin/ -sname oe
%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% -*- coding: utf-8 -*-

%% gen.escript: gen.sh script revisited for performance
%% ./$0 ‹…›  2>&1 | tee ~/log

-mode(compile).

-define(PAR_MAX, 3).

%% API

main ([SiteDir, TmpDir, ListFile]) ->
    List = read_URLs(ListFile),
    seq_gen(fabs(SiteDir), fabs(TmpDir), List, length(List));

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
    case binary:split(Raw, <<"\r\n">>, [global]) of
        [Raw] -> Bins = binary:split(Raw, <<"\n">>, [global]);
        Else ->  Bins = Else
    end,
    [binary_to_list(Bin) || Bin <- Bins, Bin =/= <<>>].

seq_gen (_SiteDir, _TmpDir, [], _) -> ok;
seq_gen (SiteDir, TmpDir, [URL|Rest], N) ->
    Arg = [ {website_dir, SiteDir}
          , {dest, filename:join(TmpDir, uuid(URL))}
          , {base, "/"}
          , {url, URL}
          , {update_only, true}
          ],
    io:format("~p ~p Arg ~10000p\n", [N,now(),Arg]),
    Res = (catch (eo_core:gen(Arg))),
    io:format("~p Res ~10000p\n", [N,Res]),
    seq_gen(SiteDir, TmpDir, Rest, N-1).

uuid (URL) ->
    case eo_scm:url(URL) of
        {true, Url} ->
            LocalPath = eo_scm:repo_local_path(Url),
            eo_util:uuid(crypto:hash(sha, LocalPath));
        false ->
            eo_util:uuid()
    end.

%% End of Module.
