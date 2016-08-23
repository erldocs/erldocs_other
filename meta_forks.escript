#!/usr/bin/env escript
%%! -Wall -pz _build/default/lib/erldocs_other/ebin/ -pz _build/default/lib/erldocs/ebin/ -pz _build/default/lib/erlydtl/ebin/ -sname meta_forks
%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% forks: try to find a repo's forks

-include("include/erldocs_other.hrl").
-mode(compile).

%% API

main ([Dir]) ->
    F = fun (Meta, Count) ->
                UUID = eo_meta:target_path(Meta),
                Cos = [Rev#rev.commit || Rev <- eo_meta:revisions(Meta)],
                lists:foreach(fun (Co) ->
                                      append(Co, UUID)
                              end, Cos),
                Count + 1
        end,
    CountRepos = eo_meta:fold(Dir, F, 0),
    io:format("# repos: ~p\n", [CountRepos]),
    G = fun ({Co, Ids}) ->
                erase(Co),
                lists:foreach(fun (Id) ->
                                      append(Id, Ids -- [Id])
                              end, Ids)
        end,
    lists:foreach(G, get()),
    H = fun ({Id, Similar0}, Count) ->
                erase(Id),
                Similar = lists:usort(lists:append(Similar0)),
                io:format("~s\n\t~p\n", [Id, Similar]),
                case Similar of
                    [] -> Count;
                    __ -> Count + 1
                end
        end,
    CountForks = lists:foldl(H, 0, get()),
    io:format("# projects sharing commits: ~p\n", [CountForks]);

main (_) ->
    usage().

%% Internals

append(Key, Value) ->
    case get(Key) of
        L when is_list(L) ->
            put(Key, [Value|L]);
        _ ->
            put(Key, [Value])
    end.

usage () ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  ‹other.erldocs.com's dir›\n",
              [filename:basename(Arg0)]),
    halt(1).

%% End of Module.
