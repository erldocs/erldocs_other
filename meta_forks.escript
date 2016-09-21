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
    io:format(standard_error, "# repos: ~p\n", [CountRepos]),
    G = fun ({Co, Ids}) ->
                erase(Co),
                A = fun (Id) ->
                            append(Id, gb_sets:delete_any(Id, Ids))
                    end,
                lists:foreach(A, gb_sets:to_list(Ids))
        end,
    lists:foreach(G, get()),
    H = fun ({Id, Similar0}, Count) ->
                erase(Id),
                Similar = gb_sets:to_list(Similar0),
                io:format(standard_error, "~s\t~p\n\t~p\n", [Id, gb_sets:size(Similar0), Similar]),
                case Similar of
                    [] -> Count;
                    _ -> Count + 1
                end
        end,
    CountForks = lists:foldl(H, 0, get()),
    %% #projects sharing >0 commits
    io:format("~p\n", [CountForks]);

main (_) ->
    usage().

%% Internals

append(Key, Value) ->
    case get(Key) of
        undefined ->
            case Value of
                [_Char|_] when is_integer(_Char) ->
                    put(Key, gb_sets:from_list([Value]));
                _ ->
                    put(Key, Value)
            end;
        S ->
            case Value of
                [_Char|_] when is_integer(_Char) ->
                    put(Key, gb_sets:add(Value, S));
                _ ->
                    put(Key, gb_sets:union(Value, S))
            end
    end.

usage () ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  ‹other.erldocs.com's dir›\n",
              [filename:basename(Arg0)]),
    halt(1).

%% End of Module.
