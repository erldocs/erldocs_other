#!/usr/bin/env escript
%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% discover: 

-include("include/erldocs_other.hrl").
-mode(compile).

%% API

main ([Dir]) ->
    F = fun (File, _Acc) ->
                io:format(" -> ~p\n", [File]),
                {ok, Terms} = file:consult(File),
                case lists:keyfind(revisions, 1, Terms) of
                    {revisions, Revs} ->
                        Discovered = [Urls || #rev{discovered = Urls} <- Revs];
                    false ->
                        Discovered = []
                end,
                print(Discovered)
        end,
    filelib:fold_files(Dir, "meta\\.txt", true, F, ignore);

main (_) ->
    usage().

%% Internals

print (L)
  when is_list(L) ->
    Urls = lists:usort(lists:append(L)),
    lists:foreach(fun (X) -> io:format("~s\n", [X]) end, Urls).

usage () ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  ‹other.erldocs.com's dir›\n",
              [filename:basename(Arg0)]),
    halt(1).

%% End of Module.
