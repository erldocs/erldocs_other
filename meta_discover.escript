#!/usr/bin/env escript
%%! -pz ebin/ -pz deps/erldocs/ebin/ -pz deps/erlydtl/ebin/ -pz deps/eunit_formatters/ebin/ -pz deps/merl/ebin/ -sname meta_discover
%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% discover: find every discovered repo.

-include("include/erldocs_other.hrl").
-mode(compile).

%% API

main ([Dir]) ->
    F = fun (Meta, _Acc) ->
                Urlsz = [Rev#rev.discovered || Rev <- eo_meta:revisions(Meta)],
                print(Urlsz)
        end,
    eo_meta:fold(Dir, F, ignore);

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
