#!/usr/bin/env escript
%%! -Wall -pz _build/default/lib/erldocs_other/ebin/ -pz _build/default/lib/erldocs/ebin/ -pz _build/default/lib/erlydtl/ebin/ -sname meta_count
%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% count: extract some stats.

-include("include/erldocs_other.hrl").
-mode(compile).

%% API

main ([Dir]) ->
    BasePath = "/home/pete/www/dev.erldocs.com",
    F = fun (Meta, Wrongs) ->
                Path = filename:join(BasePath, eo_meta:target_path(Meta)),
                Wrongs
                    + length(
                        [1
                         || #rev{builds = true
                                ,id = Title
                                } <- eo_meta:revisions(Meta),
                            not filelib:is_regular(filename:join([Path, Title, "index.html"]))
                        ])
        end,
    CountOfWrong = eo_meta:fold(Dir, F, 0),
    io:format("~p\n", [CountOfWrong]);

main (_) ->
    usage().

%% Internals

usage () ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: ~s  ‹other.erldocs.com's dir›\n",
              [filename:basename(Arg0)]),
    halt(1).

%% End of Module.
