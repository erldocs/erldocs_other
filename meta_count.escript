#!/usr/bin/env escript
%%! -pz ebin/ -pz deps/erldocs/ebin/ -pz deps/erlydtl/ebin/ -pz deps/eunit_formatters/ebin/ -pz deps/merl/ebin/ -sname meta_count
%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% count: 

-include("include/erldocs_other.hrl").
-mode(compile).

%% API

main ([Dir]) ->
    F = fun (Meta, {Repos, VersionsBuilt, VersionsFailed}) ->
                {revisions, Revs} = lists:keyfind(revisions, 1, Meta),
                Built  = length([1 || #rev{builds = true}  <- Revs]),
                Failed = length([1 || #rev{builds = false} <- Revs]),
                {Repos+1, VersionsBuilt+Built, VersionsFailed+Failed}
        end,
    {R,B,A} = eo_meta:fold(Dir, F, {0,0,0}),
    io:format("repos ~p\nbuilt ~p\nfailed ~p\n", [R,B,A]);

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
