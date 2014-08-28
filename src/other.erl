%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(other).

%% other: escript rendering a repos' erldocs.

-export([ main/1 ]).

-record(conf, { odir = ""
              , url  = ""
              }).

%% API

main ([]) ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: \n\t~s  -o ‹output dir› ‹repo URL›\n", [Arg0]),
    halt(1);
main (Args) ->
    parse(Args, #conf{}).

%% Internals

parse ([], Conf) ->
    ODir = Conf#conf.odir,
    URL  = Conf#conf.url,
    case (ODir == "") or (URL == "") of
        true  -> main([]);
        false -> run(URL, ODir)
    end;

parse (["-o", ODir | Rest], Conf) ->
    parse(Rest, Conf#conf{odir = ODir});
parse ([URL | Rest], Conf) ->
    parse(Rest, Conf#conf{url  = URL }).


run (A1, A2) ->
    try other_core:main(A1, A2)
    catch Type:Error ->
            io:format("Error running ~p:\n~p\n~p\n",
                      [?MODULE, erlang:get_stacktrace(), {Type,Error}]),
            halt(2)
    end.

%% End of Module.
