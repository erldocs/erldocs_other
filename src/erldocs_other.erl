%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs_other).

%% erldocs_other: render a repo's erldocs. Can be used as an escript.

-export([ main/1 ]).

-record(conf, { dest :: filelib:dirname()
              , url  :: string()
              , base    = eo_default:base()
              , logfile = eo_default:logfile()
              , ga      = eo_default:ga() }).

%% API

main ([]) ->
    usage();
main (Args) ->
    parse(Args, #conf{}).

%% Internals

parse ([], Conf) ->
    Dest = Conf#conf.dest,
    URL  = Conf#conf.url,
    case (Dest == "") or (URL == "") of
        true  -> usage();
        false -> run([ {dest, filename:absname(Dest)}
                     , {url,  URL}
                     , {base, Conf#conf.base}
                     , {logfile, Conf#conf.logfile}
                     , {ga,   Conf#conf.ga} ])
    end;

parse (["-o",     Dest | Rest], Conf) ->
    parse(Rest, Conf#conf{dest = Dest});
parse (["--base", Base | Rest], Conf) ->
    parse(Rest, Conf#conf{base = Base});
parse (["--logfile", LF| Rest], Conf) ->
    parse(Rest, Conf#conf{logfile = LF});
parse (["--ga",     GA | Rest], Conf) ->
    parse(Rest, Conf#conf{ga   = GA  });
parse ([URL            | Rest], Conf) ->
    parse(Rest, Conf#conf{url  = URL }).


run (Args) ->
    try eo_core:main(Args)
    catch Type:Error ->
            io:format("Error running ~p:\n\t~p\n~p\n",
                      [?MODULE, erlang:get_stacktrace(), {Type,Error}]),
            halt(2)
    end.

usage () ->
    ok = io:setopts([{encoding, unicode}]),
    Arg0 = escript:script_name(),
    io:format("Usage: \n\t~s  -o ‹output dir› [--logfile ‹path›] ‹repo URL›\n", [Arg0]),
    halt(1).

%% End of Module.
