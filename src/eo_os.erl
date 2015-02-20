%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_os).

%% eo_os: Erlang port to UNIX shell.

-export([ chksh/3, chksh/4, chksh/5
        , sh/2, sh/3, sh/4 ]).

-include("logging.hrl").

-define(ShortCmdTimeout, 5 * 1000).

%% API

chksh (Name, Fmt, Data) ->
    chk(Name, sh(Fmt, Data)).

chksh (Name, A1, A2, A3) ->
    chk(Name, sh(A1, A2, A3)).

chksh (Name, Dir, Fmt, Data, Timeout) ->
    chk(Name, sh(Dir, Fmt, Data, Timeout)).

sh (Fmt, Data) ->
    sh (Fmt, Data, ?ShortCmdTimeout).

sh (Fmt, Data, Timeout)
  when is_atom(Timeout); is_integer(Timeout) ->
    Cmd = lists:flatten(io_lib:format(Fmt++"  2>&1", Data)),
    run(Cmd, Timeout);
sh (Dir, Fmt, Data) ->
    sh (Dir, Fmt, Data, ?ShortCmdTimeout).

sh (Dir, Fmt, Data, Timeout) ->
    {ok, PreviousDir} = file:get_cwd(),
    set_cwd(Dir),
    Res = sh(Fmt, Data, Timeout),
    set_cwd(PreviousDir),
    Res.

%% Internals

set_cwd (Dir) ->
    ?PWD(Dir),
    ok = file:set_cwd(Dir).

chk (Func, ShCall) ->
    case ShCall of
        {0, _} -> ok;
        {Code, Stdout0} ->
            Stdout = iolists:filtermap(fun (X) -> X < 127 end, Stdout0),
            ?NOTE("stdout", "~p", [Stdout]),
            throw({sh,Func,error,Code,Stdout0})
    end.

run (Cmd, Timeout) ->
    ?RUN(Cmd, Timeout),
    Port = open_port({spawn,Cmd}, [exit_status]),
    loop(Port, [], Timeout).

loop (Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}}  -> loop(Port, Data++'2tup'(NewData), Timeout);
        {Port, {exit_status, S}} -> {S, Data}
    after Timeout ->
            {error, timeout}
    end.

'2tup' (Str) ->
    [ begin
          Cols = string:tokens(Line, "\t"),
          list_to_tuple(Cols)
      end || Line <- string:tokens(Str, "\n") ].

%% End of Module.
