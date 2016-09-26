%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_os).

%% eo_os: Erlang port to UNIX shell.

-export([chksh/2, chksh/3, chksh/4]).
-export([sh/1, sh/2, sh/3]).

-include("logging.hrl").

-define(ShortCmdTimeout, 5 * 1000).

%% API

chksh (Name, Cmd) ->
    chk(Name, sh(Cmd)).

chksh (Name, A1, A2) ->
    chk(Name, sh(A1, A2)).

chksh (Name, Dir, Cmd, Timeout) ->
    chk(Name, sh(Dir, Cmd, Timeout)).

sh (Dir, Cmd, Timeout) ->
    run(Dir, Cmd, Timeout).

sh (Cmd, Timeout)
  when not is_list(Timeout) ->
    sh(".", Cmd, Timeout);
sh (Dir, Cmd) ->
    sh(Dir, Cmd, ?ShortCmdTimeout).

sh (Cmd) ->
    sh(Cmd, ?ShortCmdTimeout).

%% Internals

-type unchecked_return() :: {non_neg_integer(), string()}.

-spec chk (atom(), unchecked_return()) -> ok | none().
chk (_, {0,_}) -> ok;
chk (Name, {Code,Stdout0}) ->
    KeepASCII = fun (X) -> X < 127 end,
    Stdout = iolists:filtermap(KeepASCII, Stdout0),
    ?NOTE("stdout", "~p", [Stdout]),
    throw({sh, Name, error, Code, Stdout0}).

-spec run (file:filename_all(), [nonempty_string(),...], timeout()) -> unchecked_return().
run (Dir, Cmd, Timeout) ->
    ?RUN(Dir, Cmd, Timeout),
    [Exe|Args] = Cmd,
    Executable = os:find_executable(Exe),
    false == Executable andalso ?NOTE("which", "~s not found", [Exe]),
    Port = open_port({spawn_executable, Executable}
                    ,[exit_status
                     ,use_stdio
                     ,stderr_to_stdout
                     ,{args, Args}
                     ,{cd, Dir}
                     %% ,{parallelism, false}
                     ,{line, 64 * 64}
                     ]
                    ),
    loop(Port, [], Timeout).

loop (Port, Data, Timeout) ->
    receive
        {Port, {data, {eol, Line}}} ->
            NewData = [list_to_tuple(string:tokens(Line, "\t")) | Data],
            loop(Port, NewData, Timeout);
        {Port, {exit_status, Code}} ->
            {Code, lists:reverse(Data)}
    after Timeout ->
            {error, timeout}
    end.

%% End of Module.
