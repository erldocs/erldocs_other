%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

-define(_LOG(PS1, Fmt, Args), io:format(PS1 ++ Fmt ++ "\n", Args)).
-define(_LOG(PS1, Str),       io:format(PS1 ++ Str ++ "\n")).

-define(MILESTONE(Fmt, Args), ?_LOG(" :: ", Fmt, Args)).
-define(MILESTONE(Str),       ?_LOG(" :: ", Str)).

-define(NOTE(Cat, Fmt, Args), ?_LOG(">>> [" ++ Cat ++ "] ", Fmt, Args)).
-define(NOTE(Cat, Str),       ?_LOG(">>> [" ++ Cat ++ "] ", Str)).

-define(PWD(Dir),             ?_LOG("$ ", "cd '~s'", [Dir])).
-define(RUN(Dir, Cmd, Timeout),
        ?PWD(Dir),
        ?_LOG("$ ", "~p  `~s`", [Timeout, string:join(Cmd, " ")])
       ).

-define(DBG(Fmt, Args),
        io:format(user, Fmt ++ "\n", Args)
       ).

%% End of File.
