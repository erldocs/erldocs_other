%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(other_utils).

%% other_utils: shell utilities for other_core.

-export([ rmrf/1
        , cp/3

        , git_clone/2
        , git_branches/1
        , git_tags/1
        , git_changeto/2
        ]).


%% API

rmrf (Dir) ->
    {0,_} = sh("rm -rf '~s'", [Dir]),
    ok.

cp (ChDir, Src, Dst) ->
    {0,_} = sh(ChDir, "cp -pr '~s' '~s'", [Src,Dst]),
    ok.


git_clone (Url, Dir) ->
    {0,_} = shh("git clone --no-checkout -- '~s' '~s'", [Url,Dir]),
    ok.

git_branches (RepoDir) ->
    {0,Branches} = sh(RepoDir, "git ls-remote --heads origin", []),
    [ {shorten(Commit), Branch}
      || {Commit, "refs/heads/"++Branch} <- Branches ].

git_tags (RepoDir) ->
    {0,Tags} = sh(RepoDir,
                  "git tag --list"
                  " | while read tag; do"
                  "   echo \"$tag\t$(git rev-list \"$tag\" | head -n 1)\";"
                  " done", []),
    [{shorten(Commit),Tag} || {Tag,Commit} <- Tags].

git_changeto (RepoDir, Commit) ->
    {0,_} = sh(RepoDir, "git checkout --quiet '~s'", [Commit]),
    ok.

%% Internals

shorten (Commit) ->
    lists:sublist(Commit, 7).

shh (Fmt, Data) ->
    sh(Fmt++" >/dev/null 2>&1", Data).

sh (Dir, Fmt, Data) ->
    {ok, PreviousDir} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    Res = sh(Fmt, Data),
    ok = file:set_cwd(PreviousDir),
    Res.

sh (Fmt, Data) ->
    Cmd = lists:flatten(io_lib:format(Fmt, Data)),
    run(Cmd, 30*1000).

run (Cmd, Timeout) ->
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
