%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs_other_utils).

%% erldocs_other_utils: shell utilities for erldocs_other_core.

-export([ rmrf/1
        , cp/3
        , find_files/2

        , git_clone/2
        , git_branches/1
        , git_tags/1
        , git_changeto/2
        , git_get_submodules/1
        , delete_submodules/1

        , rebar_get_deps/1
        , rebar_delete_deps/1
        ]).

-define(ShortCmdTimeout, 5*1000).

%% API

rmrf (Dir) ->
    chk(sh("rm -rf '~s'", [Dir])).

cp (ChDir, Src, Dst) ->
    chk(sh(ChDir, "cp -pr '~s' '~s'", [Src,Dst])).

find_files (Dir, Names) ->
    Tildes = lists:duplicate(length(Names), "~s"),
    Quoted = string:join(Tildes, "' -or -name '"),
    {0,R} = sh(Dir, "find . -name '"++ Quoted ++"'", Names),
    [Path || {"./"++Path} <- R].

git_clone (Url, Dir) ->
    chk(sh("git clone --no-checkout -- '~s' '~s'  >/dev/null 2>&1",
           [Url,Dir], infinity)).

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
    chk(sh(RepoDir, "git checkout --quiet '~s'", [Commit])).

git_get_submodules (RepoDir) ->
    %% (Does nothing if no .gitmodules exists)
    chk(sh(RepoDir,
           "git submodule update --init --recursive  >/dev/null 2>&1",
           [], infinity)).

delete_submodules (RepoDir) -> %No git command as of yet!
    %%cat gitmodules.txt | `which grep` -P '^\s*path\s+=' | sed 's/\s\*//' | cut -d ' ' -f 3
    %%string:tokens("  \tpath = .gitmodule/raintpl", "\t ").
    impl.%%FIXME

%%FIXME git config --get remote.origin.url (for each remote)

rebar_get_deps (RepoDir) ->
    %% Gets rid of rebar hooks (potential code execution);
    %%   copies (only) deps from rebar.config to ../rebar_deps.
    OnlyReDepsFile = "rebar_deps",
    TitledDir = filename:dirname(RepoDir),
    ReFile    = filename:join(RepoDir, "rebar.config"),
    NewReFile = filename:join(TitledDir, OnlyReDepsFile),
    {ok, ReConf} = file:consult(ReFile),
    case lists:keyfind(deps, 1, ReConf) of
        false ->
            chk(sh("touch '~s'", [NewReFile]));
        {deps, _}=Deps ->
            erldocs_other_core:to_file(NewReFile, [Deps])
    end,
    case sh(RepoDir, "rebar --config '~s' get-deps  >/dev/null 2>&1",
            [NewReFile], infinity) of
        {0, _} ->
            {0,Ls} = sh(RepoDir, "ls -1 deps/", []),
            io:format("~p\n",[[Dep || {Dep} <- Ls]]);
        {_, _} ->
            io:format("[]\n"),
            error
    end.

rebar_delete_deps (RepoDir) -> %mind rebar hooks!!
    %% rebar allows you to fetch deps into a dir ≠ deps/
    %%{0,_} = sh(RepoDir, "rebar delete-deps  >/dev/null 2>&1"),
    ok.%%FIXME

%% Internals

chk (ShCall) ->
    case ShCall of
        {0, _} -> ok;
        {Code, Stdout} -> throw({sh,error,Code,Stdout})
    end.

shorten (Commit) ->
    lists:sublist(Commit, 7).

sh (Fmt, Data) ->
    sh (Fmt, Data, ?ShortCmdTimeout).

sh (Fmt, Data, Timeout)
  when is_atom(Timeout); is_integer(Timeout) ->
    Cmd = lists:flatten(io_lib:format(Fmt, Data)),
    run(Cmd, Timeout);

sh (Dir, Fmt, Data) ->
    sh (Dir, Fmt, Data, ?ShortCmdTimeout).

sh (Dir, Fmt, Data, Timeout) ->
    {ok, PreviousDir} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    Res = sh(Fmt, Data, Timeout),
    ok = file:set_cwd(PreviousDir),
    Res.

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
