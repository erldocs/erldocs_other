%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs_other_utils).

%% erldocs_other_utils: shell utilities for erldocs_other_core.

-export([ rmrf/1
        , cp/3
        , find_files/2
        , rmr_symlinks/1
        , du/1

        , git_clone/2
        , git_branches/1
        , git_tags/1
        , git_changeto/2
        , git_get_submodules/1
        , delete_submodules/1

        , rebar_get_deps/1
        , rebar_delete_deps/1
        ]).

%% API

rmrf (Dir) ->
    eo_os:chksh(rmrf, "rm -rf '~s'", [Dir]).

cp (ChDir, Src, Dst) ->
    eo_os:chksh(cp, ChDir, "cp -pr '~s' '~s'", [Src,Dst]).

find_files (Dir, Names) ->
    Tildes = lists:duplicate(length(Names), "~s"),
    Quoted = string:join(Tildes, "' -or -name '"),
    {0,R} = eo_os:sh(Dir, "find . -name '"++ Quoted ++"'", Names),
    [Path || {"./"++Path} <- R].

rmr_symlinks (Dir) ->
    eo_os:chksh(rmr_symlinks, Dir, "find -P . -type l -delete", []).

du (Dir) ->
    {0,R} = eo_os:sh(Dir, "du . | tail -n 1", []),
    [{Size,_}] = R,
    list_to_integer(Size).


git_clone (Url, Dir) ->
    eo_os:chksh(git_clone,
                "git clone --no-checkout -- '~s' '~s'",% >/dev/null",
                [Url,Dir], infinity).

git_branches (RepoDir) ->
    {0,Branches} = eo_os:sh(RepoDir, "git ls-remote --heads origin", []),
    [ {shorten(Commit), Branch}
      || {Commit, "refs/heads/"++Branch} <- Branches ].

git_tags (RepoDir) ->
    {0,Tags} = eo_os:sh(RepoDir,
                        "git tag --list"
                        " | while read tag; do"
                        " echo \"$tag\t$(git rev-list \"$tag\" | head -n 1)\";"
                        "done", []),
    [{shorten(Commit),Tag} || {Tag,Commit} <- Tags].

git_changeto (RepoDir, Commit) ->
    eo_os:chksh(git_changeto, RepoDir, "git checkout --quiet '~s'", [Commit]).

git_get_submodules (RepoDir) ->
    eo_os:shchk(git_get_submodules, RepoDir,
                "git submodule update --init --recursive", [], infinity).

delete_submodules (_RepoDir) -> %No git command as of yet!
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
            eo_os:chksh(touch, "touch '~s'", [NewReFile]);
        {deps, _}=Deps ->
            eo_core:to_file(NewReFile, [Deps])
            %% FIXME {validate_app_modules,false}
    end,
    case eo_os:sh(RepoDir, "rebar --config '~s' get-deps",%  >/dev/null",
                  [NewReFile], infinity) of
        {0, _} ->
            case eo_os:sh(RepoDir, "ls -1 deps/", []) of
                {0, Ls} -> io:format("~p\n",[[Dep || {Dep} <- Ls]]);
                {_, _}  -> io:format("[]\n"), error
            end;
        {_, _} ->
            io:format("[]\n"),
            error
    end.

rebar_delete_deps (_RepoDir) -> %mind rebar hooks!!
    %% rebar allows you to fetch deps into a dir ≠ deps/
    %%{0,_} = sh(RepoDir, "rebar delete-deps  >/dev/null"),
    ok.%%FIXME

%% Internals

shorten (Commit) ->
    lists:sublist(Commit, 7).

%% End of Module.
