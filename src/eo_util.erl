%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_util).

%% eo_util: shell utilities for eo_core.

-export([ rmrf/1, rm_r/1, rm_r/2
        , cp/3
        , find_files/2
        , find_delete/2
        , rmr_symlinks/1
        , du/1
        , mv/2
        , mv_all/2

        , git_get_submodules/1
        , delete_submodules/1

        , hg_test/1

        , rebar_get_deps/1
        , rebar_delete_deps/1
        ]).

%% API

rmrf (Dir) ->
    eo_os:chksh(rmrf, "rm -rf '~s'", [Dir]).

rm_r (Dir) ->
    eo_os:chksh(rm_r, "rm -r '~s'", [Dir]).

rm_r (Paths, ChDir) ->
    Tildes = lists:duplicate(length(Paths), "~s"),
    Quoted = string:join(Tildes, "' '"),
    Cmd = "rm -r '"++ Quoted ++"'",
    eo_os:chksh(rm_r2, ChDir, Cmd, Paths).

cp (ChDir, Src, Dst) ->
    eo_os:chksh(cp, ChDir, "cp -pr '~s' '~s'", [Src,Dst]).

find_files (Dir, Names) ->
    Tildes = lists:duplicate(length(Names), "~s"),
    Quoted = string:join(Tildes, "' -or -name '"),
    {0,R} = eo_os:sh(Dir, "find . -name '"++ Quoted ++"'", Names),
    [Path || {"./"++Path} <- R].

find_delete (Dir, Names) ->
    Tildes = lists:duplicate(length(Names), "~s"),
    Quoted = string:join(Tildes, "' -or -name '"),
    Cmd = "find . \\( -name '"++ Quoted ++"' \\) -exec rm -r \"{}\" \\;",
    {_Code,_} = eo_os:sh(Dir, Cmd, Names),
    ok.

rmr_symlinks (Dir) ->
    eo_os:chksh(rmr_symlinks, Dir, "find -P . -type l -delete", []).

du (Dir) ->
    {0,R} = eo_os:sh(Dir, "du . | tail -n 1", []),
    [{Size,_}] = R,
    list_to_integer(Size).

mv (Paths, Dir) ->
    Tildes = lists:duplicate(length(Paths), "~s"),
    Quoted = string:join(Tildes, "' '"),
    eo_os:chksh(mv, "mv '"++ Quoted ++"' '~s'", Paths++[Dir]).

mv_all (Src, Dst) ->
    %% <>  mv {Src/*,Src/.*} Dst/  But silent about . and ..
    eo_os:chksh(mv_all, Dst,
                "find '~s' -mindepth 1 -maxdepth 1 -exec mv -t . -- {} +",
                [Src]).


git_get_submodules (RepoDir) ->
    eo_os:chksh(git_get_submodules, RepoDir,
                "git submodule update --init --recursive", [], infinity).

delete_submodules (_RepoDir) -> %No git command as of yet!
    %%cat gitmodules.txt | `which grep` -P '^\s*path\s+=' | sed 's/\s\*//' | cut -d ' ' -f 3
    %%string:tokens("  \tpath = .gitmodule/raintpl", "\t ").
    impl.%%FIXME

%%FIXME git config --get remote.origin.url (for each remote)

hg_test (Url) ->
    case eo_os:sh("hg identify '~s'", [Url]) of
        {0,_} -> true;
        {255,_} -> false
    end.

rebar_get_deps (RepoDir) ->
    %% Gets rid of rebar hooks (potential code execution);
    %%   copies (only) deps from rebar.config to ../rebar_deps.
    %% FIXME: {validate_app_modules,false}
    ReFile = filename:join(RepoDir, "rebar.config"),
    case read_deps(ReFile) of
        []   -> ignore;
        Deps -> get_deps(RepoDir, Deps)
    end,
    case eo_os:sh(RepoDir, "ls -1 deps/", []) of
        {0,R} -> Dirs0 = R;
        {_,_} -> Dirs0 = []
    end,
    Dirs = [Dep || {Dep} <- Dirs0],
    io:format("~p\n", [Dirs]),
    Dirs.

rebar_delete_deps (_RepoDir) -> %mind rebar hooks!!
    %% rebar allows you to fetch deps into a dir ≠ deps/
    %%{0,_} = sh(RepoDir, "rebar delete-deps  >/dev/null"),
    ok.%%FIXME

%% Internals

get_deps (RepoDir, Deps) ->
    TitledDir = filename:dirname(RepoDir),
    NewReFile = filename:join(TitledDir, "rebar_deps"),
    eo_core:to_file(NewReFile, [{deps,Deps}]),
    %% We don't want to stop if this fails
    _ = eo_os:sh(RepoDir, "rebar --config '~s' get-deps",%  >/dev/null",
                 [NewReFile], infinity),
    rmr_symlinks(RepoDir).

read_deps (RebarFile) ->
    case file:consult(RebarFile) of
        {ok, RebarConf} ->
            case lists:keyfind(deps, 1, RebarConf) of
                false        -> [];
                {deps, Deps} -> Deps
            end;
        _ ->
            []
    end.

%% End of Module.
