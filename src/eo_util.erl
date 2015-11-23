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
        , mv/2

        , git_get_submodules/1
        , delete_submodules/1

        , hg_test/1

        , rebar_get_deps/1
        , rebar_delete_deps/1

        , mkdir/1
        ]).

-include("logging.hrl").

%% API

rmrf (Dir) ->
    eo_os:chksh(rmrf, ["rm", "-rf", Dir]).

rm_r (Dir) ->
    eo_os:chksh(rm_r, ["rm", "-r", Dir]).

rm_r (_ChDir, []) -> ok;
rm_r (ChDir, Paths) ->
    eo_os:chksh(rm_r2, ChDir, ["rm", "-r"] ++ Paths).

cp (ChDir, Src, Dst) ->
    eo_os:chksh(cp, ChDir, "cp", ["-pr", Src, Dst]).

find_files (Dir, Names) ->
    Cmd = ["find", ".", "-name"] ++ join_iolist(["-or", "-name"], Names),
    {0,R} = eo_os:sh(Dir, Cmd),
    [Path || {"./"++Path} <- R].

find_delete (Dir, Names) ->
    Cmd = ["find", ".", "\\(", "-name"]
        ++ join_iolist(["-or","-name"], Names)
        ++ ["\\)", "-exec", "rm", "-r", "\"{}\"", "\\;"],
    {_Code,_} = eo_os:sh(Dir, Cmd),
    ok.

rmr_symlinks (Dir) ->
    eo_os:chksh(rmr_symlinks, Dir, ["find", "-P", ".", "-type", "l", "-delete"]).

mv ([], _Dir) -> ok;
mv (Paths, Dir) ->
    eo_os:chksh(mv, ["mv"] ++ Paths ++ [Dir]).


git_get_submodules (RepoDir) ->
    eo_os:sh(RepoDir, ["git", "submodule", "update", "--init", "--recursive"], infinity).

delete_submodules (_RepoDir) -> %No git command as of yet!
    %%cat gitmodules.txt | `which grep` -P '^\s*path\s+=' | sed 's/\s\*//' | cut -d ' ' -f 3
    %%string:tokens("  \tpath = .gitmodule/raintpl", "\t ").
    impl.%%FIXME

%%FIXME git config --get remote.origin.url (for each remote)

hg_test (Url) ->
    case eo_os:sh(["hg", "identify", Url]) of
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
    case eo_os:sh(RepoDir, ["ls", "-1", "deps/"]) of
        {0,R} -> Dirs0 = R;
        {_,_} -> Dirs0 = []
    end,
    Dirs = [Dep || {Dep} <- Dirs0],
    ?NOTE("rebar", "deps = ~p", [Dirs]),
    Dirs.

rebar_delete_deps (_RepoDir) -> %mind rebar hooks!!
    %% rebar allows you to fetch deps into a dir ≠ deps/
    %%{0,_} = sh(RepoDir, "rebar delete-deps  >/dev/null"),
    ok.%%FIXME


mkdir (Dir) ->
    ok = filelib:ensure_dir(Dir ++ "/").

%% Internals

get_deps (RepoDir, Deps) ->
    TitledDir = filename:dirname(RepoDir),
    NewReFile = filename:join(TitledDir, "rebar_deps"),
    eo_core:to_file(NewReFile, [{deps,Deps}]),
    %% We don't want to stop if this fails
    _ = eo_core:sh(RepoDir, ["rebar", "--config", NewReFile, "get-deps"], infinity),
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


join_iolist (Sep, [H|T]) ->
    %% [H|T] = ["a", "b", "c"].
    %% Sep = ["-or", "-name"].
    %% #=> ["a","-or","-name","b","-or","-name","c"]
    lists:append([[H]] ++ lists:append([[Sep, [X]] || X <- T])).

%% End of Module.
