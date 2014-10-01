%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_scm).

%% eo_scm: SCM read-only commands.

-export([ refs/1
        , fetch/2 ]).


%% API

refs ({git, Url, _Rev}) ->
    case eo_os:sh("git ls-remote --heads --tags '~s'", [Url]) of
        {0,R} ->
            Branches  = [{shorten(Commit), Branch}
                         || {Commit, "refs/heads/"++Branch} <- R],
            DerefTags = [{ shorten(Commit)
                         , string:sub_string(Tag, 1, length(Tag) -3)}
                         || {Commit, "refs/tags/"++Tag} <- R,
                            lists:suffix("^{}", Tag)],
            NormaTags = [{shorten(Commit), Tag}
                         || {Commit, "refs/tags/"++Tag} <- R,
                            not lists:suffix("^{}", Tag),
                            not lists:keymember(Tag, 2, DerefTags)],
            {ok, Branches, DerefTags++NormaTags};
        {_,_} ->
            error
    end.

fetch (Dir, {git, "https://github.com/"++_=Url, Rev}) ->
    ZipUrl = Url ++ "/archive/" ++ Rev ++ ".zip",
    eo_os:chksh(fetch_curl, Dir,
                "curl --fail --silent --show-error --location"
                " --output '~s.zip' '~s'",
                [Rev,ZipUrl], infinity),
    eo_os:chksh(fetch_unzip, Dir, "unzip -q '~s.zip'", [Rev]),
    %% No real need to rm Rev.zip nor *-Rev*/
    io:format("cd: ~p, mv *-~s*/* .\n", [Dir,Rev]),%%
    %% Following 2 lines <=> `shopt -s dotglob nullglob ; mv *-~s*/* .`
    eo_os:chksh(fetch_mv, Dir, "mv *-~s*/* .", [Rev]),
    eo_os:sh(Dir, "mv *-~s*/.* .", [Rev]), %% Will complain about '.' & '..'
    eo_os:chksh(fetch_rm, Dir, "rm -r '~s.zip' *-~s*/", [Rev,Rev]);

fetch (Dir, {git, "https://bitbucket.org/"++Repo, Rev}) ->
    %% Note: git-archive does not accept SHA1s
    ArchUrl = "git@bitbucket.org:" ++ Repo,
    eo_os:chksh('fetch_git-archive', Dir,
                "git archive --output repo.tar --remote='~s' '~s'",
                [ArchUrl,Rev], infinity),
    eo_os:chksh(fetch_tar, Dir, "tar xf repo.tar", []),
    eo_os:chksh(fetch_rm, Dir, "rm repo.tar", []).

%% Internals

shorten (Commit) ->
    lists:sublist(Commit, 7).

%% End of Module.
