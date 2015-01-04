%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_scm).

%% eo_scm: SCM read-only commands.

-export([ refs/1
        , fetch/2 ]).

-include("eo_common.hrl").

%% API

%% refs/1: get revisions information of repo the fastest way possible
%%   Return Tags ++ Branches :: [#rev{}]

refs ({git, Url, _Rev}) ->
    case eo_os:sh("git ls-remote --heads --tags '~s'", [Url]) of
        {0,R} ->
            Branches  = [#rev{commit=Commit, type=branch, id=Branch}
                         || {Commit, "refs/heads/"++Branch} <- R],
            DerefTags = [#rev{commit=Commit, type=tag
                             , id=string:sub_string(Tag, 1, length(Tag) -3)}
                         || {Commit, "refs/tags/"++Tag} <- R,
                            lists:suffix("^{}", Tag)],
            NormaTags = [#rev{commit=Commit, type=tag, id=Tag}
                         || {Commit, "refs/tags/"++Tag} <- R,
                            not lists:suffix("^{}", Tag),
                            not lists:keymember(Tag, 2, DerefTags)],
            {ok, DerefTags++NormaTags++Branches};
        {_,_} ->
            error  %% Not this kind of repo or does not exist.
    end;

refs ({svn, "https://code.google.com/p/"++Name, _Rev}) ->
    Url = "http://"++Name++".googlecode.com/svn",
    case eo_os:sh("svn ls --verbose '~s/branches' '~s/tags'", [Url,Url]) of
        {0,R} ->
            Dirs = [ begin
                         Row = string:tokens(X, " "),
                         [Revision | Rest] = Row,
                         Dir = lists:last(Rest),
                         {Revision, string:substr(Dir, 1, length(Dir)-1)}
                     end || {X} <- R],
            NotDot = fun ({_,X}) -> X /= "." end,
            {Bs0,Ts0} = lists:splitwith(NotDot, tl(Dirs)),
            Bs = [#rev{id=B, commit=Co, type=branch} || {Co,B} <- Bs0],
            Ts = [#rev{id=T, commit=Co, type=tag}    || {Co,T} <- tl(Ts0)],
            % Where do we put trunk?
            {ok, Ts++Bs};
        {1,_} ->
            error  %% Not this kind of repo or does not exist.
    end.
% SVN `svn help log`


%% fetch/2: get content of revision of repo the fastest way possible.
%%   Always call rmr_symlinks/1 ASAP.

fetch (Dir, {git, "https://github.com/"++_=Url, #rev{id=Branch}}) ->
    ZipUrl = Url ++ "/archive/" ++ Branch ++ ".zip",
    eo_os:chksh(fetch_curl, Dir,
                "curl --fail --silent --show-error --location"
                " --output '~s.zip' '~s'",
                [Branch,ZipUrl], infinity),
    %% FIXME: use zip:extract/1,2
    eo_os:chksh(fetch_unzip, Dir, "unzip -q '~s.zip'", [Branch]),
    erldocs_other_utils:rmr_symlinks(Dir),
    erldocs_other_utils:mv_all("*-~s*/", Branch, Dir), %% FIXME: get rid of wildcard
    %% No real need to rm Rev.zip nor *-Rev*/
    eo_os:chksh(fetch_rm, Dir, "rm -r '~s.zip' *-~s*/", [Branch,Branch]);

fetch (Dir, {git, "https://bitbucket.org/"++Repo, #rev{id=Branch}}) ->
    %% Note: git-archive does not accept SHA1s
    ArchUrl = "git@bitbucket.org:" ++ Repo,
    eo_os:chksh('fetch_git-archive', Dir,
                "git archive --output repo.tar --remote='~s' '~s'",
                [ArchUrl,Branch], infinity),
    %% FIXME: use erl_tar:extract/1,2
    eo_os:chksh(fetch_tar, Dir, "tar xf repo.tar", []),
    erldocs_other_utils:rmr_symlinks(Dir),
    %% FIXME: use file:delete/1 ?
    eo_os:chksh(fetch_rm, Dir, "rm repo.tar", []);

fetch (Dir, {svn, "https://code.google.com/p/"++Name, Rev}) ->
    case Rev#rev.type of
        branch -> Kind = "branches";
        tag    -> Kind = "tags"
    end,
    Title  = Rev#rev.id,
    Commit = Rev#rev.commit,
    SvnUrl = "http://"++Name++".googlecode.com/svn/"++Kind++"/"++Title,
    eo_os:chksh(fetch_svn, Dir, "svn export -r '~s' '~s'",
                [Commit,SvnUrl], infinity),
    erldocs_other_utils:rmr_symlinks(Dir),
    erldocs_other_utils:mv_all("'~s'/", Title, Dir).

%% Internals

%% End of Module.
