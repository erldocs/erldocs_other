%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_scm).

%% eo_scm: SCM read-only commands.

-export([ refs/1
        , fetch/2
        , repo_name/1
        , repo_local_path/1
        , method/1
        , url/1
        ]).

-export_type([ method/0
             , repo_url/0
             , source/0
             ]).

-include("erldocs_other.hrl").

-type method() :: git | svn.
-type repo_url() :: nonempty_string().
-type source() :: {method(), repo_url(), '_' | rev()}.

%% API

%% @doc Get revisions information of repo the fastest way possible
-spec refs (source()) -> {ok, [rev()]} | error.

refs ({git, Url, _Rev}) ->
    case eo_os:sh("git ls-remote --heads --tags '~s'", [Url]) of
        {0,R} ->
            Branches  = [#rev{commit=Commit, type=branch, id=Branch}
                         || {Commit, "refs/heads/"++Branch} <- R],
            DerefTags = [#rev{commit=Commit, type=tag, id=dereference(Tag)}
                         || {Commit, "refs/tags/"++Tag} <- R,
                            lists:suffix("^{}", Tag)],
            NormaTags = [#rev{commit=Commit, type=tag, id=Tag}
                         || {Commit, "refs/tags/"++Tag} <- R,
                            not lists:suffix("^{}", Tag),
                            not lists:keymember(Tag, #rev.id, DerefTags)],
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
            %% FIXME: What about trunk? (eg. https://code.google.com/p/plists/)
            {ok, Ts++Bs};
        {1,_} ->
            error  %% Not this kind of repo or does not exist.
    end.
% SVN `svn help log`


%% @doc Get content of revision of repo the fastest way possible.
%%   Implentation note: Always call rmr_symlinks/1 ASAP.
-spec fetch (filelib:dirname(), source()) -> ok.

fetch (Dir, {git, "https://github.com/"++_=Url, #rev{ id = Title
                                                    , type = Type }}) ->
    Name = lists:last(string:tokens(Url, "/")),
    %% GitHub is special…
    case {Type, Title} of
        {tag, "v"++Shortened} -> UnZipped = Name ++"-"++ Shortened;
        _                     -> UnZipped = Name ++"-"++ Title
    end,
    Zipped = Title ++".zip",
    ZipUrl = Url ++"/archive/"++ Zipped,
    eo_os:chksh(fetch_curl, Dir,
                "curl --fail --silent --show-error --location"
                " --output '~s' '~s'",
                [Zipped,ZipUrl], infinity),
    AbsZipped = filename:join(Dir, Zipped),
    {ok,_} = zip:extract(AbsZipped, [{cwd,Dir}]),
    eo_util:rmr_symlinks(Dir),
    eo_util:mv_all(UnZipped, Dir),
    file:delete(AbsZipped);

fetch (Dir, {git, "https://bitbucket.org/"++Repo, #rev{ id = Branch }}) ->
    %% Note: git-archive does not accept SHA1s
    ArchUrl = "git@bitbucket.org:"++ Repo,
    eo_os:chksh('fetch_git-archive', Dir,
                "git archive --output repo.tar --remote='~s' '~s'",
                [ArchUrl,Branch], infinity),
    AbsTarred = filename:join(Dir, "repo.tar"),
    ok = erl_tar:extract(AbsTarred, [{cwd,Dir}]),
    eo_util:rmr_symlinks(Dir),
    file:delete(AbsTarred);

fetch (Dir, {git, Url, #rev{ id = Title }}) ->
    eo_os:chksh('fetch_git-clone', Dir,
                "git clone --depth 1 '~s' --branch '~s' -- '~s'",
                [Url,Title,Dir], infinity),
    eo_util:rmr_symlinks(Dir);

fetch (Dir, {svn, "https://code.google.com/p/"++Name, #rev{ id = Title
                                                          , type = Type
                                                          , commit = Commit }}) ->
    %%svn checkout http://plists.googlecode.com/svn/trunk/ plists-read-only
    case Type of
        branch -> Kind = "branches";
        tag    -> Kind = "tags"
    end,
    SvnUrl = "http://"++Name++".googlecode.com/svn/"++Kind++"/"++Title,
    eo_os:chksh(fetch_svn, Dir, "svn export -r '~s' '~s'",
                [Commit,SvnUrl], infinity),
    eo_util:rmr_symlinks(Dir),
    eo_util:mv_all(Title, Dir).


%% @doc Extract repo's name from repo's URL.
%%   Eg: "https://github.com/erldocs/erldocs_other"
%%       -> "erldocs_other"
-spec repo_name (repo_url()) -> nonempty_string().

repo_name (Url) ->
    lists:last(string:tokens(Url, "/")).


%% @doc Extract what will be a repo's path on the docs website.
%%   Eg: "https://github.com/erldocs/erldocs_other"
%%       -> "github.com/erldocs/erldocs_other"
-spec repo_local_path (repo_url()) -> nonempty_string().

repo_local_path (Url) ->
    Exploded = string:tokens(Url, "/"),
    filename:join(tl(Exploded)).


%% @doc Guess first SCM method to try.
-spec method (string()) -> method().

method ("https://github.com/"++_) -> git;
method ("https://bitbucket.org/"++_) -> git;
method ("https://code.google.com/p/"++_) -> svn.


%% @doc Canonize input string into a recognized repo url.
%%   Eg: "https://github.com/erldocs/erldocs_other.git/…"
%%       -> {true, "https://github.com/erldocs/erldocs_other"}
%%   Note: lists:filtermap/2-ready output.
-spec url (string()) -> {true, repo_url()} | false.

url (URL0) ->
    Url = string:to_lower(URL0),
    case find("(github\\.com|bitbucket\\.org)[:/]([^:/]+)/([^/]+)", Url) of
        {match, [Site,User,Name]} ->
            {true, "https://"++Site++"/"++User++"/"++trim_dotgit(Name)};
        nomatch ->
            case find("code\\.google\\.com/p/([^/]+)", Url) of
                {match, [Name]} ->
                    {true, "https://code.google.com/p/"++Name};
                nomatch -> false
            end
    end.

%% Internals

find (RegExp, Subject) ->
    re:run(Subject, RegExp, [{capture,all_but_first,list}]).

trim_dotgit (Str) ->
    filename:basename(Str, ".git").

dereference (Tag0) ->
    DerefToken = "^{}",
    Tag = string:sub_string(Tag0, 1, length(Tag0) - length(DerefToken)),
    case lists:suffix(DerefToken, Tag) of
        false -> Tag;
        true  -> dereference(Tag)
    end.

%% End of Module.
