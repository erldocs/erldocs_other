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
-type source() :: {method(), repo_url(), '_' | eo_core:rev()}.

%% API

%% @doc Get revisions information of repo the fastest way possible
-spec refs (source()) -> {ok, [eo_core:rev()]} | error.

refs ({git, Url, _Rev}) ->
    case eo_os:sh("git ls-remote --heads --tags '~s'", [Url], infinity) of
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
    case eo_os:sh( "svn ls --verbose '~s/branches' '~s/tags' '~s/trunk'"
                 , [Url,Url,Url], infinity)
    of
        {0,R} ->
            Dirs = [ begin
                         Row = string:tokens(X, " "),
                         [Revision | Rest] = Row,
                         Dir = lists:last(Rest),
                         {Revision, trim_dangling_slash(Dir)}
                     end || {X} <- R],
            {ok, parse_svn_ls(Dirs)};
        {1,_} ->
            error  %% Not this kind of repo or does not exist.
    end.
% SVN `svn help log`


%% @doc Get content of revision of repo the fastest way possible.
%%   Implentation note: Always call rmr_symlinks/1 ASAP.
-spec fetch (filelib:dirname(), source()) -> {ok, file:filename()}.

fetch (Dir, {git, "https://github.com/"++Repo=Url, #rev{ id = Title }}) ->
    TarUrl = "https://codeload.github.com/"++ Repo ++"/tar.gz/"++ Title,
    eo_os:chksh(fetch_curl, Dir,
                "curl --fail --silent --show-error --location"
                " --output repo.tar.gz '~s'",
                [TarUrl], infinity),
    AbsTarred = filename:join(Dir, "repo.tar.gz"),
    %% Note: the tar has a root directory
    ok = erl_tar:extract(AbsTarred, [{cwd,Dir}, compressed]),
    AbsTitledPath = filename:join(Dir, repo_name(Url)),
    case [filename:join(Dir, D) || D <- filelib:wildcard("*", Dir)
                                       , D =/= "repo.tar.gz"
                                       , filelib:is_dir(filename:join(Dir, D))]
    of
        [] -> eo_util:mkdir(AbsTitledPath);
        [AbsUnTarred] ->
            eo_util:rmr_symlinks(AbsUnTarred),
            eo_util:mv([AbsUnTarred], AbsTitledPath)
    end,
    ok = file:delete(AbsTarred),
    {ok, AbsTitledPath};

fetch (Dir, {git, "https://bitbucket.org/"++Repo=Url, #rev{ id = Branch }}) ->
    %% Note: git-archive does not accept SHA1s
    ArchUrl = "git@bitbucket.org:"++ Repo,
    eo_os:chksh('fetch_git-archive', Dir,
                "git archive --output repo.tar --remote='~s' '~s'",
                [ArchUrl,Branch], infinity),
    AbsTarred = filename:join(Dir, "repo.tar"),
    AbsTitledPath = filename:join(Dir, repo_name(Url)),
    %% Note: the tarball does not have a root directory
    ok = erl_tar:extract(AbsTarred, [{cwd,AbsTitledPath}]),
    eo_util:rmr_symlinks(AbsTitledPath),
    ok = file:delete(AbsTarred),
    {ok, AbsTitledPath};

fetch (Dir, {git, Url, #rev{ id = Title }}) ->
    AbsTitledPath = filename:join(Dir, repo_name(Url)),
    eo_os:chksh('fetch_git-clone', Dir,
                "git clone --depth 1 '~s' --branch '~s' -- '~s'",
                [Url,Title,AbsTitledPath], infinity),
    eo_util:rmr_symlinks(AbsTitledPath),
    {ok, AbsTitledPath};

fetch (Dir, {svn, "https://code.google.com/p/"++Name=Url, #rev{ id = Title
                                                              , type = Type
                                                              , commit = Commit }}) ->
    case {Title,Type} of
        {"trunk",branch} ->
            SvnUrl = "http://"++Name++".googlecode.com/svn/trunk";
        {_,branch} ->
            SvnUrl = "http://"++Name++".googlecode.com/svn/branches/"++Title;
        {_,tag} ->
            SvnUrl = "http://"++Name++".googlecode.com/svn/tags/"++Title
    end,
    eo_os:chksh(fetch_svn, Dir, "svn export -r '~s' '~s'",
                [Commit,SvnUrl], infinity),
    AbsExported = filename:join(Dir, hd(filelib:wildcard("*", Dir))),
    eo_util:rmr_symlinks(AbsExported),
    AbsTitledPath = filename:join(Dir, repo_name(Url)),
    eo_util:mv([AbsExported], AbsTitledPath),
    {ok, AbsTitledPath}.


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

trim_dangling_slash (Str) ->
    case lists:suffix("/", Str) of
        true  -> lists:droplast(Str);
        false -> Str
    end.

dereference (Tag0) ->
    DerefToken = "^{}",
    Tag = string:sub_string(Tag0, 1, length(Tag0) - length(DerefToken)),
    case lists:suffix(DerefToken, Tag) of
        false -> Tag;
        true  -> dereference(Tag)
    end.

parse_svn_ls ([{_,"."}|Rest]) ->
    split_svn_ls(Rest, branch, []).

split_svn_ls ([{_,"."}|Rest], branch, Acc) ->
    split_svn_ls(Rest, tag, Acc);
split_svn_ls ([{Co,"."}|_], tag, Acc) ->
    Rev = #rev{id="trunk", commit=Co, type=branch},
    [Rev|Acc];
split_svn_ls ([{Co,Id}|Rest], Type, Acc) ->
    Rev = #rev{id=Id, commit=Co, type=Type},
    split_svn_ls(Rest, Type, [Rev|Acc]).

%% End of Module.
