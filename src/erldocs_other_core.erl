%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(erldocs_other_core).

%% erldocs_other_core: main logic of the erldocs_other module.

-export([ main/1 ]).

-define(LOG(Str, Args), io:format(" :: "++ Str, Args)).
-define(LOG(Str),       io:format(" :: "++ Str)).

-define(u, erldocs_other_utils).

%% API

main (Conf) ->
    URL0         = kf(Conf, url),
    Destination0 = kf(Conf, dest),
    {Method, Url} = method(URL0),
    RepoName = repo_name(Url),

    Dest     = filename:absname(Destination0),
    mkdir(Dest),
    TmpDir   = filename:join(Dest, RepoName),
    mkdir(TmpDir),
    DocsRoot = filename:join(Dest, "repo"),
    mkdir(DocsRoot),
    MetaFile = filename:join(Dest, "meta.terms"),

    ?LOG("Cloning repo ~p into ~p\n", [Url,TmpDir]),
    ok = clone_repo(Method, Url, TmpDir),

    ?LOG("Extracting meta information\n"),
    Meta = extract_info(Method, Url, TmpDir),
    ?LOG("Writing meta to ~p\n", [MetaFile]),
    to_file(MetaFile, Meta),

    ?LOG("Preparing repo for docs generation\n"),
    Clones = duplicate_repo(Method, RepoName, Meta, Dest),
    ?u:rmrf(TmpDir),

    %%Probably `make` cloned repos (using shell's <> & sandbox)

    ?LOG("Discovering other repos\n"),
    repo_discovery(MetaFile, Clones),

    ?LOG("Generating erldocs:\n"),
    erldocs(Conf, DocsRoot, Clones),
    ?LOG("Erldocs finishing up.\n"),
    put_repo_index(Conf, DocsRoot, Meta).

%% Internals

html_index (DocsRoot, Meta) ->
    Tags     = kf(Meta, tags),
    Branches = kf(Meta, branches),
    "<h3 id=\"tags\">Tags</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Tags) ++ "</p>"
        ++ "<br/>"
        ++ "\n\t<h3 id=\"branches\">Branches</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Branches) ++ "</p>".

list_titles (DocsRoot, Titles) ->
    Items = [ begin
                  ErldocsP = filename:join([DocsRoot, Branch, "index.html"]),
                  case file:read_file_info(ErldocsP) of
                      {ok, _} ->
                          "<a href=\""++Branch++"\">"++Branch++"</a>";
                      {error, _} ->
                          ?u:rmrf(filename:dirname(ErldocsP)),
                          Branch
                  end
              end || {_,Branch} <- Titles ],
    Spaces = lists:duplicate(3, "&nbsp;"),
    string:join(Items, Spaces).

put_repo_index (Conf, DocsRoot, Meta) ->
    Args = [ {title,   kf(Meta, target_path)}
           , {url,     kf(Meta, url)}
           , {content, html_index(DocsRoot, Meta)}
           , {base,    kf(Conf, base)}
           , {ga,      kf(Conf, ga)} ],
    {ok, HTML} = html_dtl:render(Args),
    ok = file:write_file(filename:join(DocsRoot,"index.html"), HTML),
    {ok, CSS}  = css_dtl:render([]),
    ok = file:write_file(filename:join(DocsRoot,"repo.css"), CSS).

repo_discovery (Metafile, Clones) ->
    Dumps = lists:map(
              fun ({Title, RepoPath}) ->
                      Found = ?u:find_files(RepoPath, [ "rebar.config"
                                                      , "Makefile"
                                                      , ".gitmodules" ]),
                      {Title, [ begin
                                    FilePath = filename:join(RepoPath, File),
                                    {ok, Bin} = file:read_file(FilePath),
                                    {File, Bin}
                                end || File <- Found ]}
              end, Clones),
    to_file(Metafile, [{discover,Dumps}], [append]).

erldocs (Conf, DocsRoot, Clones) ->
    lists:foreach(
      fun ({Branch, Path}) ->
              DocsDest = filename:join(DocsRoot, Branch),
              mkdir(DocsDest),
              ?LOG("Generating erldocs for ~s into ~s\n", [Path,DocsDest]),
              erldocs:main([ Path
                           , "-o", DocsDest
                           , "--ga", kf(Conf,ga) ]
                           ++ [ "-I", filename:join(Path, "include")]
                           ++ lists:flatmap(
                                fun (Inc) ->
                                        [ "-I", filename:join(Path, Inc)]
                                end,
                                filelib:wildcard(Path++"/deps/*/include"))
                          ),
              %% rm git repo
              ?u:rmrf(filename:dirname(Path))
      end, Clones).

kf (Conf, Key) ->
    {Key, Value} = lists:keyfind(Key, 1, Conf),
    Value.

duplicate_repo (git, RepoName, Meta, DestDir) ->
    Tags     = kf(Meta, tags),
    Branches = kf(Meta, branches),
    lists:map(
      fun ({Commit, Title}) ->
              Name = make_name(RepoName, Commit, Title),
              TitledRepo = filename:join([DestDir, Name, RepoName]),
              mkdir(filename:join(DestDir, Name)),
              %% cd DestDir && cp -pr RepoName TitledRepo
              ?u:cp(DestDir, RepoName, TitledRepo),
              ?u:git_changeto(TitledRepo, Commit),
              {Title, TitledRepo}
      end, Tags ++ Branches).

mkdir (Dir) ->
    ok = filelib:ensure_dir(Dir ++ "/").

make_name (RepoName, Commit, Branch) ->
    string:join([RepoName, Commit, Branch], "-").

repo_name (Url) ->
    lists:last(string:tokens(Url, "/")).

repo_local_path (Url) ->
    Exploded = string:tokens(Url, "/"),
    filename:join(tl(Exploded)).

extract_info (git, Url, TmpDir) ->
    [ {name, repo_name(Url)}
    , {target_path, repo_local_path(Url)}
    , {url, Url}
    , {method, git}
    , {branches, ?u:git_branches(TmpDir)}
    , {tags, ?u:git_tags(TmpDir)}
    ];
extract_info (Other, _, _) ->
    ?LOG("~p method not supported yet\n", [Other]),
    error.

clone_repo (git, Url, TmpDir) ->
    ?u:git_clone(Url, TmpDir);
clone_repo (_, Url, _) ->  %% Git scheme will come here later…
    ?LOG("~s scheme or host not suported yet\n", [Url]),
    error.


method (Url) ->
    case string:to_lower(Url) of
        "http:" ++Rest -> url_extract(href, Rest);
        "https:"++Rest -> url_extract(href, Rest);
        "git@"++Rest ->   url_extract(git, Rest)
    end.

url_extract (href, Rest) ->
    case re("^//github.com/([^/]+)/([^/]+)", Rest) of
        {match, [User,Proj]} ->
            {git, url_assemble(github, [User,Proj])};
        nomatch ->
            case re("^//([^/@]+@)?bitbucket.org/([^/]+)/([^/]+)", Rest) of
                {match, [_,User,Proj]} ->
                    {git, url_assemble(bitbucket, [User,Proj])}
            end
    end;

url_extract (git, Rest) ->
    case re("^github.com:([^:/]+)/([^/]+)", Rest) of
        {match, [User,Proj]} ->
            {git, url_assemble(github, [User,Proj])};
        nomatch ->
            case re("^bitbucket.org:([^:/]+)/([^/]+)", Rest) of
                {match, [User,Proj]} ->
                    {git, url_assemble(bitbucket, [User,Proj])}
            end
    end.

url_assemble (github, [User,Proj]) ->
    "https://github.com/"++ User ++"/"++ rm_dotgit_suffix(Proj);
url_assemble (bitbucket, [User,Proj]) ->
    "https://bitbucket.org/"++ User ++"/"++ rm_dotgit_suffix(Proj).

re (Pattern, Subject) ->
    re (Pattern, Subject, [{capture,all_but_first,list},unicode]).
re (Pattern, Subject, Options) ->
    re:run(Subject, Pattern, Options).

rm_dotgit_suffix (Str) ->
    case lists:suffix(".git", Str) of
        false -> Str;
        true  ->
            lists:reverse(
              lists:reverse(Str) -- lists:reverse(".git"))
    end.


to_file (Path, Data) ->
    to_file (Path, Data, []).
to_file (Path, Data, Options) ->
    Str = [io_lib:fwrite("~p.\n",[Datum]) || Datum <- Data],
    file:write_file(Path, Str, Options).

%% End of Module.
