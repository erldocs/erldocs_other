%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(other_core).

%% other_core: main logic of the “other” module.

-export([ main/1 ]).

-define(LOG(Str, Args), io:format(" :: "++ Str, Args)).
-define(LOG(Str),       io:format(" :: "++ Str)).


%% API

main (Conf) ->
    URL0         = kf(Conf, url),
    Destination0 = kf(Conf, dest),
    {Method, Url} = method(string:to_lower(URL0)),
    RepoName = repo_name(Url),

    Dest     = filename:absname(Destination0),
    mkdir(Dest),
    TmpDir   = filename:join(Dest, RepoName),
    mkdir(TmpDir),
    DocsRoot = filename:join(Dest, repo_local_path(Url)),
    mkdir(DocsRoot),
    MetaFile = filename:join(DocsRoot, "meta"),

    ?LOG("Cloning repo ~p into ~p\n", [Url,TmpDir]),
    ok = clone_repo(Method, Url, TmpDir),

    ?LOG("Extracting meta information\n"),
    Meta = extract_info(Method, Url, TmpDir),
    ?LOG("Writing meta to ~p\n", [MetaFile]),
    to_file(MetaFile, Meta),

    ?LOG("Preparing repo for docs generation\n"),
    Clones = duplicate_repo(Method, RepoName, Meta, Dest),
    other_utils:rmrf(TmpDir),

    %%Probably `make` cloned repos (using shell's <> & sandbox)

    ?LOG("Generating erldocs.\n"),
    erldocs(Conf, DocsRoot, Clones),
    ?LOG("Erldocs finishing up.\n"),
    put_repo_index(Conf, DocsRoot, Meta).

%% Internals

list_titles (DocsRoot, Titles) ->
    Items = [ begin
                  ErldocsP = filename:join([DocsRoot, Branch, "index.html"]),
                  case file:read_file_info(ErldocsP) of
                      {ok, _} ->
                          "<a href=\""++Branch++"\">"++Branch++"</a>";
                      {error, _} ->
                          other_utils:rmrf(filename:dirname(ErldocsP)),
                          Branch
                  end
              end || {_,Branch} <- Titles ],
    Spaces = lists:duplicate(3, "&nbsp;"),
    string:join(Items, Spaces).

put_repo_index (Conf, DocsRoot, Meta) ->
    Url      = kf(Meta, url),
    Repo     = kf(Meta, target_path),
    Tags     = kf(Meta, tags),
    Branches = kf(Meta, branches),
    HTML = "<h3 id=\"tags\">Tags</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Tags) ++ "</p>"
        ++ "<br/>"
        ++ "\n\t<h3 id=\"branches\">Branches</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Branches) ++ "</p>",
    Args = [ {title, Repo}
           , {url, Url}
           , {content, HTML}
           , {ga, kf(Conf,ga)} ],
    {ok, Data} = repo_dtl:render(Args),
    ok = file:write_file(filename:join(DocsRoot,"index.html"), Data).

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
              other_utils:rmrf(filename:dirname(Path)),
              %% rm repo/branch/.xml/
              other_utils:rmrf(filename:join(DocsDest, ".xml"))
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
              other_utils:cp(DestDir, RepoName, TitledRepo),
              other_utils:git_changeto(TitledRepo, Commit),
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
    , {branches, other_utils:git_branches(TmpDir)}
    , {tags, other_utils:git_tags(TmpDir)}
    ];
extract_info (Other, _, _) ->
    ?LOG("~p method not supported yet\n", [Other]),
    error.

clone_repo (git, URL, TmpDir) ->
    other_utils:git_clone(URL, TmpDir);
clone_repo (_, URL, _) ->  %% Git scheme will come here later…
    ?LOG("~s scheme or host not suported yet\n", [URL]),
    error.

method ("https://github.com/"++_=Url) -> {git, Url};
method ("https://bitbucket.org/"++_=Url) -> {git, Url};
method (Url) -> {other, Url}.

to_file (Path, Data) ->
    Str = [io_lib:fwrite("~p.\n",[Datum]) || Datum <- Data],
    file:write_file(Path, Str).

%% End of Module.
