%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(other_core).

%% other_core: main logic of the “other” module.

-export([ main/2 ]).

-define(LOG(Str, Args), io:format(" :: "++ Str, Args)).
-define(LOG(Str),       io:format(" :: "++ Str)).


%% API

-spec main (string(), file:name()) -> term().
main (URL0, Destination0) ->
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
    {_, Ts} = lists:keyfind(tags, 1, Meta),
    {_, Bs} = lists:keyfind(branches, 1, Meta),
    Branches = Ts ++ Bs,

    ?LOG("Preparing repo for docs generation\n"),
    Clones = duplicate_repo(Method, RepoName, Branches, Dest),
    other_utils:rmrf(TmpDir),
    io:format("Clones = ~p\n", [Clones]),

    %%Probably `make` cloned repos (using shell's <> & sandbox)

    ?LOG("Generating erldocs.\n"),
    erldocs(DocsRoot, Clones),
    put_repo_index(DocsRoot, Branches).

%% Internals

put_repo_index (DocsRoot, Branches) ->
    pass.

erldocs (DocsRoot, Clones) ->
    lists:foreach(
      fun ({Branch, Path}) ->
              DocsDest = filename:join(DocsRoot, Branch),
              mkdir(DocsDest),
              ?LOG("Generating erldocs for ~s into ~s\n", [Path,DocsDest]),
              erldocs:main([ Path
                           , "-o", DocsDest
                             |  [ "-I"++filename:join(Path, "include")]
                             ++ [ "-I"++filename:join(Path, Inc)
                                  || Inc <- filelib:wildcard(Path++"/deps/*/include")]
                           ]),
              other_utils:rmrf(filename:dirname(Path))
      end, Clones).

duplicate_repo (git, RepoName, Branches, DestDir) ->
    lists:map(
      fun ({Commit, Title}) ->
              Name = make_name(RepoName, Commit, Title),
              TitledRepo = filename:join([DestDir, Name, RepoName]),
              mkdir(filename:join(DestDir, Name)),
              %% cd DestDir && cp -pr RepoName TitledRepo
              other_utils:cp(DestDir, RepoName, TitledRepo),
              other_utils:git_changeto(TitledRepo, Commit),
              {Title, TitledRepo}
      end, Branches).

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
