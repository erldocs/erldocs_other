%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(other_core).

%% other_core: main logic of the “other” module.

-export([ main/2 ]).

-define(LOG(Str, Args), io:format(Str, Args)).
-define(LOG(Str),       io:format(Str)).


%% API

-spec main (string(), file:name()) -> term().
main (URL0, Destination0) ->
    {Method, Url} = method(string:to_lower(URL0)),
    Dest = filename:absname(Destination0),
    ok = filelib:ensure_dir(Dest),

    TmpDir = filename:join(Dest, repo_name(Url)),
    ok = filelib:ensure_dir(TmpDir),

    ?LOG("Cloning repo ~p into ~p\n", [Url,TmpDir]),
    ok = clone_repo(Method, Url, TmpDir),

    ?LOG("Extracting meta information\n"),
    Meta = extract_info(Method, Url, TmpDir),

    MetaFile = filename:join(Dest, "meta"),
    ?LOG("Writing meta to ~p\n", [MetaFile]),
    to_file(MetaFile, Meta).

%% Internals

repo_name (Url) ->
    lists:last(string:tokens(Url, "/")).

repo_local_path (Url) ->
    Exploded = string:tokens(Url, "/"),
    filename:join(tl(Exploded)).

extract_info (git, Url, TmpDir) ->
    Branches = other_utils:git_branches(TmpDir),
    Tags = other_utils:git_tags(TmpDir),
    [ {name, repo_name(Url)}
    , {local_path, repo_local_path(Url)}
    , {branches, Branches}
    , {tags, Tags}
    , {versions, length(Branches) + length(Tags)}
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
