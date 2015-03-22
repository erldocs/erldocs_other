%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_core).

%% eo_core: main logic of the erldocs_other module.

-export([ main/1
        , to_file/2
        , gen/1 ]).

-include("erldocs_other.hrl").
-include("logging.hrl").

-define(u, eo_util).

%% API

%% @doc On error puts log and meta then throws.
%%   Topmost function.

gen (Conf) ->
    Odir    = kf(Conf, website_dir),
    mkdir(Odir),
    Tmp     = kf(Conf, dest),
    mkdir(Tmp),
    Logfile = filename:join(Tmp, "_.txt"),
    case main([ {dest, Tmp}
              , {logfile, Logfile} ] ++ Conf) of
        {ok, Meta, _MetaFile} ->
            Url        = kf(Meta, url),
            TargetPath = kf(Meta, target_path);
        _Error ->
            URL0 = kf(Conf, url),
            {true,Url} = eo_scm:url(URL0),
            TargetPath = eo_scm:repo_local_path(Url)
    end,
    Dest = filename:join(Odir, TargetPath),
    mkdir(Dest),
    ?u:mv([Logfile,metafile(Tmp)], Dest),
    replace_dir(Dest, Tmp, Conf),
    {ok, Url, Dest, "http://other.erldocs.com/"++TargetPath}.

main (Conf) ->
    try
        main_(Conf)
    catch Type:Error ->
            E = [?MODULE, erlang:get_stacktrace(), {Type,Error}],
            ?MILESTONE("Error running ~p:\n\t~p\n~p", E),
            stop_output_redirection(),
            E
    end.

main_ (Conf) ->
    start_output_redirection(kf(Conf, logfile)),
    TimeBegin = utc(),
    URL0 = kf(Conf, url),
    {true,Url} = eo_scm:url(URL0),
    Method     = eo_scm:method(Url),
    RepoName   = eo_scm:repo_name(Url),

    Dest     = kf(Conf, dest),
%    mkdir(Dest), if nothing there, mkdir; else crash.
    TmpDir   = filename:join(Dest, RepoName),
    mkdir(TmpDir),
    DocsRoot = filename:join(Dest, "repo"),
    mkdir(DocsRoot),
    MetaFile = metafile(Dest),

    ?MILESTONE("Extracting meta information"),
    Meta = extract_info(Method, Url, TimeBegin),
    ?MILESTONE("Writing meta to ~p", [MetaFile]),
    to_file(MetaFile, Meta),

    TBs = kf(Meta, revisions),
    %% case TBs of
    %%     [] -> throw({?MODULE, repo_down_or_empty});
    %%     _ ->
            main_(TBs, Method, Url, RepoName, TmpDir,
                  Conf, Meta, MetaFile, DocsRoot, Dest, [])
    .%% end.

main_ ([TB|TBs], Method, Url, RepoName, TmpDir,
       Conf, Meta, MetaFile, DocsRoot, Dest, Discovered0) ->
    ?MILESTONE("Processing\trepo:~s\trev:~p", [RepoName,TB]),

    ?MILESTONE("Fetching repo code"),
    TitledPath = copy_repo(TB, Method, Url, RepoName, Dest),

    ?MILESTONE("Getting dependencies"),
    Deps = get_deps(TitledPath), %% FIXME: store deps per TB

    %%FIXME `make` cloned repo (using shell's redirection & sandbox)

    %%FIXME think about rmrf TitlePath/.git/, deps/*/.git/ & submodules'.
    erldocs(Conf, DocsRoot, TB, TitledPath),

    ?MILESTONE("Discovering other repos"),
    %%del_deps(TitledPath),
    case repo_discovery(TB, TitledPath) of
        {_, []}  -> Discovered = Discovered0;
        Treasure -> Discovered = [Treasure|Discovered0]
    end,

    ?u:rm_r(filename:dirname(TitledPath)),  %% rm titled repo
    main_(TBs, Method, Url, RepoName, TmpDir,
          Conf, Meta, MetaFile, DocsRoot, Dest, Discovered);

main_ ([], _Method, _Url, _RepoName, TmpDir,
       Conf, Meta, MetaFile, DocsRoot, _Dest, Treasures) ->
    ?MILESTONE("Erldocs finishing up"),
    MetaRest = [ {discovered, Treasures}
               , {time_end, utc()} ],
    to_file(MetaFile, MetaRest, [append]),
    ?u:rm_r(TmpDir),
    put_repo_index(Conf, DocsRoot, Meta),
    stop_output_redirection(),
    {ok, Meta, MetaFile}.

%% Internals

html_index (DocsRoot, Meta) ->
    Revs = kf(Meta, revisions),
    Tags     = [Rev || Rev <- Revs, Rev#rev.type == tag   ],
    Branches = [Rev || Rev <- Revs, Rev#rev.type == branch],
    "<h3 id=\"tags\">Tags</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Tags) ++ "</p>"
        ++ "<br/>"
        ++ "\n\t<h3 id=\"branches\">Branches</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Branches) ++ "</p>".

list_titles (DocsRoot, Revs) ->
    Items = [ case path_exists([DocsRoot, Title, "index.html"]) of
                  true  ->
                      "<a href=\""++Title++"\">"++Title++"</a>";
                  false ->
                      ?u:rm_r(filename:join(DocsRoot, Title)),
                      Title
              end || #rev{id=Title} <- Revs ],
    case Items of
        [] -> "(none)";
        _  ->
            Spaces = " &nbsp; ",
            string:join(Items, Spaces)
    end.

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

repo_discovery (Rev, RepoPath) ->
    FilesFound = filelib:wildcard("rebar.config*", RepoPath)
        ++ [ File || File <- [ "Makefile"
                             , ".gitmodules" ],
                     path_exists([RepoPath,File]) ],
    UrlsFound = search_files(RepoPath, FilesFound),
    {Rev, lists:usort(lists:filtermap(fun eo_scm:url/1, UrlsFound))}.

search_files (RepoPath, Files) ->
    lists:flatmap(
      fun (File) ->
              FilePath = filename:join(RepoPath, File),
              {ok, Contents} = file:read_file(FilePath),
              case File of
                  "Makefile" ->
                      discover_urls("\\s\"'();,", Contents);
                  ".gitmodules" ->
                      discover_urls("\\s=",       Contents)
                   ++ discover_urls("\\s\"", "@", Contents);
                  "rebar.config"++_ ->
                      discover_urls("\\s\"",      Contents)
                   ++ discover_urls("\\s\"", "@", Contents)
              end
      end, Files).

discover_urls (Seps, Bin) ->
    discover_urls (Seps, "://", Bin).
discover_urls (Seps, Mid, Bin) ->
    RegExp = [ "[",Seps,"]([^", Seps, "]+", Mid, "[^", Seps, "]+)[",Seps,"]" ],
    case re:run(Bin, lists:flatten(RegExp),
                [{capture,all_but_first,list}, global]) of
        {match, Urls} -> lists:append(Urls);
        nomatch -> []
    end.

erldocs (Conf, DocsRoot, #rev{id=Branch}, Path) ->
    DocsDest = filename:join(DocsRoot, Branch),
    ?MILESTONE("Generating erldocs into ~s", [DocsDest]),
    mkdir(DocsDest),
    Args = [ Path
           , "-o",     DocsDest
           , "--base", kf(Conf,base)
           , "--ga",   kf(Conf,ga)
           ]
        ++ list_abs(Path, "apps/*")
        ++ list_abs(Path, "applications/*")
        ++ lists:flatmap(fun (Dir) -> ["-I", Dir] end,
                         find_dirs("\\.hrl$", Path)),
    %% FIXME add non-deps containing src/
    %% ++ [ filename:dirname(Dir) || Dir <- find_dirs(".+", Path)
    %%                                   lists:suffix("/src", Dir) ],
    erldocs:main(Args).

find_dirs (FilePattern, Path) ->
    AccDirs = fun (File, Acc) ->
                      [filename:dirname(File)|Acc]
              end,
    Dirs = filelib:fold_files(Path, FilePattern, true, AccDirs, []),
    lists:usort(Dirs).

list_abs (Path, Wildcard) ->
    Pattern = filename:join(Path, Wildcard),
    filelib:wildcard(Pattern).

kf (Conf, Key) ->
    case lists:keyfind(Key, 1, Conf) of
        {Key, Value} -> Value;
        false ->
            %% Fetches latest code version!
            try eo_default:Key() of
                Value -> Value
            catch
                error:undef ->
                    {error,no_default_for,Key,absent_from,Conf}
            end
    end.

copy_repo (Rev = #rev{id=Branch, type=RevType},
           Method, Url, RepoName, DestDir) ->
    Name = make_name(RepoName, Branch, RevType),
    TitledPath = filename:join([DestDir, Name, RepoName]),
    mkdir(TitledPath),
    eo_scm:fetch(TitledPath, {Method,Url,Rev}),
    TitledPath.

get_deps (Path) ->
    case path_exists([Path, "rebar.config"]) of
        true  -> ?u:rebar_get_deps(Path);
        false -> ok
    end,
    case path_exists([Path, ".gitmodules"]) of
        true  -> ?u:git_get_submodules(Path);
        false -> ok
    end.

del_deps (Path) ->
    case path_exists([Path, "rebar.config"]) of
        true  -> ?u:rebar_delete_deps(Path);
        false -> ok
    end,
    case path_exists([Path, ".gitmodules"]) of
        true  -> ?u:delete_submodules(Path);
        false -> ok
    end,
    ?u:rmrf(filename:join(Path, "deps")).

path_exists (PathToJoin) ->
    Path = filename:join(PathToJoin),
    filelib:is_file(Path).

rmdir (Dir) ->
    ok = file:del_dir(Dir).

mkdir (Dir) ->
    ok = filelib:ensure_dir(Dir ++ "/").


replace_dir (Dest, Tmp, Conf) ->
    DocsRoot = filename:join(Tmp, "repo"),
    kf(Conf,base) =/= eo_default:base() andalso
        ?u:find_delete(DocsRoot, [ "repo.css",  "erldocs.css"
                                 , "jquery.js", "erldocs.js"
                                 , ".xml" ]),
    ToMove = filelib:wildcard(filename:join(DocsRoot, "*")),
    rm_dest_docs(Dest, ToMove),
    ?u:mv(ToMove, Dest),
    rmdir(DocsRoot),
    case file:del_dir(Tmp) of
        ok -> ok;
        {error, eexist} -> ok
    end.

rm_dest_docs (Dest, Docs) ->
    ToRm = [DestDoc || Doc <- Docs
                           , filelib:is_dir(Doc)
                           , begin
                                 DestDoc = filename:join(Dest, filename:basename(Doc)),
                                 filelib:is_dir(DestDoc)
                             end],
    %% Paths are absolute: no real need to tmp_cd(Dest).
    ?u:rm_r(ToRm, Dest).


make_name (RepoName, Branch, RevType) ->
    case RevType of
        tag    -> RevKind = "tag";
        branch -> RevKind = "branch"
    end,
    string:join([RepoName,RevKind,Branch], "-").

metafile (Dest) ->
    filename:join(Dest, "meta.txt").

extract_info (Method, Url, TimeBegin) ->
    case eo_scm:refs({Method, Url, '_'}) of
        {ok, TBs} -> TBs;
        error ->
            %%FIXME try another SCM?
            case ?u:hg_test(Url) of
                true  -> ?NOTE("method", "SCM is hg: not yet supported");
                false -> ?NOTE("method", "Repo may as well not exist"), ignore_for_now
            end,
            TBs = []
    end,
    ?NOTE("repo", "~p branches, ~p tags", [count(branch,TBs),count(tag,TBs)]),
    [ {name, eo_scm:repo_name(Url)}
    , {target_path, eo_scm:repo_local_path(Url)}
    , {url, Url}
    , {time_begin, TimeBegin}
    , {method, Method}
    , {revisions, TBs} ].

utc () ->
    calendar:universal_time().

count (Field, Revs) ->
    FieldCounter =
        fun (#rev{ type = Type }, Acc)
              when Type == Field ->
                Acc + 1;
            (_Else, Acc) ->
                Acc
        end,
    lists:foldl(FieldCounter, 0, Revs).


to_file (Path, Data) ->
    to_file (Path, Data, []).
to_file (Path, Data, Options) ->
    Str = [io_lib:fwrite("~p.\n",[Datum]) || Datum <- Data],
    ok = file:write_file(Path, Str, Options).

start_output_redirection (standard_io) -> ok;
start_output_redirection (LogFile) ->
    put(previous_group_leader, group_leader()),
    {ok, Fd} = file:open(LogFile, [append]),
    group_leader(Fd, self()).

stop_output_redirection () ->
    case get(previous_group_leader) of
        undefined -> ok;
        Pid ->
            Fd = group_leader(),
            group_leader(Pid, self()),
            ok = file:close(Fd)
    end.

%% End of Module.
