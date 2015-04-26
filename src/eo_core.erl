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
    inets:start(),
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
    {true,Url} = eo_scm:url(kf(Conf, url)),
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
    {ok, OldMeta, Revs, Meta} =
        extract_info(kf(Conf,update_only), Method, Url, TimeBegin),
    ?MILESTONE("Writing meta to ~p", [MetaFile]),
    to_file(MetaFile, Meta),

    {ok, ToDo, Skippable} = select_titles(OldMeta, Revs),
    TBs = [do(TB, Method, Url, RepoName, Conf, DocsRoot, Dest)
           || TB <- ToDo] ++ Skippable,
    ?MILESTONE("Finishing up"),
    MetaRest = [ {revisions, TBs}
               , {time_end, utc()}
               ],
    to_file(MetaFile, MetaRest, [append]),
    NewMeta = Meta ++ MetaRest,
    ?u:rm_r(TmpDir),
    put_repo_index(Conf, DocsRoot, NewMeta),
    stop_output_redirection(),
    {ok, NewMeta, MetaFile}.

do (Rev, Method, Url, RepoName, Conf, DocsRoot, Dest) ->
    ?MILESTONE("Processing ~s\t~1000p", [Url,Rev]),

    ?MILESTONE("Fetching repo code"),
    TitledPath = copy_repo(Rev, Method, Url, RepoName, Dest),

    ?MILESTONE("Getting dependencies"),
    Deps = get_deps(TitledPath),

    %%FIXME `make` cloned repo (using shell's redirection & sandbox)

    %%FIXME think about rmrf TitlePath/.git/, deps/*/.git/ & submodules'.
    Builds = erldocs(Conf, DocsRoot, Rev, TitledPath),
    %%del_deps(TitledPath),

    ?MILESTONE("Discovering other repos"),
    Discovered = repo_discovery(TitledPath),

    ?u:rm_r(filename:dirname(TitledPath)),  %% rm titled repo
    Rev#rev{ discovered = Discovered
           , deps = Deps
           , builds = Builds
           }.

%% Internals

html_index (DocsRoot, Meta) ->
    Revs = kf(Meta, revisions),
    IsTag = fun (#rev{type = tag}) -> true; (_branch) -> false end,
    {Tags, Branches} = lists:partition(IsTag, Revs),
    "<h3 id=\"tags\">Tags</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Tags) ++ "</p>"
        ++ "<br/>"
        ++ "\n\t<h3 id=\"branches\">Branches</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot,Branches) ++ "</p>".

list_titles (DocsRoot, Revs) ->
    Items = [ case Builds of
                  true ->
                      "<a href=\""++Title++"\">"++Title++"</a>";
                  false ->
                      Doc = filename:join(DocsRoot, Title),
                      filelib:is_dir(Doc) andalso ?u:rm_r(Doc),
                      Title
              end || #rev{ id = Title
                         , builds = Builds
                         } <- Revs ],
    case Items of
        [] -> "(none)";
        _  -> string:join(Items, " &nbsp; ")
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

repo_discovery (RepoPath) ->
    FilesFound = filelib:wildcard("rebar.config*", RepoPath)
        ++ [ File || File <- [ "Makefile"
                             , ".gitmodules" ],
                     path_exists([RepoPath,File]) ],
    UrlsFound = search_files(RepoPath, FilesFound),
    lists:usort(lists:filtermap(fun eo_scm:url/1, UrlsFound)).

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
        true  -> RebarDeps = ?u:rebar_get_deps(Path);
        false -> RebarDeps = []
    end,
    case path_exists([Path, ".gitmodules"]) of
        true  -> SubModDeps = ?u:git_get_submodules(Path);
        false -> SubModDeps = []
    end,
    RebarDeps ++ SubModDeps.

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
    ToMove = list_abs(DocsRoot, "*"),
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

extract_info (UpdateOnly, Method, Url, TimeBegin) ->
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
    TagsCount = count(tag, TBs),
    BranchesCount = count(branch, TBs),
    ?NOTE("repo", "~p branches, ~p tags", [BranchesCount, TagsCount]),
    TargetPath = eo_scm:repo_local_path(Url),
    OldMeta = consult_meta(UpdateOnly, TargetPath),
    {ok, OldMeta, TBs, [ {name, eo_scm:repo_name(Url)}
                       , {target_path, TargetPath}
                       , {url, Url}
                       , {vsn_format, 2}
                       , {vsn_pass, bump_pass(OldMeta)}
                       , {time_begin, TimeBegin}
                       , {method, Method}
                       , {count_tags, TagsCount}
                       , {count_branches, BranchesCount}
                       ]}.

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

bump_pass (OldMeta) ->
    case lists:keyfind(vsn_pass, 1, OldMeta) of
        false -> 1;
        {vsn_pass,N} -> N + 1
    end.


select_titles (OldMeta, NewRevs) ->
    case lists:keyfind(revisions, 1, OldMeta) of
        false ->            OldRevs = [];
        {revisions,Revs} -> OldRevs = Revs
    end,
    F = fun (NewRev) -> is_skippable(OldRevs, NewRev) end,
    {Skippable, Todo} = lists:partition(F, NewRevs),
    {ok, Todo, Skippable}.

is_skippable ([], _NewRev) -> false;
is_skippable ([Rev|Rest], #rev{ type = Type
                              , id = Id
                              , commit = Commit
                              } = NewRev) ->
    case (Rev#rev.type == Type andalso
          Rev#rev.id == Id andalso
          Rev#rev.commit == Commit)
        orelse Rev#rev.builds == undefined
    of
        true ->
            ?MILESTONE("Skipping ~s ~p", [NewRev#rev.type, NewRev#rev.id]),
            true;
        false -> is_skippable(Rest, NewRev)
    end.

consult_meta (false, _TargetPath) -> [];
consult_meta (true, TargetPath) ->
    Url = "http://other.erldocs.com/"++ TargetPath ++"/meta.txt",
    case httpc:request(Url) of
        {ok, {_,_,Body}} ->
            {ok, Tokens, _} = erl_scan:string(Body),
            Forms = split_after_dot(Tokens, [], []),
            [ begin
                  {ok, Term} = erl_parse:parse_term(Form),
                  Term
              end || Form <- Forms ];
        _ -> []
    end.

split_after_dot ([], _Acc, Forms) -> Forms;
split_after_dot ([Token={dot,_}|Rest], Acc, Forms) ->
    Form = lists:reverse([Token|Acc]),
    split_after_dot(Rest, [], [Form|Forms]);
split_after_dot ([Token|Rest], Acc, Forms) ->
    split_after_dot(Rest, [Token|Acc], Forms).


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
