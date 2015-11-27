%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_core).

%% eo_core: main logic of the erldocs_other module.

-include("erldocs_other.hrl").
-include("logging.hrl").

-export([ main/1
        , gen/1

        , to_file/2

        , remote_path_blacklist/0
        , local_path_blacklist/0
        ]).

%% title/0 represents the name of either a branch or a tag
-type title() :: string().
-type rev() :: #rev{}.

-export_type([ title/0
             , rev/0
             ]).

-define(FILE_BLACKLIST, "blacklist.txt").
-define(FILE_LOG, "_.txt").
-define(FILE_META, "meta.txt").
-define(REMOTE_URL(TargetPath), "http://other.erldocs.com/" ++ TargetPath).
-define(DOCS_ROOT, "repo").

%% API

%% @doc On error puts log and meta then throws.
%%   Topmost function.

gen (Conf) ->
    Odir    = kf(Conf, website_dir),
    eo_util:mkdir(Odir),
    Tmp     = kf(Conf, dest),
    eo_util:mkdir(Tmp),
    Logfile = filename:join(Tmp, ?FILE_LOG),
    case main([ {dest, Tmp}
              , {logfile, Logfile}
              ] ++ Conf)
    of
        {ok, Meta, _MetaFile} ->
            Url        = eo_meta:url(Meta),
            TargetPath = eo_meta:target_path(Meta),
            Revs       = eo_meta:revisions(Meta);
        _Error ->
            URL0 = kf(Conf, url),
            {true,Url} = eo_scm:url(URL0),
            TargetPath = eo_scm:repo_local_path(Url),
            Revs       = []
    end,
    _ = maybe_blacklist(Odir, Url, Revs),
    Dest = filename:join(Odir, TargetPath),
    eo_util:mkdir(Dest),
    eo_util:mv([Logfile, metafile(Tmp)], Dest),
    _ = replace_dir(Dest, Tmp, Conf, Revs),
    {ok, Url, Dest, ?REMOTE_URL(TargetPath)}.

main (Conf) ->
    try %%FIXME: is there something to catch here?
        main_(Conf)
    catch Type:Error ->
            E = show_error(Type, Error),
            _ = stop_output_redirection(),
            E
    end.

main_ (Conf) ->
    _ = start_output_redirection(kf(Conf, logfile)),
    TimeBegin = utc(),
    {true,Url} = eo_scm:url(kf(Conf, url)),
    Method     = eo_scm:method(Url),
    RepoName   = eo_scm:repo_name(Url),

    Dest     = kf(Conf, dest),
%    eo_util:mkdir(Dest), if nothing there, mkdir; else crash.
    TmpDir   = filename:join(Dest, RepoName),
    eo_util:mkdir(TmpDir),
    DocsRoot = filename:join(Dest, ?DOCS_ROOT),
    eo_util:mkdir(DocsRoot),
    MetaFile = metafile(Dest),

    ?MILESTONE("Extracting meta information"),
    {ok, OldMeta, Revs, Meta} =
        extract_info(kf(Conf,update_only), Method, Url, TimeBegin),
    ?MILESTONE("Writing meta to ~p", [MetaFile]),
    to_file(MetaFile, Meta),

    {ok, ToDo, Skippable} = select_titles(OldMeta, Revs),
    TBs = [try do(TB, Method, Url, RepoName, Conf, DocsRoot, Dest) of
               Rev = #rev{} -> Rev
           catch
               Type:Error ->
                   _ = show_error(Type, Error),
                   TB#rev{ builds = undefined }
           end || TB <- ToDo] ++ Skippable,
    ?MILESTONE("Finishing up"),
    MetaRest = [ {revisions, lists:sort(TBs)}
               , {time_end, utc()}
               ],
    to_file(MetaFile, MetaRest, [append]),
    NewMeta = Meta ++ MetaRest,
    eo_util:rm_r(TmpDir),
    _ = put_repo_index(Conf, DocsRoot, NewMeta),
    _ = stop_output_redirection(),
    {ok, NewMeta, MetaFile}.

do (Rev, Method, Url, RepoName, Conf, DocsRoot, Dest) ->
    ?MILESTONE("Processing ~s\t~1000p", [Url,Rev]),

    ?MILESTONE("Fetching repo code"),
    {ok, TitledPath} = copy_repo(Method, Url, RepoName, Dest, Rev),

    ?MILESTONE("Preliminary analysis"),
    ShouldBuild = is_repo_containing_erlang_code(TitledPath),
    ?NOTE("analysis", "is_repo_containing_erlang_code: ~s", [ShouldBuild]),

    case ShouldBuild of
        true ->
          ?MILESTONE("Getting dependencies"),
          Deps = get_deps(TitledPath),

          %%FIXME `make` cloned repo (using shell's redirection & sandbox)

          %%FIXME think about rmrf TitlePath/.git/, deps/*/.git/ & submodules'.
          Builds = erldocs(Conf, DocsRoot, Rev, TitledPath),
          %%del_deps(TitledPath),

          ?MILESTONE("Discovering other repos"),
          Discovered = repo_discovery(TitledPath);

        false ->
            Discovered = [],
            Deps = [],
            Builds = false
    end,

    ?NOTE("erldocs_build", "succeeded: ~s", [Builds]),
    eo_util:rm_r(filename:dirname(TitledPath)),
    Rev#rev{ discovered = Discovered
           , deps = Deps
           , builds = Builds
           , kvs = ks(has_erlang_code, ShouldBuild, Rev#rev.kvs)
           }.

%% Internals

show_error (Type, Error) ->
    E = [?MODULE, erlang:get_stacktrace(), {Type,Error}],
    ?MILESTONE("Error running ~p:\n\t~p\n~p", E),
    E.

html_index (DocsRoot, Revs) ->
    {Tags, Branches} = lists:partition(fun is_tag/1, Revs),
    "<h3 id=\"tags\">Tags</h3>"
        ++ "\n\t" ++ maybe_list_semver(DocsRoot, Tags)
        ++ "<br/>"
        ++ "\n\t<h3 id=\"branches\">Branches</h3>"
        ++ "\n\t<p>" ++ list_titles(DocsRoot, Branches) ++ "</p>".

is_tag (#rev{type = tag}) -> true;
is_tag (#rev{}) -> false.

list_titles (DocsRoot, Revs) ->
    case [list_rev(DocsRoot, Rev) || Rev <- Revs] of
        [] -> "(none)";
        Items -> table_titles(Items)
    end.

%%FIXME: escape htmlentities (and security in path names?)
list_rev (_Dir, #rev{ id = Id, builds = true }) ->
    "<a href=\"" ++ Id ++ "\">" ++ Id ++ "</a>";
list_rev (Dir, #rev{ id = Id }) ->
    %% builds = false | undefined
    Doc = filename:join(Dir, Id),
    filelib:is_dir(Doc) andalso eo_util:rm_r(Doc),
    Id.

maybe_list_semver (Dir, Tags) ->
    IsSemVer = fun (#rev{id = Id}) -> eo_vsn:is_vsn(Id) end,
    case lists:any(IsSemVer, Tags) of
        true  -> "<br/>" ++ list_semver(Dir, Tags);
        false -> "<p>" ++ list_titles(Dir, Tags) ++ "</p>"
    end.

get_semver (#rev{id = Id} = Rev) ->
    case eo_vsn:get_vsn(Id) of
        {true, SemVer} -> {true, {SemVer, Rev}};
        false -> false
    end.

list_semver (Dir, Tags) ->
    IsOther   = fun (#rev{id = Id}) -> not eo_vsn:is_vsn(Id) end,
    CmpOthers = fun (#rev{id = LId}, #rev{id = RId}) -> LId =< RId end,
    CmpSemVers =
        fun ({LSemVer,_LRev}, {RSemVer,_RRev}) ->
                eo_vsn:'=<'(LSemVer, RSemVer)
        end,
    case lists:sort(CmpOthers, lists:filter(IsOther, Tags)) of
        [] ->
            Others = [],
            OthersTop = 0;
        Others0 ->
            Others = [["(other)"] ++ Others0],
            OthersTop = length(Others0)
    end,
    SemVers = lists:sort(CmpSemVers, lists:filtermap(fun get_semver/1, Tags)),
    table_tags(Dir, Others ++ group_by_major(SemVers, OthersTop)).

group_by_major ([{SemVer,Rev}|TaggedSemVers], OthersTop) ->
    group_by_major({hd(SemVer),[Rev]}, TaggedSemVers, [], 1, OthersTop).
group_by_major ({Major,Revs}, [{[Major|_],Rev}|TaggedSemVers], Acc, Current, Top) ->
    group_by_major({Major,[Rev|Revs]}, TaggedSemVers, Acc, Current+1, Top);
group_by_major (Above, [{[NewMajor|_],Rev}|TaggedSemVers], Acc, Current, Top) ->
    case {Current, Top} of
        {Bigger, ThanThis} when Bigger >= ThanThis ->
            group_by_major({NewMajor,[Rev]}, TaggedSemVers, [{Above,Current}|Acc], 1, Bigger);
        {_Smaller, ThanThis} ->
            group_by_major({NewMajor,[Rev]}, TaggedSemVers, [{Above,Current}|Acc], 1, ThanThis)
    end;
group_by_major (Above, [], Acc, Current, Top) ->
    Max = max(Current, Top),
    lists:foldl( fun ({{Header,Lines},NLines}, Columns) ->
                         Blanks = lists:duplicate(Max - NLines, '@'),
                         [ [Header] ++ Lines ++ Blanks | Columns]
                 end , [], [{Above,Current}|Acc] ).

table_tags (Dir, Columns) ->
    [Headers0|Body0] = transpose(Columns),
    Headers = [ case Header of
                    Major when is_integer(Major) ->
                        "v" ++ integer_to_list(Major);
                    Text -> Text
                end || Header <- Headers0],
    Body = [[case Rev of
                 '@' -> "";
                 _ -> list_rev(Dir, Rev)
             end || Rev <- Revs]
            || Revs <- Body0],
    "<table>\n"
        "<thead>\n" ++ html_row(Headers) ++ "</thead>\n"
        "<tbody>\n" ++ lists:flatmap(fun html_row/1, Body) ++ "</tbody>\n"
    "</table>\n".

html_row (TDs) ->
    "<tr>\n"
        ++ lists:flatmap(fun (TD) -> "<td>" ++ TD ++ "</td>\n" end, TDs) ++
    "</tr>\n".

table_titles (Items) ->
    C = trunc(math:sqrt(length(Items))),
    Cols = split_n_times(C, Items, []),
    Body = [[case Rev of
                 '@' -> "";
                 _ -> Rev
             end || Rev <- Revs]
            || Revs <- Cols],
    "<table>\n"
        "<tbody>\n" ++ lists:flatmap(fun html_row/1, Body) ++ "</tbody>\n"
    "</table>\n".

split_n_times (_, [], Acc) -> Acc;
split_n_times (N, List, Acc)
  when length(List) >= N ->
    {Top0,Rest} = lists:split(N, List),
    Top = case length(Top0) of
              N -> Top0;
              S when S < N -> Top0 ++ lists:duplicate(N - S, '@')
          end,
    split_n_times(N, Rest, [Top|Acc]).

%% http://erlang.org/pipermail/erlang-questions/2012-October/069856.html
transpose ([[X|Xs] | Xss]) ->
    [[X | [H || [H|_] <- Xss]]
     | transpose([Xs | [T || [_|T] <- Xss]])];
transpose ([[]|Xss]) -> transpose(Xss);
transpose ([]) -> [].

put_repo_index (Conf, DocsRoot, Meta) ->
    Args = [ {title,   eo_meta:target_path(Meta)}
           , {url,     eo_meta:url(Meta)}
           , {content, html_index(DocsRoot, eo_meta:revisions(Meta))}
           , {base,    kf(Conf, base)}
           , {ga,      kf(Conf, ga)} ],
    {ok, HTML} = html_dtl:render(Args),
    ok = file:write_file(filename:join(DocsRoot,"index.html"), HTML),
    {ok, CSS}  = css_dtl:render([]),
    ok = file:write_file(filename:join(DocsRoot,"repo.css"), CSS).

repo_discovery (RepoPath) ->
    FilesFound =
        filelib:wildcard("rebar.config*", RepoPath) ++
        filelib:wildcard("deps/*/rebar.config*", RepoPath) ++
        [ File || File <- [ "Makefile"
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
                   ++ discover_surr("\\s\"", "@", Contents);
                  _ ->  %% "rebar.config"++_ ->%%FIXME maybe
                      discover_urls("\\s\"",      Contents)
                   ++ discover_surr("\\s\"", "@", Contents)
              end
      end, Files).

discover_urls (Seps, Bin) ->
    discover_surr(Seps, "://", Bin).
discover_surr (Seps, Mid, Bin) ->
    RegExp = [ "[",Seps,"]([^", Seps, "]+", Mid, "[^", Seps, "]+)[",Seps,"]" ],
    case re:run(Bin, lists:flatten(RegExp),
                [{capture,all_but_first,list}, global]) of
        {match, Urls} -> lists:append(Urls);
        nomatch -> []
    end.

erldocs (Conf, DocsRoot, #rev{id=Branch}, Path) ->
    DocsDest = filename:join(DocsRoot, Branch),
    ?MILESTONE("Generating erldocs into ~s", [DocsDest]),
    eo_util:mkdir(DocsDest),
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

ls_al (Path) ->
    list_abs(Path, "*").

ls_dirs (Path) ->
    [filename:basename(Dir) || Dir <- ls_al(Path), filelib:is_dir(Dir)].

ks (Key, Value, Kvs) ->
    case lists:keyfind(Key, 1, Kvs) of
        false -> [{Key,Value} | Kvs];
        _ ->
            lists:keyreplace(Key, 1, Kvs, {Key,Value})
    end.

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

copy_repo (Method, Url, RepoName, DestDir, #rev{ id = Branch
                                               , type = RevType
                                               } = Rev) ->
    Name = make_name(RepoName, Branch, RevType),
    TitledPath = filename:join([DestDir, Name]),
    eo_util:mkdir(TitledPath),
    eo_scm:fetch(TitledPath, {Method,Url,Rev}).

is_repo_containing_erlang_code (#rev{kvs = Kvs}) ->
    kf(Kvs, has_erlang_code);
is_repo_containing_erlang_code (Path) ->
    ExitFast = fun (_Fn, _Acc) -> throw(at_least_one) end,
    try filelib:fold_files(Path, "\\.[ehxy]rl$", true, ExitFast, none) of
        none -> false
    catch
        at_least_one -> true;
        E:R ->
            _ = show_error(E, R),
            eo_default:has_erlang_code()
    end.

get_deps (Path) ->
    case path_exists([Path, "rebar.config"]) of
        true  -> RebarDeps = eo_util:rebar_get_deps(Path);
        false -> RebarDeps = []
    end,
    case path_exists([Path, ".gitmodules"]) of
        true  -> SubModDeps = eo_util:git_get_submodules(Path);
        false -> SubModDeps = []
    end,
    RebarDeps ++ SubModDeps.

del_deps (Path) ->
    case path_exists([Path, "rebar.config"]) of
        true  -> eo_util:rebar_delete_deps(Path);
        false -> ok
    end,
    case path_exists([Path, ".gitmodules"]) of
        true  -> eo_util:delete_submodules(Path);
        false -> ok
    end,
    eo_util:rmrf(filename:join(Path, "deps")).

path_exists (PathToJoin) ->
    Path = filename:join(PathToJoin),
    filelib:is_file(Path).

rmdir (Dir) ->
    case file:del_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        Error -> Error
    end.


maybe_blacklist (_, _, []) -> false;
maybe_blacklist (Odir, Url, Revs) ->
    not lists:any(fun is_repo_containing_erlang_code/1, Revs)
        andalso blacklist_repo(Odir, Url).
blacklist_repo (Odir, Url) ->
    ?MILESTONE("Blacklisting ~p", [Url]),
    file:write_file(filename:join(Odir,?FILE_BLACKLIST), Url, [append]).

replace_dir (Dest, Tmp, Conf, Revs) ->
    DocsRoot = filename:join(Tmp, ?DOCS_ROOT),
    kf(Conf,base) =/= eo_default:base() andalso
        eo_util:find_delete(DocsRoot, [ "repo.css",  "erldocs.css"
                                      , "jquery.js", "erldocs.js"
                                      , ".xml" ]),
    rm_unskipped_and_deleted(Revs, Dest, DocsRoot),
    eo_util:mv(ls_al(DocsRoot), Dest),
    _ = rmdir(DocsRoot),
    rmdir(Tmp).

%% @doc Remove from `Dest` titles that were not skipped
%%   just before, and titles that do not reside in the repo anymore
rm_unskipped_and_deleted (Revs, Dest, DocsRoot) ->
    ODirs = ls_dirs(Dest),
    Unskipped = ls_dirs(DocsRoot),
    ToKeep = [Rev#rev.id || #rev{builds = true}=Rev <- Revs],
    ToRm = (ODirs -- ToKeep) ++ Unskipped,
    eo_util:rm_r(Dest, ToRm).


make_name (RepoName, Title, tag) ->
    make_name(RepoName, Title, "tag");
make_name (RepoName, Title, branch) ->
    make_name(RepoName, Title, "branch");
make_name (RepoName, Branch, RevKind) ->
    [ case C of
         $/ -> $_;
         _ -> C
      end || C <- string:join([RepoName,RevKind,Branch], "-") ].

metafile (Dest) ->
    filename:join(Dest, ?FILE_META).

extract_info (UpdateOnly, Method, Url, TimeBegin) ->
    case eo_scm:refs({Method, Url, '_'}) of
        {ok, TBs} -> TBs;
        error ->
            %%FIXME try another SCM?
            case eo_util:hg_test(Url) of
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
                       , {uuid, eo_scm:uuid(Url)}
                       , {vsn_format, 4}
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
    Vsn = case eo_meta:vsn_pass(OldMeta) of
              undefined -> 0;
              N -> N
          end,
    ?NOTE("vsn", "Bumping from ~p", [Vsn]),
    Vsn + 1.


select_titles (OldMeta, NewRevs) ->
    case eo_meta:revisions(OldMeta) of
        undefined -> OldRevs = [];
        Revs ->      OldRevs = Revs
    end,
    F = fun (NewRev) -> is_skippable(OldRevs, NewRev) end,
    {Skippable, Todo} = partition_map(F, NewRevs),
    %% Note: deleted revs are lost
    {ok, Todo, Skippable}.

is_skippable ([], _NewRev) -> false;
is_skippable ([#rev{ type = Type, id = Id} = OldRev | _Rest]
             , #rev{ type = Type, id = Id} = NewRev) ->
    case OldRev#rev.commit =/= NewRev#rev.commit
        orelse OldRev#rev.builds == undefined
    of
        true -> false;
        false ->
            ?MILESTONE("Skipping ~s ~p", [NewRev#rev.type, NewRev#rev.id]),
            {true, OldRev}
    end;
is_skippable ([_OldRev|Rest], NewRev) ->
    is_skippable(Rest, NewRev).

-spec partition_map (fun((A) -> boolean()|{true,B}), [A]) -> {[A|B], [A]}.
partition_map (Fun, List) ->
    lists:foldl( fun (Elt, {Satisfying,NotSatisfying}) ->
                         case Fun(Elt) of
                             %% true ->    {[Elt|Satisfying], NotSatisfying};
                             false ->      {Satisfying,  [Elt|NotSatisfying]};
                             {true,Val} -> {[Val|Satisfying], NotSatisfying}
                         end
                 end
               , {[],[]}, List).

consult_meta (false, _TargetPath) -> [];
consult_meta (true, TargetPath) ->
    case httpc:request(remote_path_meta(TargetPath)) of
        {ok, {_,_,Body}} ->
            {ok, Tokens, _} = erl_scan:string(Body),
            Forms = split_after_dot(Tokens, [], []),
            Terms = [ begin
                          {ok, Term} = erl_parse:parse_term(Form),
                          Term
                      end || Form <- Forms ],
            case eo_meta:vsn_format(Terms) of
                undefined -> [];
                2 -> bump_record_format(Terms);
                N when is_integer(N), N > 1 -> Terms
            end;
        _ -> []
    end.

remote_path_meta (TargetPath) ->
    %% "http://other.erldocs.com/"
    "https://raw.githubusercontent.com/erldocs/other.erldocs.com/gh-pages/"
        ++ TargetPath ++ "/" ?FILE_META.

remote_path_blacklist () ->
    "https://raw.githubusercontent.com/erldocs/other.erldocs.com/gh-pages/"
        ?FILE_BLACKLIST.

local_path_blacklist () ->
    ?FILE_BLACKLIST.

split_after_dot ([], _Acc, Forms) -> Forms;
split_after_dot ([Token={dot,_}|Rest], Acc, Forms) ->
    Form = lists:reverse([Token|Acc]),
    split_after_dot(Rest, [], [Form|Forms]);
split_after_dot ([Token|Rest], Acc, Forms) ->
    split_after_dot(Rest, [Token|Acc], Forms).

bump_record_format ([]) -> [];
bump_record_format ([{revisions,Revs}|Terms]) ->
    NewRevs = [#rev{ type = element(#rev.type, Rev)
                   , id = element(#rev.id, Rev)
                   , commit = element(#rev.commit, Rev)
                   , builds = element(#rev.builds, Rev)
                   , deps = element(#rev.deps, Rev)
                   , discovered = element(#rev.discovered, Rev)
                   } || Rev <- Revs],
    [{revisions,NewRevs} | Terms];
bump_record_format ([Term|Terms]) ->
    [Term | bump_record_format(Terms)].


to_file (Path, Data) ->
    to_file (Path, Data, []).
to_file (Path, Data, Options) ->
    Str = [io_lib:fwrite("~p.\n",[Datum]) || Datum <- Data],
    ok = file:write_file(Path, Str, Options).

start_output_redirection (standard_io) -> ok;
start_output_redirection (LogFile) ->
    ?DBG("logging to ~s", [LogFile]),
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
