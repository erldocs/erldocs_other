%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_meta).

%% eo_meta: utilities to access and manipulate meta.txt data.

-export([fold/3]).
-export([ load/1
	, iter/2

        , get/2
        , target_path/1
        , name/1
        , url/1
        , method/1
        , revisions/1
        , vsn_format/1
        , vsn_pass/1
        , time_begin/1
        , time_end/1
        , count_tags/1
        , count_banches/1
        , kvs/1
        , uuid/1
	]).

-export_type([ t/0
             , osite/0
             , continuation/0
             ]).

-type t() :: [{atom(),_}, ...].
-type osite() :: file:name_all().
-type continuation() :: pos_integer() | '$end'.

-type api(A) :: A | undefined.

%% API

-spec fold (osite(), fun((_, B) -> B), B) -> B.
fold (SiteDirectory, Fun, Acc0)
  when is_function(Fun, 2) ->
    F = fun (MetaFile, Acc) ->
                io:format(standard_error, " -> ~p\n", [MetaFile]),
                {ok, Meta} = file:consult(MetaFile),
                Fun(Meta, Acc)
        end,
    filelib:fold_files(SiteDirectory, "meta\\.txt", true, F, Acc0).

-spec load (osite()) -> pos_integer().
load (SiteDirectory) ->
    F = fun (Meta, Count) ->
                TargetPath = target_path(Meta),
                put(TargetPath, Meta),
                NewId = Count + 1,
                put(NewId, TargetPath),
                NewId
        end,
    Counted = fold(SiteDirectory, F, 0),
    put('$count', Counted),
    Counted.

-spec iter (fun((t()) -> A), continuation()) -> {A, continuation()}.
iter (Fun, Continuation)
  when is_function(Fun, 1) ->
    case get('$count') of
        N when N >= Continuation ->
            '$end';
        N ->
            {Fun(get(get(N))), N +1}
    end.


-spec get(atom(), t()) -> api(_).
get (Key, Meta) ->
    case lists:keyfind(Key, 1, Meta) of
        {Key, Value} -> Value;
        false -> undefined
    end.

-spec target_path(t()) -> api(nonempty_string()).
target_path (Meta) -> get(target_path, Meta).
-spec name(t()) -> api(nonempty_string()).
name (Meta) -> get(name, Meta).
-spec url(t()) -> api(eo_scm:repo_url()).
url (Meta) -> get(url, Meta).
-spec method(t()) -> api(eo_scm:method()).
method (Meta) -> get(method, Meta).
-spec revisions(t()) -> api([eo_core:rev()]).
revisions (Meta) -> get(revisions, Meta).
-spec vsn_format(t()) -> api(pos_integer()).
vsn_format (Meta) -> get(vsn_format, Meta).
-spec vsn_pass(t()) -> api(non_neg_integer()).
vsn_pass (Meta) -> get(vsn_pass, Meta).
-spec time_begin(t()) -> api(calendar:datetime()).
time_begin (Meta) -> get(time_begin, Meta).
-spec time_end(t()) -> api(calendar:datetime()).
time_end (Meta) -> get(time_end, Meta).
-spec count_tags(t()) -> api(non_neg_integer()).
count_tags (Meta) -> get(count_tags, Meta).
-spec count_banches(t()) -> api(non_neg_integer()).
count_banches (Meta) -> get(count_banches, Meta).
-spec kvs(t()) -> api(t()).
kvs (Meta) -> get(kvs, Meta).
-spec uuid(t()) -> api(eo_scm:uuid()).
uuid (Meta) -> get(uuid, Meta).

%% Internals

%% End of Module.
