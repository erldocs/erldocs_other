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
	]).

-export_type([ t/0
             , osite/0
             , continuation/0
             ]).

-type t() :: [{atom(),_}, ...].
-type osite() :: file:name_all().
-type continuation() :: pos_integer() | '$end'.

%% API

-spec fold (osite(), fun((_, B) -> B), B) -> B.
fold (SiteDirectory, Fun, Acc0)
  when is_function(Fun, 2) ->
    F = fun (MetaFile, Acc) ->
                io:format(" -> ~p\n", [MetaFile]),
                {ok, Meta} = file:consult(MetaFile),
                Fun(Meta, Acc)
        end,
    filelib:fold_files(SiteDirectory, "meta\\.txt", true, F, Acc0).

-spec load (osite()) -> pos_integer().
load (SiteDirectory) ->
    F = fun (Meta, Count) ->
                {target_path, TP} = lists:keyfind(target_path, 1, Meta),
                put(TP, Meta),
                NewId = Count + 1,
                put(NewId, TP),
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


-spec get(atom(), t()) -> _.
get (Key, Meta) ->
    {Key, Value} = lists:keyfind(Key, 1, Meta),
    Value.

-spec target_path(t()) -> nonempty_string().
target_path (Meta) -> get(target_path, Meta).
-spec name(t()) -> nonempty_string().
name (Meta) -> get(name, Meta).
-spec url(t()) -> eo_scm:repo_url().
url (Meta) -> get(url, Meta).
-spec method(t()) -> eo_scm:method().
method (Meta) -> get(method, Meta).
-spec revisions(t()) -> [eo_core:rev()].
revisions (Meta) -> get(revisions, Meta).
-spec vsn_format(t()) -> pos_integer().
vsn_format (Meta) -> get(vsn_format, Meta).
-spec vsn_pass(t()) -> non_neg_integer().
vsn_pass (Meta) -> get(vsn_pass, Meta).
-spec time_begin(t()) -> calendar:datetime().
time_begin (Meta) -> get(time_begin, Meta).
-spec time_end(t()) -> calendar:datetime().
time_end (Meta) -> get(time_end, Meta).
-spec count_tags(t()) -> non_neg_integer().
count_tags (Meta) -> get(count_tags, Meta).
-spec count_banches(t()) -> non_neg_integer().
count_banches (Meta) -> get(count_banches, Meta).

%% Internals

%% End of Module.
