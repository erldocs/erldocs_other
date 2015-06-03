%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_meta).

%% eo_meta: 

-export([fold/3]).
-export([ load/1
	, iter/2
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
                {target_path, TP} = lists:keyfind(target_path, Meta),
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

%% Internals

%% End of Module.
