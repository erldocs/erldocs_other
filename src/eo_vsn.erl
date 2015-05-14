%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_vsn).

%% eo_vsn: handle VSNs loosely satisfaying SemVer.

-export([ get_vsn/1
        , is_vsn/1
        , '=<'/2
        ]).

-export_type([ vsn/0
             ]).

-type vsn() :: nonempty_list(non_neg_integer()).

%% API

-spec get_vsn (nonempty_string()) -> {true,vsn()} | false.
get_vsn (Str) ->
    case re:run( Str, "^v?(\\d+(?:\\.\\d+)*)(?:[^\\d].*)?$"
               , [{capture,all_but_first,list}] ) of
        {match, [Found]} ->
            VSN = [list_to_integer(N) || N <- string:tokens(Found, ".")],
            {true, VSN};
        nomatch -> false
    end.


-spec is_vsn (nonempty_string()) -> boolean().
is_vsn (Str) ->
    false =/= get_vsn(Str).


-spec '=<' (vsn(), vsn()) -> boolean().
'=<' (VSNl0, VSNr0) ->
    {VSNl, VSNr} =
        case {length(VSNl0), length(VSNr0)} of
            {Same, Same} -> {VSNl0, VSNr0};
            {Big, Small} when Big > Small -> {VSNl0, pad(Big-Small, VSNr0)};
            {Small, Big} -> {pad(Big-Small, VSNl0), VSNr0}
        end,
    cmp(VSNl, VSNr).

%% Internals

cmp ([X|Tl], [X|Tr]) -> cmp(Tl, Tr);
cmp ([L|_], [R|_]) when L < R -> true;
cmp ([_|_], [_|_]) -> false;
cmp ([], []) -> true.

pad (N, VSN) ->
    VSN ++ lists:duplicate(N, 0).

%% End of Module.
