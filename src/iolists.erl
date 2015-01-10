%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(iolists).

%% iolists: functions handling iolists

-export([ to_list/1
        , to_binary/1
        , size/1
        , map/2
        , foldr/3
        , filtermap/2
        ]).

-compile({no_auto_import, [size/1]}). %% Nonsense

%% API

%%FIXME
%% 5> iolists:foldr(fun (E,Acc) -> [E|Acc] end, [], [1,[2,<<3>>]]).
%% [1,[2,<<3>>]]
foldr (_, E0, []) -> E0;
foldr (F, E0, IoList)
  when is_list(IoList) ->
    [H|T] = IoList,
    F(H, foldr(F, E0, T));
foldr (_, E0, <<>>) -> E0;
foldr (F, E0, IoList)
  when is_binary(IoList) ->
    <<E0:1/binary, Rest/binary>> = IoList,
    <<E>> = E0,
    F(E, foldr(F, E0, Rest));
foldr (_, _, IoList)
  when is_number(IoList) ->
    IoList.

map (_, []) -> [];
map (F, IoList)
  when is_list(IoList) ->
    [H|T] = IoList,
    map(F, H) ++ map(F, T);
map (_, <<>>) -> [];
map (F, IoList)
  when is_binary(IoList) ->
    <<E0:1/binary, Rest/binary>> = IoList,
    <<E>> = E0,
    [F(E) | map(F, Rest)];
map (F, IoList)
  when is_number(IoList) ->
    [F(IoList)].

%% = iolist-comprehension
filtermap (_, []) -> [];
filtermap (F, IoList)
  when is_list(IoList) ->
    [H|T] = IoList,
    filtermap(F, H) ++ filtermap(F, T);
filtermap (_, <<>>) -> [];
filtermap (F, IoList)
  when is_binary(IoList) ->
    <<E0:1/binary, Rest/binary>> = IoList,
    <<E>> = E0,
    pick(F, E) ++ filtermap(F, Rest);
filtermap (F, IoList)
  when is_tuple(IoList) ->
    Size = tuple_size(IoList),
    v_t(F, IoList, Size, Size);
filtermap (F, IoList)
  when is_number(IoList) ->
    pick(F, IoList).

%% = map(fun (X) -> X end)/1
to_list ([]) -> [];
to_list (IoList)
  when is_list(IoList) ->
    [H|T] = IoList,
    to_list(H) ++ to_list(T);
to_list (<<>>) -> [];
to_list (IoList)
  when is_binary(IoList) ->
    <<E0:1/binary, Rest/binary>> = IoList,
    <<E>> = E0,
    [E | to_list(Rest)];
to_list (IoList)
  when is_number(IoList) ->
    [IoList].

to_binary ([]) -> <<>>;
to_binary (IoList)
  when is_list(IoList) ->
    [H|T] = IoList,
    <<(to_binary(H))/binary, (to_binary(T))/binary>>;
to_binary (IoList)
  when is_binary(IoList) ->
    IoList;
to_binary (IoList)
  when is_number(IoList) ->
    int_to_binary(IoList).

%% = foldl(fun erlang:'+'/2, 0)/1
size ([]) -> 0;
size (IoList)
  when is_list(IoList) ->
    [_|T] = IoList,
    1 + size(T);
size (<<>>) -> 0;
size (IoList)
  when is_binary(IoList) ->
    <<_E0:1/binary, Rest/binary>> = IoList,
    1 + size(Rest);
size (IoList)
  when is_number(IoList) ->
    1.

%% Internals

pick (Fun, Elt) ->
    case Fun(Elt) of
        false -> [];
        true -> [Elt];
        {true,E2} -> [E2]
    end.

v_t (_, _, _, 0) -> [];
v_t (F, T, S, N) ->
    filtermap(F, element(S-N+1,T)) ++ v_t(F, T, S, N-1).

%% //github.com/uwiger/sext/blob/e04c3dd07e903172ff215ad9ba08e6afbad717b2/src/sext.erl#L509-L520
int_to_binary (I) when I =< 16#ff -> <<I:8>>;
int_to_binary (I) when I =< 16#ffff -> <<I:16>>;
int_to_binary (I) when I =< 16#ffffff -> <<I:24>>;
int_to_binary (I) when I =< 16#ffffffff -> <<I:32>>;
int_to_binary (I) when I =< 16#ffffffffff -> <<I:40>>;
int_to_binary (I) when I =< 16#ffffffffffff -> <<I:48>>;
int_to_binary (I) when I =< 16#ffffffffffffff -> <<I:56>>;
int_to_binary (I) when I =< 16#ffffffffffffffff -> <<I:64>>;
int_to_binary (I) ->
    %% Realm of the ridiculous
    list_to_binary(
      lists:dropwhile(fun(X) -> X==0 end, binary_to_list(<<I:256>>))).

%% End of Module.
