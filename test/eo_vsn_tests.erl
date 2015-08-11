%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_vsn_tests).

%% eo_vsn_tests: tests for module eo_vsn.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

'=<_test_' () ->
    [ ?_assertEqual(false, lte("0.0.1", "0.0.0"))
    , ?_assertEqual(false, lte("0.2.1", "0.1.0"))
    , ?_assertEqual(true, lte("0.2.1", "0.2.2"))
    , ?_assertEqual(false, lte("16.0.1", "1.298.342"))
    , ?_assertEqual(true, lte("1.298.342", "16.0.1"))
    , ?_assertEqual(false, lte("release-1.0.1", "near_alpha-0.9999.9999"))
    ].

%% Internals

lte (Barba, Papa) ->
    eo_vsn:'=<'(o(Barba), o(Papa)).

o (Str) ->
    {true, Vsn} = eo_vsn:get_vsn(Str),
    Vsn.

%% End of Module.
