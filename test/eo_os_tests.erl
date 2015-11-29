%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_os_tests).

%% eo_os_tests: tests for module eo_os.

-include_lib("eunit/include/eunit.hrl").


%% API tests.

exists_test_ () ->
    [ ?_assertNotEqual(false, os:find_executable("git"))
    , ?_assertNotEqual(false, os:find_executable("svn"))
    , ?_assertNotEqual(false, os:find_executable("rm"))
    , ?_assertNotEqual(false, os:find_executable("curl"))
    , ?_assertNotEqual(false, os:find_executable("find"))
    , ?_assertNotEqual(false, os:find_executable("mv"))
    , ?_assertNotEqual(false, os:find_executable("hg"))
    , ?_assertNotEqual(false, os:find_executable("ls"))
    , ?_assertNotEqual(false, os:find_executable("rebar"))
    ].

%% Internals

%% End of Module.
