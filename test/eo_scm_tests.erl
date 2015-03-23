%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_scm_tests).

%% eo_scm_tests: tests for module eo_scm.

-include_lib("eunit/include/eunit.hrl").

-include("erldocs_other.hrl").

%% API tests.

bitbucket_test_ () ->
    [ do(git, "https://bitbucket.org/fenollp/ring"
        , {ok,[ #rev{ type = branch
                    , id = "master"
                    , commit = "eb7432dfa6d52200daa69e8975a42ea8104bf5ea" }
              ]})
    ].

github_test_ () ->
    [ do(git, "https://github.com/fenollp/erlang-dot"
        , {ok,[ #rev{ type = tag
                    , id = "v0.1"
                    , commit = "cb90a4b457846f119fd23aea5b00fa2b6ecf3f3c" }
              , #rev{ type = tag
                    , id = "v0.1.1"
                    , commit = "961120be8315220138ee65c8f90420e3e6733ef5" }
              , #rev{ type = branch
                    , id = "master"
                    , commit = "8f2e22e4aa425220e928d669fa6a08d3df1fd4a6" }
              ]})
    ].

googlecode_test_ () ->
    [ do(svn, "https://code.google.com/p/plists"
        , {ok, [ #rev{ type = branch
                     , id = "trunk"
                     , commit = "11" }
               ]})
    ].

%% Internals

do (Method, Url, Expected) ->
    Refs = eo_scm:refs({Method, Url, #rev{}}),
    ?_assertEqual(Expected, Refs).

%% End of Module.
