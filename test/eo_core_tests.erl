%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_core_tests).

%% eo_core_tests: tests for module eo_core.

-export([new_test_tar/1]).

-include("erldocs_other.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API.

new_test_tar (Url) ->
    DataDir = "test/eo_core_DATA",
    UUID = make_name(Url),
    JailDir = filename:join("ebin", UUID),
    DestTar = filename:join(DataDir, UUID) ++ ".tar",
    eo_util:mkdir(JailDir),
    GenOpts = [ {website_dir, JailDir}
              , {dest,        JailDir}
              , {url, Url}
              , {update_only, false}
              ],
    {ok, _Url, _Dest, _Link} = eo_core:gen(GenOpts),
    ArchGot = list_files(JailDir),
    ok = erl_tar:create(DestTar, ArchGot),
    rmr(JailDir).

%% API tests.

gen_test_ () ->
    [] ++
        bitbucket() ++
        github() ++
        googlecode() ++
        [].

%% Internals

bitbucket () ->
    [ do("https://bitbucket.org/fenollp/ring")
    , do("https://bitbucket.org/fenollp/tmln-google")
    , do("https://bitbucket.org/fenollp/asql") ].

github () ->
    [ do("https://github.com/fenollp/erlang-dot")
    , do("https://github.com/fenollp/patmat") ].

googlecode () ->
    [ do("https://code.google.com/p/plists") ].


rmr (Dir) ->
    os:cmd("rm -r '" ++ Dir ++ "'").

%% do :: (repo_url()) -> eunit_stuff()
do (Url) ->
    %% Note: CWD = Git root
    %% Generated: ebin/website.tld,user,reponame/{,tmp_random}
    {setup
    , fun () ->
              Dir = filename:join("ebin", make_name(Url)),
              rmr(Dir),
              Dir
      end
    , fun rmr/1

    , fun (_ReturnOfSetup) ->
              DataDir = "test/eo_core_DATA",
              UUID = make_name(Url),
              JailDir = filename:join("ebin", UUID),
              eo_util:mkdir(JailDir),
              {ok, ArchExpected} = erl_tar:table(
                                     filename:join(DataDir, UUID) ++ ".tar"),
              GenOpts = [ {website_dir, JailDir}
                        , {dest,        JailDir}
                        , {url, Url}
                        , {update_only, false}
                        ],
              {ok, Url, _Dest, _Link} = eo_core:gen(GenOpts),
              ArchGot = list_files(JailDir),
              ?_assertEqual(lists:usort(ArchExpected), lists:usort(ArchGot))
      end}.

make_name (Url0) ->
    {true, Url} = eo_scm:url(Url0),
    Bin = list_to_binary(eo_scm:repo_local_path(Url)),
    binary_to_list(
      binary:replace(Bin, <<"/">>, <<",">>, [global])).

list_files (Dir) ->
    ListInsert = fun (X, Tail) -> [X|Tail] end,
    filelib:fold_files(Dir, ".+", true, ListInsert, []).

%% End of Module.
