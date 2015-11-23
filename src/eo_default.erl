%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_default).

%% eo_conf: utils for lib-wide configuration.

-export([ base/0
        , logfile/0
        , ga/0
        , update_only/0
        , has_erlang_code/0
        ]).


%% API

base () ->
    "./".

logfile () ->
    standard_io.

ga () ->
    "UA-54292016-1".

update_only () ->
    true.

has_erlang_code () ->
    false.

%% Internals

%% End of Module.
