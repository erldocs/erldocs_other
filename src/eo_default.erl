%% Copyright © 2015 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_default).

%% eo_conf: utils for lib-wide configuration.

-export([ base/0
        , logfile/0
        , ga/0
        ]).


%% API

base () ->
    "./".

logfile () ->
    standard_io.

ga () ->
    "UA-54292016-1".

%% Internals

%% End of Module.
