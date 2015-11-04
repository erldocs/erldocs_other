%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

-record(rev, { type        :: branch | tag  %% Distinguish tag from branch
             , id          :: eo_core:title()
             , commit = "" :: nonempty_string()
             , builds      :: boolean() | undefined
             , deps = []   :: [eo_core:title()]
             , discovered = [] :: [eo_core:rev()]
             , kvs = []    :: [tuple()]
             }).

%% End of File.
