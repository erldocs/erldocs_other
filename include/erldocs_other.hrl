%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-

%% title/0 represents the name of either a branch or a tag
-type title() :: string().

-record(rev, { type        :: branch | tag  %% Distinguish tag from branch
             , id          :: title()
             , commit = "" :: string()
             }).

-type rev() :: #rev{}.

%% End of File.
