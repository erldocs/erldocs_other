%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-


-record(rev, { type        :: branch | tag
             , id          :: string()
             , commit = "" :: string()
             }).


%% End of File.
