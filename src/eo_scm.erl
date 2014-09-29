%% Copyright © 2014 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(eo_scm).

%% eo_scm: SCM read-only commands.

-export([ refs/1 ]).


%% API

refs ({git, Url, _Rev}) ->
    case eo_os:sh("git ls-remote --heads --tags '~s'", [Url]) of
        {0,R} ->
            Branches  = [{shorten(Commit), Branch}
                         || {Commit, "refs/heads/"++Branch} <- R],
            DerefTags = [{ shorten(Commit)
                         , string:sub_string(Tag, 1, length(Tag) -3)}
                         || {Commit, "refs/tags/"++Tag} <- R,
                            lists:suffix("^{}", Tag)],
            NormaTags = [{shorten(Commit), Tag}
                         || {Commit, "refs/tags/"++Tag} <- R,
                            not lists:suffix("^{}", Tag),
                            not lists:keymember(Tag, 2, DerefTags)],
            {ok, Branches, DerefTags++NormaTags};
        {_,_} ->
            error
    end.

%% Internals

shorten (Commit) ->
    lists:sublist(Commit, 7).

%% End of Module.
