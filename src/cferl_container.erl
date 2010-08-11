%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_container, [Connection, Name]).
-author('David Dossot <david@dossot.net>').

%% Public API
-export([name/0, files/0, delete/0, new_file/1]).

%% TODO comment!
name() ->
  Name.
  
%% TODO add bytes() count() refresh()

%% TODO comment!
files() ->
  %% FIXME implement!
  [].

%% TODO comment!
delete() ->
  Connection:send_storage_request(delete, <<"/", Name/binary>>, raw),
  %% FIXME handle 204, 404, 409 and other
  ok.

%% TODO comment!
new_file(Name) when is_binary(Name) ->
  %% FIXME implement!
  ok.
