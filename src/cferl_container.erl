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
-export([name/0, delete/0]).

%% TODO comment!
name() ->
  Name.
  
%% TODO add: bytes() count() refresh() is_public() is_empty() log_retention()

%% TODO comment!
delete() ->
  Connection:send_storage_request(delete, <<"/", Name/binary>>, raw),
  %% FIXME handle 204, 404, 409 and other
  ok.

%% TODO add: set_log_retention() make_public() make_private()

%% TODO add: object_exists, objects_details, objects_names, new_object
