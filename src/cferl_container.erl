%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
%%% @type cferl_error() = {error, not_found} | {error, unauthorized} | {error, {unexpected_response, Other}}.

-module(cferl_container, [Connection, Name]).
-author('David Dossot <david@dossot.net>').

%% Public API
-export([name/0, delete/0]).

%% TODO comment!
name() ->
  Name.
  
%% TODO add: bytes() count() refresh() is_public() is_empty() log_retention()

%% @doc Delete the current container (which must be empty).
%% @spec delete() -> ok | Error
%%   Error = {error, not_empty} | cferl_error()
delete() ->
  Result = Connection:send_storage_request(delete, <<"/", Name/binary>>, raw),
  %% FIXME handle 204, 404, 409 and other
  delete_result(Result).

delete_result({ok, "204", _, _}) ->
  ok;
delete_result({ok, "409", _, _}) ->
  {error, not_empty};
delete_result(Other) ->
  cferl_lib:error_result(Other).


%% TODO add: set_log_retention() make_public() make_private()

%% TODO add: object_exists, objects_details, objects_names, new_object
