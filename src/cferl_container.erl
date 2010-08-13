%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
%%% @type cferl_error() = {error, not_found} | {error, unauthorized} | {error, {unexpected_response, Other}}.

-module(cferl_container, [Connection, Name, Bytes, Count, CdnDetails]).
-author('David Dossot <david@dossot.net>').

%% Public API
-export([name/0, bytes/0, count/0, is_empty/0, is_public/0,
         delete/0]).

%% @doc Name of the current container.
%% @spec name() -> binary()
name() ->
  Name.

%% @doc Size in bytes of the current container.
%% @spec bytes() -> integer()
bytes() ->
  Bytes.

%% @doc Number of objects in the current container.
%% @spec count() -> integer()
count() ->
  Count.

%% @doc Determine if the current container is empty.
%% @spec is_empty() -> true | false
is_empty() ->
  count() == 0.

%% @doc Determine if the current container is public (CDN-enabled).
%% @spec is_public() -> true | false
is_public() ->
  proplists:get_value(cdn_enabled, CdnDetails).

%% TODO add: log_retention() cdn_url() cdn_ttl()

%% @doc Delete the current container (which must be empty).
%% @spec delete() -> ok | Error
%%   Error = {error, not_empty} | cferl_error()
delete() ->
  Result = Connection:send_storage_request(delete, <<"/", Name/binary>>, raw),
  delete_result(Result).

delete_result({ok, "204", _, _}) ->
  ok;
delete_result({ok, "409", _, _}) ->
  {error, not_empty};
delete_result(Other) ->
  cferl_lib:error_result(Other).

%% TODO add: make_public() make_private() refresh() set_log_retention()

%% TODO add: object_exists() objects_details() objects_names() new_object()
