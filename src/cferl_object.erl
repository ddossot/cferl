%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
%%% @type cferl_error() = {error, not_found} | {error, unauthorized} | {error, {unexpected_response, Other}}.
%%% @type cferl_object() = term(). Reference to the cferl_object parameterized module.

-module(cferl_object, [Connection, Container, ObjectDetails, ObjectPath, HttpHeaders]).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

-export([name/0, bytes/0, last_modified/0, content_type/0, etag/0, metadata/0,
         set_metadata/1, refresh/0]).

%% @doc Name of the current object.
%% @spec name() -> binary()
name() ->
  ObjectDetails#cf_object_details.name.

%% @doc Size in bytes of the current object.
%% @spec bytes() -> integer()
bytes() ->
  ObjectDetails#cf_object_details.bytes.

%% @doc Date and time of the last modification of the current object.
%% @spec last_modified() -> {Date:date(), Time:time()}
last_modified() ->
  ObjectDetails#cf_object_details.last_modified.

%% @doc Content type of the current object.
%% @spec content_type() -> binary()
content_type() ->
  ObjectDetails#cf_object_details.content_type.

%% @doc Etag of the current object.
%% @spec etag() -> binary()
etag() ->
  ObjectDetails#cf_object_details.etag.

%% @doc Meta-data of the current object.
%%   The "X-Meta-Object-" prefix is stripped off the underlying HTTP header name. 
%% @spec metadata() -> [{Key::binary(),Value::binary()}]
metadata() ->
  cferl_lib:extract_object_meta_headers(HttpHeaders).

%% @doc Set meta-data for the current object. All pre-existing meta-data is replaced by the new one.
%%   The "X-Meta-Object-" prefix will be automatically prepended to form the HTTP header names.
%% @spec set_metadata([{Key::binary(),Value::binary()}]) -> ok | Error
%%   Error = cferl_error()
set_metadata(MetaData) ->
  MetaHttpHeaders = [{?OBJECT_META_HEADER_PREFIX ++ binary_to_list(Key), binary_to_list(Value)} || {Key, Value} <- MetaData],
  Result = Connection:send_storage_request(post, ObjectPath, MetaHttpHeaders, raw),
  set_metadata_result(Result).

set_metadata_result({ok, "202", _, _}) ->
  ok;
set_metadata_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Refresh the current object reference, including all the meta information.
%% @spec refresh() -> {ok, Object} | Error
%%   Object = cferl_object()
%%   Error = cferl_error()
refresh() ->
  Container:get_object(name()).

% TODO add: data/data_stream (read/write), delete
