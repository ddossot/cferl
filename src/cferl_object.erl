%%%
%%% @doc Handling of a single storage object.
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

-export([name/0, bytes/0, last_modified/0, content_type/0, etag/0,
         metadata/0, set_metadata/1, refresh/0,
         read_data/0, read_data/2, read_data_stream/1, read_data_stream/3,
         write_data/2, write_data/3, write_data_stream/3, write_data_stream/4,
         delete/0]).

%% @doc Name of the current object.
%% @spec name() -> binary()
name() ->
  ObjectDetails#cf_object_details.name.

%% @doc Size in bytes of the current object.
%% @spec bytes() -> integer()
bytes() ->
  ObjectDetails#cf_object_details.bytes.

%% @doc Date and time of the last modification of the current object.
%% @spec last_modified() -> {Date::date(), Time::time()}
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
  MetaHttpHeaders = [{<<?OBJECT_META_HEADER_PREFIX, Key/binary>>, Value} || {Key, Value} <- MetaData],
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

%% @doc Read the data stored for the current object.
%% @spec read_data() -> {ok, Data::binary()} | Error
%%   Error = cferl_error()
read_data() ->
  do_read_data([]).

%% @doc Read the data stored for the current object, reading 'size' bytes from the 'offset'.
%% @spec read_data(Offset::integer(), Size::integer()) -> {ok, Data::binary()} | Error
%%   Error = cferl_error()
read_data(Offset, Size) when is_integer(Offset), is_integer(Size) ->
  do_read_data([data_range_header(Offset, Size)]).

do_read_data(RequestHeaders) when is_list(RequestHeaders) ->
  Result = Connection:send_storage_request(get, ObjectPath, RequestHeaders, raw),
  do_read_data_result(Result).
  
do_read_data_result({ok, ResponseCode, _, ResponseBody})
  when ResponseCode =:= "200"; ResponseCode =:= "206" ->
  
  {ok, ResponseBody};

do_read_data_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Read the data stored for the current object and feed by chunks it into a function.
%%   The function of arity 1 will receive: {error, Cause::term()} | {ok, Data:binary()} | eof
%% @spec read_data(DataFun::function()) -> ok | Error
%%   Error = cferl_error()
read_data_stream(DataFun) when is_function(DataFun, 1) ->
  do_read_data_stream(DataFun, []).

%% @doc Read the data stored for the current object, reading 'size' bytes from the 'offset', and feed by chunks it into a function.
%%   The function of arity 1 will receive: {error, Cause::term()} | {ok, Data:binary()} | eof
%% @spec read_data(DataFun::function(), Offset::integer(), Size::integer()) -> ok | Error
%%   Error = cferl_error()
read_data_stream(DataFun, Offset, Size)
  when is_function(DataFun, 1), is_integer(Offset), is_integer(Size) ->
  
  do_read_data_stream(DataFun, [data_range_header(Offset, Size)]).

do_read_data_stream(DataFun, RequestHeaders)
  when is_function(DataFun, 1), is_list(RequestHeaders) ->
  
  Result = Connection:send_storage_request(get, ObjectPath, RequestHeaders, DataFun),
  do_read_data_stream_result(Result).
  
do_read_data_stream_result({ibrowse_req_id, Req_id}) ->
  ok;
do_read_data_stream_result(Other) ->
  cferl_lib:error_result(Other).

data_range_header(Offset, Size) when is_integer(Offset), is_integer(Size) ->
  {"Range", io_lib:format("bytes=~B-~B", [Offset, Offset+Size-1])}.

%% @doc Write data for the current object.
%% @spec write_data(Data::binary(), ContentType::binary()) -> ok | Error
%%   Error = {error, invalid_content_length} | {error, mismatched_etag} | cferl_error()
write_data(Data, ContentType) when is_binary(Data), is_binary(ContentType) ->
  write_data(Data, ContentType, []).

%% @doc Write data for the current object.
%% @spec write_data(Data::binary(), ContentType::binary(), RequestHeaders) -> ok | Error
%%   Error = {error, invalid_content_length} | {error, mismatched_etag} | cferl_error()
%%   RequestHeaders = [{Name::binary(), Value::binary()}]
write_data(Data, ContentType, RequestHeaders)
  when is_binary(Data), is_binary(ContentType), is_list(RequestHeaders) ->
  
  do_write_data(Data, ContentType, RequestHeaders).
  
%% @doc Write streamed data for the current object.
%%   The data generating function must be of arity 0 and return {ok, Data::binary()} | eof.
%% @spec write_data_stream(DataFun::function(), ContentType::binary(), ContentLength::integer()) -> ok | Error
%%   Error = {error, invalid_content_length} | {error, mismatched_etag} | cferl_error()
write_data_stream(DataFun, ContentType, ContentLength)
  when is_function(DataFun, 0), is_binary(ContentType), is_integer(ContentLength) ->
  
  write_data_stream(DataFun, ContentType, ContentLength, []).
  
%% @doc Write streamed data for the current object.
%%   The data generating function must be of arity 0 and return {ok, Data::binary()} | eof.
%% @spec write_data_stream(DataFun::function(), ContentType::binary(), ContentLength::integer(), RequestHeaders) -> ok | Error
%%   Error = {error, invalid_content_length} | {error, mismatched_etag} | cferl_error()
%%   RequestHeaders = [{Name::binary(), Value::binary()}]
write_data_stream(DataFun, ContentType, ContentLength, RequestHeaders)
  when is_function(DataFun, 0), is_binary(ContentType), is_integer(ContentLength), is_list(RequestHeaders) ->
  
  do_write_data(DataFun,
                ContentType,
                [{<<"Content-Length">>, list_to_binary(integer_to_list(ContentLength))} | RequestHeaders]).

do_write_data(DataSource, ContentType, RequestHeaders) ->
  Result = Connection:send_storage_request(put,
                                           ObjectPath,
                                           [{"Content-Type", ContentType}|RequestHeaders],
                                           DataSource,
                                           raw),
  do_write_data_result(Result).

do_write_data_result({ok, "201", _, _}) ->
  ok;
do_write_data_result({ok, "412", _, _}) ->
  {error, invalid_content_length};
do_write_data_result({ok, "422", _, _}) ->
  {error, mismatched_etag};
do_write_data_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Delete the current storage object.
%% @spec delete() -> ok | Error
%%   Error = cferl_error()
delete() ->
  Container:delete_object(name()).

