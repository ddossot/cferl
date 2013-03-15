%%%
%%% @doc Handling of a single storage object.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_object).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

%% Public API
-export([name/1, 
         bytes/1, 
         last_modified/1, 
         content_type/1, 
         etag/1,
         metadata/1, 
         set_metadata/3, 
         refresh/2,
         read_data/2, 
         read_data/4, 
         read_data_stream/3, 
         read_data_stream/5,
         write_data/4, 
         write_data/5, 
         write_data_stream/5, 
         write_data_stream/6,
         delete/2]).
  
%% Exposed for internal usage
-export([new/4]).

%% @doc Name of the current object.
-spec name(#cf_object{}) -> binary().
name(Object) when ?IS_OBJECT(Object) ->
  ObjectDetails = Object#cf_object.object_details,
  ObjectDetails#cf_object_details.name.

%% @doc Size in bytes of the current object.
-spec bytes(#cf_object{}) -> integer().
bytes(Object) when ?IS_OBJECT(Object) ->
  ObjectDetails = Object#cf_object.object_details,
  ObjectDetails#cf_object_details.bytes.

%% @doc Date and time of the last modification of the current object.
-spec last_modified(#cf_object{}) -> {Date::term(), Time::term()}.
last_modified(Object) when ?IS_OBJECT(Object) ->
  ObjectDetails = Object#cf_object.object_details,
  ObjectDetails#cf_object_details.last_modified.

%% @doc Content type of the current object.
-spec content_type(#cf_object{}) -> binary().
content_type(Object) when ?IS_OBJECT(Object) ->
  ObjectDetails = Object#cf_object.object_details,
  ObjectDetails#cf_object_details.content_type.

%% @doc Etag of the current object.
-spec etag(#cf_object{}) -> binary().
etag(Object) when ?IS_OBJECT(Object) ->
  ObjectDetails = Object#cf_object.object_details,
  ObjectDetails#cf_object_details.etag.

%% @doc Meta-data of the current object.
%%   The "X-Meta-Object-" prefix is stripped off the underlying HTTP header name. 
-spec metadata(#cf_object{}) -> [{Key::binary(),Value::binary()}].
metadata(Object) when ?IS_OBJECT(Object) ->
  HttpHeaders = Object#cf_object.http_headers,
  cferl_lib:extract_object_meta_headers(HttpHeaders).

%% @doc Set meta-data for the current object. All pre-existing meta-data is replaced by the new one.
%%   The "X-Meta-Object-" prefix will be automatically prepended to form the HTTP header names.
-spec set_metadata(#cf_connection{}, #cf_object{}, [{Key::binary(),Value::binary()}]) -> ok | cferl_lib:cferl_error().
set_metadata(Connection, Object, MetaData) 
    when ?IS_CONNECTION(Connection), ?IS_OBJECT(Object), is_list(MetaData) ->
  MetaHttpHeaders = [{<<?OBJECT_META_HEADER_PREFIX, Key/binary>>, Value} || {Key, Value} <- MetaData],
  Result = cferl_connection:send_storage_request(Connection, post, Object#cf_object.object_path, MetaHttpHeaders, raw),
  set_metadata_result(Result).

set_metadata_result({ok, "202", _, _}) ->
  ok;
set_metadata_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Refresh the current object reference, including all the meta information.
-spec refresh(#cf_connection{}, #cf_object{}) -> {ok, #cf_object{}} | cferl_lib:cferl_error().
refresh(Connection, Object) when ?IS_CONNECTION(Connection), ?IS_OBJECT(Object) ->
  cferl_container:get_object(Connection, Object#cf_object.container, name(Object)).

%% @doc Read the data stored for the current object.
-spec read_data(#cf_connection{}, #cf_object{}) -> {ok, Data::binary()} | cferl_lib:cferl_error().
read_data(Connection, Object)  ->
  do_read_data(Connection, Object, []).

%% @doc Read the data stored for the current object, reading 'size' bytes from the 'offset'.
-spec read_data(#cf_connection{}, #cf_object{}, Offset::integer(), Size::integer()) -> {ok, Data::binary()} | cferl_lib:cferl_error().
read_data(Connection, Object, Offset, Size) when is_integer(Offset), is_integer(Size) ->
  do_read_data(Connection, Object, [data_range_header(Offset, Size)]).

do_read_data(Connection, Object, RequestHeaders) 
    when ?IS_CONNECTION(Connection), ?IS_OBJECT(Object), is_list(RequestHeaders) ->
  Result = cferl_connection:send_storage_request(Connection, get, Object#cf_object.object_path, RequestHeaders, raw),
  do_read_data_result(Result).
  
do_read_data_result({ok, ResponseCode, _, ResponseBody})
  when ResponseCode =:= "200"; ResponseCode =:= "206" ->
  
  {ok, ResponseBody};

do_read_data_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Read the data stored for the current object and feed by chunks it into a function.
%%   The function of arity 1 will receive: {error, Cause::term()} | {ok, Data:binary()} | eof
-spec read_data_stream(#cf_connection{}, #cf_object{}, DataFun::function()) -> ok | cferl_lib:cferl_error().
read_data_stream(Connection, Object, DataFun)  ->
  do_read_data_stream(Connection, Object, DataFun, []).

%% @doc Read the data stored for the current object, reading 'size' bytes from the 'offset', and feed by chunks it into a function.
%%   The function of arity 1 will receive: {error, Cause::term()} | {ok, Data:binary()} | eof
-spec read_data_stream(#cf_connection{}, #cf_object{}, DataFun::function(), Offset::integer(), Size::integer()) -> ok | cferl_lib:cferl_error().
read_data_stream(Connection, Object, DataFun, Offset, Size) when is_integer(Offset), is_integer(Size) ->
  do_read_data_stream(Connection, Object, DataFun, [data_range_header(Offset, Size)]).

do_read_data_stream(Connection, Object, DataFun, RequestHeaders)
    when ?IS_CONNECTION(Connection), ?IS_OBJECT(Object), is_function(DataFun, 1), is_list(RequestHeaders) ->
  Result = cferl_connection:send_storage_request(Connection, get, Object#cf_object.object_path, RequestHeaders, DataFun),
  do_read_data_stream_result(Result).
  
do_read_data_stream_result({ibrowse_req_id, _Req_id}) ->
  ok;
do_read_data_stream_result(Other) ->
  cferl_lib:error_result(Other).

data_range_header(Offset, Size) when is_integer(Offset), is_integer(Size) ->
  {"Range", io_lib:format("bytes=~B-~B", [Offset, Offset+Size-1])}.

%% @doc Write data for the current object.
-spec write_data(#cf_connection{}, #cf_object{}, Data::binary(), ContentType::binary()) -> 
  ok | {error, invalid_content_length} | {error, mismatched_etag} | cferl_lib:cferl_error().
write_data(Connection, Object, Data, ContentType) when is_binary(Data), is_binary(ContentType) ->
  write_data(Connection, Object, Data, ContentType, []).

%% @doc Write data for the current object.
-spec write_data(#cf_connection{}, #cf_object{}, Data::binary(), ContentType::binary(), [{binary(), binary()}]) -> 
  ok | {error, invalid_content_length} | {error, mismatched_etag} | cferl_lib:cferl_error().
write_data(Connection, Object, Data, ContentType, RequestHeaders)
    when  is_binary(Data), is_binary(ContentType), is_list(RequestHeaders) ->
  do_write_data(Connection, Object, Data, ContentType, RequestHeaders).
  
%% @doc Write streamed data for the current object.
%%   The data generating function must be of arity 0 and return {ok, Data::binary()} | eof.
-spec write_data_stream(#cf_connection{}, #cf_object{}, DataFun::function(), ContentType::binary(), ContentLength::integer()) -> 
  ok | {error, invalid_content_length} | {error, mismatched_etag} | cferl_lib:cferl_error().
write_data_stream(Connection, Object, DataFun, ContentType, ContentLength)
    when is_function(DataFun, 0), is_binary(ContentType), is_integer(ContentLength) ->
  write_data_stream(Connection, Object, DataFun, ContentType, ContentLength, []).
  
%% @doc Write streamed data for the current object.
%%   The data generating function must be of arity 0 and return {ok, Data::binary()} | eof.
-spec write_data_stream(#cf_connection{}, #cf_object{}, DataFun::function(), 
                        ContentType::binary(), ContentLength::integer(), RequestHeaders::[{Name::binary(), Value::binary()}]) -> 
  ok | {error, invalid_content_length} | {error, mismatched_etag} | cferl_lib:cferl_error().
write_data_stream(Connection, Object, DataFun, ContentType, ContentLength, RequestHeaders)
    when is_function(DataFun, 0), is_binary(ContentType), is_integer(ContentLength), is_list(RequestHeaders) ->
  do_write_data(Connection, Object, DataFun, ContentType,
                [{<<"Content-Length">>, list_to_binary(integer_to_list(ContentLength))} | RequestHeaders]).

do_write_data(Connection, Object, DataSource, ContentType, RequestHeaders) 
    when ?IS_CONNECTION(Connection), ?IS_OBJECT(Object) ->
  Result = cferl_connection:send_storage_request(Connection, 
                                              put,
                                              Object#cf_object.object_path,
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
-spec delete(#cf_connection{}, #cf_object{}) -> ok | cferl_lib:cferl_error().
delete(Connection, Object) when ?IS_CONNECTION(Connection), ?IS_OBJECT(Object) ->
  cferl_container:delete_object(Connection, Object#cf_object.container, name(Object)).

-spec new(#cf_container{}, #cf_object_details{}, string(), [{string(), string()}]) -> #cf_object{}.
new(Container, ObjectDetails, ObjectPath, HttpHeaders) ->
  #cf_object{container      = Container,
             object_details = ObjectDetails,
             object_path    = ObjectPath,
             http_headers   = HttpHeaders 
            }.
