%%%
%%% @doc Management of a container's storage objects.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
%%% @type cferl_error() = {error, not_found} | {error, unauthorized} | {error, {unexpected_response, Other}}.
%%% @type cf_container_cdn_config() = record(). Record of type cf_container_cdn_config.
%%% @type cf_object_query_args() = record(). Record of type cf_object_query_args.
%%% @type cf_object_details() = record(). Record of type cf_object_details.
%%% @type cferl_object() = term(). Reference to the cferl_object parameterized module.

-module(cferl_container, [Connection, ContainerDetails, ContainerPath, CdnDetails]).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

%% Public API
-export([name/0, bytes/0, count/0, is_empty/0, is_public/0, cdn_url/0, cdn_ttl/0, log_retention/0,
         make_public/0, make_public/1, make_private/0, set_log_retention/1,
         refresh/0, delete/0,
         get_objects_names/0, get_objects_names/1, get_objects_details/0, get_objects_details/1,
         object_exists/1, get_object/1, create_object/1, delete_object/1,
         ensure_dir/1]).

%% @doc Name of the current container.
%% @spec name() -> binary()
name() ->
  ContainerDetails#cf_container_details.name.

%% @doc Size in bytes of the current container.
%% @spec bytes() -> integer()
bytes() ->
  ContainerDetails#cf_container_details.bytes.

%% @doc Number of objects in the current container.
%% @spec count() -> integer()
count() ->
  ContainerDetails#cf_container_details.count.

%% @doc Determine if the current container is empty.
%% @spec is_empty() -> true | false
is_empty() ->
  count() == 0.

%% @doc Determine if the current container is public (CDN-enabled).
%% @spec is_public() -> true | false
is_public() ->
  proplists:get_value(cdn_enabled, CdnDetails).

%% @doc CDN of the container URL, if it is public.
%% @spec cdn_url() -> binary()
cdn_url() ->
  proplists:get_value(cdn_uri, CdnDetails).

%% @doc TTL (in seconds) of the container, if it is public.
%% @spec cdn_ttl() -> integer()
cdn_ttl() ->
  proplists:get_value(ttl, CdnDetails).

%% @doc Determine if log retention is enabled on this container (which must be public).
%% @spec log_retention() -> true | false
log_retention() ->
  is_public() andalso proplists:get_value(log_retention, CdnDetails).

%% @doc Make the current container publicly accessible on CDN, using the default configuration (ttl of 1 day and no ACL).
%% @spec make_public() -> ok | Error
%%   Error = cferl_error()
make_public() ->
  make_public(#cf_container_cdn_config{}).

%% @doc Make the current container publicly accessible on CDN, using the provided configuration.
%%   ttl is in seconds.
%%   user_agent_acl and referrer_acl are Perl-compatible regular expression used to limit access to this container. 
%% @spec make_public(CdnConfig) -> ok | Error
%%   CdnConfig = cf_container_cdn_config()
%%   Error = cferl_error()
make_public(CdnConfig) when is_record(CdnConfig, cf_container_cdn_config) ->
  PutResult = Connection:send_cdn_management_request(put, ContainerPath, raw),
  make_public_put_result(CdnConfig, PutResult).

make_public_put_result(CdnConfig, {ok, ResponseCode, _, _})
  when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  
  CdnConfigHeaders = cferl_lib:cdn_config_to_headers(CdnConfig),
  Headers = [{"X-CDN-Enabled", "True"} | CdnConfigHeaders],
  PostResult = Connection:send_cdn_management_request(post, ContainerPath, Headers, raw),
  make_public_post_result(PostResult);
make_public_put_result(_, Other) ->
  cferl_lib:error_result(Other).

make_public_post_result({ok, ResponseCode, _, _})
  when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
make_public_post_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Make the current container private.
%%   If it was previously public, it will remain accessible on the CDN until its TTL is reached. 
%% @spec make_private() -> ok | Error
%%   Error = cferl_error()
make_private() ->
  Headers = [{"X-CDN-Enabled", "False"}],
  PostResult = Connection:send_cdn_management_request(post, ContainerPath, Headers, raw),
  make_private_result(PostResult).

make_private_result({ok, ResponseCode, _, _})
  when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
make_private_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Activate or deactivate log retention for current container.
%% @spec set_log_retention(true | false) -> ok | Error
%%   Error = cferl_error()
set_log_retention(true) ->
  do_set_log_retention("True");
set_log_retention(false) ->
  do_set_log_retention("False").
  
do_set_log_retention(State) ->
  Headers = [{"x-log-retention", State}],
  PostResult = Connection:send_cdn_management_request(post, ContainerPath, Headers, raw),
  set_log_retention_result(PostResult).
  
set_log_retention_result({ok, ResponseCode, _, _})
  when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
set_log_retention_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Refresh the current container reference.
%% @spec refresh() -> {ok, Container} | Error
%%   Container = cferl_container()
%%   Error = cferl_error()
refresh() ->
  Connection:get_container(name()).

%% @doc Delete the current container (which must be empty).
%% @spec delete() -> ok | Error
%%   Error = {error, not_empty} | cferl_error()
delete() ->
  Connection:delete_container(name()).

%% @doc Retrieve all the object names in the current container (within the limits imposed by Cloud Files server).
%% @spec get_objects_names() -> {ok, [binary()]} | Error
%%   Error = cferl_error()
get_objects_names() ->
  get_objects_names(#cf_object_query_args{}).

%% @doc Retrieve the object names in the current container, filtered by the provided query arguments.
%%   If you supply the optional limit, marker, prefix or path arguments, the call will return the number of objects specified in limit,
%%   starting at the object index specified in marker, selecting objects whose names start with prefix or search within the pseudo-filesystem
%%   path.
%% @spec get_objects_names(QueryArgs) -> {ok, [binary()]} | Error
%%   QueryArgs = cf_object_query_args()
%%   Error = cferl_error()
get_objects_names(QueryArgs) when is_record(QueryArgs, cf_object_query_args) ->
  QueryString = cferl_lib:object_query_args_to_string(QueryArgs),
  Result = Connection:send_storage_request(get, ContainerPath ++ QueryString, raw),
  get_objects_names_result(Result).

get_objects_names_result({ok, "204", _, _}) ->
  {ok, []};
get_objects_names_result({ok, "200", _, ResponseBody}) ->
  {ok, [list_to_binary(ObjectName) || ObjectName <- string:tokens(ResponseBody, "\n")]};
get_objects_names_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve details for all the objects in the current container (within the limits imposed by Cloud Files server).
%% @spec get_objects_details() -> {ok, [cf_object_details()]} | Error
%%   Error = cferl_error()
get_objects_details() ->
  get_objects_details(#cf_object_query_args{}).
  
%% @doc Retrieve the object details in the current container, filtered by the provided query arguments.
%%   If you supply the optional limit, marker, prefix or path arguments, the call will return the number of objects specified in limit,
%%   starting at the object index specified in marker, selecting objects whose names start with prefix or search within the pseudo-filesystem
%%   path.
%% @spec get_objects_details(QueryArgs) -> {ok, [cf_object_details()]} | Error
%%   QueryArgs = cf_object_query_args()
%%   Error = cferl_error()
get_objects_details(QueryArgs) when is_record(QueryArgs, cf_object_query_args) ->
  QueryString = cferl_lib:object_query_args_to_string(QueryArgs),
  Result = Connection:send_storage_request(get, ContainerPath ++ QueryString, json),
  get_objects_details_result(Result).
  
get_objects_details_result({ok, "204", _, _}) ->
  {ok, []};
get_objects_details_result({ok, "200", _, ResponseBody}) ->
  BuildRecordFun =
    fun({struct, Proplist}) ->
      LastModifiedBin = proplists:get_value(<<"last_modified">>, Proplist), 
      <<Year :4/binary, "-",
        Month:2/binary, "-",
        Day  :2/binary, "T",
        Hour :2/binary, ":",
        Min  :2/binary, ":",
        Sec  :2/binary, ".",
        _MSec:6/binary>> = LastModifiedBin,
      
      % drop the microseconds, not supported by RFC 1123
      LastModified = {{bin_to_int(Year), bin_to_int(Month), bin_to_int(Day)},
                      {bin_to_int(Hour), bin_to_int(Min), bin_to_int(Sec)}},
      
      #cf_object_details{
        name = proplists:get_value(<<"name">>, Proplist),
        bytes = proplists:get_value(<<"bytes">>, Proplist),
        last_modified = LastModified,
        content_type = proplists:get_value(<<"content_type">>, Proplist),
        etag = proplists:get_value(<<"hash">>, Proplist)
      }
    end,
    
  ObjectsInfo = lists:map(BuildRecordFun,
                          mochijson2:decode(ResponseBody)), 
  {ok, ObjectsInfo};
get_objects_details_result(Other) ->
  cferl_lib:error_result(Other).
  
%% @doc Test the existence of an object in the current container.
%% @spec object_exists(ObjectName::binary()) -> true | false
object_exists(ObjectName) when is_binary(ObjectName) ->
  Result = Connection:send_storage_request(head, get_object_path(ObjectName), raw),
  object_exists_result(Result).

object_exists_result({ok, ResponseCode, _, _})
  when ResponseCode =:= "200"; ResponseCode =:= "204" ->
  true;
object_exists_result(_) ->
  false.

%% @doc Get a reference to an existing storage object.
%% @spec get_object(Name::binary) -> {ok, Object} | Error
%%   Object = cferl_object()
%%   Error = cferl_error()
get_object(ObjectName) when is_binary(ObjectName) ->
  Result = Connection:send_storage_request(head, get_object_path(ObjectName), raw),
  get_object_result(ObjectName, Result).

get_object_result(ObjectName, {ok, ResponseCode, ResponseHeaders, _})
  when ResponseCode =:= "200"; ResponseCode =:= "204" ->
  
  ObjectDetails = #cf_object_details{
    name = ObjectName,
    bytes = cferl_lib:get_int_header("Content-Length", ResponseHeaders),
    last_modified = httpd_util:convert_request_date(cferl_lib:get_string_header("Last-Modified", ResponseHeaders)),
    content_type = cferl_lib:get_binary_header("Content-Type", ResponseHeaders),
    etag = cferl_lib:get_binary_header("Etag", ResponseHeaders)
  },
  
  {ok, cferl_object:new(Connection, THIS, ObjectDetails, get_object_path(ObjectName), ResponseHeaders)};

get_object_result(_, Other) ->
  cferl_lib:error_result(Other).

%% @doc Create a reference to a new storage object.
%%   Nothing is actually created until data gets written in the object.
%%   If an object with the provided name already exists, a reference to this object is returned.
%% @spec create_object(Name::binary) -> {ok, Object} | Error
%%   Object = cferl_object()
%%   Error = cferl_error()
create_object(ObjectName) when is_binary(ObjectName) ->
  case get_object(ObjectName) of
    {ok, Object} ->
      Object;
    
    _ ->
      ObjectDetails = #cf_object_details{name = ObjectName},
      {ok, cferl_object:new(Connection, THIS, ObjectDetails, get_object_path(ObjectName), [])}
  end.

%% @doc Delete an existing storage object.
%% @spec delete_object(Name::binary) -> ok | Error
%%   Error = cferl_error()
delete_object(ObjectName) when is_binary(ObjectName) ->
  Result = Connection:send_storage_request(delete, get_object_path(ObjectName), raw),
  delete_object_result(Result).

delete_object_result({ok, "204", _, _}) ->
  ok;
delete_object_result(Other) ->
  cferl_lib:error_result(Other).
  
%% @doc Ensure that all the directories exist in an object path.
%%   Passing &lt;&lt;"photos/plants/fern.jpg">>, will ensure that the &lt;&lt;"photos">> and &lt;&lt;"photos/plants">> directories exist.
%% @spec ensure_dir(ObjectPath::binary()) -> ok
ensure_dir(ObjectPath) when is_binary(ObjectPath) ->
  CreateDirectoryFun =
    fun(Directory) ->
      {ok, DirectoryObject} = create_object(Directory),
      % push the object on the server only if its content-type is not good
      case DirectoryObject:content_type() of
        ?DIRECTORY_OBJECT_CONTENT_TYPE ->
          noop;
        _ ->
          ok = DirectoryObject:write_data(<<>>,
                                          ?DIRECTORY_OBJECT_CONTENT_TYPE,
                                          [{<<"Content-Length">>, <<"0">>}])
      end
    end,
  
  lists:foreach(CreateDirectoryFun, cferl_lib:path_to_sub_dirs(ObjectPath)),
  ok.

%% Private functions
get_object_path(ObjectName) when is_binary(ObjectName) ->
  ContainerPath ++ "/" ++ cferl_lib:url_encode(ObjectName).

bin_to_int(Bin) when is_binary(Bin) ->
  list_to_integer(binary_to_list(Bin)).

