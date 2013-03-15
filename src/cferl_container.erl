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

-module(cferl_container).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

%% Public API
-export([name/1, 
         bytes/1, 
         count/1, 
         is_empty/1, 
         is_public/1, 
         cdn_url/1, 
         cdn_ttl/1, 
         log_retention/1,
         make_public/2, 
         make_public/3, 
         make_private/2, 
         set_log_retention/3,
         refresh/2, 
         delete/2,
         get_objects_names/2, 
         get_objects_names/3, 
         get_objects_details/2, 
         get_objects_details/3,
         object_exists/3, 
         get_object/3, 
         create_object/3, 
         delete_object/3,
         ensure_dir/3
        ]).

%% Exposed for internal usage
-export([new/3 ]).


%% @doc Name of the current container.
-spec name(#cf_container{}) -> binary().
name(Container) when ?IS_CONTAINER(Container) ->
  ContainerDetails = Container#cf_container.container_details,
  ContainerDetails#cf_container_details.name.

%% @doc Size in bytes of the current container.
-spec bytes(#cf_container{}) -> integer().
bytes(Container) when ?IS_CONTAINER(Container) ->
  ContainerDetails = Container#cf_container.container_details,
  ContainerDetails#cf_container_details.bytes.

%% @doc Number of objects in the current container.
-spec count(#cf_container{}) -> integer().
count(Container) when ?IS_CONTAINER(Container) ->
  ContainerDetails = Container#cf_container.container_details,
  ContainerDetails#cf_container_details.count.

%% @doc Determine if the current container is empty.
-spec is_empty(#cf_container{}) -> true | false.
is_empty(Container) ->
  count(Container) == 0.

%% @doc Determine if the current container is public (CDN-enabled).
-spec is_public(#cf_container{}) -> true | false.
is_public(Container) when ?IS_CONTAINER(Container) ->
  CdnDetails = Container#cf_container.cdn_details,
  proplists:get_value(cdn_enabled, CdnDetails).

%% @doc CDN of the container URL, if it is public.
-spec cdn_url(#cf_container{}) -> binary().
cdn_url(Container) when ?IS_CONTAINER(Container) ->
  CdnDetails = Container#cf_container.cdn_details,
  proplists:get_value(cdn_uri, CdnDetails).

%% @doc TTL (in seconds) of the container, if it is public.
-spec cdn_ttl(#cf_container{}) -> integer().
cdn_ttl(Container) when ?IS_CONTAINER(Container) ->
  CdnDetails = Container#cf_container.cdn_details,
  proplists:get_value(ttl, CdnDetails).

%% @doc Determine if log retention is enabled on this container (which must be public).
-spec log_retention(#cf_container{}) -> true | false.
log_retention(Container) when ?IS_CONTAINER(Container) ->
  CdnDetails = Container#cf_container.cdn_details,
  is_public(Container) andalso proplists:get_value(log_retention, CdnDetails).

%% @doc Make the current container publicly accessible on CDN, using the default configuration (ttl of 1 day and no ACL).
-spec make_public(#cf_connection{}, #cf_container{}) -> ok | cferl_lib:cferl_error().
make_public(Connection, Container) when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container) ->
  make_public(Connection, Container, #cf_container_cdn_config{}).

%% @doc Make the current container publicly accessible on CDN, using the provided configuration.
%%   ttl is in seconds.
%%   user_agent_acl and referrer_acl are Perl-compatible regular expression used to limit access to this container. 
-spec make_public(#cf_connection{}, #cf_container{}, #cf_container_cdn_config{}) -> ok | cferl_lib:cferl_error().
make_public(Connection, Container, CdnConfig) 
    when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container), is_record(CdnConfig, cf_container_cdn_config) ->
  PutResult = cferl_connection:send_cdn_management_request(Connection, put, Container#cf_container.container_path, raw),
  make_public_put_result(Connection, Container, CdnConfig, PutResult).

make_public_put_result(Connection, Container, CdnConfig, {ok, ResponseCode, _, _})
  when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  
  CdnConfigHeaders = cferl_lib:cdn_config_to_headers(CdnConfig),
  Headers = [{"X-CDN-Enabled", "True"} | CdnConfigHeaders],
  PostResult = cferl_connection:send_cdn_management_request(Connection, post, Container#cf_container.container_path, Headers, raw),
  make_public_post_result(PostResult);
make_public_put_result(_, _, _, Other) ->
  cferl_lib:error_result(Other).

make_public_post_result({ok, ResponseCode, _, _})
  when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
make_public_post_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Make the current container private.
%%   If it was previously public, it will remain accessible on the CDN until its TTL is reached. 
-spec make_private(#cf_connection{}, #cf_container{}) -> ok | cferl_lib:cferl_error().
make_private(Connection, Container) when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container) ->
  Headers = [{"X-CDN-Enabled", "False"}],
  PostResult = cferl_connection:send_cdn_management_request(Connection, post, Container#cf_container.container_path, Headers, raw),
  make_private_result(PostResult).

make_private_result({ok, ResponseCode, _, _})
  when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
make_private_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Activate or deactivate log retention for current container.
-spec set_log_retention(#cf_connection{}, #cf_container{}, true | false) -> ok | cferl_lib:cferl_error().
set_log_retention(Connection, Container, true) when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container) ->
  do_set_log_retention(Connection, Container, "True");
set_log_retention(Connection, Container, false) when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container) ->
  do_set_log_retention(Connection, Container, "False").
  
do_set_log_retention(Connection, Container, State) ->
  Headers = [{"x-log-retention", State}],
  PostResult = cferl_connection:send_cdn_management_request(Connection, post, Container#cf_container.container_path, Headers, raw),
  set_log_retention_result(PostResult).
  
set_log_retention_result({ok, ResponseCode, _, _})
  when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
set_log_retention_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Refresh the current container reference.
-spec refresh(#cf_connection{}, #cf_container{}) -> {ok, #cf_container{}} | cferl_lib:cferl_error().
refresh(Connection, Container) when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container) ->
  cferl_connection:get_container(Connection, name(Container)).

%% @doc Delete the current container (which must be empty).
-spec delete(#cf_connection{}, #cf_container{}) -> ok | {error, not_empty} | cferl_lib:cferl_error().
delete(Connection, Container) when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container) ->
  cferl_connection:delete_container(Connection, name(Container)).

%% @doc Retrieve all the object names in the current container (within the limits imposed by Cloud Files server).
-spec get_objects_names(#cf_connection{}, #cf_container{}) -> {ok, [binary()]} | cferl_lib:cferl_error().
get_objects_names(Connection, Container) ->
  get_objects_names(Connection, Container, #cf_object_query_args{}).

%% @doc Retrieve the object names in the current container, filtered by the provided query arguments.
%%   If you supply the optional limit, marker, prefix or path arguments, the call will return the number of objects specified in limit,
%%   starting at the object index specified in marker, selecting objects whose names start with prefix or search within the pseudo-filesystem
%%   path.
-spec get_objects_names(#cf_connection{}, #cf_container{}, #cf_object_query_args{}) -> {ok, [binary()]} | cferl_lib:cferl_error().
get_objects_names(Connection, Container, QueryArgs) 
    when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container), is_record(QueryArgs, cf_object_query_args) ->
  QueryString = cferl_lib:object_query_args_to_string(QueryArgs),
  Result = cferl_connection:send_storage_request(Connection, get, Container#cf_container.container_path ++ QueryString, raw),
  get_objects_names_result(Result).

get_objects_names_result({ok, "204", _, _}) ->
  {ok, []};
get_objects_names_result({ok, "200", _, ResponseBody}) ->
  {ok, [list_to_binary(ObjectName) || ObjectName <- string:tokens(binary_to_list(ResponseBody), "\n")]};
get_objects_names_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve details for all the objects in the current container (within the limits imposed by Cloud Files server).
-spec get_objects_details(#cf_connection{}, #cf_container{}) -> {ok, [#cf_object_details{}]} | cferl_lib:cferl_error().
%%   Error = cferl_error()
get_objects_details(Connection, Container) ->
  get_objects_details(Connection, Container, #cf_object_query_args{}).
  
%% @doc Retrieve the object details in the current container, filtered by the provided query arguments.
%%   If you supply the optional limit, marker, prefix or path arguments, the call will return the number of objects specified in limit,
%%   starting at the object index specified in marker, selecting objects whose names start with prefix or search within the pseudo-filesystem
%%   path.
-spec get_objects_details(#cf_connection{}, #cf_container{}, #cf_object_query_args{}) -> 
  {ok, [#cf_object_details{}]} | cferl_lib:cferl_error().
get_objects_details(Connection, Container, QueryArgs) 
    when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container), is_record(QueryArgs, cf_object_query_args) ->
  QueryString = cferl_lib:object_query_args_to_string(QueryArgs),
  Result = cferl_connection:send_storage_request(Connection, get, Container#cf_container.container_path ++ QueryString, json),
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
-spec object_exists(#cf_connection{}, #cf_container{}, ObjectName::binary()) -> true | false.
object_exists(Connection, Container, ObjectName) when ?IS_CONNECTION(Connection), is_binary(ObjectName) ->
  Result = cferl_connection:send_storage_request(Connection, head, get_object_path(Container, ObjectName), raw),
  object_exists_result(Result).

object_exists_result({ok, ResponseCode, _, _})
  when ResponseCode =:= "200"; ResponseCode =:= "204" ->
  true;
object_exists_result(_) ->
  false.

%% @doc Get a reference to an existing storage object.
-spec get_object(#cf_connection{}, #cf_container{}, Name::binary()) -> {ok, #cf_object{}} | cferl_lib:cferl_error().
get_object(Connection, Container, ObjectName) 
    when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container), is_binary(ObjectName) ->
  Result = cferl_connection:send_storage_request(Connection, head, get_object_path(Container, ObjectName), raw),
  get_object_result(ObjectName, Container, Result).

get_object_result(ObjectName, Container, {ok, ResponseCode, ResponseHeaders, _})
  when ResponseCode =:= "200"; ResponseCode =:= "204" ->
  
  ObjectDetails = #cf_object_details{
    name = ObjectName,
    bytes = cferl_lib:get_int_header("Content-Length", ResponseHeaders),
    last_modified = httpd_util:convert_request_date(cferl_lib:get_string_header("Last-Modified", ResponseHeaders)),
    content_type = cferl_lib:get_binary_header("Content-Type", ResponseHeaders),
    etag = cferl_lib:get_binary_header("Etag", ResponseHeaders)
  },
  
  {ok, cferl_object:new(Container, ObjectDetails, get_object_path(Container, ObjectName), ResponseHeaders)};

get_object_result(_, _, Other) ->
  cferl_lib:error_result(Other).

%% @doc Create a reference to a new storage object.
%%   Nothing is actually created until data gets written in the object.
%%   If an object with the provided name already exists, a reference to this object is returned.
-spec create_object(#cf_connection{}, #cf_container{}, Name::binary()) -> {ok, #cf_object{}} | cferl_lib:cferl_error().
%%   Error = cferl_error()
create_object(Connection, Container, ObjectName) 
    when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container), is_binary(ObjectName) ->
  case get_object(Connection, Container, ObjectName) of
    {ok, Object} ->
      {ok, Object};
    _ ->
      ObjectDetails = #cf_object_details{name = ObjectName},
      {ok, cferl_object:new(Container, ObjectDetails, get_object_path(Container, ObjectName), [])}
  end.

%% @doc Delete an existing storage object.
-spec delete_object(#cf_connection{}, #cf_container{}, Name::binary()) -> ok | cferl_lib:cferl_error().
delete_object(Connection, Container, ObjectName)
    when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container), is_binary(ObjectName) ->
  Result = cferl_connection:send_storage_request(Connection, delete, get_object_path(Container, ObjectName), raw),
  delete_object_result(Result).

delete_object_result({ok, "204", _, _}) ->
  ok;
delete_object_result(Other) ->
  cferl_lib:error_result(Other).
  
%% @doc Ensure that all the directories exist in an object path.
%%   Passing &lt;&lt;"photos/plants/fern.jpg">>, will ensure that the &lt;&lt;"photos">> and &lt;&lt;"photos/plants">> directories exist.
-spec ensure_dir(#cf_connection{}, #cf_container{}, ObjectPath::binary()) -> ok.
ensure_dir(Connection, Container, ObjectPath) 
    when ?IS_CONNECTION(Connection), ?IS_CONTAINER(Container), is_binary(ObjectPath) ->
  CreateDirectoryFun =
    fun(Directory) ->
      {ok, DirectoryObject} = create_object(Connection, Container, Directory),
      % push the object on the server only if its content-type is not good
      case cferl_object:content_type(DirectoryObject) of
        ?DIRECTORY_OBJECT_CONTENT_TYPE ->
          noop;
        _ ->
          ok = cferl_object:write_data(Connection, DirectoryObject, <<>>,
                                       ?DIRECTORY_OBJECT_CONTENT_TYPE,
                                       [{<<"Content-Length">>, <<"0">>}])
      end
    end,
  
  lists:foreach(CreateDirectoryFun, cferl_lib:path_to_sub_dirs(ObjectPath)),
  ok.

%% Friend functions
-spec new(#cf_container_details{}, string(), [{atom(), term()}]) -> #cf_container{}.
new(ContainerDetails, Path, CdnDetails) ->
  #cf_container{container_details = ContainerDetails,
                container_path    = Path,
                cdn_details       = CdnDetails}.

%% Private functions
get_object_path(Container, ObjectName) when ?IS_CONTAINER(Container), is_binary(ObjectName) ->
  Container#cf_container.container_path ++ "/" ++ cferl_lib:url_encode(ObjectName).

bin_to_int(Bin) when is_binary(Bin) ->
  list_to_integer(binary_to_list(Bin)).

