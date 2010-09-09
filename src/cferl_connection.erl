%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
%%% @type cferl_error() = {error, not_found} | {error, unauthorized} | {error, {unexpected_response, Other}}.
%%% @type cf_account_info() = record(). Record of type cf_account_info.
%%% @type cf_container_query_args() = record(). Record of type cf_container_query_args.
%%% @type cf_container_details() = record(). Record of type cf_container_details.
%%% @type cferl_container() = term(). Reference to the cferl_container parameterized module.

-module(cferl_connection, [Version, AuthToken, StorageUrl, CdnManagementUrl]).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

%% Public API
-export([get_account_info/0,
         get_containers_names/0, get_containers_names/1,
         get_containers_details/0, get_containers_details/1,
         container_exists/1, get_container/1, create_container/1, delete_container/1,
         get_public_containers_names/1]).

%% Exposed for internal usage
-export([send_storage_request/3, send_storage_request/4, send_storage_request/5,
         send_cdn_management_request/3, send_cdn_management_request/4]).

%% @doc Retrieve the account information.
%% @spec get_account_info() -> {ok, AccountInfo} | Error
%%   AccountInfo = cf_account_info()
%%   Error = cferl_error()
get_account_info() ->
  Result = send_storage_request(head, "", raw),
  get_account_info_result(Result).
  
get_account_info_result({ok, "204", ResponseHeaders, _}) ->
  {ok,
    #cf_account_info{ 
      bytes_used = 
         cferl_lib:get_int_header("x-account-bytes-used", ResponseHeaders),
      container_count =
         cferl_lib:get_int_header("x-account-container-count", ResponseHeaders)
  }};
get_account_info_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve all the containers names (within the limits imposed by Cloud Files server).
%% @spec get_containers_names() -> {ok, [binary()]} | Error
%%   Error = cferl_error()
get_containers_names() ->
  Result = send_storage_request(get, "", raw),
  get_containers_names_result(Result).

%% @doc Retrieve the containers names filtered by the provided query arguments.
%%   If you supply the optional limit and marker arguments, the call will return the number of containers specified in limit, starting after the object named in marker.
%% @spec get_containers_names(QueryArgs) -> {ok, [binary()]} | Error
%%   QueryArgs = cf_container_query_args()
%%   Error = cferl_error()
get_containers_names(QueryArgs) when is_record(QueryArgs, cf_container_query_args) ->
  QueryString = cferl_lib:container_query_args_to_string(QueryArgs),
  Result = send_storage_request(get, QueryString, raw),
  get_containers_names_result(Result).

get_containers_names_result({ok, "204", _, _}) ->
  {ok, []};
get_containers_names_result({ok, "200", _, ResponseBody}) ->
  {ok, [list_to_binary(Name) || Name <- string:tokens(ResponseBody, "\n")]};
get_containers_names_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve all the containers information (within the limits imposed by Cloud Files server).
%% @spec get_containers_details() -> {ok, [cf_container_details()]} | Error
%%   Error = cferl_error()
get_containers_details() ->
  get_containers_details(#cf_container_query_args{}).

%% @doc Retrieve the containers information filtered by the provided query arguments.
%% @spec get_containers_details(QueryArgs) -> {ok, [cf_container_details()]} | Error
%%   QueryArgs = cf_container_query_args()
%%   Error = cferl_error()
get_containers_details(QueryArgs) when is_record(QueryArgs, cf_container_query_args) ->
  QueryString = cferl_lib:container_query_args_to_string(QueryArgs),
  Result = send_storage_request(get, QueryString, json),
  get_containers_details_result(Result).

get_containers_details_result({ok, "204", _, _}) ->
  {ok, []};
get_containers_details_result({ok, "200", _, ResponseBody}) ->
  BuildRecordFun =
    fun({struct, Proplist}) ->
      #cf_container_details{
        name = proplists:get_value(<<"name">>, Proplist),
        bytes = proplists:get_value(<<"bytes">>, Proplist),
        count = proplists:get_value(<<"count">>, Proplist)
      }
    end,
    
  ContainersInfo = lists:map(BuildRecordFun,
                             mochijson2:decode(ResponseBody)), 
  {ok, ContainersInfo};
get_containers_details_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Test the existence of a container.
%% @spec container_exists(Name::binary()) -> true | false
container_exists(Name) when is_binary(Name) ->
  Result = send_storage_request(head, get_container_path(Name), raw),
  container_exists_result(Result).

container_exists_result({ok, "204", _, _}) ->
  true;
container_exists_result(_) ->
  false.

%% @doc Get a reference to an existing container.
%% @spec get_container(Name::binary) -> {ok, Container} | Error
%%   Container = cferl_container()
%%   Error = cferl_error()
get_container(Name) when is_binary(Name) ->
  Result = send_storage_request(head, get_container_path(Name), raw),
  get_container_result(Name, Result).

get_container_result(Name, {ok, "204", ResponseHeaders, _}) ->
  ContainerDetails = #cf_container_details{
        name = Name,
        bytes = cferl_lib:get_int_header("x-container-bytes-used", ResponseHeaders),
        count = cferl_lib:get_int_header("x-container-object-count", ResponseHeaders)
      },
  {ok, CdnDetails} = get_container_cdn_details(Name),
  {ok, cferl_container:new(THIS, ContainerDetails, get_container_path(Name), CdnDetails)};
get_container_result(_, Other) ->
  cferl_lib:error_result(Other).

get_container_cdn_details(Name) ->
  Result = send_cdn_management_request(head, get_container_path(Name), raw),
  get_container_cdn_details_result(Result).

get_container_cdn_details_result({ok, "204", ResponseHeaders, _}) ->
  {ok, build_cdn_details_proplist(ResponseHeaders)};
get_container_cdn_details_result(_Other) ->
  {ok, build_cdn_details_proplist([])}.

build_cdn_details_proplist(Headers) ->
  [
    {cdn_enabled, cferl_lib:get_boolean_header("x-cdn-enabled", Headers)},
    {ttl, cferl_lib:get_int_header("x-ttl", Headers)},
    {cdn_uri, cferl_lib:get_binary_header("x-cdn-uri", Headers)},
    {user_agent_acl, cferl_lib:get_binary_header("x-user-agent-acl", Headers)},
    {referrer_acl, cferl_lib:get_binary_header("x-referrer-acl", Headers)},
    {log_retention, cferl_lib:get_boolean_header("x-log-retention", Headers)}
  ].
  
%% @doc Create a new container (name must not be already used).
%% @spec create_container(Name::binary) -> {ok, Container} | Error
%%   Container = cferl_container()
%%   Error = {error, already_existing} | cferl_error()
create_container(Name) when is_binary(Name) ->
  Result = send_storage_request(put, get_container_path(Name), raw),
  create_container_result(Name, Result).

create_container_result(Name, {ok, "201", _, _}) ->
  get_container(Name);
create_container_result(_, {ok, "202", _, _}) ->  
  {error, already_existing};
create_container_result(_, Other) ->
  cferl_lib:error_result(Other).
  
%% @doc Delete a container (which must be empty).
%% @spec delete_container(Name::binary) -> ok | Error
%%   Error = {error, not_empty} | cferl_error()
delete_container(Name) when is_binary(Name) ->
  Result = send_storage_request(delete, get_container_path(Name), raw),
  delete_container_result(Result).

delete_container_result({ok, "204", _, _}) ->
  ok;
delete_container_result({ok, "409", _, _}) ->
  {error, not_empty};
delete_container_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve the names of public (CDN-enabled) containers, whether they are still public (active) or happen to have been exposed in the past(all_time).
%% @spec get_public_containers_names(TimeFilter::active | all_time) -> {ok, [binary()]} | Error
%%   Error = cferl_error()
get_public_containers_names(active) ->
  Result = send_cdn_management_request(get, "?enabled_only=true", raw),
  get_public_containers_names_result(Result);
get_public_containers_names(all_time) ->
  Result = send_cdn_management_request(get, "", raw),
  get_public_containers_names_result(Result).

get_public_containers_names_result({ok, "204", _, _}) ->
  {ok, []};
get_public_containers_names_result({ok, "200", _, ResponseBody}) ->
  {ok, [list_to_binary(Name) || Name <- string:tokens(ResponseBody, "\n")]};
get_public_containers_names_result(Other) ->
  cferl_lib:error_result(Other).

%% Friend functions
%% @hidden
send_storage_request(Method, PathAndQuery, Accept)
  when is_atom(Method), is_atom(Accept) ->
    send_storage_request(Method, PathAndQuery, [], Accept).

send_storage_request(Method, PathAndQuery, Headers, Accept)
  when is_atom(Method), is_list(Headers), is_atom(Accept) ->
    send_request(StorageUrl, Method, PathAndQuery, Headers, <<>>, Accept).

send_storage_request(Method, PathAndQuery, Headers, Body, Accept)
  when is_atom(Method), is_list(Headers), is_binary(Body), is_atom(Accept) ->
    send_request(StorageUrl, Method, PathAndQuery, Headers, Body, Accept).

%% @hidden
send_cdn_management_request(Method, PathAndQuery, Accept)
  when is_atom(Method), is_atom(Accept) ->
    send_request(CdnManagementUrl, Method, PathAndQuery, [], <<>>, Accept).

%% @hidden
send_cdn_management_request(Method, PathAndQuery, Headers, Accept)
  when is_atom(Method), is_list(Headers), is_atom(Accept) ->
    send_request(CdnManagementUrl, Method, PathAndQuery, Headers, <<>>, Accept).

%% @hidden
get_container_path(Name) when is_binary(Name) ->
  "/" ++ cferl_lib:url_encode(Name).
  
%% Private functions
send_request(BaseUrl, Method, PathAndQuery, Headers, Body, Accept)
  when is_atom(Method), is_binary(PathAndQuery), is_list(Headers), is_binary(Body), is_atom(Accept) ->
    send_request(BaseUrl, Method, binary_to_list(PathAndQuery), Headers, Body, Accept);
    
send_request(BaseUrl, Method, PathAndQuery, Headers, Body, raw)
  when is_atom(Method), is_list(PathAndQuery), is_list(Headers), is_binary(Body) ->
    do_send_request(BaseUrl, Method, PathAndQuery, Headers, Body);
    
send_request(BaseUrl, Method, PathAndQuery, Headers, Body, json)
  when is_atom(Method), is_list(PathAndQuery), is_list(Headers), is_binary(Body) ->
    do_send_request(BaseUrl,
                    Method,
                    build_json_query_string(PathAndQuery),
                    Headers,
                    Body).
  
do_send_request(BaseUrl, Method, PathAndQuery, Headers, Body)
  when is_list(BaseUrl), is_list(PathAndQuery), is_list(Headers), is_atom(Method), is_binary(Body) ->
    ibrowse:send_req(BaseUrl ++ PathAndQuery,
                     [{"User-Agent", "cferl (CloudFiles Erlang API) v" ++ Version},
                      {"X-Auth-Token", AuthToken} | Headers],
                      Method,
                      Body).

build_json_query_string(PathAndQuery) when is_list(PathAndQuery) ->
  PathAndQuery ++
  case lists:member($?, PathAndQuery) of
    true -> "&";
    false -> "?"
  end ++
  "format=json".

