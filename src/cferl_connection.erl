%%%
%%% @doc Management of an account's containers.
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

-module(cferl_connection).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

%% Public API
-export([get_account_info/1,
         get_containers_names/1, 
         get_containers_names/2,
         get_containers_details/1, 
         get_containers_details/2,
         container_exists/2, 
         get_container/2, 
         create_container/2, 
         delete_container/2,
         get_public_containers_names/2]).

%% Exposed for internal usage
-export([new/4,
         send_storage_request/4, 
         send_storage_request/5, 
         send_storage_request/6,
         send_cdn_management_request/4, 
         send_cdn_management_request/5,
         async_response_loop/1]).


%% @doc Retrieve the account information.
-spec get_account_info(#cf_connection{}) -> {ok, #cf_account_info{}} | cferl_lib:cferl_error(). 
get_account_info(Conn) when ?IS_CONNECTION(Conn) ->
  Result = send_storage_request(Conn, head, "", raw),
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
-spec get_containers_names(#cf_connection{}) -> {ok, [binary()]} | cferl_lib:cferl_error().
get_containers_names(Conn) when ?IS_CONNECTION(Conn) ->
  Result = send_storage_request(Conn, get, "", raw),
  get_containers_names_result(Result).

%% @doc Retrieve the containers names filtered by the provided query arguments.
%%   If you supply the optional limit and marker arguments, the call will return the number of containers specified in limit, starting after the object named in marker.
-spec get_containers_names(#cf_connection{}, #cf_container_query_args{}) -> {ok, [binary()]} | cferl_lib:cferl_error().
get_containers_names(Conn, QueryArgs) when ?IS_CONNECTION(Conn), is_record(QueryArgs, cf_container_query_args) ->
  QueryString = cferl_lib:container_query_args_to_string(QueryArgs),
  Result = send_storage_request(Conn, get, QueryString, raw),
  get_containers_names_result(Result).

get_containers_names_result({ok, "204", _, _}) ->
  {ok, []};
get_containers_names_result({ok, "200", _, ResponseBody}) ->
  {ok, [list_to_binary(Name) || Name <- string:tokens(binary_to_list(ResponseBody), "\n")]};
get_containers_names_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve all the containers information (within the limits imposed by Cloud Files server).
-spec get_containers_details(#cf_connection{}) -> {ok, [#cf_container_details{}]} | cferl_lib:cferl_error().
get_containers_details(Conn) when ?IS_CONNECTION(Conn) ->
  get_containers_details(Conn, #cf_container_query_args{}).

%% @doc Retrieve the containers information filtered by the provided query arguments.
-spec get_containers_details(#cf_connection{}, #cf_container_query_args{}) -> 
  {ok, [#cf_container_details{}]} | cferl_lib:cferl_error().
get_containers_details(Conn, QueryArgs) when ?IS_CONNECTION(Conn), is_record(QueryArgs, cf_container_query_args) ->
  QueryString = cferl_lib:container_query_args_to_string(QueryArgs),
  Result = send_storage_request(Conn, get, QueryString, json),
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
-spec container_exists(#cf_connection{}, Name::binary()) -> true | false.
container_exists(Conn, Name) when ?IS_CONNECTION(Conn), is_binary(Name) ->
  Result = send_storage_request(Conn, head, get_container_path(Name), raw),
  container_exists_result(Result).

container_exists_result({ok, "204", _, _}) ->
  true;
container_exists_result(_) ->
  false.

%% @doc Get a reference to an existing container.
-spec get_container(#cf_connection{}, Name::binary()) -> {ok, #cf_container{}} | cferl_lib:cferl_error(). 
get_container(Conn, Name) when ?IS_CONNECTION(Conn), is_binary(Name) ->
  Result = send_storage_request(Conn, head, get_container_path(Name), raw),
  get_container_result(Conn, Name, Result).

get_container_result(Conn, Name, {ok, "204", ResponseHeaders, _}) ->
  ContainerDetails = #cf_container_details{
        name = Name,
        bytes = cferl_lib:get_int_header("x-container-bytes-used", ResponseHeaders),
        count = cferl_lib:get_int_header("x-container-object-count", ResponseHeaders)
      },
  {ok, CdnDetails} = get_container_cdn_details(Conn, Name),
  {ok, cferl_container:new(ContainerDetails, get_container_path(Name), CdnDetails)};
get_container_result(_, _, Other) ->
  cferl_lib:error_result(Other).

get_container_cdn_details(Conn, Name) ->
  Result = send_cdn_management_request(Conn, head, get_container_path(Name), raw),
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
-spec create_container(#cf_connection{}, Name::binary()) -> 
  {ok, #cf_container{}} | {error, already_existing} | cferl_lib:cferl_error().
create_container(Conn, Name) when ?IS_CONNECTION(Conn), is_binary(Name) ->
  Result = send_storage_request(Conn, put, get_container_path(Name), raw),
  create_container_result(Conn, Name, Result).

create_container_result(Conn, Name, {ok, "201", _, _}) ->
  get_container(Conn, Name);
create_container_result(_, _, {ok, "202", _, _}) ->  
  {error, already_existing};
create_container_result(_, _, Other) ->
  cferl_lib:error_result(Other).
  
%% @doc Delete a container (which must be empty).
-spec delete_container(#cf_connection{}, Name::binary()) -> ok | {error, not_empty} | cferl_lib:cferl_error().
%%   Error = {error, not_empty} | cferl_error()
delete_container(Conn, Name) when ?IS_CONNECTION(Conn), is_binary(Name) ->
  Result = send_storage_request(Conn, delete, get_container_path(Name), raw),
  delete_container_result(Result).

delete_container_result({ok, "204", _, _}) ->
  ok;
delete_container_result({ok, "409", _, _}) ->
  {error, not_empty};
delete_container_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve the names of public (CDN-enabled) containers, whether they are still public (active) or happen to have been exposed in the past(all_time).
-spec get_public_containers_names(#cf_connection{}, TimeFilter::active | all_time) -> {ok, [binary()]} | cferl_lib:cferl_error(). 
get_public_containers_names(Conn, active) when ?IS_CONNECTION(Conn) ->
  Result = send_cdn_management_request(Conn, get, "?enabled_only=true", raw),
  get_public_containers_names_result(Result);
get_public_containers_names(Conn, all_time) when ?IS_CONNECTION(Conn) ->
  Result = send_cdn_management_request(Conn, get, "", raw),
  get_public_containers_names_result(Result).

get_public_containers_names_result({ok, "204", _, _}) ->
  {ok, []};
get_public_containers_names_result({ok, "200", _, ResponseBody}) ->
  {ok, [list_to_binary(Name) || Name <- string:tokens(binary_to_list(ResponseBody), "\n")]};
get_public_containers_names_result(Other) ->
  cferl_lib:error_result(Other).

%% Friend functions
%% @hidden
-spec new(Version::string(), 
          AuthToken :: string(), 
          StorageUrl :: string(),
          CdnManagementUrl :: string()) -> #cf_connection{}.
new(Version, AuthToken, StorageUrl, CdnManagementUrl) ->
  #cf_connection{
    version            = Version,
    auth_token         = AuthToken,
    storage_url        = StorageUrl,
    cdn_management_url = CdnManagementUrl }.

%% @hidden
send_storage_request(Connection, Method, PathAndQuery, Accept)
  when is_atom(Method),
       is_atom(Accept) or is_function(Accept, 1) ->
       
    send_storage_request(Connection, Method, PathAndQuery, [], Accept).

send_storage_request(Connection, Method, PathAndQuery, Headers, Accept)
  when is_atom(Method), is_list(Headers),
       is_atom(Accept) or is_function(Accept, 1) ->
       
    send_request(Connection#cf_connection.storage_url, 
                 Method, PathAndQuery, 
                 handle_headers(Connection, Headers), 
                 <<>>, Accept).

send_storage_request(Connection, Method, PathAndQuery, Headers, Body, Accept)
  when is_atom(Method), is_list(Headers),
       is_binary(Body) or is_function(Body, 0),
       is_atom(Accept) or is_function(Accept, 1) ->
       
    send_request(Connection#cf_connection.storage_url, 
                 Method, PathAndQuery, 
                 handle_headers(Connection, Headers), 
                 Body, Accept).

%% @hidden
send_cdn_management_request(Connection, Method, PathAndQuery, Accept)
  when is_atom(Method), is_atom(Accept) ->
    send_request(Connection#cf_connection.cdn_management_url, 
                 Method, PathAndQuery, 
                 handle_headers(Connection, []), 
                 <<>>, Accept).

%% @hidden
send_cdn_management_request(Connection, Method, PathAndQuery, Headers, Accept)
  when is_atom(Method), is_list(Headers), is_atom(Accept) ->
    send_request(Connection#cf_connection.cdn_management_url, 
                 Method, PathAndQuery, 
                 handle_headers(Connection, Headers), 
                 <<>>, Accept).

%% @hidden
get_container_path(Name) when is_binary(Name) ->
  "/" ++ cferl_lib:url_encode(Name).
  
%% Private functions
send_request(BaseUrl, Method, PathAndQuery, Headers, Body, Accept)
  when is_atom(Method), is_binary(PathAndQuery), is_list(Headers), is_binary(Body),
       is_atom(Accept) or is_function(Accept, 1) ->
       
    send_request(BaseUrl, Method, binary_to_list(PathAndQuery), Headers, Body, Accept);
    
send_request(BaseUrl, Method, PathAndQuery, Headers, Body, ResultFun)
  when is_atom(Method), is_list(PathAndQuery), is_list(Headers),
       is_binary(Body) or is_function(Body, 0),
       is_function(ResultFun, 1) ->

    ResultPid = proc_lib:spawn(fun() -> async_response_loop(ResultFun) end), 
    Options = [{stream_to, {ResultPid, once}}],
    do_send_request(BaseUrl, Method, PathAndQuery, Headers, Body, Options);

send_request(BaseUrl, Method, PathAndQuery, Headers, Body, raw)
  when is_atom(Method), is_list(PathAndQuery), is_list(Headers),
       is_binary(Body) or is_function(Body, 0) ->
       
    do_send_request(BaseUrl, Method, PathAndQuery, Headers, Body, []);
    
send_request(BaseUrl, Method, PathAndQuery, Headers, Body, json)
  when is_atom(Method), is_list(PathAndQuery), is_list(Headers),
       is_binary(Body) or is_function(Body, 0) ->
       
    do_send_request(BaseUrl,
                    Method,
                    build_json_query_string(PathAndQuery),
                    Headers,
                    Body,
                    []).
  
do_send_request(BaseUrl, Method, PathAndQuery, Headers, Body, Options)
  when is_list(BaseUrl), is_list(PathAndQuery), is_list(Headers), is_atom(Method),
       is_binary(Body) or is_function(Body, 0),
       is_list(Options) ->
       
    ibrowse:send_req(BaseUrl ++ PathAndQuery,
                      cferl_lib:binary_headers_to_string(Headers),
                      Method,
                      Body,
                      [{response_format, binary} | Options]).

handle_headers(Connection, Headers) ->
  [{"User-Agent", "cferl (CloudFiles Erlang API) v" ++ Connection#cf_connection.version}, 
   {"X-Auth-Token", Connection#cf_connection.auth_token} 
   | Headers].

build_json_query_string(PathAndQuery) when is_list(PathAndQuery) ->
  PathAndQuery ++
  case lists:member($?, PathAndQuery) of
    true -> "&";
    false -> "?"
  end ++
  "format=json".

%% @hidden
async_response_loop(ResultFun) when is_function(ResultFun, 1) ->
  receive
    {ibrowse_async_headers, Req_id, StatCode, _ResponseHeaders} ->
      case StatCode of
        [$2|_] ->
          stream_next_chunk(ResultFun, Req_id);
          
        nomatch ->
          ResultFun({error, {unexpected_status_code, StatCode}})
      end;
      
    {ibrowse_async_response, _Req_id, Error = {error, _}} ->
      ResultFun(Error);
      
    {ibrowse_async_response, Req_id, Data} ->
      ResultFun({ok, Data}),
      stream_next_chunk(ResultFun, Req_id);
      
    {ibrowse_async_response_end, _Req_id} ->
      ResultFun(eof)
    
  after ?DEFAULT_REQUEST_TIMEOUT ->
    ResultFun({error, time_out})
  end.
  
stream_next_chunk(ResultFun, Req_id) ->
  case ibrowse:stream_next(Req_id) of
    ok ->
      async_response_loop(ResultFun);
    Error ->
      ResultFun(Error)
    end.

