%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
%%% @type cferl_error() = {error, not_found} | {error, unauthorized} | {error, {unexpected_response, Other}}.
%%% @type cf_account_info() = record(). Record of type cf_account_info.
%%% @type cf_query_args() = record(). Record of type cf_query_args.
%%% @type cf_container_details() = record(). Record of type cf_container_details.

-module(cferl_connection, [AuthToken, StorageUrl, CdnManagementUrl]).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

%% Public API
-export([get_account_info/0,
         get_containers_names/0, get_containers_names/1,
         get_containers_details/0, get_containers_details/1,
         container_exists/1, create_container/1]).

%% Exposed for internal usage
-export([send_storage_request/3]).

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
         get_int_header("x-account-bytes-used", ResponseHeaders),
      container_count =
         get_int_header("x-account-container-count", ResponseHeaders)
  }};
get_account_info_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve all the containers names (within the limits imposed by Cloud Files server).
%% @spec get_containers_names() -> {ok, Names::[binary()]} | Error
%%   Error = cferl_error()
get_containers_names() ->
  Result = send_storage_request(get, "", raw),
  get_containers_names_result(Result).

%% @doc Retrieve the containers names filtered by the provided query arguments.
%% @spec get_containers_names(QueryArgs) -> {ok, Names::[binary()]} | Error
%%   QueryArgs = cf_query_args()
%%   Error = cferl_error()
get_containers_names(QueryArgs) when is_record(QueryArgs, cf_query_args) ->
  QueryString = cferl_lib:query_args_to_string(QueryArgs),
  Result = send_storage_request(get, QueryString, raw),
  get_containers_names_result(Result).

get_containers_names_result({ok, "200", _, ResponseBody}) ->
  {ok, [list_to_binary(Name) || Name <- string:tokens(ResponseBody, "\n")]};
get_containers_names_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Retrieve all the containers information (within the limits imposed by Cloud Files server).
%% @spec get_containers_details() -> {ok, [cf_container_details()]} | Error
%%   Error = cferl_error()
get_containers_details() ->
  Result = send_storage_request(get, "", json),
  get_containers_details_result(Result).

%% @doc Retrieve the containers information filtered by the provided query arguments.
%% @spec get_containers_details(QueryArgs) -> {ok, [cf_container_details()]} | Error
%%   QueryArgs = cf_query_args()
%%   Error = cferl_error()
get_containers_details(QueryArgs) when is_record(QueryArgs, cf_query_args) ->
  QueryString = cferl_lib:query_args_to_string(QueryArgs),
  Result = send_storage_request(get, QueryString, json),
  get_containers_details_result(Result).

get_containers_details_result({ok, "204", _, _}) ->
  {ok, []};
get_containers_details_result({ok, "200", _, ResponseBody}) ->
  AtomizeKeysFun =
    fun({struct, Proplist}) ->
      #cf_container_details{
        name = proplists:get_value(<<"name">>, Proplist),
        bytes = proplists:get_value(<<"bytes">>, Proplist),
        count = proplists:get_value(<<"count">>, Proplist)
      }
    end,
    
  ContainersInfo = lists:map(AtomizeKeysFun,
                            mochijson2:decode(ResponseBody)), 
  {ok, ContainersInfo};
get_containers_details_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Test the existence of a container.
%% @spec container_exists(Name::binary()) -> true | false
container_exists(Name) when is_binary(Name) ->
  Result = send_storage_request(head, <<"/", Name/binary>>, raw),
  container_exists_result(Result).

container_exists_result({ok, "204", _, _}) ->
  true;
container_exists_result(_) ->
  false.

%% TODO add: get_container

%% TODO comment! -> {ok, Container::term()} | {error, already_existing} | {error, {unexpected_response, Cause::term()}}
create_container(Name) when is_binary(Name) ->
  Result = send_storage_request(put, <<"/", Name/binary>>, raw),
  create_container_result(Name, Result).

create_container_result(Name, {ok, "201", _, _}) ->
  {ok, cferl_container:new(THIS, Name)};
create_container_result(_, {ok, "202", _, _}) ->  
  {error, already_existing};
create_container_result(_, Other) ->
  cferl_lib:error_result(Other).
  
%% TODO add: get_public_containers_names

%% Friend functions
%% @hidden
send_storage_request(Method, PathAndQuery, Accept)
  when is_atom(Method), is_binary(PathAndQuery), is_atom(Accept) ->
    send_storage_request(Method, binary_to_list(PathAndQuery), Accept);
    
send_storage_request(Method, PathAndQuery, raw)
  when is_atom(Method), is_list(PathAndQuery) ->
    do_send_storage_request(PathAndQuery, Method);
    
send_storage_request(Method, PathAndQuery, json)
  when is_atom(Method), is_list(PathAndQuery) ->
    do_send_storage_request(build_json_query_string(PathAndQuery),
                            Method).
  
do_send_storage_request(PathAndQuery, Method)
  when is_list(PathAndQuery), is_atom(Method) ->
    ibrowse:send_req(StorageUrl ++ PathAndQuery,
                     [{"User-Agent", "cferl (CloudFiles Erlang API)"},
                     {"X-Auth-Token", AuthToken}],
                     Method).

%% Private functions
build_json_query_string(PathAndQuery) when is_list(PathAndQuery) ->
  PathAndQuery ++
  case lists:member($?, PathAndQuery) of
    true -> "&";
    false -> "?"
  end ++
  "format=json".

get_int_header(Name, Headers) when is_list(Headers) ->
  list_to_integer(cferl_lib:caseless_get_proplist_value(Name, Headers)).

