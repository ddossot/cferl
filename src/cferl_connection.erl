%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_connection, [AuthToken, StorageUrl, CdnManagementUrl]).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

%% Public API
-export([get_account_info/0,
         get_containers_info/0, get_containers_info/2,
         create_container/1]).

%% Exposed for internal usage
-export([send_storage_request/2]).

%% FIXME comment + README
get_account_info() ->
  Result = send_storage_request("", head),
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
  {error, {unexpected_response, Other}}.

%% FIXME add: container_exists

%% FIXME add: get_containers_name

%% FIXME comment!
get_containers_info() ->
  get_containers_info("").
get_containers_info(Marker, Limit) when is_integer(Limit), is_list(Marker) ->
  get_containers_info("limit=" ++ integer_to_list(Limit)
                   ++ "&marker=" ++ ibrowse_lib:url_encode(Marker)).

get_containers_info(SelectionCriteria) when is_list(SelectionCriteria) ->
  QueryString = build_json_query_string(SelectionCriteria),
  Result = send_storage_request("?" ++ QueryString, get),
  get_containers_info_result(Result).

get_containers_info_result({ok, "204", _, _}) ->
  {ok, []};
get_containers_info_result({ok, "200", _, ResponseBody}) ->
  AtomizeKeysFun =
    fun({struct, Proplist}) ->
      #cf_container_info{
        name = proplists:get_value(<<"name">>, Proplist),
        bytes = proplists:get_value(<<"bytes">>, Proplist),
        count = proplists:get_value(<<"count">>, Proplist)
      }
    end,
    
  ContainersInfo = lists:map(AtomizeKeysFun,
                            mochijson2:decode(ResponseBody)), 
  {ok, ContainersInfo};
get_containers_info_result(Other) ->
  {error, {unexpected_response, Other}}.

%% FIXME add: get_container

%% FIXME comment! -> {ok, Container::term()} | {error, already_existing} | {error, {unexpected_response, Cause::term()}}
create_container(Name) when is_binary(Name) ->
  Result = send_storage_request(<<"/", Name/binary>>, put),
  create_container_result(Name, Result).

create_container_result(Name, {ok, "201", _, _}) ->
  {ok, cferl_container:new(THIS, Name)};
create_container_result(_, {ok, "202", _, _}) ->  
  {error, already_existing};
create_container_result(_, Other) ->
  {error, {unexpected_response, Other}}.
  
%% FIXME add: public_containers  

%% FIXME comment!
send_storage_request(PathAndQuery, Method)
  when is_binary(PathAndQuery), is_atom(Method) ->
    send_storage_request(binary_to_list(PathAndQuery), Method);
    
send_storage_request(PathAndQuery, Method)
  when is_list(PathAndQuery), is_atom(Method) ->
    ibrowse:send_req(StorageUrl ++ PathAndQuery,
                     [{"User-Agent", "cferl (CloudFiles Erlang API)"},
                     {"X-Auth-Token", AuthToken}],
                     Method).

%% Private functions
build_json_query_string("") ->
  "format=json";
build_json_query_string(Parameters) when is_list(Parameters) ->
  "format=json&" ++ Parameters.

get_int_header(Name, Headers) when is_list(Headers) ->
  list_to_integer(cferl_lib:caseless_get_proplist_value(Name, Headers)).

