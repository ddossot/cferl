%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_connection, [AuthToken, StorageUrl, CdnManagementUrl]).
-author('David Dossot <david@dossot.net>').

%% Public API
-export([get_info/0,
         containers/0, containers/2, create_container/1]).

%% Exposed for internal usage
-export([send_storage_request/2]).

%% FIXME comment + README
get_info() ->
  Result = send_storage_request("", head),
  handle_get_info_result(Result).

%% FIXME add: container_exists
%% FIXME add: container

%% FIXME comment!
containers() ->
  do_containers("").
containers(Marker, Limit) when is_integer(Limit), is_list(Marker) ->
  do_containers("limit=" ++ integer_to_list(Limit)
             ++ "&marker=" ++ ibrowse_lib:url_encode(Marker)).

%% FIXME comment! -> {ok, Container::term()} | {error, already_existing} | {error, Cause::term()}
create_container(Name) when is_binary(Name) ->
  Result = send_storage_request(<<"/", Name/binary>>, put),
  hanle_new_container_result(Name, Result).

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
handle_get_info_result({ok, "204", ResponseHeaders, _}) ->
  {ok, [{account_bytes_used,
         list_to_integer(cferl_lib:caseless_get_proplist_value("x-account-bytes-used", ResponseHeaders))},
        {account_container_count,
         list_to_integer(cferl_lib:caseless_get_proplist_value("x-account-container-count", ResponseHeaders))}
       ]};
handle_get_info_result(Other) ->
  {error, {unexpected_response, Other}}.

do_containers(SelectionCriteria) when is_list(SelectionCriteria) ->
  QueryString = build_json_query_string(SelectionCriteria),
  Result = send_storage_request("?" ++ QueryString, get),
  handle_containers_result(Result).

handle_containers_result({ok, "204", _, _}) ->
  {ok, cferl_containers:new(THIS, [])};
handle_containers_result({ok, "200", _, ResponseBody}) ->
  AtomizeKeysFun =
    fun({struct, Proplist}) ->
      [{list_to_atom(binary_to_list(K)), V} || {K,V} <- Proplist]
    end,
    
  Containers = lists:map(AtomizeKeysFun,
                         mochijson2:decode(ResponseBody)), 
  {ok, cferl_containers:new(THIS, Containers)};
handle_containers_result(Other) ->
  {error, {unexpected_response, Other}}.

hanle_new_container_result(Name, {ok, "201", _, _}) ->
  {ok, cferl_container:new(THIS, Name)};
hanle_new_container_result(_, {ok, "202", _, _}) ->  
  {error, already_existing};
hanle_new_container_result(_, Other) ->
  {error, {unexpected_response, Other}}.

build_json_query_string("") ->
  "format=json";
build_json_query_string(Parameters) when is_list(Parameters) ->
  "format=json&" ++ Parameters.

