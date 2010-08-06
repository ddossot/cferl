%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_connection, [AuthToken, StorageUrl, CdnManagementUrl]).
-author('David Dossot <david@dossot.net>').

-export([containers/0, containers/2]).

%% FIXME comment!
containers() ->
  do_containers("").
containers(Limit, Marker) when is_integer(Limit), is_list(Marker) ->
  do_containers("limit=" ++ integer_to_list(Limit)
             ++ "&marker=" ++ ibrowse_lib:url_encode(Marker)).

%% Private functions
do_containers(SelectionCriteria) when is_list(SelectionCriteria) ->
  QueryString = build_json_query_string(SelectionCriteria),
  
  Result = 
    ibrowse:send_req(StorageUrl ++ "?" ++ QueryString,
                     [{"X-Auth-Token", AuthToken}],
                     get),
                     
  handle_containers_result(Result).

handle_containers_result({ok, "204", _, _}) ->
  {ok, cferl_container:new(THIS, [])};
handle_containers_result({ok, "200", _, ResponseBody}) ->
  AtomizeKeysFun =
    fun({struct, Proplist}) ->
      [{list_to_atom(binary_to_list(K)), V} || {K,V} <- Proplist]
    end,
    
  Containers = lists:map(AtomizeKeysFun,
                         mochijson2:decode(ResponseBody)), 
  {ok, cferl_container:new(THIS, Containers)};
handle_containers_result(Other) ->
  {error, {unexpected_response, container_names_result, Other}}.
  
build_json_query_string("") ->
  "format=json";
build_json_query_string(Parameters) when is_list(Parameters) ->
  "format=json&" ++ Parameters.

