%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_connection, [AuthToken, StorageUrl, CdnManagementUrl]).
-author('David Dossot <david@dossot.net>').

-export([container_names/0, container_names/2]).

%% FIXME comment!
container_names() ->
  do_container_names("").
container_names(Limit, Marker) when is_integer(Limit), is_list(Marker) ->
  do_container_names("limit=" ++ integer_to_list(Limit)
                  ++ "&marker=" ++ ibrowse_lib:url_encode(Marker)).

%% Private functions
do_container_names(SelectionCriteria) when is_list(SelectionCriteria) ->
  QueryString = build_json_query_string(SelectionCriteria),
  
  Result = 
    ibrowse:send_req(StorageUrl ++ "?" ++ QueryString,
                     [{"X-Auth-Token", AuthToken}],
                     get),
                     
  handle_container_names_result(Result).

handle_container_names_result({ok, "204", _, _}) ->
  {ok, []};
handle_container_names_result({ok, "200", _, ResponseBody}) ->
  ExtractorFun =
    fun({struct, Proplist}) ->
      NameBin = proplists:get_value(<<"name">>, Proplist),
      binary_to_list(NameBin)
    end,
    
  ContainerNames = lists:map(ExtractorFun, mochijson2:decode(ResponseBody)), 
  {ok, ContainerNames};
handle_container_names_result(Other) ->
  {error, {unexpected_response, container_names_result, Other}}.
  
build_json_query_string("") ->
  "format=json";
build_json_query_string(Parameters) when is_list(Parameters) ->
  "format=json&" ++ Parameters.

