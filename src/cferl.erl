%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

-export([connect/2]).

%% @doc Authenticate and open connection.
%% @spec connect(Username::string(), ApiKey::string()) -> {ok, CloudFiles} | {error, Reason}
%%   CloudFiles = term()
%%   Reason = term()
%% 
connect(Username, ApiKey) when is_list(Username), is_list(ApiKey) ->
  Result = 
    ibrowse:send_req("https://" ++ ?API_BASE_URL ++ ":443" ++ ?VERSION_PATH,
                     [{"X-Auth-User", Username}, {"X-Auth-Key", ApiKey}],
                     get),
                     
  handle_connect_result(Result).
  
%% Private functions
handle_connect_result({ok, "204", ResponseHeaders, _ResponseBody}) ->
  {ok, cferl_connection:new(cferl_lib:caseless_get_proplist_value("x-auth-token", ResponseHeaders),
                            cferl_lib:caseless_get_proplist_value("x-storage-url", ResponseHeaders),
                            cferl_lib:caseless_get_proplist_value("x-cdn-management-url", ResponseHeaders))};
handle_connect_result({ok, "401", _, _}) ->
  {error, unauthorized};
handle_connect_result({ok, ResponseStatus, ResponseHeaders, ResponseBody}) ->
  {error, {unexpected_status, ResponseStatus, ResponseHeaders, ResponseBody}};
handle_connect_result(Error = {error, _}) ->
  Error.

