%%%
%%% @doc Authentication and connection with Rackspace Cloud Files.
%%% @author David Dossot <david@dossot.net>
%%% @author Tilman Holschuh <tilman.holschuh@gmail.com>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

-export([connect/2, connect/3]).
-define(APPLICATION, cferl).

-type(username() :: string() | binary()).
-type(api_key() :: string() | binary()).
-type(auth_service() :: string() | binary() | us | uk).

%% @doc Authenticate and open connection (US).
-spec connect(username(), api_key()) -> {ok, #cf_connection{}} | cferl_lib:cferl_error().
connect(Username, ApiKey) when is_binary(Username), is_binary(ApiKey) ->
  connect(binary_to_list(Username),
          binary_to_list(ApiKey));
connect(Username, ApiKey) when is_list(Username), is_list(ApiKey) ->
  connect(Username, ApiKey, us).

%% @doc Authenticate and open connection.
-spec connect(username(), api_key(), auth_service()) -> {ok, #cf_connection{}} | cferl_lib:cferl_error(). 
connect(Username, ApiKey, us) ->  
  AuthUrl = "https://" ++ ?US_API_BASE_URL ++ ":443" ++ ?VERSION_PATH,
  connect(Username, ApiKey, AuthUrl);
connect(Username, ApiKey, uk)  ->  
  AuthUrl = "https://" ++ ?UK_API_BASE_URL ++ ":443" ++ ?VERSION_PATH,
  connect(Username, ApiKey, AuthUrl);
connect(Username, ApiKey, AuthUrl) when is_binary(Username), is_binary(ApiKey), is_binary(AuthUrl) ->  
  connect(binary_to_list(Username), binary_to_list(ApiKey), binary_to_list(AuthUrl));
connect(Username, ApiKey, AuthUrl) when is_list(Username), is_list(ApiKey), is_list(AuthUrl) ->
  ensure_started(),
  
  Result = 
    ibrowse:send_req(AuthUrl,
                     [{"X-Auth-User", Username}, {"X-Auth-Key", ApiKey}],
                     get),
                     
  connect_result(Result).
  
%% Private functions

%% @doc Ensure started for the sake of verifying that required applications are running.
ensure_started() ->
  ensure_started(?APPLICATION).

ensure_started(App) ->
  ensure_started(App, application:start(App)).

ensure_started(_App, ok ) -> ok;
ensure_started(_App, {error, {already_started, _App}}) -> ok;
ensure_started(App, {error, {not_started, Dep}}) ->
  ok = ensure_started(Dep),
  ensure_started(App);
ensure_started(App, {error, Reason}) ->
  erlang:error({app_start_failed, App, Reason}).

connect_result({ok, "204", ResponseHeaders, _ResponseBody}) ->
  {ok, Version} = application:get_key(?APPLICATION, vsn),
  {ok, cferl_connection:new(Version,
                            cferl_lib:get_string_header("x-auth-token", ResponseHeaders),
                            cferl_lib:get_string_header("x-storage-url", ResponseHeaders),
                            cferl_lib:get_string_header("x-cdn-management-url", ResponseHeaders))};
connect_result(Other) ->
  cferl_lib:error_result(Other).

