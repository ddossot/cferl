%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
%%% @type cferl_connection() = term(). Reference to the cferl_connection parameterized module.
%%% @type cferl_error() = {error, not_found} | {error, unauthorized} | {error, {unexpected_response, Other}}.

-module(cferl).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

-export([connect/2]).
-define(APPLICATION, cferl).

%% @doc Authenticate and open connection.
%% @spec connect(Username, ApiKey) -> {ok, CloudFiles} | Error
%%   Username = string() | binary()
%%   ApiKey = string() | binary()
%%   CloudFiles = cferl_connection()
%%   Error = cferl_error()
connect(Username, ApiKey) when is_binary(Username), is_binary(ApiKey) ->
  connect(binary_to_list(Username),
          binary_to_list(ApiKey));
connect(Username, ApiKey) when is_list(Username), is_list(ApiKey) ->
  ensure_started(),
  
  Result = 
    ibrowse:send_req("https://" ++ ?API_BASE_URL ++ ":443" ++ ?VERSION_PATH,
                     [{"X-Auth-User", Username}, {"X-Auth-Key", ApiKey}],
                     get),
                     
  connect_result(Result).
  
%% Private functions

%% @doc Ensure started for the sake of verifying that required applications are running.
ensure_started() ->
  ensure_started(
    lists:any(fun({Application, _Description, _Vsn}) ->
                Application == ?APPLICATION
              end,
              application:which_applications())).
              
ensure_started(true) ->
  ok;
ensure_started(false) ->
  application:start(?APPLICATION).

connect_result({ok, "204", ResponseHeaders, _ResponseBody}) ->
  {ok, Version} = application:get_key(?APPLICATION, vsn),
  {ok, cferl_connection:new(Version,
                            cferl_lib:get_string_header("x-auth-token", ResponseHeaders),
                            cferl_lib:get_string_header("x-storage-url", ResponseHeaders),
                            cferl_lib:get_string_header("x-cdn-management-url", ResponseHeaders))};
connect_result(Other) ->
  cferl_lib:error_result(Other).

