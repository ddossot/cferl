%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_integration_tests).
-author('David Dossot <david@dossot.net>').

-export([start/0]).

start() ->
  application:start(sasl),
  application:start(ssl),
  application:start(ibrowse),

  {ok, [Username]} = io:fread("Username : ", "~s"),
  {ok, [ApiKey]} = io:fread("API Key : ", "~s"),
  run_tests(Username, ApiKey),
  init:stop().
  
%% Tests
run_tests(Username, ApiKey) ->
  CloudFiles = connect_test(Username, ApiKey),
  container_tests(CloudFiles),
  ok.

connect_test(Username, ApiKey) ->
  {error, unauthorized} = cferl:connect("_fake_user_name", "_fake_api_key"),
  {ok, CloudFiles} = cferl:connect(Username, ApiKey),
  io:format("Connection successful~n"),
  CloudFiles.

container_tests(CloudFiles) ->
  container_test(CloudFiles, CloudFiles:container_names()),
  container_test(CloudFiles, CloudFiles:container_names(0, "")),
  container_test(CloudFiles, CloudFiles:container_names(100, "a")).

container_test(_, {ok, []}) ->
  io:format("No file container found~n");
container_test(CloudFiles, {ok, Containers}) ->
  io:format("Found file container names: ~p~n", [Containers]).

