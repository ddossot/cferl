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
  container_test(CloudFiles:containers()),
  container_test(CloudFiles:containers(0, "")),
  container_test(CloudFiles:containers(100, "a")).

container_test({ok, Containers}) ->
  io:format("Found ~B container(s) named: ~p~n",
            [Containers:size(), Containers:names()]).

