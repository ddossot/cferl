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
  print_info(CloudFiles:get_account_info()),
  container_tests(CloudFiles),
  ok.

print_info({ok, Info}) ->
  io:format("Account info: ~p~n", [Info]).
  
connect_test(Username, ApiKey) ->
  {error, unauthorized} = cferl:connect("_fake_user_name", "_fake_api_key"),
  {ok, CloudFiles} = cferl:connect(Username, ApiKey),
  io:format("Connection successful~n"),
  CloudFiles.

container_tests(CloudFiles) ->
  print_containers(CloudFiles:get_containers_info()),
  print_containers(CloudFiles:get_containers_info("", 0)),
  
  {ok, Container} = CloudFiles:create_container(<<"foo">>),
  io:format("Created container: ~p~n", [Container:name()]),
  
  print_containers(CloudFiles:get_containers_info("a", 100)),
  
  ok = Container:delete(),
  io:format("Deleted container: ~p~n", [Container:name()]),
  
  print_containers(CloudFiles:get_containers_info()).

print_containers({ok, Containers}) ->
  io:format("Found ~B container(s) named: ~p~n",
            [Containers:size(), Containers:names()]).

