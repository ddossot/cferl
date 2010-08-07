%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_integration_tests).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

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
  print_account_info(CloudFiles:get_account_info()),
  container_tests(CloudFiles),
  ok.
  
connect_test(Username, ApiKey) ->
  {error, unauthorized} = cferl:connect("_fake_user_name", "_fake_api_key"),
  {ok, CloudFiles} = cferl:connect(Username, ApiKey),
  io:format("Connection successful~n"),
  CloudFiles.

print_account_info({ok, Info}) ->
  io:format("Account info: ~p~n", [Info]).

container_tests(CloudFiles) ->
  % should return nothing
  print_containers_info(CloudFiles:get_containers_info(#cf_query_args{limit=0})),
  
  {ok, Container} = CloudFiles:create_container(<<"foo">>),
  io:format("Created container: ~p~n", [Container:name()]),
  
  print_containers_info(CloudFiles:get_containers_info(#cf_query_args{marker= <<"a">>})),
  
  ok = Container:delete(),
  io:format("Deleted container: ~p~n", [Container:name()]),
  
  print_containers_info(CloudFiles:get_containers_info()),
  ok.

print_containers_info({ok, ContainersInfo}) ->
  io:format("Found ~B container(s): ~p~n",
            [length(ContainersInfo), ContainersInfo]).

