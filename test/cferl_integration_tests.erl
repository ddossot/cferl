%%%
%%% @doc Integration tests and demo code generation.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_integration_tests).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

-export([start/0]).
-define(PRINT_CODE(Code), io:format("    ~s~n", [Code])).
-define(PRTFM_CODE(Format, Data), ?PRINT_CODE(io_lib:format(Format, Data))).
-define(PRINT_CALL(Call),
          io:format("    ~s.~n", [re:replace(??Call, " ", "", [global])]),
          Call).

start() ->
  application:start(ssl),
  application:start(ibrowse),

  {ok, [Username]} = io:fread("Username : ", "~s"),
  {ok, [ApiKey]} = io:fread("API Key : ", "~s"),
  io:format("~n"),
  run_tests(Username, ApiKey),
  init:stop().
  
%% Tests
run_tests(Username, ApiKey) ->
  CloudFiles = connect_test(Username, ApiKey),
  print_account_info(CloudFiles),
  container_tests(CloudFiles),
  ok.
  
connect_test(Username, ApiKey) ->
  {error, unauthorized} = cferl:connect("_fake_user_name", "_fake_api_key"),
  ?PRINT_CODE("# Connect to Cloud Files (warning: cache/use CloudFiles for a maximum of 24 hours!)"),
  ?PRINT_CALL({ok, CloudFiles} = cferl:connect(Username, ApiKey)),
  ?PRINT_CODE(""),
  CloudFiles.

print_account_info(CloudFiles) ->
  ?PRINT_CODE("# Retrieve the account information record"),
  ?PRINT_CALL({ok, Info} = CloudFiles:get_account_info()),
  ?PRTFM_CODE("Info = #cf_account_info{bytes_used=~B, container_count=~B}",
              [Info#cf_account_info.bytes_used, Info#cf_account_info.container_count]),
  ?PRINT_CODE("").
              
container_tests(CloudFiles) ->
  ?PRINT_CODE("# Retrieve names of all existing containers (within the limits imposed by Cloud Files server)"),
  ?PRINT_CALL({ok, Names} = CloudFiles:get_containers_names()),
  ?PRTFM_CODE("Names=~p~n", [Names]),
  
  ?PRINT_CODE("# Retrieve names of a maximum of 3 existing containers"),
  ?PRINT_CALL({ok, ThreeNamesMax} = CloudFiles:get_containers_names(#cf_container_query_args{limit=3})),
  ?PRTFM_CODE("ThreeNamesMax=~p~n", [ThreeNamesMax]),
  
  % retrieve 0 container
  {ok, []} = CloudFiles:get_containers_details(#cf_container_query_args{limit=0}),
  
  ?PRINT_CODE("# Retrieve names of all containers currently CDN activated"),
  ?PRINT_CALL({ok, PublicNames} = CloudFiles:get_public_containers_names(active)),
  ?PRTFM_CODE("PublicNames=~p~n", [PublicNames]),
  
  ?PRINT_CODE("# Retrieve details for all existing containers (within the server limits)"),
  ?PRINT_CALL({ok, ContainersDetails} = CloudFiles:get_containers_details()),
  ?PRINT_CODE("# ContainersDetails is a list of #cf_container_details records"),
  ?PRINT_CALL([Detail|_]=ContainersDetails),
  ?PRTFM_CODE("Detail = #cf_container_details{name=~p, bytes=~B, count=~B}",
              [Detail#cf_container_details.name,
               Detail#cf_container_details.bytes,
               Detail#cf_container_details.count]),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve details for a maximum of 5 containers whose names start at cf"),
  ?PRINT_CALL({ok, CfContainersDetails} = CloudFiles:get_containers_details(#cf_container_query_args{marker= <<"cf">>, limit=5})),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Get a container reference by name"),
  ?PRINT_CALL({ok, Container} = CloudFiles:get_container(Detail#cf_container_details.name)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Get container details from its reference"),
  ?PRINT_CALL(ContainerName = Container:name()),
  ?PRINT_CALL(ContainerBytes = Container:bytes()),
  ?PRINT_CALL(ContainerSize = Container:count()),
  ?PRINT_CALL(ContainerIsEmpty = Container:is_empty()),
  ?PRTFM_CODE("# > Name: ~p - Bytes: ~p - Size: ~p - IsEmpty: ~p",
              [ContainerName, ContainerBytes, ContainerSize, ContainerIsEmpty]),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Check a container's existence"),
  ?PRINT_CALL(false = CloudFiles:container_exists(<<"new_container">>)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Create a new container"),
  ?PRINT_CALL({ok, NewContainer} = CloudFiles:create_container(<<"new_container">>)),
  ?PRINT_CODE(""),
  ?PRINT_CALL(true = CloudFiles:container_exists(<<"new_container">>)),
  ?PRINT_CODE(""),
  ?PRINT_CALL(<<"new_container">> = NewContainer:name()),
  ?PRINT_CALL(0 = NewContainer:bytes()),
  ?PRINT_CALL(0 = NewContainer:count()),
  ?PRINT_CALL(true = NewContainer:is_empty()),
  ?PRINT_CALL(false = NewContainer:is_public()),
  ?PRINT_CODE(""),
  
  % TODO make public, call get_public_containers_names(active)
  
  ?PRINT_CODE("# Refresh an existing container"),
  ?PRINT_CALL({ok, RefreshedContainer} = NewContainer:refresh()),
  ?PRINT_CODE(""),
  
  % TODO make private, call get_public_containers_names(active & all_time)
  
  ?PRINT_CODE("# Delete an existing container"),
  ?PRINT_CALL(ok = NewContainer:delete()),
  ?PRINT_CODE(""),
  
  % ensure deleting missing container is properly handled
  {error, not_found} = NewContainer:delete(),
  
  ok.

