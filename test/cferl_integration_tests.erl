%%%
%%% @doc Integration tests and demo code generation.
%%% @author David Dossot <david@dossot.net>
%%% @author Tilman Holschuh <tilman.holschuh@gmail.com>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_integration_tests).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

-export([start/0, data_producer_loop/1]).
-define(PRINT_CODE(Code), io:format("    ~s~n", [Code])).
-define(PRTFM_CODE(Format, Data), ?PRINT_CODE(io_lib:format(Format, Data))).
-define(PRINT_CALL(Call),
          io:format("    ~s.~n", [re:replace(??Call, " ", "", [global])]),
          Call).

start() ->
  application:start(crypto),
  application:start(public_key),
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
  ?PRINT_CALL({ok, Info} = cferl_connection:get_account_info(CloudFiles)),
  ?PRTFM_CODE("Info = #cf_account_info{bytes_used=~B, container_count=~B}",
              [Info#cf_account_info.bytes_used, Info#cf_account_info.container_count]),
  ?PRINT_CODE("").
              
container_tests(CloudFiles) ->
  ?PRINT_CODE("# Retrieve names of all existing containers (within the limits imposed by Cloud Files server)"),
  ?PRINT_CALL({ok, Names} = cferl_connection:get_containers_names(CloudFiles)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve names of a maximum of 3 existing containers"),
  ?PRINT_CALL({ok, ThreeNamesMax} = cferl_connection:get_containers_names(CloudFiles, #cf_container_query_args{limit=3})),
  ?PRINT_CODE(""),
  
  % retrieve 0 container
  {ok, []} = cferl_connection:get_containers_details(CloudFiles, #cf_container_query_args{limit=0}),
  
  ?PRINT_CODE("# Retrieve names of all containers currently CDN activated"),
  ?PRINT_CALL({ok, CurrentPublicNames} = cferl_connection:get_public_containers_names(CloudFiles, active)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve names of all containers that are currently or have been CDN activated"),
  ?PRINT_CALL({ok, AllTimePublicNames} = cferl_connection:get_public_containers_names(CloudFiles, all_time)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve details for all existing containers (within the server limits)"),
  ?PRINT_CALL({ok, ContainersDetails} = cferl_connection:get_containers_details(CloudFiles)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# ContainersDetails is a list of #cf_container_details records"),
  ?PRINT_CALL([ContainerDetails|_]=ContainersDetails),
  ?PRTFM_CODE("ContainerDetails = #cf_container_details{name=~p, bytes=~B, count=~B}",
              [ContainerDetails#cf_container_details.name,
               ContainerDetails#cf_container_details.bytes,
               ContainerDetails#cf_container_details.count]),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve details for a maximum of 5 containers whose names start at cf"),
  ?PRINT_CALL({ok, CfContainersDetails} = cferl_connection:get_containers_details(CloudFiles, #cf_container_query_args{marker= <<"cf">>, limit=5})),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Get a container reference by name"),
  ?PRINT_CALL({ok, Container} = cferl_connection:get_container(CloudFiles, ContainerDetails#cf_container_details.name)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Get container details from its reference"),
  ?PRINT_CALL(ContainerName = cferl_container:name(Container)),
  ?PRINT_CALL(ContainerBytes = cferl_container:bytes(Container)),
  ?PRINT_CALL(ContainerSize = cferl_container:count(Container)),
  ?PRINT_CALL(ContainerIsEmpty = cferl_container:is_empty(Container)),
  ?PRINT_CODE(""),
  ?PRTFM_CODE("# -> Name: ~p - Bytes: ~p - Size: ~p - IsEmpty: ~p",
              [ContainerName, ContainerBytes, ContainerSize, ContainerIsEmpty]),
  ?PRINT_CODE(""),
  
  NewContainerName = make_new_container_name(),
  
  ?PRINT_CODE("# Check a container's existence"),
  ?PRINT_CALL(false = cferl_connection:container_exists(CloudFiles, NewContainerName)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Create a new container"),
  ?PRINT_CALL({ok, NewContainer} = cferl_connection:create_container(CloudFiles, NewContainerName)),
  ?PRINT_CODE(""),
  ?PRINT_CALL(true = cferl_connection:container_exists(CloudFiles, NewContainerName)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("Check attributes of this newly created container"),
  ?PRINT_CALL(NewContainerName = cferl_container:name(NewContainer)),
  ?PRINT_CALL(0 = cferl_container:bytes(NewContainer)),
  ?PRINT_CALL(0 = cferl_container:count(NewContainer)),
  ?PRINT_CALL(true = cferl_container:is_empty(NewContainer)),
  ?PRINT_CALL(false = cferl_container:is_public(NewContainer)),
  ?PRINT_CALL(<<>> = cferl_container:cdn_url(NewContainer)),
  ?PRINT_CALL(0 = cferl_container:cdn_ttl(NewContainer)),
  ?PRINT_CALL(false = cferl_container:log_retention(NewContainer)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Make the container public on the CDN (using the default TTL and ACLs)"),
  ?PRINT_CALL(ok = cferl_container:make_public(CloudFiles, NewContainer)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Activate log retention on the new container"),
  ?PRINT_CALL(ok = cferl_container:set_log_retention(CloudFiles, NewContainer, true)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Refresh an existing container and check its attributes"),
  ?PRINT_CALL({ok, RefreshedContainer} = cferl_container:refresh(CloudFiles, NewContainer)),
  ?PRINT_CALL(true = cferl_container:is_public(RefreshedContainer)),
  ?PRINT_CODE(""),
  ?PRINT_CALL(io:format("    ~s~n~n", [cferl_container:cdn_url(RefreshedContainer)])),
  ?PRINT_CALL(86400 = cferl_container:cdn_ttl(RefreshedContainer)),
  ?PRINT_CALL(true = cferl_container:log_retention(RefreshedContainer)),
  ?PRINT_CODE(""),
  
  % ensure container has no object name
  {ok, []} = cferl_container:get_objects_names(CloudFiles, RefreshedContainer),
  {ok, []} = cferl_container:get_objects_names(CloudFiles, RefreshedContainer, #cf_object_query_args{limit=10}),

  % ensure container has no object details
  {ok, []} = cferl_container:get_objects_details(CloudFiles, RefreshedContainer),
  {ok, []} = cferl_container:get_objects_details(CloudFiles, RefreshedContainer, #cf_object_query_args{limit=10}),
  
  ?PRINT_CALL(ObjectName = <<"test.xml">>),
  
  % ensure new object doesn't exist
  false = cferl_container:object_exists(CloudFiles, RefreshedContainer, ObjectName),
  
  ?PRINT_CODE("# Create an object *reference*, nothing is sent to the server yet"),
  ?PRINT_CALL({ok, Object} = cferl_container:create_object(CloudFiles, RefreshedContainer, ObjectName)),
  ?PRINT_CODE("# As expected, it doesn't exist yet"),
  ?PRINT_CALL(false = cferl_container:object_exists(CloudFiles, RefreshedContainer, ObjectName)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Write data in the object, which creates it on the server"),
  ?PRINT_CALL(ok = cferl_object:write_data(CloudFiles, Object, <<"<test />">>, <<"application/xml">>)),
  ?PRINT_CODE("# Now it exists!"),
  ?PRINT_CALL(true = cferl_container:object_exists(CloudFiles, RefreshedContainer, ObjectName)),
  ?PRINT_CODE("# And trying to re-create it just returns it"),
  ?PRINT_CALL({ok, ExistingObject} = cferl_container:create_object(CloudFiles, RefreshedContainer, ObjectName)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Set custom meta-data on it"),
  ?PRINT_CALL(ok = cferl_object:set_metadata(CloudFiles, Object, [{<<"Key123">>, <<"my 123 Value">>}])),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# An existing object can be accessed directly from its container"),
  ?PRINT_CALL({ok, GotObject} = cferl_container:get_object(CloudFiles, RefreshedContainer, ObjectName)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Object names and details can be queried"),
  ?PRINT_CALL({ok, [ObjectName]} = cferl_container:get_objects_names(CloudFiles, RefreshedContainer)),
  ?PRINT_CALL({ok, [ObjectName]} = cferl_container:get_objects_names(CloudFiles, RefreshedContainer, #cf_object_query_args{limit=1})),
  ?PRINT_CALL({ok, [ObjectDetails]} = cferl_container:get_objects_details(CloudFiles, RefreshedContainer)),
  ?PRTFM_CODE("ObjectDetails = #cf_object_details{name=~p, bytes=~B, last_modified=~1024p, content_type=~s, etag=~s}",
              [ObjectDetails#cf_object_details.name,
               ObjectDetails#cf_object_details.bytes,
               ObjectDetails#cf_object_details.last_modified,
               ObjectDetails#cf_object_details.content_type,
               ObjectDetails#cf_object_details.etag]),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Read the whole data"),
  ?PRINT_CALL({ok, <<"<test />">>} = cferl_object:read_data(CloudFiles, Object)),
  ?PRINT_CODE("# Read the data with an offset and a size"),
  ?PRINT_CALL({ok, <<"test">>} = cferl_object:read_data(CloudFiles, Object, 1, 4)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Refresh the object so its attributes and metadata are up to date"),
  ?PRINT_CALL({ok, RefreshedObject} = cferl_object:refresh(CloudFiles, Object)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Get object attributes"),
  ?PRINT_CALL(ObjectName = cferl_object:name(RefreshedObject)),
  ?PRINT_CALL(8 = cferl_object:bytes(RefreshedObject)),
  ?PRINT_CALL({{D,M,Y},{H,Mi,S}} = cferl_object:last_modified(RefreshedObject)),
  ?PRINT_CALL(<<"application/xml">> = cferl_object:content_type(RefreshedObject)),
  ?PRINT_CALL(Etag = cferl_object:etag(RefreshedObject)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Get custom meta-data"),
  ?PRINT_CALL([{<<"Key123">>, <<"my 123 Value">>}] = cferl_object:metadata(RefreshedObject)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Delete the object"),
  ?PRINT_CALL(ok = cferl_object:delete(CloudFiles, RefreshedObject)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Data can be streamed to the server from a generating function"),
  ?PRINT_CALL({ok, StreamedObject} = cferl_container:create_object(CloudFiles, RefreshedContainer, <<"streamed.txt">>)),

  DataPid = spawn_data_producer(),
  WriteDataFun =
    fun() ->
      DataPid ! {self(), get_data},
      receive
        Data -> Data
        after 5000 -> eof
      end
    end,

  ?PRINT_CALL(cferl_object:write_data_stream(CloudFiles, StreamedObject, WriteDataFun, <<"text/plain">>, 1000)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Data can be streamed from the server to a receiving function"),
  ReadDataFun = fun(_Data) -> ok end,
  ?PRINT_CALL(ok = cferl_object:read_data_stream(CloudFiles, StreamedObject, ReadDataFun)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Create all the directory elements for a particular object path"),
  ?PRINT_CALL(ok = cferl_container:ensure_dir(CloudFiles, RefreshedContainer, <<"photos/plants/fern.jpg">>)),
  ?PRINT_CALL(true = cferl_container:object_exists(CloudFiles, RefreshedContainer, <<"photos">>)),
  ?PRINT_CALL(true = cferl_container:object_exists(CloudFiles, RefreshedContainer, <<"photos/plants">>)),
  ?PRINT_CODE(""),

  % delete the streamed object
  ok = cferl_container:delete_object(CloudFiles, RefreshedContainer, <<"streamed.txt">>),
  
  % delete the path elements
  ok = cferl_container:delete_object(CloudFiles, RefreshedContainer, <<"photos">>),
  ok = cferl_container:delete_object(CloudFiles, RefreshedContainer, <<"photos/plants">>),
  
  % ensure log retention can be stopped
  ok = cferl_container:set_log_retention(CloudFiles, RefreshedContainer, false),
  
  ?PRINT_CODE("# Make the container private"),
  ?PRINT_CALL(ok = cferl_container:make_private(CloudFiles, RefreshedContainer)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Delete an existing container (must be empty)"),
  ?PRINT_CALL(ok = cferl_container:delete(CloudFiles, RefreshedContainer)),
  ?PRINT_CODE(""),
  
  % ensure deleting missing container is properly handled
  {error, not_found} = cferl_container:delete(CloudFiles, NewContainer),
  
  ok.
  
make_new_container_name() ->
  {ok, HostName} = inet:gethostname(),
  {M,S,U} = now(),
  ContainerName = "cferl_int_test"
               ++ integer_to_list(M)
               ++ "-"
               ++ integer_to_list(S)
               ++ "-"
               ++ integer_to_list(U)
               ++ "-"
               ++ HostName,
  list_to_binary(ContainerName).

spawn_data_producer() ->
  spawn(?MODULE, data_producer_loop, [0]).

data_producer_loop(Index) ->
  receive
    {Pid, get_data} when Index < 10 ->
      Pid ! {ok, string:copies(integer_to_list(Index), 100)},
      data_producer_loop(Index + 1);
      
    {Pid, get_data} ->
      Pid ! eof;
      
    _ ->
      ok
  end.

