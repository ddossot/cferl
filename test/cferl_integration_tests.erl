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

-export([start/0, data_producer_loop/1]).
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
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve names of a maximum of 3 existing containers"),
  ?PRINT_CALL({ok, ThreeNamesMax} = CloudFiles:get_containers_names(#cf_container_query_args{limit=3})),
  ?PRINT_CODE(""),
  
  % retrieve 0 container
  {ok, []} = CloudFiles:get_containers_details(#cf_container_query_args{limit=0}),
  
  ?PRINT_CODE("# Retrieve names of all containers currently CDN activated"),
  ?PRINT_CALL({ok, CurrentPublicNames} = CloudFiles:get_public_containers_names(active)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve names of all containers that are currently or have been CDN activated"),
  ?PRINT_CALL({ok, AllTimePublicNames} = CloudFiles:get_public_containers_names(all_time)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve details for all existing containers (within the server limits)"),
  ?PRINT_CALL({ok, ContainersDetails} = CloudFiles:get_containers_details()),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# ContainersDetails is a list of #cf_container_details records"),
  ?PRINT_CALL([ContainerDetails|_]=ContainersDetails),
  ?PRTFM_CODE("ContainerDetails = #cf_container_details{name=~p, bytes=~B, count=~B}",
              [ContainerDetails#cf_container_details.name,
               ContainerDetails#cf_container_details.bytes,
               ContainerDetails#cf_container_details.count]),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Retrieve details for a maximum of 5 containers whose names start at cf"),
  ?PRINT_CALL({ok, CfContainersDetails} = CloudFiles:get_containers_details(#cf_container_query_args{marker= <<"cf">>, limit=5})),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Get a container reference by name"),
  ?PRINT_CALL({ok, Container} = CloudFiles:get_container(ContainerDetails#cf_container_details.name)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Get container details from its reference"),
  ?PRINT_CALL(ContainerName = Container:name()),
  ?PRINT_CALL(ContainerBytes = Container:bytes()),
  ?PRINT_CALL(ContainerSize = Container:count()),
  ?PRINT_CALL(ContainerIsEmpty = Container:is_empty()),
  ?PRINT_CODE(""),
  ?PRTFM_CODE("# -> Name: ~p - Bytes: ~p - Size: ~p - IsEmpty: ~p",
              [ContainerName, ContainerBytes, ContainerSize, ContainerIsEmpty]),
  ?PRINT_CODE(""),
  
  NewContainerName = make_new_container_name(),
  
  ?PRINT_CODE("# Check a container's existence"),
  ?PRINT_CALL(false = CloudFiles:container_exists(NewContainerName)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Create a new container"),
  ?PRINT_CALL({ok, NewContainer} = CloudFiles:create_container(NewContainerName)),
  ?PRINT_CODE(""),
  ?PRINT_CALL(true = CloudFiles:container_exists(NewContainerName)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("Check attributes of this newly created container"),
  ?PRINT_CALL(NewContainerName = NewContainer:name()),
  ?PRINT_CALL(0 = NewContainer:bytes()),
  ?PRINT_CALL(0 = NewContainer:count()),
  ?PRINT_CALL(true = NewContainer:is_empty()),
  ?PRINT_CALL(false = NewContainer:is_public()),
  ?PRINT_CALL(<<>> = NewContainer:cdn_url()),
  ?PRINT_CALL(0 = NewContainer:cdn_ttl()),
  ?PRINT_CALL(false = NewContainer:log_retention()),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Make the container public on the CDN (using the default TTL and ACLs)"),
  ?PRINT_CALL(ok = NewContainer:make_public()),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Activate log retention on the new container"),
  ?PRINT_CALL(ok = NewContainer:set_log_retention(true)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Refresh an existing container and check its attributes"),
  ?PRINT_CALL({ok, RefreshedContainer} = NewContainer:refresh()),
  ?PRINT_CALL(true = RefreshedContainer:is_public()),
  ?PRINT_CODE(""),
  ?PRINT_CALL(io:format("    ~s~n~n", [RefreshedContainer:cdn_url()])),
  ?PRINT_CALL(86400 = RefreshedContainer:cdn_ttl()),
  ?PRINT_CALL(true = RefreshedContainer:log_retention()),
  ?PRINT_CODE(""),
  
  % ensure container has no object name
  {ok, []} = RefreshedContainer:get_objects_names(),
  {ok, []} = RefreshedContainer:get_objects_names(#cf_object_query_args{limit=10}),

  % ensure container has no object details
  {ok, []} = RefreshedContainer:get_objects_details(),
  {ok, []} = RefreshedContainer:get_objects_details(#cf_object_query_args{limit=10}),
  
  ?PRINT_CALL(ObjectName = <<"test.xml">>),
  
  % ensure new object doesn't exist
  false = RefreshedContainer:object_exists(ObjectName),
  
  ?PRINT_CODE("# Create an object *reference*, nothing is sent to the server yet"),
  ?PRINT_CALL({ok, Object} = RefreshedContainer:create_object(ObjectName)),
  ?PRINT_CODE("# As expected, it doesn't exist yet"),
  ?PRINT_CALL(false = RefreshedContainer:object_exists(ObjectName)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Write data in the object, which creates it on the server"),
  ?PRINT_CALL(ok = Object:write_data(<<"<test />">>, <<"application/xml">>)),
  ?PRINT_CODE("# Now it exists!"),
  ?PRINT_CALL(true = RefreshedContainer:object_exists(ObjectName)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Set custom meta-data on it"),
  ?PRINT_CALL(ok = Object:set_metadata([{<<"Key123">>, <<"my 123 Value">>}])),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# An existing object can be accessed directly from its container"),
  ?PRINT_CALL({ok, GotObject} = RefreshedContainer:get_object(ObjectName)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Object names and details can be queried"),
  ?PRINT_CALL({ok, [ObjectName]} = RefreshedContainer:get_objects_names()),
  ?PRINT_CALL({ok, [ObjectName]} = RefreshedContainer:get_objects_names(#cf_object_query_args{limit=1})),
  ?PRINT_CALL({ok, [ObjectDetails]} = RefreshedContainer:get_objects_details()),
  ?PRTFM_CODE("ObjectDetails = #cf_object_details{name=~p, bytes=~B, last_modified=~1024p, content_type=~s, etag=~s}",
              [ObjectDetails#cf_object_details.name,
               ObjectDetails#cf_object_details.bytes,
               ObjectDetails#cf_object_details.last_modified,
               ObjectDetails#cf_object_details.content_type,
               ObjectDetails#cf_object_details.etag]),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Read the whole data"),
  ?PRINT_CALL({ok, <<"<test />">>} = Object:read_data()),
  ?PRINT_CODE("# Read the data with an offset and a size"),
  ?PRINT_CALL({ok, <<"test">>} = Object:read_data(1, 4)),
  ?PRINT_CODE(""),

  ?PRINT_CODE("# Refresh the object so its attributes and metadata are up to date"),
  ?PRINT_CALL({ok, RefreshedObject} = Object:refresh()),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Get object attributes"),
  ?PRINT_CALL(ObjectName = RefreshedObject:name()),
  ?PRINT_CALL(8 = RefreshedObject:bytes()),
  ?PRINT_CALL({{D,M,Y},{H,Mi,S}} = RefreshedObject:last_modified()),
  ?PRINT_CALL(<<"application/xml; charset=UTF-8">> = RefreshedObject:content_type()),
  ?PRINT_CALL(Etag = RefreshedObject:etag()),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Get custom meta-data"),
  ?PRINT_CALL([{<<"Key123">>, <<"my 123 Value">>}] = RefreshedObject:metadata()),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Delete the object"),
  ?PRINT_CALL(ok = RefreshedObject:delete()),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Data can be streamed to the server from a generating function"),
  ?PRINT_CALL({ok, StreamedObject} = RefreshedContainer:create_object(<<"streamed.txt">>)),

  DataPid = spawn_data_producer(),
  WriteDataFun =
    fun() ->
      DataPid ! {self(), get_data},
      receive
        Data -> Data
        after 5000 -> eof
      end
    end,

  ?PRINT_CALL(StreamedObject:write_data_stream(WriteDataFun, <<"text/plain">>, 1000)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Data can be streamed from the server to a receiving function"),
  ReadDataFun = fun(_Data) -> ok end,
  ?PRINT_CALL(ok = StreamedObject:read_data_stream(ReadDataFun)),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Create all the directory elements for a particular object path"),
  ?PRINT_CALL(ok = RefreshedContainer:ensure_dir(<<"photos/plants/fern.jpg">>)),
  ?PRINT_CALL(true = RefreshedContainer:object_exists(<<"photos">>)),
  ?PRINT_CALL(true = RefreshedContainer:object_exists(<<"photos/plants">>)),
  ?PRINT_CODE(""),

  % delete the streamed object
  ok = RefreshedContainer:delete_object(<<"streamed.txt">>),
  
  % delete the path elements
  ok = RefreshedContainer:delete_object(<<"photos">>),
  ok = RefreshedContainer:delete_object(<<"photos/plants">>),
  
  % ensure log retention can be stopped
  ok = RefreshedContainer:set_log_retention(false),
  
  ?PRINT_CODE("# Make the container private"),
  ?PRINT_CALL(ok = RefreshedContainer:make_private()),
  ?PRINT_CODE(""),
  
  ?PRINT_CODE("# Delete an existing container (must be empty)"),
  ?PRINT_CALL(ok = RefreshedContainer:delete()),
  ?PRINT_CODE(""),
  
  % ensure deleting missing container is properly handled
  {error, not_found} = NewContainer:delete(),
  
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

