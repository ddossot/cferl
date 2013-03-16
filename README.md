# Rackspace Cloud Files Erlang Client

## Description

This is an Erlang interface into the Rackspace Cloud Files service. It has been largely inspired by the existing [Ruby](http://github.com/rackspace/ruby-cloudfiles) API.


## Building

**cferl** relies on [rebar](http://bitbucket.org/basho/rebar/wiki/Home) for its build and dependency management and targets Erlang/OTP R13B04 or above.

Simply run:

    rebar get-deps compile eunit

Optionally, to generate the *cferl* documentation, run:

    rebar skip_deps=true doc

Optionally, to run the integration tests (and generate the code samples visible below), run:

    ./int_tests

If you run the integration tests, you'll need your API key and at least one pre-existing container. Note that a test container will be created and some queries could take a while if you have lots of containers.


## Using

**cferl** requires that the ssl and ibrowse applications be started prior to using it.

The following, which is output when running the integration tests, demonstrates a typical usage of the API. Refer to the documentation for the complete reference.

```erlang
# Connect to Cloud Files (warning: cache/use CloudFiles for a maximum of 24 hours!)
{ok,CloudFiles}=cferl:connect(Username,ApiKey).

# Retrieve the account information record
{ok,Info}=cferl_connection:get_account_info(CloudFiles).
Info = #cf_account_info{bytes_used=1735871382, container_count=5}

# Retrieve names of all existing containers (within the limits imposed by Cloud Files server)
{ok,Names}=cferl_connection:get_containers_names(CloudFiles).

# Retrieve names of a maximum of 3 existing containers
{ok,ThreeNamesMax}=cferl_connection:get_containers_names(CloudFiles,#cf_container_query_args{limit=3}).

# Retrieve names of all containers currently CDN activated
{ok,CurrentPublicNames}=cferl_connection:get_public_containers_names(CloudFiles,active).

# Retrieve names of all containers that are currently or have been CDN activated
{ok,AllTimePublicNames}=cferl_connection:get_public_containers_names(CloudFiles,all_time).

# Retrieve details for all existing containers (within the server limits)
{ok,ContainersDetails}=cferl_connection:get_containers_details(CloudFiles).

# ContainersDetails is a list of #cf_container_details records
[ContainerDetails|_]=ContainersDetails.
ContainerDetails = #cf_container_details{name=<<".CDN_ACCESS_LOGS">>, bytes=261, count=1}

# Retrieve details for a maximum of 5 containers whose names start at cf
{ok,CfContainersDetails}=cferl_connection:get_containers_details(CloudFiles,#cf_container_query_args{marker=<<"cf">>,limit=5}).

# Get a container reference by name
{ok,Container}=cferl_connection:get_container(CloudFiles,ContainerDetails#cf_container_details.name).

# Get container details from its reference
ContainerName=cferl_container:name(Container).
ContainerBytes=cferl_container:bytes(Container).
ContainerSize=cferl_container:count(Container).
ContainerIsEmpty=cferl_container:is_empty(Container).

# -> Name: <<".CDN_ACCESS_LOGS">> - Bytes: 261 - Size: 1 - IsEmpty: false

# Check a container's existence
false=cferl_connection:container_exists(CloudFiles,NewContainerName).

# Create a new container
{ok,NewContainer}=cferl_connection:create_container(CloudFiles,NewContainerName).

true=cferl_connection:container_exists(CloudFiles,NewContainerName).

Check attributes of this newly created container
NewContainerName=cferl_container:name(NewContainer).
0=cferl_container:bytes(NewContainer).
0=cferl_container:count(NewContainer).
true=cferl_container:is_empty(NewContainer).
false=cferl_container:is_public(NewContainer).
<<>>=cferl_container:cdn_url(NewContainer).
0=cferl_container:cdn_ttl(NewContainer).
false=cferl_container:log_retention(NewContainer).

# Make the container public on the CDN (using the default TTL and ACLs)
ok=cferl_container:make_public(CloudFiles,NewContainer).

# Activate log retention on the new container
ok=cferl_container:set_log_retention(CloudFiles,NewContainer,true).

# Refresh an existing container and check its attributes
{ok,RefreshedContainer}=cferl_container:refresh(CloudFiles,NewContainer).
true=cferl_container:is_public(RefreshedContainer).

io:format("~s~n~n",[cferl_container:cdn_url(RefreshedContainer)]).
http://05f98f987aa9393ccd8c-3d04f8822c5760cb271501bb0c358085.r17.cf1.rackcdn.com

86400=cferl_container:cdn_ttl(RefreshedContainer).
true=cferl_container:log_retention(RefreshedContainer).

ObjectName=<<"test.xml">>.
# Create an object *reference*, nothing is sent to the server yet
{ok,Object}=cferl_container:create_object(CloudFiles,RefreshedContainer,ObjectName).
# As expected, it doesn't exist yet
false=cferl_container:object_exists(CloudFiles,RefreshedContainer,ObjectName).

# Write data in the object, which creates it on the server
ok=cferl_object:write_data(CloudFiles,Object,<<"<test/>">>,<<"application/xml">>).
# Now it exists!
true=cferl_container:object_exists(CloudFiles,RefreshedContainer,ObjectName).
# And trying to re-create it just returns it
{ok,ExistingObject}=cferl_container:create_object(CloudFiles,RefreshedContainer,ObjectName).

# Set custom meta-data on it
ok=cferl_object:set_metadata(CloudFiles,Object,[{<<"Key123">>,<<"my123Value">>}]).

# An existing object can be accessed directly from its container
{ok,GotObject}=cferl_container:get_object(CloudFiles,RefreshedContainer,ObjectName).

# Object names and details can be queried
{ok,[ObjectName]}=cferl_container:get_objects_names(CloudFiles,RefreshedContainer).
{ok,[ObjectName]}=cferl_container:get_objects_names(CloudFiles,RefreshedContainer,#cf_object_query_args{limit=1}).
{ok,[ObjectDetails]}=cferl_container:get_objects_details(CloudFiles,RefreshedContainer).
ObjectDetails = #cf_object_details{name=<<"test.xml">>, bytes=8, last_modified={{2013,3,16},{0,8,47}}, content_type=application/xml, etag=4366c359d1a7b9b248fa262775613699}

# Read the whole data
{ok,<<"<test/>">>}=cferl_object:read_data(CloudFiles,Object).
# Read the data with an offset and a size
{ok,<<"test">>}=cferl_object:read_data(CloudFiles,Object,1,4).

# Refresh the object so its attributes and metadata are up to date
{ok,RefreshedObject}=cferl_object:refresh(CloudFiles,Object).

# Get object attributes
ObjectName=cferl_object:name(RefreshedObject).
8=cferl_object:bytes(RefreshedObject).
{{D,M,Y},{H,Mi,S}}=cferl_object:last_modified(RefreshedObject).
<<"application/xml">>=cferl_object:content_type(RefreshedObject).
Etag=cferl_object:etag(RefreshedObject).

# Get custom meta-data
[{<<"Key123">>,<<"my123Value">>}]=cferl_object:metadata(RefreshedObject).

# Delete the object
ok=cferl_object:delete(CloudFiles,RefreshedObject).

# Data can be streamed to the server from a generating function
{ok,StreamedObject}=cferl_container:create_object(CloudFiles,RefreshedContainer,<<"streamed.txt">>).
cferl_object:write_data_stream(CloudFiles,StreamedObject,WriteDataFun,<<"text/plain">>,1000).

# Data can be streamed from the server to a receiving function
ok=cferl_object:read_data_stream(CloudFiles,StreamedObject,ReadDataFun).

# Create all the directory elements for a particular object path
ok=cferl_container:ensure_dir(CloudFiles,RefreshedContainer,<<"photos/plants/fern.jpg">>).
true=cferl_container:object_exists(CloudFiles,RefreshedContainer,<<"photos">>).
true=cferl_container:object_exists(CloudFiles,RefreshedContainer,<<"photos/plants">>).

# Make the container private
ok=cferl_container:make_private(CloudFiles,RefreshedContainer).

# Delete an existing container (must be empty)
ok=cferl_container:delete(CloudFiles,RefreshedContainer).
```

## More information

Read the Rackspace Cloud Files API specification: <http://www.rackspacecloud.com/cloud_hosting_products/files/api>

Contact the author: <david@dossot.net>

## Copyright (c) 2010-2013 David Dossot - MIT License
