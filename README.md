Rackspace Cloud Files Erlang Client
===================================

> Version 0.9 - Currently under initial development!

Description
-----------

This is an Erlang interface into the Rackspace Cloud Files service. It has been largely inspired by the existing [Ruby](http://github.com/rackspace/ruby-cloudfiles) API.


Building
--------

cferl relies on [rebar](http://bitbucket.org/basho/rebar/wiki/Home) for its build and dependency management and targets Erlang/OTP R13B04 or above.

Simply run:

    ./rebar get-deps compile eunit

Optinally, to generate the cferl documentation, run:

    ./rebar delete-deps doc

Optinally, to run the integration tests (and generate the code samples visible below), run:

    ./int_tests

If you run the integration tests, you'll need your API key and at least one pre-existing container. Note that a test container will be created and some queries could take a while if you have lots of containers.


Using
-----

cferl requires that the ssl and ibrowse applications be started prior to using it.

The following, which is output when running the integration tests, demonstrates a typical usage of the API. Refer to the documentation for the complete reference.


    # Connect to Cloud Files (warning: cache/use CloudFiles for a maximum of 24 hours!)
    {ok,CloudFiles}=cferl:connect(Username,ApiKey).
    
    # Retrieve the account information record
    {ok,Info}=CloudFiles:get_account_info().
    Info = #cf_account_info{bytes_used=360, container_count=1}
    
    # Retrieve names of all existing containers (within the limits imposed by Cloud Files server)
    {ok,Names}=CloudFiles:get_containers_names().
    
    # Retrieve names of a maximum of 3 existing containers
    {ok,ThreeNamesMax}=CloudFiles:get_containers_names(#cf_container_query_args{limit=3}).
    
    # Retrieve names of all containers currently CDN activated
    {ok,CurrentPublicNames}=CloudFiles:get_public_containers_names(active).
    
    # Retrieve names of all containers that are currently or have been CDN activated
    {ok,AllTimePublicNames}=CloudFiles:get_public_containers_names(all_time).
    
    # Retrieve details for all existing containers (within the server limits)
    {ok,ContainersDetails}=CloudFiles:get_containers_details().
    
    # ContainersDetails is a list of #cf_container_details records
    [ContainerDetails|_]=ContainersDetails.
    ContainerDetails = #cf_container_details{name=<<"cferl-test">>, bytes=360, count=1}
    
    # Retrieve details for a maximum of 5 containers whose names start at cf
    {ok,CfContainersDetails}=CloudFiles:get_containers_details(#cf_container_query_args{marker=<<"cf">>,limit=5}).
    
    # Get a container reference by name
    {ok,Container}=CloudFiles:get_container(ContainerDetails#cf_container_details.name).
    
    # Get container details from its reference
    ContainerName=Container:name().
    ContainerBytes=Container:bytes().
    ContainerSize=Container:count().
    ContainerIsEmpty=Container:is_empty().
    
    # -> Name: <<"cferl-test">> - Bytes: 360 - Size: 1 - IsEmpty: false
    
    # Check a container's existence
    false=CloudFiles:container_exists(NewContainerName).
    
    # Create a new container
    {ok,NewContainer}=CloudFiles:create_container(NewContainerName).
    
    true=CloudFiles:container_exists(NewContainerName).
    
    Check attributes of this newly created container
    NewContainerName=NewContainer:name().
    0=NewContainer:bytes().
    0=NewContainer:count().
    true=NewContainer:is_empty().
    false=NewContainer:is_public().
    <<>>=NewContainer:cdn_url().
    0=NewContainer:cdn_ttl().
    false=NewContainer:log_retention().
    
    # Make the container public on the CDN (using the default TTL and ACLs)
    ok=NewContainer:make_public().
    
    # Activate log retention on the new container
    ok=NewContainer:set_log_retention(true).
    
    # Refresh an existing container and check its attributes
    {ok,RefreshedContainer}=NewContainer:refresh().
    true=RefreshedContainer:is_public().
    
    io:format("~s~n~n",[RefreshedContainer:cdn_url()]).
    http://c0025137.cdn1.cloudfiles.rackspacecloud.com

    86400=RefreshedContainer:cdn_ttl().
    true=RefreshedContainer:log_retention().
    
    ObjectName=<<"test.xml">>.
    # Create an object *reference*, nothing is sent to the server yet
    {ok,Object}=RefreshedContainer:create_object(ObjectName).
    # As expected, it doesn't exist yet
    false=RefreshedContainer:object_exists(ObjectName).
    
    # Write data in the object, which creates it on the server
    ok=Object:write_data(<<"<test/>">>,<<"application/xml">>).
    # Now it exists!
    true=RefreshedContainer:object_exists(ObjectName).
    
    # Set custom meta-data on it
    ok=Object:set_metadata([{<<"Key123">>,<<"my123Value">>}]).
    
    # An existing object can be accessed directly from its container
    {ok,GotObject}=RefreshedContainer:get_object(ObjectName).
    
    # Object names and details can be queried
    {ok,[ObjectName]}=RefreshedContainer:get_objects_names().
    {ok,[ObjectName]}=RefreshedContainer:get_objects_names(#cf_object_query_args{limit=1}).
    {ok,[ObjectDetails]}=RefreshedContainer:get_objects_details().
    ObjectDetails = #cf_object_details{name=<<"test.xml">>, bytes=8, last_modified={{2010,9,9},{5,31,10}}, content_type=application/xml, etag=4366c359d1a7b9b248fa262775613699}
    
    # Read the whole data
    {ok,<<"<test/>">>}=Object:read_data().
    # Read the data with an offset and a size
    {ok,<<"test">>}=Object:read_data(1,4).
    
    # Refresh the object so its attributes and metadata are up to date
    {ok,RefreshedObject}=Object:refresh().
    
    # Get object attributes
    ObjectName=RefreshedObject:name().
    8=RefreshedObject:bytes().
    {{D,M,Y},{H,Mi,S}}=RefreshedObject:last_modified().
    <<"application/xml;charset=UTF-8">>=RefreshedObject:content_type().
    Etag=RefreshedObject:etag().
    
    # Get custom meta-data
    [{<<"Key123">>,<<"my123Value">>}]=RefreshedObject:metadata().
    
    # Delete the object
    ok=RefreshedObject:delete().
    
    # Make the container private
    ok=RefreshedContainer:make_private().
    
    # Delete an existing container
    ok=RefreshedContainer:delete().
    

More information
----------------

Read the Rackspace Cloud Files API specification: <http://www.rackspacecloud.com/cloud_hosting_products/files/api>

Contact the author: <david@dossot.net>

