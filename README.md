Rackspace Cloud Files Erlang Client
===================================

> Version 0.1 - Currently under initial development!

Description
-----------

This is an Erlang interface into the Rackspace Cloud Files service. It has been largely inspired by the existing [Ruby](http://github.com/rackspace/ruby-cloudfiles) API.


Building
--------

cferl relies on [rebar](http://bitbucket.org/basho/rebar/wiki/Home) for its build and dependency management and targets Erlang/OTP R13B04 or above.

Simply run:

    ./rebar get-deps compile eunit

To generate the cferl documentation, run:

    ./rebar delete-deps doc


Using
-----

cferl requires that the ssl and ibrowse applications be started prior to using it.

The following, which is the output when running the integration tests, demonstrates a typical usage of the API. Refer to the documentation for the complete reference.

    # Connect to Cloud Files (warning: the underlying authentication toke will only last for 24 hours!)
    {ok,CloudFiles}=cferl:connect(Username,ApiKey).
    
    # Retrieve the account information record
    {ok,Info}=CloudFiles:get_account_info().
    Info = #cf_account_info{bytes_used=360, container_count=1}
    
    # Retrieve names of all existing containers (within the limits imposed by Cloud Files server)
    {ok,Names}=CloudFiles:get_containers_names().
    Names=[<<"cferl-test">>]

    # Retrieve names of a maximum of 3 existing containers
    {ok,ThreeNamesMax}=CloudFiles:get_containers_names(#cf_container_query_args{limit=3}).
    ThreeNamesMax=[<<"cferl-test">>]

    # Retrieve details for all existing containers (within the server limits)
    {ok,ContainersDetails}=CloudFiles:get_containers_details().
    # ContainersDetails is a list of #cf_container_details records
    [Detail|_]=ContainersDetails.
    Detail = #cf_container_details{name=<<"cferl-test">>, bytes=360, count=1}
    
    # Retrieve details for a maximum of 5 containers whose names start at cf
    {ok,CfContainersDetails}=CloudFiles:get_containers_details(#cf_container_query_args{marker=<<"cf">>,limit=5}).
    
    # Get a container reference by name
    {ok,Container}=CloudFiles:get_container(<<"cferl-test">>).
    
    # Get container details from its reference
    ContainerName=Container:name().
    ContainerBytes=Container:bytes().
    ContainerSize=Container:count().
    ContainerIsEmpty=Container:is_empty().
    # > Name: <<"cferl-test">> - Bytes: 360 - Size: 1 - IsEmpty: false
    
    # Check a container's existence
    false=CloudFiles:container_exists(<<"new_container">>).
    
    # Create a new container
    {ok,NewContainer}=CloudFiles:create_container(<<"new_container">>).
    
    true=CloudFiles:container_exists(<<"new_container">>).
    
    <<"new_container">>=NewContainer:name().
    0=NewContainer:bytes().
    0=NewContainer:count().
    true=NewContainer:is_empty().
    false=NewContainer:is_public().
    
    # Delete an existing container
    ok=NewContainer:delete().


More information
----------------

Read the Rackspace Cloud Files API specification: <http://www.rackspacecloud.com/cloud_hosting_products/files/api>
Contact the author: <david@dossot.net>

