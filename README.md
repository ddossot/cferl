Rackspace Cloud Files Erlang Client
===================================

> Version 0.1 - Currently under initial development


Building
--------

cferl relies on [rebar](http://bitbucket.org/basho/rebar/wiki/Home) for its build and dependency management.

Simply run:

    ./rebar get-deps compile eunit

Alternatively, you can run the integration tests which also builds everything:

    ./int_tests

Be sure to have your API key ready before doing so, as they will be needed.

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
    {ok,ThreeNames}=CloudFiles:get_containers_names(#cf_query_args{limit=3}).
    ThreeNames=[<<"cferl-test">>]

    # Retrieve information for all existing containers (within the server limits)
    {ok,ContainersInfo}=CloudFiles:get_containers_info().
    # ContainersInfo is a list of #cf_container_info records
    [Info|_]=ContainersInfo.
    Info = #cf_container_info{name=<<"cferl-test">>, bytes=360, count=1}
    
    # Retrieve information for a maximum of 5 containers whose names start at cf
    {ok,CfContainersInfo}=CloudFiles:get_containers_info(#cf_query_args{marker=<<"cf">>,limit=5}).
    
    # Check a container's existence
    false=CloudFiles:container_exists(<<"new_container">>).
    
    # Create a new container
    {ok,Container}=CloudFiles:create_container(<<"new_container">>).
    true=CloudFiles:container_exists(<<"new_container">>).
    
    # Delete an existing container
    ok=Container:delete().

More information
----------------

Read the Rackspace Cloud Files API specification: <http://www.rackspacecloud.com/cloud_hosting_products/files/api>
Contact the author: <david@dossot.net>

