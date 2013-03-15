%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-define(US_API_BASE_URL, "identity.api.rackspacecloud.com").
-define(UK_API_BASE_URL, "lon.identity.api.rackspacecloud.com").
-define(VERSION_PATH, "/v1.0").

-define(DEFAULT_REQUEST_TIMEOUT, 30000).
-define(OBJECT_META_HEADER_PREFIX, "X-Object-Meta-").
-define(DIRECTORY_OBJECT_CONTENT_TYPE, <<"application/directory">>).

-define(IS_CONNECTION(C), is_record(C, cf_connection)).
-define(IS_CONTAINER(C), is_record(C, cf_container)).
-define(IS_OBJECT(O), is_record(O, cf_object)).

-record(cf_connection, {version :: string(), 
                        auth_token :: string(), 
                        storage_url :: string(), 
                        cdn_management_url :: string()
                       }).

-record(cf_account_info, {bytes_used, container_count}).

-record(cf_container_details, {name, bytes, count}).
-record(cf_container, {container_details :: #cf_container_details{},
                       container_path :: string(), 
                       cdn_details :: [{atom(), term()}]
                      }).

-record(cf_container_query_args, {marker, limit}).
-record(cf_container_cdn_config, {ttl = 86400, user_agent_acl, referrer_acl}).

-record(cf_object_details, {name, bytes = 0, last_modified, content_type, etag}).
-record(cf_object, {container :: #cf_container{}, 
                    object_details :: #cf_object_details{}, 
                    object_path :: string(), 
                    http_headers :: [{string(), string()}]
                   }).

-record(cf_object_query_args, {marker, limit, prefix, path}).

