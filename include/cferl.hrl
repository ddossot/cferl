%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-define(API_BASE_URL, "auth.api.rackspacecloud.com").
-define(VERSION_PATH, "/v1.0").

-define(OBJECT_META_HEADER_PREFIX, "X-Object-Meta-").

-record(cf_account_info, {bytes_used, container_count}).

-record(cf_container_query_args, {marker, limit}).
-record(cf_container_details, {name, bytes, count}).
-record(cf_container_cdn_config, {ttl = 86400, user_agent_acl, referrer_acl}).

-record(cf_object_query_args, {marker, limit, prefix, path}).
-record(cf_object_details, {name, bytes, last_modified, content_type, etag}).

