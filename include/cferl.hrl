%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-define(API_BASE_URL, "auth.api.rackspacecloud.com").
-define(VERSION_PATH, "/v1.0").

-record(cf_account_info, {bytes_used, container_count}).
-record(cf_container_info, {name, bytes, size}).
