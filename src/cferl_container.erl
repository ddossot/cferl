%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
%%% @type cferl_error() = {error, not_found} | {error, unauthorized} | {error, {unexpected_response, Other}}.
%%% @type cf_container_cdn_config = record(). Record of type cf_container_cdn_config.

-module(cferl_container, [Connection, Name, Bytes, Count, CdnDetails]).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

%% Public API
-export([name/0, bytes/0, count/0, is_empty/0, is_public/0, cdn_url/0, cdn_ttl/0, log_retention/0,
         make_public/0, make_public/1, make_private/0, set_log_retention/1,
         refresh/0, delete/0]).

%% @doc Name of the current container.
%% @spec name() -> binary()
name() ->
  Name.

%% @doc Size in bytes of the current container.
%% @spec bytes() -> integer()
bytes() ->
  Bytes.

%% @doc Number of objects in the current container.
%% @spec count() -> integer()
count() ->
  Count.

%% @doc Determine if the current container is empty.
%% @spec is_empty() -> true | false
is_empty() ->
  count() == 0.

%% @doc Determine if the current container is public (CDN-enabled).
%% @spec is_public() -> true | false
is_public() ->
  proplists:get_value(cdn_enabled, CdnDetails).

%% @doc CDN of the container URL, if it is public.
%% @spec cdn_url() -> binary().
cdn_url() ->
  proplists:get_value(cdn_uri, CdnDetails).

%% @doc TTL (in seconds) of the container, if it is public.
%% @spec cdn_ttl() -> integer().
cdn_ttl() ->
  proplists:get_value(ttl, CdnDetails).

%% @doc Determine if log retention is enabled on this container (which must be public).
%% @spec log_retention() -> true | false
log_retention() ->
  is_public() andalso proplists:get_value(log_retention, CdnDetails).

%% @doc Make the current container publicly accessible on CDN, using the default configuration (ttl of 1 day and no ACL).
%% @spec make_public() -> ok | Error
%%   Error = cferl_error()
make_public() ->
  make_public(#cf_container_cdn_config{}).

%% @doc Make the current container publicly accessible on CDN, using the provided configuration.
%%   ttl is in seconds.
%%   user_agent_acl and referrer_acl are Perl-compatible regular expression used to limit access to this container. 
%% @spec make_public(CdnConfig) -> ok | Error
%%   CdnConfig = cf_container_cdn_config()
%%   Error = cferl_error()
make_public(CdnConfig) when is_record(CdnConfig, cf_container_cdn_config) ->
  PutResult = Connection:send_cdn_management_request(put, Connection:get_container_path(Name), raw),
  make_public_put_result(CdnConfig, PutResult).

make_public_put_result(CdnConfig, {ok, ResponseCode, _, _}) when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  CdnConfigHeaders = cferl_lib:cdn_config_to_headers(CdnConfig),
  Headers = [{"X-CDN-Enabled", "True"} | CdnConfigHeaders],
  PostResult = Connection:send_cdn_management_request(post, Connection:get_container_path(Name), Headers, raw),
  make_public_post_result(PostResult);
make_public_put_result(_, Other) ->
  cferl_lib:error_result(Other).

make_public_post_result({ok, ResponseCode, _, _}) when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
make_public_post_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Make the current container private.
%%   If it was previously public, it will remain accessible on the CDN until its TTL is reached. 
%% @spec make_private() -> ok | Error
%%   Error = cferl_error()
make_private() ->
  Headers = [{"X-CDN-Enabled", "False"}],
  PostResult = Connection:send_cdn_management_request(post, Connection:get_container_path(Name), Headers, raw),
  make_private_result(PostResult).

make_private_result({ok, ResponseCode, _, _}) when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
make_private_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Activate or deactivate log retention for current container.
%% @spec set_log_retention(true | false) -> ok | Error
%%   Error = cferl_error()
set_log_retention(true) ->
  do_set_log_retention("True");
set_log_retention(false) ->
  do_set_log_retention("False").
  
do_set_log_retention(State) ->
  Headers = [{"x-log-retention", State}],
  PostResult = Connection:send_cdn_management_request(post, Connection:get_container_path(Name), Headers, raw),
  set_log_retention_result(PostResult).
  
set_log_retention_result({ok, ResponseCode, _, _}) when ResponseCode =:= "201"; ResponseCode =:= "202" ->
  ok;
set_log_retention_result(Other) ->
  cferl_lib:error_result(Other).

%% @doc Refresh the current container reference.
%% @spec refresh() -> {ok, Container} | Error
%%   Container = cferl_container()
%%   Error = cferl_error()
refresh() ->
  Connection:get_container(Name).

%% @doc Delete the current container (which must be empty).
%% @spec delete() -> ok | Error
%%   Error = {error, not_empty} | cferl_error()
delete() ->
  Result = Connection:send_storage_request(delete, Connection:get_container_path(Name), raw),
  delete_result(Result).

delete_result({ok, "204", _, _}) ->
  ok;
delete_result({ok, "409", _, _}) ->
  {error, not_empty};
delete_result(Other) ->
  cferl_lib:error_result(Other).

%% TODO add: object_exists() objects_details() objects_names() new_object()
