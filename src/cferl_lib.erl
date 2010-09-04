%%%
%%% @doc Internal utilities
%%% @author David Dossot <david@dossot.net>
%%% @hidden
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_lib).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

-export([error_result/1,
         caseless_get_proplist_value/2,
         container_query_args_to_string/1, cdn_config_to_headers/1,
         object_query_args_to_string/1,
         url_encode/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Authenticate and open connection.
%% @spec error_result(HttpResponse) -> Error
%%   HttpResponse = tuple()
%%   Error = cferl_error()
error_result({ok, "404", _, _}) ->
  {error, not_found};
error_result({ok, "401", _, _}) ->
  {error, unauthorized};
error_result(Other) ->
  {error, {unexpected_response, Other}}.

%% @doc Get a value from a proplist with a case insentive search on key.
%% @spec caseless_get_proplist_value(Key::string(), Proplist::list()) -> Result::term() | undefined 
caseless_get_proplist_value(Key, Proplist) when is_list(Key), is_list(Proplist) ->
  proplists:get_value(string:to_lower(Key),
                      to_lower_case_keys(Proplist)).

%% @doc Convert a cf_container_query_args record into an URL encoded query string.
%% @spec container_query_args_to_string(QueryArgs::record()) -> string()
container_query_args_to_string(#cf_container_query_args{marker=Marker, limit=Limit}) ->
  QueryElements =
    [
      case Marker of
        _ when is_binary(Marker) -> "marker=" ++ url_encode(Marker);
        _ -> undefined
      end,
      case Limit of
        _ when is_integer(Limit) -> "limit=" ++ integer_to_list(Limit);
        _ -> undefined
      end
    ],
    
  query_args_to_string(string:join(filter_undefined(QueryElements), "&")).
    
%% @doc Convert a cf_container_cdn_config into a list of HTTP headers.
%% @spec cdn_config_to_headers(CdnConfig::record()) -> [{HeaderName, HeaderValue}]
cdn_config_to_headers(#cf_container_cdn_config{ttl=Ttl, user_agent_acl=UaAcl, referrer_acl = RAcl}) ->
  CdnConfigHeaders =
  [
    case Ttl of
      _ when is_integer(Ttl) -> {"X-TTL", integer_to_list(Ttl)};
      _ -> undefined
    end,
    case UaAcl of
      _ when is_binary(UaAcl) -> {"X-User-Agent-ACL", url_encode(UaAcl)};
      _ -> undefined
    end,
    case RAcl of
      _ when is_binary(RAcl) -> {"X-Referrer-ACL", url_encode(RAcl)};
      _ -> undefined
    end
  ],
  
  filter_undefined(CdnConfigHeaders).

%% @doc Convert a cf_object_query_args record into an URL encoded query string.
%% @spec object_query_args_to_string(QueryArgs::record()) -> string()
object_query_args_to_string(#cf_object_query_args{marker=Marker, limit=Limit, prefix=Prefix, path=Path}) ->
  QueryElements =
    [
      case Marker of
        _ when is_integer(Marker) -> "marker=" ++ integer_to_list(Marker);
        _ -> undefined
      end,
      case Limit of
        _ when is_integer(Limit) -> "limit=" ++ integer_to_list(Limit);
        _ -> undefined
      end,
      case Prefix of
        _ when is_binary(Prefix) -> "prefix=" ++ url_encode(Prefix);
        _ -> undefined
      end,
      case Path of
        _ when is_binary(Path) -> "path=" ++ url_encode(Path);
        _ -> undefined
      end
    ],
    
  query_args_to_string(string:join(filter_undefined(QueryElements), "&")).

%% @doc Encodes a binary URL element into a string.
%% @spec url_encode(Bin::binary()) -> string().
url_encode(Bin) when is_binary(Bin) ->
  ibrowse_lib:url_encode(binary_to_list(Bin)).

%% Private functions
query_args_to_string("") ->
  "";
query_args_to_string(QueryString) ->
  "?" ++ QueryString.

to_lower_case_keys(Proplist) ->
  [{string:to_lower(K), V} || {K, V} <- Proplist].

filter_undefined(List) when is_list(List) ->
  lists:filter(fun(Entry) -> Entry =/= undefined end, List).
  
%% Tests
-ifdef(TEST).

caseless_get_proplist_value_test() ->
  ?assert(undefined == caseless_get_proplist_value("CcC", [{"AaA", 1}, {"bBb", 2}])),
  ?assert(2 == caseless_get_proplist_value("bbb", [{"AaA", 1}, {"bBb", 2}])),
  ok.

container_query_args_to_string_test() ->
  ?assert("" == container_query_args_to_string(#cf_container_query_args{})),
  ?assert("?limit=12" == container_query_args_to_string(#cf_container_query_args{limit=12})),
  ?assert("?marker=abc" == container_query_args_to_string(#cf_container_query_args{marker= <<"abc">>})),
  ?assert("?marker=def&limit=25" == container_query_args_to_string(#cf_container_query_args{limit=25,marker= <<"def">>})),
  ?assert("" == container_query_args_to_string(#cf_container_query_args{marker="bad_value"})),
  ?assert("" == container_query_args_to_string(#cf_container_query_args{limit=bad_value})),
  ok.

cdn_config_to_headers_test() ->
  ?assert([{"X-TTL", "86400"}] == cdn_config_to_headers(#cf_container_cdn_config{})),
  ?assert([{"X-TTL", "3000"}] == cdn_config_to_headers(#cf_container_cdn_config{ttl=3000})),
  ?assert([{"X-TTL", "3000"},{"X-User-Agent-ACL", "ua_acl"}] == cdn_config_to_headers(#cf_container_cdn_config{ttl=3000,user_agent_acl= <<"ua_acl">>})),
  ?assert([{"X-TTL", "3000"},{"X-User-Agent-ACL", "ua_acl"},{"X-Referrer-ACL","r_acl"}] == cdn_config_to_headers(#cf_container_cdn_config{ttl=3000,user_agent_acl= <<"ua_acl">>,referrer_acl= <<"r_acl">>})),
  ?assert([] == cdn_config_to_headers(#cf_container_cdn_config{ttl=bad_value})),
  ?assert([{"X-TTL", "86400"}] == cdn_config_to_headers(#cf_container_cdn_config{user_agent_acl=bad_value})),
  ?assert([{"X-TTL", "86400"}] == cdn_config_to_headers(#cf_container_cdn_config{referrer_acl=bad_value})),
  ok.
  
object_query_args_to_string_test() ->
  ?assert("" == object_query_args_to_string(#cf_object_query_args{})),
  ?assert("?limit=12" == object_query_args_to_string(#cf_object_query_args{limit=12})),
  ?assert("?marker=2" == object_query_args_to_string(#cf_object_query_args{marker=2})),
  ?assert("?marker=3&limit=25" == object_query_args_to_string(#cf_object_query_args{limit=25,marker=3})),
  ?assert("?marker=3&limit=25&prefix=prefoo&path=patbar" == object_query_args_to_string(#cf_object_query_args{prefix= <<"prefoo">>, path= <<"patbar">>, limit=25,marker=3})),
  ?assert("" == object_query_args_to_string(#cf_object_query_args{marker="bad_value"})),
  ?assert("" == object_query_args_to_string(#cf_object_query_args{limit=bad_value})),
  ?assert("" == object_query_args_to_string(#cf_object_query_args{prefix=123})),
  ?assert("" == object_query_args_to_string(#cf_object_query_args{path=true})),
  ok.

-endif.
