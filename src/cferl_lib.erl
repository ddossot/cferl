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
         get_int_header/2, get_boolean_header/2, get_binary_header/2, get_string_header/2,
         container_query_args_to_string/1, cdn_config_to_headers/1,
         object_query_args_to_string/1,
         url_encode/1, extract_object_meta_headers/1]).

-define(TEST_HEADERS, [{"int", "123"}, {"bool", "true"}, {"str", "abc"}]).

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

%% @doc Get an integer value from a proplist with a case insentive search on key.
%%   Return 0 if the key is not found. 
%% @spec get_int_header(Key::string(), Proplist::list()) -> integer() 
get_int_header(Name, Headers) when is_list(Headers) ->
  list_to_int(caseless_get_proplist_value(Name, Headers)).

%% @doc Get a boolean value from a proplist with a case insentive search on key.
%%   Return false if the key is not found. 
%% @spec get_int_header(Key::string(), Proplist::list()) -> boolean() 
get_boolean_header(Name, Headers) when is_list(Headers) ->
  list_to_boolean(caseless_get_proplist_value(Name, Headers)).

%% @doc Get a binary value from a proplist with a case insentive search on key.
%%   Return <<>> if the key is not found. 
%% @spec get_binary_header(Key::string(), Proplist::list()) -> binary() 
get_binary_header(Name, Headers) when is_list(Headers) ->
  list_to_bin(caseless_get_proplist_value(Name, Headers)).

%% @doc Get a string value from a proplist with a case insentive search on key.
%%   Return "" if the key is not found. 
%% @spec get_string_header(Key::string(), Proplist::list()) -> string() 
get_string_header(Name, Headers) when is_list(Headers) ->
  list_to_string(caseless_get_proplist_value(Name, Headers)).

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

%% @doc Encode a binary URL element into a string.
%% @spec url_encode(Bin::binary()) -> string().
url_encode(Bin) when is_binary(Bin) ->
  ibrowse_lib:url_encode(binary_to_list(Bin)).

%% @doc Extract the HTTP headers that are object metadata, remove their prefix and turn them into binary.
%% @spec extract_object_meta_headers(HttpHeaders::proplist()) -> [{Key::binary(),Value::binary()}].
extract_object_meta_headers(HttpHeaders) when is_list(HttpHeaders) ->
  {ok, Re} = re:compile("^" ++ ?OBJECT_META_HEADER_PREFIX, [caseless]),
  
  MetaHeaders =
    lists:filter(fun({Key, _}) ->
                   re:run(Key, Re) =/= nomatch
                 end,
                 HttpHeaders),
  [{re:replace(Key, Re, <<>>, [{return, binary}]), list_to_binary(Value)} || {Key, Value} <- MetaHeaders].

%% Private functions

caseless_get_proplist_value(Key, Proplist) when is_list(Key), is_list(Proplist) ->
  proplists:get_value(string:to_lower(Key),
                      to_lower_case_keys(Proplist)).

list_to_int(List) when is_list(List) ->
  list_to_integer(List);
list_to_int(_) ->
  0.

list_to_boolean(List) when is_list(List) ->
  string:to_lower(List) == "true";
list_to_boolean(_) ->
  false.

list_to_bin(List) when is_list(List) ->
  list_to_binary(List);
list_to_bin(_) ->
  <<>>.
  
list_to_string(List) when is_list(List) ->
  List;
list_to_string(_) ->
  "".

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
  ?assert(undefined == caseless_get_proplist_value("foo", ?TEST_HEADERS)),
  ?assert("abc" == caseless_get_proplist_value("STR", ?TEST_HEADERS)),
  ok.
  
get_int_header_test() ->
  ?assert(0 == get_int_header("foo", ?TEST_HEADERS)),
  ?assert(123 == get_int_header("INT", ?TEST_HEADERS)),
  ok.
  
get_boolean_header_test() ->
  ?assert(false == get_boolean_header("foo", ?TEST_HEADERS)),
  ?assert(true == get_boolean_header("BOOL", ?TEST_HEADERS)),
  ok.
  
get_binary_header_test() ->
  ?assert(<<>> == get_binary_header("foo", ?TEST_HEADERS)),
  ?assert(<<"abc">> == get_binary_header("STR", ?TEST_HEADERS)),
  ok.
  
get_string_header_test() ->
  ?assert("" == get_string_header("foo", ?TEST_HEADERS)),
  ?assert("abc" == get_string_header("STR", ?TEST_HEADERS)),
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

extract_object_meta_headers_test() ->
  TestHeaders = [
    {"Date", "Thu, 07 Jun 2007 20:59:39 GMT"},
    {"Server", "Apache"},
    {"Last-Modified", "Fri, 12 Jun 2007 13:40:18 GMT"},
    {"ETag", "8a964ee2a5e88be344f36c22562a6486"},
    {"Content-Length", "512000"},
    {"Content-Type", "text/plain; charset=UTF-8"},
    {"X-Object-Meta-Meat", "Bacon"},
    {"x-object-meta-fruit", "Orange"},
    {"X-Object-Meta-Veggie", "Turnip"},
    {"x-object-meta-fruit", "Cream"}],
  
  ExpectedMetas = [
    {<<"Meat">>, <<"Bacon">>},
    {<<"fruit">>, <<"Orange">>},
    {<<"Veggie">>, <<"Turnip">>},
    {<<"fruit">>, <<"Cream">>}],
    
  ?assert(ExpectedMetas == extract_object_meta_headers(TestHeaders)),
  ok.
  
-endif.
