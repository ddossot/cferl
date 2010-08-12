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
         caseless_get_proplist_value/2, query_args_to_string/1]).

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

%% @doc Turn a cf_container_query_args record into a ULR encoded query string.
%% @spec query_args_to_string(QueryArgs::record()) -> string()
query_args_to_string(#cf_container_query_args{marker=Marker, limit=Limit}) ->
  QueryElements =
    [
      case Marker of
        undefined -> undefined;
        _ -> "marker=" ++ ibrowse_lib:url_encode(binary_to_list(Marker))
      end,
      case Limit of
        undefined -> undefined;
        _ -> "limit=" ++ integer_to_list(Limit)
      end
    ],
    
  query_args_to_string_result(
    string:join(lists:filter(fun(QE) -> QE =/= undefined end, QueryElements), "&")).
    
query_args_to_string_result("") ->
  "";
query_args_to_string_result(QueryString) ->
  "?" ++ QueryString.

%% Private functions
to_lower_case_keys(Proplist) ->
  [{string:to_lower(K), V} || {K, V} <- Proplist].

  
%% Tests
-ifdef(TEST).

caseless_get_proplist_value_test() ->
  ?assert(undefined == caseless_get_proplist_value("CcC", [{"AaA", 1}, {"bBb", 2}])),
  ?assert(2 == caseless_get_proplist_value("bbb", [{"AaA", 1}, {"bBb", 2}])).

query_args_to_string_test() ->
  ?assert("" == query_args_to_string(#cf_container_query_args{})),
  ?assert("?limit=12" == query_args_to_string(#cf_container_query_args{limit=12})),
  ?assert("?marker=abc" == query_args_to_string(#cf_container_query_args{marker= <<"abc">>})),
  ?assert("?marker=def&limit=25" == query_args_to_string(#cf_container_query_args{limit=25,marker= <<"def">>})).

-endif.
