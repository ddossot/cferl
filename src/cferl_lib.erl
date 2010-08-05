%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_lib).
-author('David Dossot <david@dossot.net>').

-export([caseless_get_proplist_value/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Get a value from a proplist with a case insentive search on key.
%% @spec caseless_get_proplist_value(Key::string(), Proplist::list()) -> Result::term() | undefined 
caseless_get_proplist_value(Key, Proplist) when is_list(Key), is_list(Proplist) ->
  proplists:get_value(string:to_lower(Key),
                      to_lower_case_keys(Proplist)).

%% Private functions
to_lower_case_keys(Proplist) ->
  [{string:to_lower(K), V} || {K, V} <- Proplist].

  
%% Tests
-ifdef(TEST).

caseless_get_proplist_value_test() ->
    ?assert(undefined == caseless_get_proplist_value("CcC", [{"AaA", 1}, {"bBb", 2}])),
    ?assert(2 == caseless_get_proplist_value("bbb", [{"AaA", 1}, {"bBb", 2}])).

-endif.
