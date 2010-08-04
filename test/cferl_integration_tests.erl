%%%
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_integration_tests).
-author('David Dossot <david@dossot.net>').

-export([start/0]).

start() ->
  application:start(sasl),
  application:start(ssl),
  application:start(ibrowse),

  {ok, [Username]} = io:fread("Username : ", "~s"),
  {ok, [ApiKey]} = io:fread("API Key : ", "~s"),
  CloudFiles = cferl:connect(Username, ApiKey),
  io:format("~n~p~n", [CloudFiles]),
  
  init:stop().
