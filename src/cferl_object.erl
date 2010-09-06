%%%
%%% @doc Rackspace Cloud Files Erlang Client
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(cferl_object, [Connection, ObjectDetails, ObjectPath, MetaData]).
-author('David Dossot <david@dossot.net>').
-include("cferl.hrl").

-export([name/0, bytes/0, last_modified/0, content_type/0, etag/0]).

%% @doc Name of the current object.
%% @spec name() -> binary()
name() ->
  ObjectDetails#cf_object_details.name.

%% @doc Size in bytes of the current object.
%% @spec bytes() -> integer()
bytes() ->
  ObjectDetails#cf_object_details.bytes.

%% @doc Date and time of the last modification of the current object.
%% @spec last_modified() -> {Date:date(), Time:time()}
last_modified() ->
  ObjectDetails#cf_object_details.last_modified.

%% @doc Content type of the current object.
%% @spec content_type() -> binary()
content_type() ->
  ObjectDetails#cf_object_details.content_type.

%% @doc Etag of the current object.
%% @spec etag() -> binary()
etag() ->
  ObjectDetails#cf_object_details.etag.

% TODO add: metadata (get/set), data (get/set), data_stream (get/set) delete
