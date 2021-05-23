%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2021 12:16 AM
%%%-------------------------------------------------------------------
-module(ninjatrace).
-author("bnjm").

-include("types.hrl").

%% API
-export([servers/0, devices/0]).

%% @doc
%% Returns a list of all servers currently in the cluster
%% @end
-spec(servers() -> [node()]).
servers() ->
  Devices = devices(),
  lists:filter(fun(B) -> lists:member(B, Devices) =/= true  end, nodes()).

%% @doc
%% Returns a list of all devices available in the cluster
%% @end
-spec(devices() -> [device()]).
devices() ->
  ninjatrace_device_server:registered_devices().