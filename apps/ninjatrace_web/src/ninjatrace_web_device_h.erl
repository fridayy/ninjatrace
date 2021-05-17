%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2021 8:22 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_web_device_h).
-author("bnjm").

%% API
-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, to_json}
  ], Req, State}.

to_json(Req, State) ->
  Devices = ninjatrace_device_server:registered_devices(),
  DevicesMap = lists:map(fun(Device) ->
    #{
      name => Device,
      sensors => sensors_to_json(Device)
    } end, Devices),
  {jsone:encode(DevicesMap), Req, State}.

sensors_to_json(Device) ->
  {ok, Sensors} = ninjatrace_device:sensors(Device),
  lists:map(fun({Module, SensorConfig}) -> #{name => Module:name(), config => SensorConfig} end, Sensors).