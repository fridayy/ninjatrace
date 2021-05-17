%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Abstraction over a currently running and connected devices
%%% @end
%%%-------------------------------------------------------------------
-module(ninjatrace_device).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([info/1, sensors/1]).

-include("types.hrl").

-record(state, {
  sensors :: list(sensor_spec())
}).

%% API

%% @doc
%% Returns aggregated information about a particular device.
%% @end
-spec(info(device()) -> {ok, [sensor_info()]} | {error, atom()}).
info(Device) when is_atom(Device) ->
  gen_server:call({?MODULE, Device}, get_info);

info(Device) when is_binary(Device) ->
  case ninjatrace_device_server:is_registered(Device) of
    {ok, DeviceAtom} -> info(DeviceAtom);
    Error -> Error
  end.

%% @doc
%% Returns the currently enabled sensors for a particular device.
%% @end
-spec(sensors(device() | string()) -> {ok, [sensor_spec()]} | {error, no_device}).
sensors(Device) ->
  gen_server:call({?MODULE, Device}, get_sensors);

sensors(Device) when is_binary(Device) ->
  case ninjatrace_device_server:is_registered(Device) of
    {ok, DeviceAtom} -> {ok, sensors(DeviceAtom)};
    Error -> Error
  end.

start_link(Sensors) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Sensors, []).

init(Sensors) ->
  ninjatrace_logger:info(?MODULE, "Active Sensors: ~p", [Sensors]),
  {ok, #state{sensors = Sensors}}.

handle_call(get_info, _From, #state{sensors = Sensors} = State) ->
  Info = lists:map(fun({SensorModule, _Config}) -> #{SensorModule:name() => SensorModule:info()} end, Sensors),
  {reply, {ok, Info}, State};

handle_call(get_sensors, _From, #state{sensors = Sensors} = State) ->
  {reply, {ok, Sensors}, State};

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.
