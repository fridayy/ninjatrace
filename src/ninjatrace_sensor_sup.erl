%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%% Supervises all active sensors
%%% @end
%%%-------------------------------------------------------------------
-module(ninjatrace_sensor_sup).

-behaviour(supervisor).

-include("types.hrl").
-export([start_link/1, init/1]).

start_link(Sensors) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Sensors).

-spec(init([sensor_spec()]) -> {ok, #{}}).
init(Sensors) ->
  ninjatrace_logger:info(?MODULE, "Starting active Sensors = ~p~n", [Sensors]),
  SensorSpecs = lists:map(fun({SensorName, SensorConfig}) ->
    #{id => SensorName,
      start => {SensorName, start_link, [SensorConfig]},
      restart => permanent,
      shutdown => 2000,
      type => worker}
                          end, Sensors),
  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    SensorSpecs
    }
  }.
