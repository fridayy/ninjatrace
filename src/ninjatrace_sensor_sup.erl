%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%% Supervises all active sensors
%%% @end
%%%-------------------------------------------------------------------
-module(ninjatrace_sensor_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

  Sensors = application:get_env(ninjatrace, sensors, []),
  % load the the active sensors from the configuration
  ninjatrace_logger:info(?MODULE, "Starting active Sensors = ~p~n", [Sensors]),
  SensorSpecs = lists:map(fun(Id) ->
    #{id => Id,
      start => {Id, start_link, []},
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
