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
  TempSensor = #{id => ninjatrace_temp_sensor,
    start => {ninjatrace_temp_sensor, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker},

  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [
      TempSensor
    ]}
  }.
