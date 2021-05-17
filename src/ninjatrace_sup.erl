%%%-------------------------------------------------------------------
%% @doc ninjatrace top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ninjatrace_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

start_link() -> start_link(device).
start_link(Type) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Type]).

init([Type]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 10,
    period => 5},
  ChildSpecs = determine_child_specs(Type),
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
determine_child_specs(server) -> [
  #{
    id => ninjatrace_device_server,
    start => {ninjatrace_device_server, start_link, []}
  }
];
determine_child_specs(device) ->
  Sensors = application:get_env(ninjatrace, sensors, []),
  [
    #{
      id => ninjatrace_sensor_sup,
      start => {ninjatrace_sensor_sup, start_link, [Sensors]}
    },
    #{
      id => ninjatrace_device,
      start => {ninjatrace_device, start_link, [Sensors]}
    }
  ].