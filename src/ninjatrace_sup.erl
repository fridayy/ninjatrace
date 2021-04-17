%%%-------------------------------------------------------------------
%% @doc ninjatrace top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ninjatrace_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    Sensors = application:get_env(ninjatrace, sensors, []),
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 5},
    ChildSpecs = [
        #{
            id => ninjatrace_sensor_sup,
            start => {ninjatrace_sensor_sup, start_link, [Sensors]}
        },
        #{
            id => ninjatrace_device,
            start => {ninjatrace_device, start_link, [Sensors]}
        }
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
