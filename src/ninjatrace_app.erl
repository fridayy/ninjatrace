%%%-------------------------------------------------------------------
%% @doc ninjatrace public API
%% @end
%%%-------------------------------------------------------------------

-module(ninjatrace_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).

start() ->
    Type = application:get_env(ninjatrace, mode, device),
    case Type of
        % start the web application only in server mode
        server ->
            ninjatrace_logger:info(?MODULE, "Starting in server mode"),
            ninjatrace_mnesia:boot(),
            {ok, _} = application:ensure_all_started(ninjatrace_web);
        device ->
            ninjatrace_logger:info(?MODULE, ("Starting in device mode"))
    end,
    ninjatrace_sup:start_link(Type).

start(_StartType, _StartArgs) ->
    start().

stop(_State) ->
    ok.