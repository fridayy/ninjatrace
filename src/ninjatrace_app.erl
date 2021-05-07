%%%-------------------------------------------------------------------
%% @doc ninjatrace public API
%% @end
%%%-------------------------------------------------------------------

-module(ninjatrace_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).

start() ->
    Type = application:get_env(ninjatrace, type, device),
    case Type of
        server -> ninjatrace_web:init();
        device -> noop
    end,
    ninjatrace_sup:start_link(Type).

start(_StartType, _StartArgs) ->
    start().

stop(_State) ->
    ok.