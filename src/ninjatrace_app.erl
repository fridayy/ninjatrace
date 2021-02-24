%%%-------------------------------------------------------------------
%% @doc ninjatrace public API
%% @end
%%%-------------------------------------------------------------------

-module(ninjatrace_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ninjatrace_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
