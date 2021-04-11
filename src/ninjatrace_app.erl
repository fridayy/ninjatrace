%%%-------------------------------------------------------------------
%% @doc ninjatrace public API
%% @end
%%%-------------------------------------------------------------------

-module(ninjatrace_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).

start() ->
    StartCowboy = application:get_env(ninjatrace, server, false),
    init_cowboy(StartCowboy),
    ninjatrace_sup:start_link().

start(_StartType, _StartArgs) ->
    start().

stop(_State) ->
    ok.

%% internal functions
init_cowboy(true) ->
    %% TODO: dynamic routes would be nice for each device
    %% https://ninenines.eu/docs/en/cowboy/2.6/guide/routing/
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, ninjatrace, "index.html"}},
            {"/websocket/:device", ninjatrace_ws_handler, []}
            ]}
    ]),
    {ok, _} = cowboy:start_clear(ninjatrace_cowboy_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    );

init_cowboy(false) ->
    noop.