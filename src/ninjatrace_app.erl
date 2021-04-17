%%%-------------------------------------------------------------------
%% @doc ninjatrace public API
%% @end
%%%-------------------------------------------------------------------

-module(ninjatrace_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).

start() ->
    Type = application:get_env(ninjatrace, type, device),
    init_cowboy(Type),
    ninjatrace_sup:start_link(Type).

start(_StartType, _StartArgs) ->
    start().

stop(_State) ->
    ok.

%% internal functions
init_cowboy(server) ->
    %% TODO: dynamic routes would be nice for each device
    %% https://ninenines.eu/docs/en/cowboy/2.6/guide/routing/
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, ninjatrace, "index.html"}},
            {"/info/:device_name/", ninjatrace_info_handler, []},
            {"/websocket/:device_name", ninjatrace_ws_handler, []}
            ]}
    ]),
    {ok, _} = cowboy:start_clear(ninjatrace_cowboy_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    );

%% no need to start cowboy on devices
init_cowboy(device) ->
    noop.