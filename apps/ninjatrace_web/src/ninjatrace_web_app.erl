%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ninjatrace_web_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  init().

stop(_State) ->
  ok.

init() ->
  %% TODO: dynamic routes would be nice for each device
  %% https://ninenines.eu/docs/en/cowboy/2.6/guide/routing/
  ninjatrace_logger:info(?MODULE, "Starting Web on port: 8080"),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, ninjatrace_web, "index.html"}},
      {"/assets/[...]", cowboy_static, {priv_dir, ninjatrace_web, "/assets"}},
      {"/devices", ninjatrace_web_device_h, []},
      {"/info/:device_name", ninjatrace_web_info_h, []},
      {"/info/ws/:device_name", ninjatrace_web_websocket_h, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(ninjatrace_cowboy_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ).
