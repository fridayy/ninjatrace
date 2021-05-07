%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2021 9:26 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_web).
-author("bnjm").

%% API
-export([init/0]).

%% internal functions
init() ->
  %% TODO: dynamic routes would be nice for each device
  %% https://ninenines.eu/docs/en/cowboy/2.6/guide/routing/
  application:ensure_all_started(cowboy),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, ninjatrace, "index.html"}},
      {"/assets/[...]", cowboy_static, {priv_dir, ninjatrace, "/assets"}},
      {"/devices", ninjatrace_info_handler, []},
      {"/info/:device_name", ninjatrace_info_handler, []},
      {"/info/ws/:device_name", ninjatrace_ws_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(ninjatrace_cowboy_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ).
