%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2021 7:36 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_ws_handler).
-author("bnjm").

%% API
-export([init/2, websocket_init/1, websocket_info/2, terminate/3, websocket_handle/2]).

init(Req, State) ->
  {cowboy_websocket, Req, State}.

websocket_init([]) ->
  ninjatrace_logger:info(?MODULE, "new websocket connection: ~p", [self()]),
  {ok, []}.

websocket_handle(_Data, State) ->
  ninjatrace_logger:info(?MODULE, "received message from client"),
  {ok, State}.

websocket_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, Req, _State) ->
  io:format("websocket connection terminated~n~p~n", [maps:get(peer, Req)]),
  ok.