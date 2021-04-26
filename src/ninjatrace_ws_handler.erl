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
  DeviceName = cowboy_req:binding(device_name, Req),
  case ninjatrace_device_server:is_node(DeviceName) of
    {ok, Node} -> {cowboy_websocket, Req, Node};
    {error, no_node} ->
      Res = cowboy_req:reply(404, #{
      <<"content_type">> => <<"text/html">>
    }, <<"no such device">>, Req),
      {ok, Res, State}
  end.

websocket_init(DeviceName) ->
  ninjatrace_logger:info(?MODULE, "new websocket connection: ~p", [self()]),
  ninjatrace_device_server:subscribe(DeviceName, self()),
  {ok, DeviceName}.

websocket_handle(_Data, State) ->
  ninjatrace_logger:info(?MODULE, "received message from client - ignoring"),
  {ok, State}.

websocket_info({ok, Map}, State) when is_map(Map) ->
  {reply, {text, jsone:encode(Map, [{float_format, [{decimals, 6}]}])}, State};

websocket_info({error, Reason}, State) ->
  {reply, {text, jsone:encode(#{error => Reason})}, State}.

terminate(_Reason, Req, DeviceName) ->
  io:format("websocket connection terminated~n~p~n", [maps:get(peer, Req)]),
  ninjatrace_device_server:unsubscribe(DeviceName, self()),
  ok.