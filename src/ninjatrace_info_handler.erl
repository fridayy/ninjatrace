%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Handles the /info/:device route
%%% @end
%%% Created : 17. Apr 2021 12:04 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_info_handler).
-author("bnjm").

%% API
-export([init/2, content_types_provided/2]).
-export([rest_info_response/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, rest_info_response}
  ], Req, State}.

rest_info_response(Req, State) ->
  get_device_info(Req, State, {
    fun(Node) -> jsone:encode(ninjatrace_device:info(Node)) end,
    fun(_InvalidDevice) -> jsone:encode(#{error => unknown_device}) end
  }).

%% internal
get_device_info(Req, State, {PosRes, NegRes}) ->
  DeviceName = cowboy_req:binding(device_name, Req, <<"Unknown">>),
  case is_device_node(DeviceName) of
    {error, Reason} ->
      {NegRes(Reason), Req, State};
    {ok, Node} ->
      {PosRes(Node), Req, State}
  end.

is_device_node(DeviceName) when is_binary(DeviceName) ->
  try
    DeviceNameAtom = binary_to_existing_atom(DeviceName),
    case lists:any(fun(Node) -> Node =:= DeviceNameAtom end, nodes()) of
      true -> {ok, DeviceNameAtom};
      false -> {error, unknown_device}
    end
  catch
    error: badarg -> {error, unknown_device}
  end.
