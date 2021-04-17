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
-export([html_info_response/2, rest_info_response/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, rest_info_response}
  ], Req, State}.

rest_info_response(Req, State) ->
  check_active_device(Req, State, {
    fun(DeviceName) -> "{\"name\": \"" ++ DeviceName ++ "\", \"cpu_temp\": 0.0}" end,
    fun(_InvalidDevice) -> "{\"error\": \"unknown device\"}" end
  }).

html_info_response(Req, State) ->
  check_active_device(Req, State, {
    fun(DeviceName) -> "<html><body>
      <b>Active:" ++ DeviceName ++ "</b>
    </body></html>" end,
    fun(InvalidName) -> "<html><body>
      <b>Not Active:" ++ InvalidName ++ "</b>
    </body></html>" end
  }).

%% internal
check_active_device(Req, State, {PosRes, NegRes}) ->
  DeviceName = cowboy_req:binding(device_name, Req, <<"Unknown">>),
  case is_device_node(DeviceName) of
    false ->
      {NegRes(binary_to_list(DeviceName)), Req, State};
    true ->
      {PosRes(binary_to_list(DeviceName)), Req, State}
  end.

is_device_node(DeviceName) when is_binary(DeviceName) ->
  try
    DeviceNameAtom = binary_to_existing_atom(DeviceName),
    lists:any(fun(Node) -> Node =:= DeviceNameAtom end, nodes())
  catch
    error: badarg -> false
  end.
