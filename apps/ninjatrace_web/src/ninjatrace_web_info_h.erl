%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Handles the /info/:device route
%%% @end
%%% Created : 17. Apr 2021 12:04 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_web_info_h).
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
  Device = cowboy_req:binding(device_name, Req, <<"Unknown">>),
  case ninjatrace_device:info(Device) of
    {ok, Info} -> {jsone:encode(Info), Req, State};
    {error, Reason} -> {jsone:encode(#{error => Reason}), Req, State}
  end.