%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2021 4:14 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_gps).
-author("bnjm").

%% API
-export([on_gprmc/1]).

-spec(on_gprmc(pid()) -> ok | {error, atom()}).
on_gprmc(Pid) ->
  ok.

-spec(parse_gprmc(string()) -> {}).
parse_gprmc(Line) ->
  ok.

