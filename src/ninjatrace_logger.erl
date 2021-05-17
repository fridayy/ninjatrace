%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Whacky hacky io:format logger for debugging purposes
%%% @end
%%% Created : 06. Apr 2021 9:28 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_logger).
-author("bnjm").

%% API
-export([info/3, info/2]).

info(Module, Format) ->
  info(Module, Format, []).

info(Module, Format, Args) ->
  error_logger:info_msg("[~p]" ++ Format, [ Module | Args]).