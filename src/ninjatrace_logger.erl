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
  io_format_log(info, Module, Format,Args).

io_format_log(Level, Module, Format, Args) ->
  ModuleAttached = [atom_to_list(Module) | Args],
  LevelAttached = [atom_to_list(Level) | ModuleAttached],
  io:format("[~p][~p] " ++ Format ++ "~n", LevelAttached).