%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% File operation utilities
%%% @end
%%% Created : 03. Apr 2021 12:22 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_file).
-author("bnjm").

%% API
-export([list_dir/1, read_every/3, close/1]).

%% Returns a list absolute paths to all directories in Dir
-spec(list_dir(string()) -> [string()]).
list_dir(Dir) when is_list(Dir) ->
  BaseDir = sanitize_dir_str(Dir),
  case file:list_dir(BaseDir) of
    {ok, Dirs} -> [BaseDir ++ D || D <- Dirs, filelib:is_dir(BaseDir ++ D)];
    {error, Reason} -> throw("Can not list in '" ++ BaseDir ++ "' due to: '" ++ erlang:atom_to_list(Reason) ++ "'")
  end.

-spec(read_every(string(), integer(), fun()) -> pid()).
read_every(Path, Interval, Callback) ->
  spawn(fun() ->
    {ok, Dev} = file:open(Path, read),
    read_every_loop(Dev, Interval, Callback)
        end).

read_every_loop(Device, Interval, Callback) ->
  receive
    close -> file:close(Device)
  after Interval ->
    do_read(Device, read, Callback),
    read_every_loop(Device, Interval, Callback)
  end.

close(Pid) ->
  Pid ! close,
  ok.

%% private
do_read(IoDevice, Mode, CallbackFn) ->
  case do_read_mode(IoDevice, Mode) of
    {ok, Content} -> CallbackFn(Content);
    eof -> file:close(IoDevice)
  end.

do_read_mode(IoDevice, read) -> file:read(IoDevice, 1024);
do_read_mode(IoDevice, line) -> file:read_line(IoDevice).

-spec(ends_with_slash(string()) -> boolean()).
ends_with_slash(Str) ->
  "/" =:= string:find(Str, "/", trailing).

-spec(sanitize_dir_str(string()) -> string()).
sanitize_dir_str(Str) ->
  case ends_with_slash(Str) of
    true -> Str;
    false -> Str ++ "/"
  end.
