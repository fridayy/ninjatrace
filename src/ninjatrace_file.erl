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

-ifdef(TEST).
-compile(export_all).
-endif.

%% API
-export([list_dir/1, read_line_every/3, read_line_every/4, close/1, is_device_file/1]).

-type path() :: string().
-type file_read_pid() :: pid().
-type interval_ms() :: 1..10000.

%%  @doc Returns a list absolute paths to all directories in Dir
-spec(list_dir(path()) -> [path()]).
list_dir(Dir) when is_list(Dir) ->
  BaseDir = sanitize_dir_str(Dir),
  case file:list_dir(BaseDir) of
    {ok, Dirs} -> [BaseDir ++ D || D <- Dirs, filelib:is_dir(BaseDir ++ D)];
    {error, Reason} -> throw("Can not list in '" ++ BaseDir ++ "' due to: '" ++ erlang:atom_to_list(Reason) ++ "'")
  end.

%% @doc Reads the contents from the given file every n-milliseconds (interval) and invokes the given callback.
%% Returns the pid of the handling process which should be used to close the file resource and stop the reading process.
%% see: close()
%% TODO: reduce arity, add callbacks via tuple (on_next, on_error, on_complete)
%% TODO: this will ease fail fast for the gen_server using this
-spec(read_line_every(path(), interval_ms(), integer(), function()) -> file_read_pid()).
read_line_every(Path, Interval, ReadAhead, Callback) ->
  spawn(
    fun() ->
      {ok, Dev} = file:open(Path, [read, binary, raw, {read_ahead, ReadAhead}]),
      read_every_loop(Dev, Interval, Callback)
    end
  ).

-spec(read_line_every(path(), interval_ms(), function()) -> file_read_pid()).
read_line_every(Path, Interval, Callback) ->
  read_line_every(Path, Interval, 128, Callback).

%% @doc Closes the file reading process.
-spec(close(file_read_pid()) -> ok).
close(Pid) ->
  Pid ! close,
  ok.

-spec(is_device_file(path()) -> boolean()).
is_device_file(Path) ->
  case file:read_file_info(Path) of
    {ok, Info} ->
      erlang:element(3, Info) =:= device;
    _Else -> false
  end.

%% private
read_every_loop(Device, Interval, Callback) ->
  receive
    close -> file:close(Device)
  after Interval ->
    do_read_line(Device, Callback),
    read_every_loop(Device, Interval, Callback)
  end.

do_read_line(IoDevice, CallbackFn) ->
  case file:read_line(IoDevice) of
    {ok, Content} -> CallbackFn(Content);
    _Any -> file:close(IoDevice) % TODO: determine behaviour (einval? eof?)
  end.


-spec(ends_with_slash(string()) -> boolean()).
ends_with_slash(Str) ->
  "/" =:= string:find(Str, "/", trailing).

-spec(sanitize_dir_str(string()) -> string()).
sanitize_dir_str(Str) ->
  case ends_with_slash(Str) of
    true -> Str;
    false -> Str ++ "/"
  end.
