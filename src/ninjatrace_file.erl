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
-export([list_dir/1]).

%% Returns a list absolute paths to all directories in Dir
-spec(list_dir(string()) -> [string()]).
list_dir(Dir) when is_list(Dir) ->
  BaseDir = sanitize_dir_str(Dir),
  case file:list_dir(BaseDir) of
    {ok, Dirs} -> [BaseDir ++ D || D <- Dirs, filelib:is_dir(BaseDir ++ D)];
    {error, Reason} -> throw("Can not list in '" ++ BaseDir ++ "' due to: '" ++ erlang:atom_to_list(Reason) ++ "'")
  end.

%% private
-spec(ends_with_slash(string()) -> boolean()).
ends_with_slash(Str) ->
  "/" =:= string:find(Str, "/", trailing).

-spec(sanitize_dir_str(string()) -> string()).
sanitize_dir_str(Str) ->
  case ends_with_slash(Str) of
    true -> Str;
    false -> Str ++ "/"
  end.
