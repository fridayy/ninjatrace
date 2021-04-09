%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2021 12:35 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_file_SUITE).
-author("bnjm").

-define(TEMP_DIR, "/tmp/").

-include_lib("eunit/include/eunit.hrl").

stream_file_test_() ->
  {setup,
    fun stream_file_fixture/0,
    fun stream_file_teardown/1,
    fun stream_file_exercise/1
  }.

stream_file_fixture() ->
  Time = erlang:integer_to_list(erlang:system_time()),
  Path = ?TEMP_DIR ++ "eunit-test" ++ Time ++ ".txt",
  Size = 10 * (1024 * 1024), %10 mb
  file:write_file(Path, <<0:(Size * 8)>>), % to bits
  {Path, Size}.

stream_file_teardown({Path, Size}) ->
  file:delete(Path),
  ok.

stream_file_exercise({Path, Size}) ->
  Self = self(),
  ninjatrace_file:stream_file(Path, read, fun(_Content) ->
    Self ! called
                                          end),
  CalledCount = receive_until(1, (Size div 1024)),
  [
    ?_assertEqual(10240, CalledCount)
  ].

receive_until(Current, Limit) ->
  receive
    called when Current =:= Limit ->
      ?debugMsg("done"),
      Current;
    called ->
      ?debugFmt("Current: ~p / ~p ~n", [Current, Limit]),
      receive_until(Current + 1, Limit);
    _Any -> failed_unknown
  after
    5000 -> failed_timeout
  end.

list_dir_test_() ->
  {setup,
    fun dir_test_fixture/0,
    fun dir_test_teardown/1,
    fun dir_test_exercise/1
  }.

list_dir_non_happy_test() ->
  ?assertThrow("Can not list in '/324u4uhdhsfsdf/' due to: 'enoent'",
    ninjatrace_file:list_dir("/324u4uhdhsfsdf")).

dir_test_fixture() ->
  %% fixture
  TestPath = ?TEMP_DIR ++ "eunit-test",
  ok = file:make_dir(TestPath),
  ok = file:make_dir(TestPath ++ "/a"),
  ok = file:make_dir(TestPath ++ "/b"),
  TestPath.

dir_test_exercise(Path) ->
  Result = ninjatrace_file:list_dir(Path),
  [
    ?_assertEqual(length(Result), 2),
    ?_assertEqual(lists:member("/tmp/eunit-test/a", Result), true),
    ?_assertEqual(lists:member("/tmp/eunit-test/b", Result), true)
  ].

dir_test_teardown(Path) ->
  file:del_dir_r(Path).

tmp_exists() ->
  filelib:is_dir(?TEMP_DIR).