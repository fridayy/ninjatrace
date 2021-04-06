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