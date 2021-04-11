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

%% read line every section
read_line_every_test_() ->
  {setup,
    fun read_line_every_fixture/0,
    fun read_line_every_teardown/1,
    fun read_line_every_exercise/1
  }.

read_line_every_fixture() ->
  {ok, Dir} = file:get_cwd(),
  ?debugFmt("cwd=~s", [Dir]),
  Dir ++ "/test/ninjatrace_file_SUITE_data/some_file.txt".

read_line_every_teardown(_Path) ->
  noop.

read_line_every_exercise(Path) ->
  Self = self(),
  ?debugFmt("Path=~s", [Path]),
  Pid = ninjatrace_file:read_line_every(Path, 10, fun(C) ->
                                              Self ! {line, C}
                                             end),
  Result = content_gatherer(0, 3, []),
  [
    ?_assertEqual([{3,<<"d\n">>},{2,<<"c\n">>},{1,<<"b\n">>},{0,<<"a\n">>}], Result)
  ].

content_gatherer(Current, Expectation, Items) ->
  receive
    {line, Line} when Current =/= Expectation -> content_gatherer(Current + 1, Expectation, [{Current, Line} | Items]);
    {line, Line} -> [{Current, Line} | Items];
    Any -> unexpected_message
    after 100 -> timeout
  end.

%% list dir test section
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
