%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ninjatrace_temp_sensor).

-behaviour(gen_server).

-export([cpu_temp/0]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  cpu_temp_path :: string()
}).

cpu_temp() ->
  gen_server:call(?MODULE, {get_temp, cpu}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, CpuTempPath} = application:get_env(ninjatrace, cpu_temp_path),
  ninjatrace_logger:info(?MODULE, "Using CPU temp path: ~p", [CpuTempPath]),
  {ok, #state{
    cpu_temp_path = CpuTempPath
  }}.

handle_call({get_temp, cpu}, From, State = #state{cpu_temp_path = CpuPath}) ->
  {reply, read_temp_from(CpuPath), State};

handle_call(Request, _From, State = #state{}) ->
  ninjatrace_logger:info(?MODULE, "Unexpected call: ~p", [Request]),
  {reply, ok, State}.

handle_cast(Request, State = #state{}) ->
  ninjatrace_logger:info(?MODULE, "Unexpected cast ~p", [Request]),
  {noreply, State}.

handle_info(_Info, State = #state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(read_temp_from(string()) -> {ok, integer(), celsius} | {error, string()}).
read_temp_from(SensorPath) ->
  case file:open(SensorPath, read) of
    {ok, IoDevice} ->
      {ok, Line} = file:read_line(IoDevice),
      file:close(IoDevice),
      SanitizedLine = string:chomp(Line),
      Celsius = list_to_integer(SanitizedLine) / 1000,
      {ok, Celsius, celsius};
    {error, Reason} -> {error, Reason}
  end.
