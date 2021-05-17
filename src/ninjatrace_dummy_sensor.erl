%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2021 11:37 AM
%%%-------------------------------------------------------------------
-module(ninjatrace_dummy_sensor).
-author("bnjm").

-behavior(gen_server).

-include("types.hrl").
%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/1, name/0, info/0]).

-define(TIMEOUT, 1000).
-record(state, {angle :: float()}).


name() -> dummy_sensor.

info() ->
  gen_server:call(?MODULE, get_info).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec(init(any()) -> {ok, #state{}}).
init(_Any) ->
  {ok, #state{}, 0}.

handle_call(get_info, _From, #state{angle = Angle} = State) ->
  {reply, #{angle => Angle}, State, ?TIMEOUT};

handle_call(Request, From, State) ->
  ninjatrace_logger:info("Unhandled call: ~p", [Request]),
  {reply, ok, State, ?TIMEOUT}.

handle_cast(Request, State) ->
  ninjatrace_logger:info("Unhandled cast ~p", [Request]),
  {noreply, State, ?TIMEOUT}.

handle_info(timeout, _State) ->
  NewAngle = rand:uniform(),
  {noreply, #state{angle = NewAngle}, ?TIMEOUT};

handle_info(Msg, State) ->
  ninjatrace_logger:info(?MODULE, "Unhandled info: ~p", [Msg]),
  {noreply, State}.