%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%  Abstraction over a currently running and connected device
%%% @end
%%%-------------------------------------------------------------------
-module(ninjatrace_device).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([info/0, info/1, sensors/0, sensors/1]).

-define(SERVER, ?MODULE).

-record(state, {
  sensors :: list()
}).

%% API
info() ->
  gen_server:call(?MODULE, get_info).

info(Node) when is_atom(Node) ->
  gen_server:call({?MODULE, Node}, get_info).

%% @doc
%% Returns the sensors for the current device (node).
%% @end
sensors() ->
  gen_server:call(?MODULE, get_sensors).

%% @doc
%% Returns the currently enabled sensors for a particular device represented as a node.
%% Server nodes won't have any sensors enabled.
%% @end
sensors(Node) ->
  gen_server:call({?MODULE, Node}, get_sensors).

start_link(Sensors) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Sensors, []).

init(Args) ->
  {ok, #state{sensors = Args}}.

handle_call(get_info, _From, #state{sensors = Sensors} = State) ->
  Info = lists:map(fun(MSensor) -> #{MSensor:name() => MSensor:info()} end, Sensors),
  {reply, #{info => Info}, State};

handle_call(get_sensors, _From, #state{sensors = Sensors} = State) ->
  {reply, #{sensors => Sensors}, State};

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
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