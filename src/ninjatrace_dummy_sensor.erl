%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% A sensor that generates dummy data at a give pace
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
-record(state, {lat :: float(), lng :: float()}).


name() -> gps_sensor.

info() ->
  gen_server:call(?MODULE, get_info).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec(init(any()) -> {ok, #state{}}).
init(_Any) ->
  {ok, #state{lat = 47.02497197663792,lng = 15.40070730367752}, 0}.

handle_call(get_info, _From, #state{lat = Lat, lng = Lng} = State) ->
  {reply, #{lat => Lat, lng => Lng}, State, ?TIMEOUT};

handle_call(Request, _From, State) ->
  ninjatrace_logger:info("Unhandled call: ~p", [Request]),
  {reply, ok, State, ?TIMEOUT}.

handle_cast(Request, State) ->
  ninjatrace_logger:info("Unhandled cast ~p", [Request]),
  {noreply, State, ?TIMEOUT}.

handle_info(timeout, #state{lat = Lat, lng = Lng} = State) ->
  {NewLat, NewLng} = generate_gps_data(Lat, Lng),
  {noreply, #state{lat = NewLat, lng = NewLng}, ?TIMEOUT};

handle_info(Msg, State) ->
  ninjatrace_logger:info(?MODULE, "Unhandled info: ~p", [Msg]),
  {noreply, State}.

%% private

generate_gps_data(Lat, Lng) ->
  NewLat = Lat + (rand:uniform() / 1000),
  NewLng = Lng + (rand:uniform() / 1000),
  {NewLat, NewLng}.
