%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(ninjatrace_gps_sensor).

-behaviour(gen_server).

-include("types.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([info/0, name/0]).

-type gprmc_map() :: #{
type := gprmc,
time := string(),
valid := boolean(),
latitude := maybe(float()), % TODO: create type for ‘ddmm.mmmm' degree/minutes
ns := maybe(north |south),
longitude := maybe(float()),
ew := maybe(east | west),
speed := maybe(string()), % speed in knots,
cog := maybe(string()), % course over ground
date := maybe(string()), % ddmmyy
pos_mode := maybe(no_fix | auto_fix | diff_fix)
}.

-define(SERVER, ?MODULE).

-record(state, {
  lat :: float(),
  lng :: float()
}).

%% API
name() -> gps_sensor.

info() ->
  gen_server:call(?MODULE, get_info).

%% gen_server
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  SensorPath = application:get_env(ninjatrace, gps_sensor_path, "/dev/ttyS0"),
  case ninjatrace_file:is_device(SensorPath) of
    false -> error(no_serial_port);
    true ->
      Self = self(),
      ninjatrace_file:read_line_every("/dev/ttyS0", 500, fun(Line) ->
        case parse_nmea(Line) of
          nil -> noop; %no op
          NmeaMessage -> Self ! {nmea, NmeaMessage}
        end
                                                         end),
      ninjatrace_logger:info(?MODULE, "gps sensor started"),
      {ok, #state{}}
  end.

handle_call(get_info, _From, #state{lat = Lat, lng = Lng} = State) ->
  {reply, #{ lat => Lat, lng => Lng}, State};

handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info({nmea, Message}, State) ->
  {Lat, Lng } = to_lat_lon(Message),
  {noreply, #state{lat = Lat, lng = Lng}};

handle_info(Info, State = #state{}) ->
  ninjatrace_logger:info(?MODULE, "Unexpected Info: ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%% private

%% @doc Converts the dms values of the gprmc map to lat long
%% TODO: determine impact of the precision loss due to erlang:trunc/1
-spec(to_lat_lon(gprmc_map()) -> {number(), number()}).
to_lat_lon(#{latitude := Latitude, ns := north, longitude := Longitude, ew := east}) ->
  {convert_from_dms(Latitude, false), convert_from_dms(Longitude, false)};

to_lat_lon(#{latitude := Latitude, ns := south, longitude := Longitude, ew := east}) ->
  {convert_from_dms(Latitude, true), convert_from_dms(Longitude, false)};

to_lat_lon(#{latitude := Latitude, ns := south, longitude := Longitude, ew := west}) ->
  {convert_from_dms(Latitude, true), convert_from_dms(Longitude, true)};

to_lat_lon(#{latitude := Latitude, ns := north, longitude := Longitude, ew := west}) ->
  {convert_from_dms(Latitude, true), convert_from_dms(Longitude, true)}.

convert_from_dms(DmsFloat, Invert) ->
  Degree = erlang:trunc(DmsFloat / 100),
  Minutes = ((DmsFloat / 100) - Degree) * 100,
  Result = Degree + (Minutes / 60),
  if Invert =:= true ->
    Result * -1;
    true ->
      Result
  end.

%% nmea parsing
-spec(parse_nmea(binary()) -> gprmc_map() | nil).
parse_nmea(<<$$, $G, $P, $R, $M, $C, $,, Rest/bits>>) ->
  parse_gprmc(string:chomp(Rest));

parse_nmea(_Any) ->
  nil.

-spec(parse_gprmc(binary()) -> gprmc_map()).
parse_gprmc(<<Time:10/binary, $,, $V, _/bits>>) ->
  #{type => gprmc,
    time => binary_to_list(Time),
    valid => false};

parse_gprmc(<<Time:10/binary, $,, $A, $,, Latitude:9/binary, $,, NorthSouth:1/binary, $,, Longitude:10/binary, $,
  , EastWest:1/binary, $,, Speed:4/binary, $,, Cog:4/binary, $,, Date:6/binary, $,, $,, $,, PosMode:1/binary, _Rest/bits>>) ->
  new_gprmc(Time, Latitude, NorthSouth, Longitude, EastWest, Speed, Cog, Date, PosMode);

parse_gprmc(<<Time:10/binary, $,, $A, $,, Latitude:9/binary, $,, NorthSouth:1/binary, $,, Longitude:10/binary, $,
  , EastWest:1/binary, $,, Speed:4/binary, $,, Cog:5/binary, $,, Date:6/binary, $,, $,, $,, PosMode:1/binary, _Rest/bits>>) ->
  new_gprmc(Time, Latitude, NorthSouth, Longitude, EastWest, Speed, Cog, Date, PosMode);

parse_gprmc(<<Time:10/binary, $,, $A, $,, Latitude:9/binary, $,, NorthSouth:1/binary, $,, Longitude:10/binary, $,
  , EastWest:1/binary, $,, Speed:4/binary, $,, Cog:6/binary, $,, Date:6/binary, $,, $,, $,, PosMode:1/binary, _Rest/bits>>) ->
  new_gprmc(Time, Latitude, NorthSouth, Longitude, EastWest, Speed, Cog, Date, PosMode).

new_gprmc(Time, Latitude, NorthSouth, Longitude, EastWest, Speed, Cog, Date, PosMode) ->
  #{type => gprmc,
    time => binary_to_list(Time),
    valid => true,
    latitude => binary_to_float(Latitude),
    ns => parse_cardinal_point(NorthSouth),
    longitude => binary_to_float(Longitude),
    ew => parse_cardinal_point(EastWest),
    speed => binary_to_float(Speed),
    cog => binary_to_float(Cog),
    date => binary_to_list(Date),
    pos_mode => parse_pos_mode(PosMode)
  }.

parse_cardinal_point(<<"N">>) -> north;
parse_cardinal_point(<<"S">>) -> south;
parse_cardinal_point(<<"E">>) -> east;
parse_cardinal_point(<<"W">>) -> west;
parse_cardinal_point(Else) -> throw("Unknown cardinal: " ++ Else).

parse_pos_mode(<<"N">>) -> no_fix;
parse_pos_mode(<<"A">>) -> auto_fix;
parse_pos_mode(<<"D">>) -> diff_fix;
parse_pos_mode(Else) -> throw("Unknown positioning mode: " ++ Else).
