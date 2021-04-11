%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2021 9:13 PM
%%%-------------------------------------------------------------------
-module(ninjatrace_gps_sensor_SUITE).
-author("bnjm").

-include_lib("eunit/include/eunit.hrl").

% parser tests

to_lat_lng_test() ->
  Input = #{
    type => gprmc,
    time => "185841.000",
    valid => true,
    latitude => 5301.8579,
    ns => north,
    longitude => 1224.8752,
    ew => east,
    speed => 0.0,
    cog => 15.50,
    date => "100421",
    pos_mode => diff_fix
  },
  {Lat, Lng} = ninjatrace_gps_sensor:to_lat_lon(Input),
  ?assertEqual(53.030965, Lat),
  ?assertEqual(12.414586666666667, Lng).

gprmc_test() ->
  Result0 = ninjatrace_gps_sensor:parse_nmea(<<"$GPRMC,185841.000,A,5301.8579,N,01224.8752,E,0.00,15.50,100421,,,D*56\n">>),
  Result1 = ninjatrace_gps_sensor:parse_nmea(<<"$GPRMC,134745.082,V,,,,,0.00,0.00,100421,,,N*41\n">>),
  Result2 = ninjatrace_gps_sensor:parse_nmea(<<"$GPRMC,115332.000,A,5301.8400,N,01224.8781,E,0.00,341.29,110421,,,D*6C\n">>),
  Result3 = ninjatrace_gps_sensor:parse_nmea(<<"$GPRMC,120741.000,A,5301.8521,N,01224.8684,E,1.07,1.22,110421,,,D*66\n">>),
  ?assertEqual(#{
    type => gprmc,
    time => "185841.000",
    valid => true,
    latitude => 5301.8579,
    ns => north,
    longitude => 1224.8752,
    ew => east,
    speed => 0.0,
    cog => 15.50,
    date => "100421",
    pos_mode => diff_fix

  }, Result0),
  ?assertEqual(#{type => gprmc, time => "134745.082", valid => false}
    , Result1),
  ?assertEqual(#{
    type => gprmc,
    time => "115332.000",
    valid => true,
    latitude => 5301.8400,
    ns => north,
    longitude => 1224.8781,
    ew => east,
    speed => 0.0,
    cog => 341.29,
    date => "110421",
    pos_mode => diff_fix

  }, Result2),
  ?assertEqual(
    #{
      type => gprmc,
      time => "120741.000",
      valid => true,
      latitude => 5301.8521,
      ns => north,
      longitude => 1224.8684,
      ew => east,
      speed => 1.07,
      cog => 1.22,
      date => "110421",
      pos_mode => diff_fix

    }, Result3
  ).
