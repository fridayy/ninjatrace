%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Shared types over all applications
%%% @end
%%% Created : 10. Apr 2021 3:57 PM
%%%-------------------------------------------------------------------
-author("bnjm").

-type(maybe(T) :: T | nil).
-type(device() :: node()).
-type(sensor_info() :: #{ atom() := any()}).
-type(sensor_config() :: #{ atom() := any()}).
-type(sensor_spec() :: {atom(), sensor_config()}).