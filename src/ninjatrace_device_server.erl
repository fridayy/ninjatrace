%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc

%%% Allows interested clients (websocket handlers etc.) to subscribe to specific devices and receive
%%% an updated device state at a given interval.
%%% Basically the device_server is a proxy for the connected devices.This avoids spamming the process mailboxes
%%% of the poor raspberrypi's and keep most of the load on the server.
%%% @end
%%%-------------------------------------------------------------------
-module(ninjatrace_device_server).

-behaviour(gen_server).

-include("types.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start_link/0, mnesia_init/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([subscriptions/0, subscribe/2, unsubscribe/2, registered_devices/0, is_registered/1]).

%% the query interval for all devices
-define(INTERVAL, 1000).

-mnesia_init({mnesia_init}).

-record(subscriptions, {
  device :: node(),
  subscribers :: [pid()]
}).

%% API
-spec(subscribe(node(), pid()) -> ok).
subscribe(Node, ReceiverPid) when is_atom(Node) and is_pid(ReceiverPid) ->
  gen_server:call(?MODULE, {subscribe, Node, ReceiverPid}).

-spec(unsubscribe(node(), pid()) -> ok).
unsubscribe(Node, ReceiverPid) when is_atom(Node) and is_pid(ReceiverPid) ->
  gen_server:cast(?MODULE, {unsubscribe, Node, ReceiverPid}),
  ok.

-spec(subscriptions() -> #{device := node(), subscribers := [pid()]}).
subscriptions() ->
  lists:foldl(fun({Device, Subs}, Acc) ->
    maps:put(Device, Subs, Acc)
              end, maps:new(), read_all()).

-spec(registered_devices() -> [device()]).
registered_devices() ->
  read_all_devices().

-spec(is_registered(device() |string()) -> {ok, device()} | {error, no_device}).
is_registered(Device) when is_atom(Device) ->
  case read_one_device(Device) of
    {ok, Device, _} -> {ok, Device};
    _ -> {error, no_device}
  end;

is_registered(Device) when is_binary(Device) ->
  try
    DeviceAtom = binary_to_existing_atom(Device),
    is_registered(DeviceAtom)
  catch
    error: badarg -> {error, no_device}
  end.

mnesia_init(new) ->
  ninjatrace_logger:info(?MODULE, "Init subscription table"),
  {subscriptions, [{attributes, record_info(fields, subscriptions)}]};

mnesia_init(copy) ->
  ninjatrace_logger:info(?MODULE, "Copying subscription table"),
  {subscriptions, ram_copies}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  net_kernel:monitor_nodes(true, [nodedown_reason]),
  {ok, {}}.

handle_call({subscribe, Node, Pid}, _From, State) ->
  case add_subscription(Node, Pid) of
    ok ->
      start_timer(Node),
      {reply, ok, State};
    _ ->
      {reply, {error, unknown}, State}
  end,
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({unsubscribe, Node, Pid}, State) ->
  remove_subscription(Node, Pid),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({timeout, _TimerRef, {Node, timeouts, Timeouts}}, State) ->
  {ok, Device, Subscribers} = read_one_device(Node),
  try
    Response = ninjatrace_device:info(Device),
    lists:foreach(fun(Pid) -> Pid ! {ok, Response} end, Subscribers),
    start_timer(Device)
  catch
    exit:{timeout, _} ->
      lists:foreach(fun(Pid) -> Pid ! {error, timeout} end, Subscribers),
      start_timer(Device, Timeouts + 1);
    exit:{nodedown, Node} ->
      lists:foreach(fun(Pid) -> Pid ! {error, device_down} end, Subscribers),
      remove_device(Node)
  end,
  {noreply, State};

handle_info({nodeup, Node, _Info}, State) ->
  case is_device(Node) of
    {ok, Device} ->
      add_device(Device),
      {noreply, State};
    {error, Reason} ->
      ninjatrace_logger:info(?MODULE, "Could not add node as device ~p", [Reason]),
      {noreply, State}
  end;

handle_info({nodedown, Node, _Info}, State) ->
  remove_device(Node),
  {noreply, State};

handle_info(Info, State) ->
  ninjatrace_logger:info(?MODULE, "Unnown info: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% internal
is_device(DeviceName) when is_binary(DeviceName) ->
  try
    PotentialNode = binary_to_existing_atom(DeviceName),
    is_device(PotentialNode)
  catch
    error: badarg -> {error, no_device}
  end;

is_device(Node) when is_atom(Node) ->
  ninjatrace_logger:info(?MODULE, "Checking if ~p is a device", [Node]),
  case rpc:call(Node, application, get_env, [ninjatrace, mode, unknown]) of
    device -> {ok, Node};
    _Else -> {error, no_device}
  end.

read_one_device(Device) ->
  case mnesia:dirty_read(subscriptions, Device) of
      [{subscriptions, Device, Subs}] -> {ok, Device, Subs};
      [] -> {error, no_device}
  end.

read_all_devices() ->
  {atomic, Devices} = mnesia:transaction(fun() ->
    Table = mnesia:table(subscriptions),
    Query = qlc:q([D || {subscriptions, D, _} <- Table]),
    qlc:eval(Query)
                                         end),
  Devices.

read_all() ->
  {atomic, Items} = mnesia:transaction(fun() ->
    Table = mnesia:table(subscriptions),
    Query = qlc:q([{D, S} || {subscriptions, D, S} <- Table]),
    qlc:eval(Query)
                                       end),
  Items.

add_subscription(Node, Pid) ->
  ninjatrace_logger:info(?MODULE, "Adding subscription ~p -> ~p", [Pid, Node]),
  {atomic, ok} = mnesia:transaction(fun() ->
    [{subscriptions, Device, Subs}] = mnesia:read(subscriptions, Node),
    ok = mnesia:write(#subscriptions{device = Device, subscribers = [Pid | Subs]}),
    ninjatrace_logger:info(?MODULE, "Successfully added subscription ~p -> ~p", [Pid, Node])
                                    end),
  ok.

add_device(Node) ->
  add_device(Node, []).

add_device(Node, Pids) ->
  ninjatrace_logger:info(?MODULE, "Adding device ~p -> ~p", [Node, Pids]),
  {atomic, ok} = mnesia:transaction(fun() ->
    ok = mnesia:write(#subscriptions{device = Node, subscribers = Pids}),
    ninjatrace_logger:info(?MODULE, "Device added ~p -> ~p", [Node, Pids])
                                    end),
  ok.

remove_subscription(Node, Pid) ->
  ninjatrace_logger:info(?MODULE, "Removing subscription ~p -> ~p", [Pid, Node]),
  {atomic, ok} = mnesia:transaction(fun() ->
    [{subscriptions, Device, Subs}] = mnesia:read(subscriptions, Node),
    ok = mnesia:write(#subscriptions{device = Device, subscribers = lists:delete(Pid, Subs)}),
    ninjatrace_logger:info(?MODULE, "Removed subscription ~p -> ~p", [Node, Pid])
                                    end),
  ok.

remove_device(Node) ->
  {atomic, ok} = mnesia:transaction(fun() ->
    mnesia:delete({subscriptions, Node})
                                    end),
  ok.

start_timer(Node) -> start_timer(Node, 0).
start_timer(Node, Timeouts) -> erlang:start_timer(?INTERVAL, self(), {Node, timeouts, Timeouts}).