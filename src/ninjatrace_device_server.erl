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

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([subscriptions/0, subscribe/2, unsubscribe/2, is_node/1]).

-define(SERVER, ?MODULE).

-type(subscriptions() :: #{ node() := [pid()] }).

%% TODO: use mnesia to store subscribers
-record(state, {
  subscriptions :: subscriptions()
}).

%% API
-spec(subscribe(node(), pid()) -> ok).
subscribe(Node, ReceiverPid) ->
  gen_server:call(?MODULE, {subscribe, Node, ReceiverPid}).

-spec(unsubscribe(node(), pid()) -> ok).
unsubscribe(Node, ReceiverPid) ->
  gen_server:cast(?MODULE, {unsubscribe, Node, ReceiverPid}),
  ok.

subscriptions() ->
  gen_server:call(?MODULE, subscriptions).

-spec(is_node(binary()) -> {ok, node()} | {error, no_node}).
is_node(DeviceName) when is_binary(DeviceName) ->
  try
    DeviceNameAtom = binary_to_existing_atom(DeviceName),
    case lists:any(fun(Node) -> Node =:= DeviceNameAtom end, nodes()) of
      true -> {ok, DeviceNameAtom};
      false -> {error, no_node}
    end
  catch
    error: badarg -> {error, no_node}
  end.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  net_kernel:monitor_nodes(true, [nodedown_reason]),
  {ok, #state{subscriptions = #{}}}.

handle_call(subscriptions, _From, #state{subscriptions = Subscriptions} = State) ->
  {reply, Subscriptions, State};

handle_call({subscribe, Node, Pid}, _From, #state{subscriptions = Subscriptions}) ->
  %% TODO: monitor the subscription - otherwise it will zombie around in the map
  case maps:is_key(Node, Subscriptions) of
    true ->
      NewSubscriptions = do_subscribe(Node, Pid, Subscriptions),
      NewState = #state{subscriptions = NewSubscriptions},
      {reply, ok, NewState};
    false ->
      {reply, {error, not_registered}}
  end;

handle_call(_Request, _From, #state{} = State) ->
  {reply, ok, State}.

handle_cast({unsubscribe, Node, Pid}, #state{subscriptions = Subscriptions} = State) ->
  case maps:is_key(Node, Subscriptions) of
    true ->
      NewSubscriptions = do_unsubscribe(Node, Pid, Subscriptions),
      NewState = #state{subscriptions = NewSubscriptions},
      {noreply, NewState};
    false -> {noreply, State}
  end;

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

handle_info({timeout, _TimerRef, {Node, timeouts, Timeouts}}, #state{subscriptions = Subs} = State) ->
  Subscribers = maps:get(Node, Subs),
  try
    Response = ninjatrace_device:info(Node, 1000),
    lists:foreach(fun(Pid) -> Pid ! {ok, Response} end, Subscribers),
    tick(Node)
  catch
    exit:{timeout, _} ->
      lists:foreach(fun(Pid) -> Pid ! {error, timeout} end, Subscribers),
      tick(Node, Timeouts + 1);
    exit:{nodedown, Node} ->
      lists:foreach(fun(Pid) -> Pid ! {error, device_down} end, Subscribers),
      maps:remove(Node, Subscribers)
  end,
  {noreply, State};

handle_info({nodeup, Node, _Info}, #state{subscriptions = Subscriptions} = State) ->
  case is_device(Node) of
    true ->
      NewSubscriptions = maps:put(Node, [], Subscriptions),
      NewState = #state{subscriptions = NewSubscriptions},
      {noreply, NewState};
    false -> {noreply, State}
  end;

handle_info({nodedown, Node, _Info}, #state{subscriptions = Subscriptions} = State) ->
  case is_device(Node) of
    true ->
      NewSubscriptions = maps:remove(Node, Subscriptions),
      {noreply, #state{subscriptions = NewSubscriptions}};
    false -> {noreply, State}
  end;

handle_info(Info, State = #state{}) ->
  ninjatrace_logger:info(?MODULE, "Unnown info: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_device(DeviceName) when is_binary(DeviceName) ->
  try
    PotentialNode = binary_to_existing_atom(DeviceName),
    case lists:any(fun(Node) -> Node =:= PotentialNode end, nodes()) of
      true -> {ok, PotentialNode};
      false -> {error, unknown_device}
    end
  catch
    error: badarg -> false
  end;

is_device(Node) when is_atom(Node) ->
  case rpc:call(Node, application, get_env, [ninjatrace, type, device]) of
    device -> true;
    _Else -> false
  end.

do_subscribe(Node, Pid, Subscriptions) ->
  maps:update_with(Node, fun(Pids) -> case Pids of
                                        [] ->
                                          tick(Node),
                                          [Pid | Pids];
                                        _Else ->
                                          [Pid | Pids]
                                      end
                         end, Subscriptions).

do_unsubscribe(Node, Pid, Subscriptions) ->
  maps:update_with(Node, fun(Pids) ->
    lists:filter(fun(P) -> P =/= Pid end, Pids)
                         end,
    Subscriptions).

tick(Node) -> tick(Node, 0).
tick(Node, Timeouts) ->
  erlang:start_timer(1000, self(), {Node, timeouts, Timeouts}).