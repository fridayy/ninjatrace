%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% Provides automatic mnesia clustering facilities for ninjatrace
%%% @end
%%% Created : 23. May 2021 11:02 AM
%%%-------------------------------------------------------------------
-module(ninjatrace_mnesia).
-author("bnjm").


-include_lib("stdlib/include/qlc.hrl").
-define(ATTRIBUTE, mnesia_init).

-ifdef(TEST).
-compile([export_all]).
-endif.

-compile([export_all]).
%% API
-export([boot/0]).

boot() ->
  application:set_env(mnesia, dir, "/tmp/nt-mnesia"),
  mnesia:delete_schema([node()]),
  initialize_schema(),
  mnesia:start(),
  initialize_tables().

initialize_schema() ->
  case mnesia:system_info(extra_db_nodes) of
    [] ->
      mnesia:create_schema([node()]),
      ninjatrace_logger:info(?MODULE, "Schema initialized");
    _ -> copy_schema()
  end.

initialize_tables() ->
  ninjatrace_logger:info(?MODULE, "Initializing tables"),
  TableInitFunctions = get_table_init_functions(),
  case mnesia:system_info(extra_db_nodes) of
    [] -> create_tables(TableInitFunctions);
    _ -> copy_tables(TableInitFunctions)
  end.

create_tables(TableInitMF) ->
  lists:foreach(fun({M, F}) ->
    {Table, Options} = apply(M, F, [new]),
    {atomic, _} = mnesia:create_table(Table, Options)
                end, TableInitMF),
  ninjatrace_logger:info(?MODULE, "Tables successfully created"),
  ok.

copy_tables(TableInitMF) ->
  lists:foreach(fun({M, F}) ->
    {Table, StorageType} = apply(M, F, [copy]),
    mnesia:add_table_copy(Table, node(), StorageType)
                end, TableInitMF),
  Tables = mnesia:system_info(tables),
  mnesia:wait_for_tables(Tables, 5000),
  ninjatrace_logger:info(?MODULE, "Successfully replicated tables").

get_table_init_functions() ->
  {ok, Modules} = application:get_key(ninjatrace, modules),
  lists:filtermap(fun(Module) -> has_specified_attribute(Module) end, Modules).

has_specified_attribute(Module) when is_atom(Module) ->
  case lists:keyfind(?ATTRIBUTE, 1, Module:module_info(attributes)) of
    false -> false;
    {?ATTRIBUTE, [{F}]} -> {true, {Module, F}}
  end.

copy_schema() ->
  mnesia:add_table_copy(schema, node(), ram_copies).