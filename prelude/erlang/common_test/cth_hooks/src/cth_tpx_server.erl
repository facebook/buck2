%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.

%% @format
-module(cth_tpx_server).
-behaviour(gen_server).

%% Public API
-export([
    start_link/1,
    get/1,
    modify/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export_type([
    handle/0
]).

-type handle() :: pid().

-type state() :: cth_tpx:shared_state().

-type call_reqs() ::
    get
    | {modify, fun((state()) -> {term(), state()})}.


%% ---- PUBLIC API ---------
-spec start_link(InitialState :: state()) -> handle().
start_link(InitialState) ->
    {ok, Handle} = gen_server:start_link(?MODULE, InitialState, []),
    Handle.

-spec get(Handle :: handle()) -> CurrentState :: state().
get(Handle) ->
    call(Handle, get).

-spec modify(Handle :: handle(), Fun :: fun((state()) -> {A, state()})) -> A.
modify(Handle, Fun) ->
    call(Handle, {modify, Fun}).


%% ---- gen_server callbacks ----------

-spec call(Handle :: handle(), Req :: call_reqs()) -> dynamic().
call(Handle, Req) ->
    gen_server:call(Handle, Req, 6000).

-spec init(InitialState :: state()) -> {ok, state()}.
init(InitialState) ->
    {ok, InitialState}.

-spec handle_call
    (Req :: call_reqs(), From :: term(), State :: state()) -> {reply, Reply :: term(), NewState :: state()}.
handle_call(get, _From, State) ->
    {reply, State, State};
handle_call({modify, Fun}, _From, State) ->
    {A, NewState} = Fun(State),
    {reply, A, NewState}.

-spec handle_cast(Request :: term(), State :: term()) -> {noreply, NewState :: term()}.
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(Info :: term(), State :: term()) -> {noreply, NewState :: term()}.
handle_info(_, State) ->
    {noreply, State}.
