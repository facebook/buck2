%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%%%-------------------------------------------------------------------
%%% @doc
%%% CT handles logging and printing by sending a message to the ct_logs
%%%  process. We intercept those messages for test shell by starting a
%%%  gen_server that intercepts the messages and prints them to the test
%%%  shell. We do this instead of using the ct_logs process to have more
%%%  control over the output and to avoid starting ct processes that
%%%  might interfere with test shell's functionality.
%%% @end
%%% % @format

-module(ct_daemon_logger).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%% Public API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, handle_cast/2]).

-type state() :: #{}.

-spec init(Args) -> Result when
    Args :: term(),
    Result :: {ok, state()}.
init(_) -> {ok, #{}}.

-spec handle_info(Info, State) -> {noreply, State} when
    Info :: term(),
    State :: state().
handle_info({log, _SyncOrAsync, _FromPid, _GL, _Category, _Importance, Content, _EscChars} = _Info, State) when
    is_list(Content)
->
    % Mimics behaviour from the logger_loop function in ct_logs.erl
    IoList = lists:foldl(
        fun
            ({Format, Args}, IoList) when is_list(Format), is_list(Args) ->
                [io_lib:format(Format, Args), "\n", IoList];
            (_, IoList) ->
                IoList
        end,
        [],
        Content
    ),
    io:format("~ts~n", [IoList]),
    {noreply, State};
handle_info(_Info, State) ->
    % ignore
    {noreply, State}.

-spec handle_call(Request, From, State) -> {noreply, State} when
    Request :: term(),
    From :: gen_server:from(),
    State :: state().
handle_call(_Info, _From, State) -> {noreply, State}.

-spec handle_cast(Request, State) -> {noreply, State} when
    Request :: term(),
    State :: state().
handle_cast(_Info, State) -> {noreply, State}.

%% @doc mocks for ct_logs functions
-spec start(file:filename_all()) -> ok.
start(OutputDir) ->
    LogFile = test_logger:get_log_file(OutputDir, ct_daemon),
    ok = test_logger:configure_logger(LogFile),

    {ok, _} = gen_server:start_link({local, ct_logs}, ?MODULE, #{}, []),
    ok.
