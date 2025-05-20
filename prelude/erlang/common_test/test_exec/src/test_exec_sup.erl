%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(test_exec_sup).

-compile([warn_missing_spec_all]).
-moduledoc """
Supervisor that starts two genserver sequentially: the epmd_manager, that will
starts the epmd daemon, and the ct_runner, that will launch the test.
If one of them stops it entails termination of the whole tree.
""".

-behavior(supervisor).

-export([init/1, start_link/1]).

-include_lib("common/include/buck_ct_records.hrl").

-spec start_link(#test_env{}) -> {ok, pid()} | 'ignore' | {error, term()}.
start_link(#test_env{} = TestEnv) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [TestEnv]).

-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} | ignore.
init([#test_env{} = TestEnv]) ->
    {ok,
        {
            #{
                % strategy doesn't matter as
                % none of the children are to be restarted
                strategy => one_for_all,
                intensity => 0,
                period => 1
            },
            [
                #{
                    id => epmd_manager,
                    start => {epmd_manager, start_link, [TestEnv]},
                    restart => temporary,
                    shutdown => 1000,
                    type => worker
                },
                #{
                    id => ct_runner,
                    start => {ct_runner, start_link, [TestEnv]},
                    restart => temporary,
                    shutdown => 1000,
                    type => worker
                }
            ]
        }}.
