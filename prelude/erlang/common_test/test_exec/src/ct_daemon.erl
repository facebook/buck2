%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(ct_daemon).
-moduledoc """
Daemon for running Common Test in an iterative way from an Erlang Shell
""".

-export([
    start/1, start/2,
    stop/0,
    alive/0,
    run/1,
    list/0, list/1,
    ping/0,
    push_module/1,
    push_paths/1,
    set_gl/0,
    discover/1,
    load_changed/0,
    setup_state/0,
    output_dir/0,
    priv_dir/0,
    test_node/0
]).

-doc """
start a test-node with random name and shortname
""".
-spec start(ErlCommand) -> ok when
    ErlCommand :: [binary()].
start(ErlCommand) ->
    ct_daemon_node:start(ErlCommand).

-doc """
starts the test node with the given distribution mode and node name
""".
-spec start(ErlCommand, Config) -> ok | {error, {crash_on_startup, integer()}} when
    ErlCommand :: [binary()],
    Config :: ct_daemon_node:config().
start(ErlCommand, NodeInfo) ->
    ct_daemon_node:start(ErlCommand, NodeInfo).

-doc """
stops the test node
""".
-spec stop() -> node().
stop() ->
    ct_daemon_node:stop().

-doc """
returns if the test-node is alive
""".
-spec alive() -> boolean().
alive() ->
    ct_daemon_node:alive().

-doc """
run test from scratch
""".
-spec run(
    Test ::
        string()
        | non_neg_integer()
        | {discovered, [#{suite => module(), name => string()}]}
) ->
    #{string() => ct_daemon_core:run_result()} | ct_daemon_runner:discover_error() | node_down.
run(Test) ->
    do_call({run, Test}).

-spec ping() -> {pong, term()} | node_down.
ping() ->
    do_call(ping).

-spec load_changed() -> [module()] | node_down.
load_changed() ->
    do_call(load_changed).

-spec set_gl() -> ok | node_down.
set_gl() ->
    do_call({gl, group_leader()}).

-spec list() -> [{module(), [{non_neg_integer(), string()}]}] | node_down.
list() ->
    do_call(list).

-spec list(RegEx :: string()) ->
    [{module(), [{non_neg_integer(), string()}]}] | {invalid_regex, {string(), non_neg_integer()}} | node_down.
list(RegEx) ->
    case re:compile(RegEx) of
        {ok, Pattern} ->
            case list() of
                node_down ->
                    node_down;
                Listing ->
                    [
                        {Suite, [Test || Test = {_Id, Name} <- Tests, re:run(Name, Pattern) =/= nomatch]}
                     || {Suite, Tests} <- Listing
                    ]
            end;
        {error, ErrSpec} ->
            {invalid_regex, ErrSpec}
    end.

-spec discover(pos_integer() | string()) ->
    [#{suite := module(), name := string()}]
    | ct_daemon_runner:discover_error()
    | node_down.
discover(RegExOrId) ->
    do_call({discover, RegExOrId}).

-spec setup_state() -> [atom()] | undefined.
setup_state() ->
    case alive() of
        true ->
            case do_call(setup) of
                node_down -> undefined;
                Result -> Result
            end;
        _ ->
            undefined
    end.

-spec output_dir() -> file:filename_all() | node_down.
output_dir() ->
    do_call(output_dir).

-spec priv_dir() -> file:filename_all() | undefined | node_down.
priv_dir() ->
    do_call(priv_dir).

-spec push_paths(Paths :: [file:filename_all()]) -> ok.
push_paths(Paths) ->
    case alive() of
        true ->
            do_cast({code_paths, Paths});
        false ->
            ok
    end.

-spec push_module(module()) -> ok.
push_module(Module) ->
    case alive() of
        true ->
            do_cast({load_module, Module});
        false ->
            ok
    end.

-spec test_node() -> node() | undefined.
test_node() ->
    case global:whereis_name(ct_daemon_runner:name(node())) of
        undefined -> undefined;
        Pid -> erlang:node(Pid)
    end.

%% call abstraction:
-spec do_call(term()) -> dynamic() | node_down.
do_call(Request) ->
    try
        gen_server:call({global, ct_daemon_runner:name(node())}, Request, infinity)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            node_down
    end.

-spec do_cast(term()) -> ok.
do_cast(Request) ->
    gen_server:cast({global, ct_daemon_runner:name(node())}, Request).
