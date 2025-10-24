%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(cth_tpx).
-compile(warn_missing_spec_all).

-export([is_running_in_sandcastle/0]).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/4]).
-export([post_init_per_group/5]).
-export([pre_end_per_group/4]).
-export([post_end_per_group/5]).

-export([pre_init_per_testcase/4]).
-export([post_init_per_testcase/5]).
-export([pre_end_per_testcase/4]).
-export([post_end_per_testcase/5]).

-export([on_tc_fail/4]).
-export([on_tc_skip/4]).

-export([terminate/1]).

%% please dialyzer
-export([ok_group/1, fail_group/1]).

%% For tests purposes

-include("method_ids.hrl").

%% ----------------------- Types --------------------------

%  `SUCCESS`, `FAILURE`, `ASSUMPTION_VIOLATION`, `DISABLED`, `EXCLUDED`, `DRY_RUN`

%% -----------------------------------------------------------------------------
%%            Types
%% -----------------------------------------------------------------------------

-export_type([
    shared_state/0
]).

-type tree_node() :: cth_tpx_test_tree:tree_node().
-type group_path() :: cth_tpx_test_tree:group_path().
-type outcome() :: cth_tpx_test_tree:outcome().

-record(state, {
    io_buffer :: pid() | undefined,
    suite :: ct_suite(),
    groups :: group_path(),
    starting_times :: starting_times(),
    tree_results :: tree_node(),
    previous_group_failed :: boolean() | undefined,
    output :: {file, string()} | stdout
}).

-type hook_opts() :: #{role := cth_tpx_role:role(), result_json => string()}.

-type shared_state() :: #state{}.
-type hook_state() :: #{
    id := term(),
    role := cth_tpx_role:role(),
    server := cth_tpx_server:handle()
}.
-type starting_times() :: #{method_id() => float()}.

-type ct_suite() :: module().
-type ct_groupname() :: ct_suite:ct_groupname().
-type ct_testname() :: ct_suite:ct_testname().
-type ct_config() :: ct_suite:ct_config().
-type ct_config_or_skip_or_fail() ::
    ct_config() | {skip, term()} | {fail, term()}.
-type ct_config_or_skip_or_fail_or_term() ::
    ct_config_or_skip_or_fail() | ok | term().

%% -----------------------------------------------------------------------------

-spec second_timestamp() -> float().
second_timestamp() ->
    os:system_time(millisecond) / 1000.

%% -----------------------------------------------------------------------------
%%    Registering and collecting results.
%% -----------------------------------------------------------------------------

% General workflow:
% ct will call methods pre_ post_ method before each method init, case, end methods from
% the test_suite.
% Based on the state in each of these, we create a result that will be passed to the method
% add_result/4.
% This one will register the results into a tree, using the method cth_tpx_test_tree:register_result/4.
% Once the whole run is finished, the method terminate/1 is called.
% This one will, for each requested_test creates and output a method_result, using the
% previously constructed tree_result.

%%%%%%%%%%%%%%%%%% This part is similar to the one in cth_tespilot (execpt for some minor modifications
%% in representing init / main/ end testcases as {Phase, Name})

%% -----------------------------------------------------------------------------
%% Format Functions
%% -----------------------------------------------------------------------------

-spec fmt_skip(Reason) -> unicode:chardata() when
    Reason :: term().
fmt_skip(Reason) ->
    fmt_stack(Reason, "SKIPPED").

-spec fmt_fail(Reason) -> unicode:chardata() when
    Reason :: term().
fmt_fail(Reason) ->
    fmt_stack(Reason, "FAILED").

-spec fmt_init_or_end(Suite, Callback, Reason, Label) -> unicode:chardata() when
    Suite :: ct_suite(),
    Callback :: init_per_suite | end_per_suite | init_per_group | end_per_group | init_per_testcase | end_per_testcase,
    Reason :: term(),
    Label :: unicode:chardata().
fmt_init_or_end(Suite, Callback, {failed, {Suite, Callback, ActualReason}}, Label) ->
    fmt_init_or_end(Suite, Callback, ActualReason, Label);
fmt_init_or_end(_Suite, Callback, Reason, Label) ->
    fmt_stack(Reason, io_lib:format("~ts ~ts", [Callback, Label])).

-spec fmt_stack(Reason, Label) -> unicode:chardata() when
    Reason :: term(),
    Label :: unicode:chardata().
fmt_stack({'EXIT', {Reason, ST}}, Label) ->
    fmt_stack({Reason, ST}, Label);
fmt_stack(Reason, _Label) ->
    Output = ct_error_printer:format_error(Reason, true),
    io_lib:format("~ts", [Output]).

%% -----------------------------------------------------------------------------
%% CT hooks functions
%% -----------------------------------------------------------------------------

-doc """
Return a unique id for this CTH.
""".
-spec id(hook_opts()) -> {?MODULE, cth_tpx_role:role()}.
id(#{role := Role}) ->
    {?MODULE, Role}.

-doc """
Always called before any other callback function. Use this to initiate
any common state.
""".
-spec init(_Id :: term(), Opts :: hook_opts()) -> {ok, hook_state(), integer()}.
init(Id, Opts = #{role := Role}) ->
    ServerName = '$cth_tpx$server$',
    case Role of
        top ->
            Output =
                case maps:get(result_json, Opts, undefined) of
                    undefined -> stdout;
                    FN -> {file, FN}
                end,
            init_role_top(Id, ServerName, Output);
        bot ->
            init_role_bot(Id, ServerName)
    end.

-spec init_role_top(Id :: term(), ServerName :: atom(), Output :: stdout | {file, string()}) ->
    {ok, hook_state(), integer()}.
init_role_top(Id, ServerName, Output) ->
    % IoBuffer that will catpures all the output produced by ct
    IoBuffer = whereis(cth_tpx_io_buffer),
    case IoBuffer of
        undefined ->
            undefined;
        Pid when is_pid(Pid) ->
            unregister(user),
            unregister(cth_tpx_io_buffer),
            register(user, Pid)
    end,
    SharedState = #state{
        output = Output,
        starting_times = #{},
        io_buffer = IoBuffer,
        groups = [],
        tree_results = cth_tpx_test_tree:new_node(unknown_suite)
    },
    Handle = cth_tpx_server:start_link(SharedState),
    % Register so that init_role_bot can find it
    register(ServerName, Handle),
    HookState = #{
        id => Id,
        role => top,
        server => Handle
    },
    {ok, HookState, cth_tpx_role:role_priority(top)}.

-spec init_role_bot(Id :: term(), ServerName :: atom()) -> {ok, hook_state(), integer()}.
init_role_bot(Id, ServerName) ->
    % Put there by init_role_top
    case whereis(ServerName) of
        Handle when is_pid(Handle) ->
            unregister(ServerName),
            HookState = #{
                id => Id,
                role => bot,
                server => Handle
            },
            {ok, HookState, cth_tpx_role:role_priority(bot)}
    end.

-doc """
Called before init_per_suite is called.
""".
-spec pre_init_per_suite(ct_suite(), ct_config(), hook_state()) -> {ct_config_or_skip_or_fail(), hook_state()}.
pre_init_per_suite(Suite, Config, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Config, fun(State) ->
        initialize_stdout_capture(State),
        State1 = capture_starting_time(State, ?INIT_PER_SUITE),
        {Config, State1#state{
            suite = Suite,
            groups = [],
            tree_results = cth_tpx_test_tree:new_node(Suite),
            previous_group_failed = false
        }}
    end).

-spec post_init_per_suite(ct_suite(), ct_config(), ct_config_or_skip_or_fail_or_term(), hook_state()) ->
    {ct_config_or_skip_or_fail_or_term(), hook_state()}.
post_init_per_suite(Suite, _Config, {skip, {failed, _} = Reason} = Error, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        Desc = fmt_init_or_end(Suite, init_per_suite, Reason, ~"FAILED"),
        {Error, add_result(?INIT_PER_SUITE, failed, Desc, State)}
    end);
post_init_per_suite(Suite, _Config, {skip, Reason} = Error, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        % In this case the init_per_suite returns with a {skip, Reason}
        % It then passed fine.
        Desc = fmt_init_or_end(Suite, init_per_suite, Reason, ~"SKIPPED"),
        {Error, add_result(?INIT_PER_SUITE, passed, Desc, State)}
    end);
post_init_per_suite(Suite, _Config, {fail, Reason} = Error, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        Desc = fmt_init_or_end(Suite, init_per_suite, Reason, ~"FAILED"),
        {Error, add_result(?INIT_PER_SUITE, failed, Desc, State)}
    end);
post_init_per_suite(Suite, _Config, Error, HookState) when not is_list(Error) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        Desc = fmt_init_or_end(Suite, init_per_suite, Error, ~"FAILED"),
        {Error, add_result(?INIT_PER_SUITE, failed, Desc, State)}
    end);
post_init_per_suite(_Suite, _Config, Return, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Return, fun(State) ->
        {Return, add_result(?INIT_PER_SUITE, passed, ~"", State)}
    end).

-spec pre_end_per_suite(ct_suite(), ct_config(), hook_state()) -> {ct_config(), hook_state()}.
pre_end_per_suite(_Suite, Config, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Config, fun(State) ->
        initialize_stdout_capture(State),
        {Config, capture_starting_time(State, ?END_PER_SUITE)}
    end).

-spec post_end_per_suite(ct_suite(), ct_config(), ct_config_or_skip_or_fail_or_term(), hook_state()) ->
    {ct_config_or_skip_or_fail_or_term(), hook_state()}.
post_end_per_suite(
    Suite,
    _Config,
    {skip, Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0) ->
        Desc = fmt_init_or_end(Suite, end_per_suite, Reason, ~"SKIPPED"),
        State1 = add_result(?END_PER_SUITE, skipped, Desc, State0),
        {Error, clear_suite(State1)}
    end);
post_end_per_suite(
    Suite,
    _Config,
    {fail, Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0) ->
        Desc = fmt_init_or_end(Suite, end_per_suite, Reason, ~"FAILED"),
        State1 = add_result(?END_PER_SUITE, failed, Desc, State0),
        {Error, clear_suite(State1)}
    end);
post_end_per_suite(
    Suite,
    _Config,
    {error, Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0) ->
        Desc = fmt_init_or_end(Suite, end_per_suite, Reason, ~"FAILED"),
        State1 = add_result(?END_PER_SUITE, failed, Desc, State0),
        {Error, clear_suite(State1)}
    end);
post_end_per_suite(_Suite, _Config, Return, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Return, fun(State0) ->
        %% clean TC state
        State1 = add_result(?END_PER_SUITE, passed, ~"", State0),
        {Return, clear_suite(State1)}
    end).

-spec clear_suite(#state{}) -> #state{}.
clear_suite(#state{io_buffer = IoBuffer} = State) ->
    case IoBuffer of
        undefined -> ok;
        Pid -> io_buffer:stop_capture(Pid)
    end,
    State#state{
        io_buffer = undefined,
        suite = undefined,
        groups = [],
        starting_times = #{}
    }.

-spec pre_init_per_group(ct_suite(), ct_groupname(), ct_config(), hook_state()) -> {ct_config(), hook_state()}.
pre_init_per_group(_SuiteName, _Group, Config, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Config, fun
        (State = #state{groups = [_ | Groups], previous_group_failed = true}) ->
            initialize_stdout_capture(State),
            State1 = capture_starting_time(State, ?INIT_PER_GROUP),
            {Config, State1#state{groups = Groups, previous_group_failed = false}};
        (#state{} = State) ->
            initialize_stdout_capture(State),
            {Config, capture_starting_time(State, ?INIT_PER_GROUP)}
    end).

-spec post_init_per_group(ct_suite(), ct_groupname(), ct_config(), ct_config_or_skip_or_fail_or_term(), hook_state()) ->
    {ct_config_or_skip_or_fail_or_term(), hook_state()}.
post_init_per_group(
    Suite,
    Group,
    _Config,
    {skip, {failed, _} = Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0 = #state{groups = Groups}) ->
        State1 = State0#state{groups = [Group | Groups]},
        Desc = fmt_init_or_end(Suite, init_per_group, Reason, ~"FAILED"),
        State2 = add_result(?INIT_PER_GROUP, failed, Desc, State1),
        {Error, fail_group(State2)}
    end);
post_init_per_group(
    Suite,
    Group,
    _Config,
    {skip, Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0 = #state{groups = Groups}) ->
        State1 = State0#state{groups = [Group | Groups]},
        Desc = fmt_init_or_end(Suite, init_per_group, Reason, ~"SKIPPED"),
        State2 = add_result(?INIT_PER_GROUP, skipped, Desc, State1),
        {Error, fail_group(State2)}
    end);
post_init_per_group(
    Suite,
    Group,
    _Config,
    {fail, Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0 = #state{groups = Groups}) ->
        State1 = State0#state{groups = [Group | Groups]},
        Desc = fmt_init_or_end(Suite, init_per_group, Reason, ~"FAILED"),
        State2 = add_result(?INIT_PER_GROUP, failed, Desc, State1),
        {Error, fail_group(State2)}
    end);
post_init_per_group(Suite, Group, _Config, Error, HookState) when not is_list(Error) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0 = #state{groups = Groups}) ->
        State1 = State0#state{groups = [Group | Groups]},
        Desc = fmt_init_or_end(Suite, init_per_group, Error, ~"FAILED"),
        State2 = add_result(?INIT_PER_GROUP, failed, Desc, State1),
        {Error, fail_group(State2)}
    end);
post_init_per_group(_Suite, Group, _Config, Return, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Return, fun(State0 = #state{groups = Groups}) ->
        State1 = State0#state{groups = [Group | Groups]},
        State2 = add_result(?INIT_PER_GROUP, passed, ~"", State1),
        {Return, ok_group(State2)}
    end).

-spec ok_group(#state{}) -> #state{}.
ok_group(State) ->
    State#state{previous_group_failed = false}.

-spec fail_group(#state{}) -> #state{}.
fail_group(State) ->
    State#state{previous_group_failed = true}.

-spec pre_end_per_group(ct_suite(), ct_groupname(), ct_config(), hook_state()) -> {ct_config(), hook_state()}.
pre_end_per_group(_SuiteName, _Group, Config, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Config, fun(State) ->
        initialize_stdout_capture(State),
        {Config, capture_starting_time(State, ?END_PER_GROUP)}
    end).

-spec post_end_per_group(ct_suite(), ct_groupname(), ct_config(), ct_config_or_skip_or_fail_or_term(), hook_state()) ->
    {ct_config_or_skip_or_fail_or_term(), hook_state()}.
post_end_per_group(
    Suite,
    _Group,
    _Config,
    {skip, Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0 = #state{groups = Groups}) ->
        Desc = fmt_init_or_end(Suite, end_per_group, Reason, ~"SKIPPED"),
        State1 = add_result(?END_PER_GROUP, skipped, Desc, State0),
        {Error, State1#state{groups = tl(Groups)}}
    end);
post_end_per_group(
    Suite,
    _Group,
    _Config,
    {fail, Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0 = #state{groups = Groups}) ->
        Desc = fmt_init_or_end(Suite, end_per_group, Reason, ~"FAILED"),
        State1 = add_result(?END_PER_GROUP, failed, Desc, State0),
        {Error, State1#state{groups = tl(Groups)}}
    end);
post_end_per_group(
    Suite,
    _Group,
    _Config,
    {error, Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0 = #state{groups = Groups}) ->
        Desc = fmt_init_or_end(Suite, end_per_group, Reason, ~"FAILED"),
        State1 = add_result(?END_PER_GROUP, failed, Desc, State0),
        {Error, State1#state{groups = tl(Groups)}}
    end);
post_end_per_group(_Suite, _Group, _Config, Return, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Return, fun(State0 = #state{groups = Groups}) ->
        State1 = add_result(?END_PER_GROUP, passed, ~"", State0),
        {Return, State1#state{groups = tl(Groups)}}
    end).

-spec pre_init_per_testcase(ct_suite(), ct_testname(), ct_config(), hook_state()) -> {ct_config(), hook_state()}.
pre_init_per_testcase(_Suite, TestCase, Config, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Config, fun(State) ->
        initialize_stdout_capture(State),
        %% store name and start time for current test case
        %% We capture time twice:
        %%  1) For the init_per_testcase.
        %%  2) For the whole testcase = init + actual_testcase + end
        %% The reason behind is that capturing the timing for the actual_testcase
        %% is not straightforward, as there is no pre/post method for it.
        State1 = capture_starting_time(
            capture_starting_time(State, {TestCase, ?INIT_PER_TESTCASE}), {TestCase, ?MAIN_TESTCASE}
        ),
        {Config, State1}
    end).

-spec post_init_per_testcase(ct_suite(), ct_testname(), ct_config(), ct_config_or_skip_or_fail_or_term(), hook_state()) ->
    {ct_config_or_skip_or_fail_or_term(), hook_state()}.
post_init_per_testcase(
    Suite,
    TestCase,
    _Config,
    {skip, {failed, _} = Reason} = Error,
    HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        %% ct skip because of failed init is reported as error
        Desc = fmt_init_or_end(Suite, init_per_testcase, Reason, ~"FAILED"),
        {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, failed, Desc, State)}
    end);
post_init_per_testcase(
    Suite, TestCase, _Config, {skip, Reason} = Error, HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        %% other skips (user skip) are reported as skips
        Desc = fmt_init_or_end(Suite, init_per_testcase, Reason, ~"SKIPPED"),
        {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, skipped, Desc, State)}
    end);
post_init_per_testcase(
    Suite, TestCase, _Config, {fail, Reason} = Error, HookState
) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        %% fails are reported as errors
        Desc = fmt_init_or_end(Suite, init_per_testcase, Reason, ~"FAILED"),
        {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, failed, Desc, State)}
    end);
post_init_per_testcase(Suite, TestCase, _Config, {error, Reason} = Error, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        %% terms are reported as errors except ok (missing in CT doc)
        Desc = fmt_init_or_end(Suite, init_per_testcase, Reason, ~"FAILED"),
        {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, failed, Desc, State)}
    end);
post_init_per_testcase(Suite, TestCase, _Config, Error, HookState) when
    not is_list(Error) andalso ok =/= Error
->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State) ->
        %% terms are reported as errors except ok (missing in CT doc)
        Desc = fmt_init_or_end(Suite, init_per_testcase, Error, ~"FAILED"),
        {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, failed, Desc, State)}
    end);
post_init_per_testcase(_Suite, TestCase, _Config, Return, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Return, fun(State) ->
        %% everything else is ok
        State1 = add_result({TestCase, ?INIT_PER_TESTCASE}, passed, ~"", State),
        {Return, State1}
    end).

%% add test result to state
-spec add_result(method_id(), outcome(), unicode:chardata(), shared_state()) -> shared_state().
add_result(
    Method,
    Outcome,
    Desc,
    State = #state{
        groups = Groups,
        starting_times = ST0,
        tree_results = TreeResults,
        io_buffer = IoBuffer,
        output = {file, OutputFile}
    }
) ->
    StdOut =
        case IoBuffer of
            undefined ->
                "";
            BufferPid ->
                {Io, Truncated} = io_buffer:flush(BufferPid),
                case Truncated of
                    true ->
                        StdOutLocation =
                            case is_running_in_sandcastle() of
                                true ->
                                    "tab Diagnostics: Artifacts/ct_executor.stdout.txt";
                                _ ->
                                    filename:join(
                                        filename:dirname(OutputFile), "ct_executor.stdout.txt"
                                    )
                            end,
                        unicode_characters_to_string(
                            io_lib:format(
                                "The stdout logs have been truncated, see ~ts for the full suite stdout. Showing tail below\n~s",
                                [StdOutLocation, Io]
                            )
                        );
                    false ->
                        Io
                end
        end,

    QualifiedName = method_name(Method, Groups),
    TS = second_timestamp(),
    Result0 = #{
        name => QualifiedName,
        outcome => Outcome,
        details => unicode_characters_to_string(Desc),
        std_out => StdOut
    },
    Result =
        case ST0 of
            #{Method := StartedTime} ->
                Result0#{
                    startedTime => StartedTime,
                    endedTime => TS
                };
            _ ->
                %% If no test data (skipped test cases/groups/suits)
                %% then started time doesn't exist.
                Result0
        end,
    ST1 = maps:remove(Method, ST0),
    NewTreeResults = cth_tpx_test_tree:register_result(TreeResults, Result, Groups, Method),
    State#state{starting_times = ST1, tree_results = NewTreeResults}.

-spec method_name(method_id(), group_path()) -> string().
method_name(Method, Groups) ->
    MethodName =
        case Method of
            {TestCase, Phase} -> io_lib:format("~ts.~ts", [atom_to_list(TestCase), atom_to_list(Phase)]);
            MethodName0 -> atom_to_list(MethodName0)
        end,
    cth_tpx_test_tree:qualified_name(Groups, MethodName).

-spec pre_end_per_testcase(ct_suite(), ct_testname(), ct_config(), hook_state()) -> {ct_config(), hook_state()}.
pre_end_per_testcase(_Suite, TC, Config, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Config, fun(State) ->
        {Config, capture_starting_time(State, {TC, ?END_PER_TESTCASE})}
    end).

-spec post_end_per_testcase(ct_suite(), ct_testname(), ct_config(), ct_config_or_skip_or_fail_or_term(), hook_state()) ->
    {ct_config_or_skip_or_fail_or_term(), hook_state()}.
post_end_per_testcase(_Suite, TC, _Config, ok = Return, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Return, fun(State0) ->
        State1 = add_result({TC, ?END_PER_TESTCASE}, passed, ~"", State0),
        {ok, add_result({TC, ?MAIN_TESTCASE}, passed, ~"", State1)}
    end);
post_end_per_testcase(Suite, TC, Config, Error, HookState) ->
    on_shared_state(HookState, ?FUNCTION_NAME, Error, fun(State0) ->
        NextState =
            case lists:keyfind(tc_status, 1, Config) of
                {tc_status, ok} ->
                    %% Test case passed, but we still ended in an error
                    %% same description as ct output
                    %% first report testcase itself
                    State1 = add_result({TC, ?MAIN_TESTCASE}, passed, ~"", State0),
                    %% now report end per failure
                    Reason =
                        case Error of
                            {skip, Reason0} -> Reason0;
                            {fail, Reason0} -> Reason0;
                            _ -> Error
                        end,
                    Desc = fmt_init_or_end(Suite, end_per_testcase, Reason, ~"FAILED"),
                    add_result({TC, ?END_PER_TESTCASE}, failed, Desc, State1);
                _ ->
                    %% Test case failed, in which case on_tc_fail already reports it
                    add_result({TC, ?END_PER_TESTCASE}, passed, ~"", State0)
            end,
        {Error, NextState}
    end).

-spec on_tc_fail(Suite, Callback, Reason, HookState) -> hook_state() when
    Suite :: ct_suite(),
    Callback ::
        init_per_suite
        | end_per_suite
        | ct_testname()
        | {ct_testname(), ct_groupname()}
        | {init_per_group | end_per_group, ct_groupname()},
    Reason :: term(),
    HookState :: hook_state().
on_tc_fail(_SuiteName, init_per_suite, _, HookState) ->
    HookState;
on_tc_fail(_SuiteName, end_per_suite, _, HookState) ->
    HookState;
on_tc_fail(_SuiteName, {init_per_group, _GroupName}, _, HookState) ->
    HookState;
on_tc_fail(_SuiteName, {end_per_group, _GroupName}, _, HookState) ->
    HookState;
on_tc_fail(_SuiteName, {TC, _Group}, Reason, HookState) ->
    modify_shared_state(HookState, ?FUNCTION_NAME, fun(State) ->
        Desc = fmt_fail(Reason),
        add_result({TC, ?MAIN_TESTCASE}, failed, Desc, State)
    end);
on_tc_fail(_SuiteName, TC, Reason, HookState) ->
    modify_shared_state(HookState, ?FUNCTION_NAME, fun(State) ->
        Desc = fmt_fail(Reason),
        add_result({TC, ?MAIN_TESTCASE}, failed, Desc, State)
    end).

-spec on_tc_skip(Suite, Callback, Reason, HookState) -> hook_state() when
    Suite :: ct_suite(),
    Callback ::
        init_per_suite
        | end_per_suite
        | ct_testname()
        | {ct_testname(), ct_groupname()}
        | {init_per_group | end_per_group, ct_groupname()},
    Reason :: {tc_auto_skip, term()} | {tc_user_skip, term()},
    HookState :: hook_state().
on_tc_skip(_SuiteName, init_per_suite, _, HookState) ->
    HookState;
on_tc_skip(_SuiteName, end_per_suite, _, HookState) ->
    HookState;
on_tc_skip(_SuiteName, {init_per_group, _GroupName}, _, HookState) ->
    HookState;
on_tc_skip(_SuiteName, {end_per_group, _GroupName}, _, HookState) ->
    HookState;
on_tc_skip(_SuiteName, {TC, _Group}, Reason, HookState) ->
    modify_shared_state(HookState, ?FUNCTION_NAME, fun(State) ->
        handle_on_tc_skip(TC, Reason, State)
    end);
on_tc_skip(_SuiteName, TC, Reason, HookState) ->
    modify_shared_state(HookState, ?FUNCTION_NAME, fun(State) ->
        handle_on_tc_skip(TC, Reason, State)
    end).

-spec handle_on_tc_skip(TC, Reason, State) -> State when
    TC :: ct_testname(),
    Reason :: {tc_auto_skip, term()} | {tc_user_skip, term()},
    State :: #state{}.
handle_on_tc_skip(TC, {tc_auto_skip, Reason}, State = #state{suite = Suite}) ->
    Desc = fmt_fail(Reason),
    NewState = add_result({TC, ?MAIN_TESTCASE}, failed, Desc, State),
    NewState#state{suite = Suite};
handle_on_tc_skip(TC, {tc_user_skip, Reason}, State = #state{suite = Suite}) ->
    Desc = fmt_skip(Reason),
    NewState = add_result({TC, ?MAIN_TESTCASE}, skipped, Desc, State),
    NewState#state{suite = Suite}.

-doc """
Called when the scope of the CTH is done
""".
-spec terminate(hook_state()) -> ok | {error, _Reason}.
terminate(#{role := top, server := Handle}) ->
    #state{output = Output, tree_results = TreeResults} = cth_tpx_server:get(Handle),
    write_output(Output, term_to_binary(TreeResults));
terminate(#{role := bot}) ->
    ok.

-spec write_output({file, string()} | stdout, binary()) -> ok.
write_output({file, FN}, JSON) ->
    io:format("Writing result file ~tp~n", [FN]),
    ok = filelib:ensure_dir(FN),
    ok = file:write_file(FN, JSON, [raw, binary]);
write_output(stdout, JSON) ->
    io:format(user, "~tp", [JSON]).

-spec initialize_stdout_capture(shared_state()) -> ok.
initialize_stdout_capture(#state{io_buffer = IoBuffer} = _State) ->
    case IoBuffer of
        undefined ->
            ok;
        Pid when erlang:is_pid(Pid) ->
            io_buffer:stop_capture(Pid),
            io_buffer:flush(Pid),
            io_buffer:start_capture(Pid)
    end.

-spec capture_starting_time(shared_state(), method_id()) -> shared_state().
capture_starting_time(#state{starting_times = ST0} = State, MethodId) ->
    State#state{starting_times = ST0#{MethodId => second_timestamp()}}.

-spec on_shared_state(hook_state(), Caller, Default, Action) -> {A, hook_state()} when
    Caller :: cth_tpx_role:responsibility(),
    Default :: A,
    Action :: fun((shared_state()) -> {A, shared_state()}).
on_shared_state(HookState = #{role := Role, server := Handle}, Caller, Default, Action) ->
    case cth_tpx_role:is_responsible(Role, Caller) of
        true ->
            A = cth_tpx_server:modify(Handle, Action),
            {A, HookState};
        false ->
            {Default, HookState}
    end.

-spec modify_shared_state(hook_state(), Caller, Action) -> hook_state() when
    Caller :: cth_tpx_role:responsibility(),
    Action :: fun((shared_state()) -> shared_state()).
modify_shared_state(HookState, Caller, Action) ->
    {ok, NewHookState} = on_shared_state(HookState, Caller, _Default = ok, fun(State) ->
        {ok, Action(State)}
    end),
    NewHookState.

-spec is_running_in_sandcastle() -> boolean().
is_running_in_sandcastle() ->
    case os:getenv("SANDCASTLE_DIFF_ID") of
        [$D | _] ->
            true;
        _ ->
            case os:getenv("SANDCASTLE") of
                false -> false;
                _ -> true
            end
    end.

-spec unicode_characters_to_string(unicode:chardata()) -> string().
unicode_characters_to_string(Chars) ->
    case unicode:characters_to_list(Chars) of
        String when is_list(String) -> String
    end.
