%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% % @format
-module(cth_tpx).

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

%% For tests purposes
-export([register_result/4, get_result/2, qualifiedName/2]).

%% ----------------------- Types --------------------------

%  `SUCCESS`, `FAILURE`, `ASSUMPTION_VIOLATION`, `DISABLED`, `EXCLUDED`, `DRY_RUN`

%% -----------------------------------------------------------------------------
%%            Records and types
%% -----------------------------------------------------------------------------

-record(tree_node, {
    name :: string(),
    init_method = none :: option(method_result()),
    end_method = none :: option(method_result()),
    test_cases = #{} :: #{string() => test_leaf()},
    sub_groups = #{} :: #{string() => tree_node()}
}).
-record(test_leaf, {name :: string(), init_method = none, end_method = none, main_method = none}).
-record(state, {
    io_buffer :: pid() | undefined,
    id :: string(),
    suite :: string(),
    groups :: list(string()),
    starting_times :: starting_times(),
    tree_results :: tree_node(),
    previous_group_failed :: string(),
    output :: {file, string()} | stdout
}).

-type inits_ends_groups_methods() ::
    init_per_suite | end_per_suite | init_per_group | end_per_group.
-type inits_ends_case() :: init_per_case | end_per_case.
-type method_id() :: inits_ends_groups_methods() | {atom(), inits_ends_case()}.
-type tree() :: test_leaf() | tree_node().
-type case_result() :: #{
    inits := list(method_result()),
    main := method_result(),
    ends := list(method_result())
}.
-type tree_node() :: #tree_node{}.
-type test_leaf() :: #test_leaf{}.
-type state() :: #state{}.
-type starting_times() :: #{method_id() => float()}.
-type option(Type) :: Type | none.
-type method_result() :: #{
    name := string(),
    startedTime := float(),
    endedTime := float(),
    outcome := atom(),
    details := string(),
    std_out := string()
}.

-define(INIT_PER_SUITE, '[init_per_suite]').
-define(INIT_PER_GROUP, '[init_per_group]').
-define(INIT_PER_TESTCASE, '[init_per_testcase]').
-define(END_PER_TESTCASE, '[end_per_testcase]').
-define(END_PER_GROUP, '[end_per_group]').
-define(END_PER_SUITE, '[end_per_suite]').
-define(MAIN_TESTCASE, '[main_testcase]').

-export_type([tree_node/0]).
-export_type([method_result/0, case_result/0]).

%% -----------------------------------------------------------------------------

-spec second_timestamp() -> float().
second_timestamp() ->
    os:system_time(millisecond) / 1000.

%% The groups order expected here is [leaf_group, ...., root_group]
-spec qualifiedName(list(atom()), string()) -> string().
qualifiedName(Groups, TestCase) ->
    StringGroups = [atom_to_list(Group) || Group <- Groups],
    JoinedGroups = string:join(lists:reverse(StringGroups), ":"),
    Raw = io_lib:format("~s.~s", [JoinedGroups, TestCase]),
    unicode:characters_to_list(Raw, latin1).

%% -----------------------------------------------------------------------------
%%    Registering and collecting results.
%% -----------------------------------------------------------------------------

% General workflow:
% ct will call methods pre_ post_ method before each method init, case, end methods from
% the test_suite.
% Based on the state in each of these, we create a result that will be passed to the method
% add_result/4.
% This one will register the results into a tree, using the method register_result/4.
% Once the whole run is finished, the method terminate/1 is called.
% This one will, for each requested_test creates and output a method_result, using the
% previously constructed tree_result.

%% @doc Transforms the pre_method_result into a method_result and inserts it inside the TreeResult
-spec register_result(tree_node(), method_result(), list(string()), method_id()) -> tree_node().
register_result(TreeResult, Result, Groups, MethodId) ->
    insert_result(TreeResult, Result, lists:reverse(Groups), MethodId).

%% @doc Inserts the method_result inside the tree.
-spec insert_result(tree_node(), method_result(), list(string()), method_id()) -> tree_node().
insert_result(TreeNode, ResultTest, [Group | Groups], MethodId) ->
    Children = TreeNode#tree_node.sub_groups,
    GroupNode = maps:get(Group, Children, #tree_node{name = Group}),
    NewChildren = Children#{Group => insert_result(GroupNode, ResultTest, Groups, MethodId)},
    TreeNode#tree_node{sub_groups = NewChildren};
insert_result(TreeNode, ResultTest, [], MethodId) ->
    case MethodId of
        Init when Init =:= ?INIT_PER_SUITE; Init =:= ?INIT_PER_GROUP ->
            TreeNode#tree_node{init_method = ResultTest};
        End when End =:= ?END_PER_SUITE; End =:= ?END_PER_GROUP ->
            TreeNode#tree_node{end_method = ResultTest};
        {NameCase, Phase} ->
            Cases = TreeNode#tree_node.test_cases,
            TestLeaf = maps:get(NameCase, Cases, #test_leaf{name = NameCase}),
            NewTestLeaf =
                case Phase of
                    ?INIT_PER_TESTCASE ->
                        TestLeaf#test_leaf{init_method = ResultTest};
                    ?MAIN_TESTCASE ->
                        TestLeaf#test_leaf{main_method = ResultTest};
                    ?END_PER_TESTCASE ->
                        TestLeaf#test_leaf{end_method = ResultTest}
                end,
            TreeNode#tree_node{test_cases = Cases#{NameCase => NewTestLeaf}}
    end.

%% @doc Provides a result for the RequestedResults based on the collected results.
%% The format of the requested_results is a map from a list of groups to the list of test_cases that are sub-cases from the last group from the list.
-spec get_result(tree_node(), #{list(string) => list(string)}) -> list(case_result()).
get_result(TreeResult, RequestedResults) ->
    maps:fold(
        fun(Groups, CasesRequests, AccExt) ->
            lists:map(
                fun(CaseRequest) ->
                    collect_result(TreeResult, Groups, CaseRequest)
                end,
                CasesRequests
            ) ++
                AccExt
        end,
        [],
        RequestedResults
    ).

%% @doc Provides a result for a given specific requested_result.
-spec collect_result(tree(), list(string()), string()) -> case_result().
collect_result(TreeResult, Groups, TestCase) ->
    QualifiedName = qualifiedName(lists:reverse(Groups), TestCase),
    LeafResult = collect_result(TreeResult, [], [], Groups, TestCase, QualifiedName),
    #{ends := EndsResults, main := MainResult} = LeafResult,
    MainResultWithEndFailure = report_end_failure(EndsResults, MainResult),
    LeafResult#{main => MainResultWithEndFailure}.

report_end_failure([], ResultAcc) ->
    ResultAcc;
report_end_failure([#{outcome := passed} | Rest], ResultAcc) ->
    report_end_failure(Rest, ResultAcc);
report_end_failure([#{outcome := skipped} | Rest], ResultAcc) ->
    report_end_failure(Rest, ResultAcc);
report_end_failure(
    [#{outcome := EndOutcome, details := EndDetails, name := EndName} | Rest],
    #{name := TestName, outcome := ResultOutcome, details := ResultDetails} = ResultAcc
) ->
    MergedOutcome = merge_outcome(EndOutcome, ResultOutcome),
    EndFailedDetails =
        [io_lib:format("~p ~p because ~p failed with ~n", [TestName, MergedOutcome, EndName]), EndDetails],
    MergedDetails =
        case ResultOutcome of
            passed ->
                EndFailedDetails;
            _ ->
                lists:flatten(
                    io_lib:format("~s~n~n~s", [ResultDetails, EndFailedDetails])
                )
        end,
    report_end_failure(Rest, ResultAcc#{outcome => MergedOutcome, details => MergedDetails}).

merge_outcome(failed, _) -> failed;
merge_outcome(_, failed) -> failed;
merge_outcome(timeout, _) -> timeout;
merge_outcome(_, timeout) -> timeout;
merge_outcome(skipped, _) -> skipped;
merge_outcome(_, skipped) -> skipped;
merge_outcome(passed, Other) -> Other.

%% @doc Collects all the inits / ends methods results linked to a requested_result.
-spec collect_result(
    tree(),
    list(method_result()),
    list(method_result()),
    list(string()),
    string(),
    string()
) -> case_result().
collect_result(Node, Inits, Ends, Groups, TestCase, QualifiedName) ->
    {NewInits, OptMain, NewEnds} = collect_node(Node, Inits, Ends, QualifiedName),
    case OptMain of
        none ->
            {Child, NewGroups} = get_child(Node, Groups, TestCase),
            collect_result(Child, NewInits, NewEnds, NewGroups, TestCase, QualifiedName);
        MainResult ->
            #{inits => lists:reverse(NewInits), main => MainResult, ends => NewEnds}
    end.

-spec get_child(tree(), list(string()), string()) -> {tree(), list(string())}.
get_child(Node, [Group | Groups], _TestCase) ->
    {maps:get(Group, Node#tree_node.sub_groups, #tree_node{name = Group}), Groups};
get_child(Node, [], TestCase) ->
    {maps:get(TestCase, Node#tree_node.test_cases, #test_leaf{name = TestCase}), []}.

-spec collect_node(tree(), list(method_result()), list(method_result()), string()) ->
    {list(method_result()), option(method_result()), list(method_result())}.
%% Collect the results from init_testcase, end_testcase and the main testcase for a given requested result.
%% Proceeds with some additional logic if the result is missing or skipped.
collect_node(
    #test_leaf{init_method = OptMethodInit, end_method = OptMethodEnd} = TestLeaf,
    Inits,
    Ends,
    QualifiedName
) ->
    {NewInits, NewEnds} = update_inits_ends(Inits, Ends, OptMethodInit, OptMethodEnd),
    InitsPassed = lists:all(
        fun
            (#{outcome := failed}) -> false;
            (#{outcome := timeout}) -> false;
            (_) -> true
        end,
        NewInits
    ),
    %% Do NOT try to collect a result when one of the inits failed
    MainResult =
        case {InitsPassed, TestLeaf#test_leaf.main_method} of
            {false, _} ->
                get_missing_result(NewInits, QualifiedName);
            {true, none} ->
                get_missing_result(NewInits, QualifiedName);
            {true, Result} ->
                case maps:get(outcome, Result) of
                    skipped -> handle_skipped_result(NewInits, Result);
                    _ -> Result#{std_out => merge_std_out(TestLeaf)}
                end
        end,
    {NewInits, MainResult, NewEnds};
collect_node(
    #tree_node{init_method = OptMethodInit, end_method = OptMethodEnd} = _TreeNode,
    Inits,
    Ends,
    _QualifiedName
) ->
    {NewInits, NewEnds} = update_inits_ends(Inits, Ends, OptMethodInit, OptMethodEnd),
    {NewInits, none, NewEnds}.

-spec update_inits_ends(
    list(method_result()),
    list(method_result()),
    option(method_result()),
    option(method_result())
) -> {list(method_result()), list(method_result())}.
update_inits_ends(Inits, Ends, OptMethodInit, OptMethodEnd) ->
    {adds_if_present(OptMethodInit, Inits), adds_if_present(OptMethodEnd, Ends)}.

-spec adds_if_present(option(X), list(X)) -> list(X).
adds_if_present(Optional, List) ->
    case Optional of
        none -> List;
        Obj -> [Obj | List]
    end.

%% Merge the StdOut from the init_per_testcase, main_testcase, and end_per_testcase
-spec merge_std_out(#test_leaf{}) -> string().
merge_std_out(
    #test_leaf{init_method = OptMethodInit, main_method = OptMainMethod, end_method = OptMethodEnd} =
        _TestLeaf
) ->
    InitStdOut =
        case OptMethodInit of
            none -> "";
            _ -> maps:get(std_out, OptMethodInit)
        end,
    MainStdOut =
        case OptMainMethod of
            none ->
                "";
            _ ->
                maps:get(std_out, OptMainMethod)
        end,
    EndStdOut =
        case OptMethodEnd of
            none -> "";
            _ -> maps:get(std_out, OptMethodEnd)
        end,
    unicode:characters_to_list(InitStdOut ++ MainStdOut ++ EndStdOut).

%% @doc Creates a method_result for a requested method for which no result was registered.
%% Attemps to locate if one of the inits is responsible for the missing result.
-spec get_missing_result(list(method_result()), string()) -> method_result().
get_missing_result(Inits, QualifiedName) ->
    MainResult =
        #{
            name => unicode:characters_to_list(
                io_lib:format("~s.[main_testcase]", [QualifiedName])
            ),
            outcome => failed,
            details => "no results for this test were recorded",
            std_out => ""
        },
    handle_missing_results(Inits, MainResult).

%% @doc Generates an user informative message in the case of the missing result by attempting to find the right init to blame.
-spec handle_missing_results(list(method_result()), method_result()) -> method_result().
handle_missing_results([], MainResult) ->
    MainResult;
handle_missing_results([Init | Inits], MainResult) ->
    InitStdOut = unicode:characters_to_list(
        maps:get(name, Init) ++ " stdout: " ++ maps:get(std_out, Init)
    ),
    case maps:get(outcome, Init) of
        failed ->
            MainResult#{
                details =>
                    unicode:characters_to_list(
                        io_lib:format(
                            "no results for this test were recorded because init ~s failed with error message : \n ~s",
                            [maps:get(name, Init), maps:get(details, Init)]
                        )
                    ),
                std_out => InitStdOut
            };
        timeout ->
            MainResult#{
                details => unicode:characters_to_list(
                    io_lib:format(
                        "no results for this test were recorded because init ~s timed-out with error message : \n ~s",
                        [maps:get(name, Init), maps:get(details, Init)]
                    )
                ),
                std_out => InitStdOut
            };
        skipped ->
            handle_skipped_result([Init | Inits], MainResult);
        omitted ->
            MainResult#{
                details => unicode:characters_to_list(
                    io_lib:format(
                        "no results for this test were recorded because init ~s was omitted with message : \n ~s",
                        [maps:get(name, Init), maps:get(details, Init)]
                    )
                ),
                std_out => InitStdOut
            };
        passed ->
            handle_skipped_result([Init | Inits], MainResult)
    end.

%% A result can be erlang skipped if it is either user skipped or skipped because of an init failure.
%% Skip is an error state in tpx. If it is user skipped, the test is reported as omitted, which is not an error state.
%% In the case where it is skipped because of init failure, it is reported as failed with appropriate user messge reporting
%% to the init to be blamed.
-spec handle_skipped_result(list(method_result()), method_result()) -> method_result().
handle_skipped_result([], MainResult) ->
    MainResult;
handle_skipped_result([Init | Inits], MainResult) ->
    InitStdOut = unicode:characters_to_list(
        maps:get(name, Init) ++ " stdout: " ++ maps:get(std_out, Init)
    ),
    case maps:get(outcome, Init) of
        failed ->
            MainResult#{
                outcome => failed,
                details =>
                    unicode:characters_to_list(
                        io_lib:format(
                            "Failed because init ~s failed, with error message : \n ~s",
                            [maps:get(name, Init), maps:get(details, Init)]
                        )
                    ),
                std_out => InitStdOut
            };
        timeout ->
            MainResult#{
                outcome => timeout,
                details =>
                    unicode:characters_to_list(
                        io_lib:format(
                            "Timed-out because init ~s timed-out, with error message : \n ~s",
                            [maps:get(name, Init), maps:get(details, Init)]
                        )
                    ),
                std_out => InitStdOut
            };
        passed ->
            handle_skipped_result(Inits, MainResult);
        skipped ->
            handle_skipped_result(Inits, MainResult);
        omitted ->
            MainResult#{
                outcome => failed,
                details =>
                    unicode:characters_to_list(
                        io_lib:format(
                            "Failed because init ~s was omitted, with error message : \n ~s",
                            [maps:get(name, Init), maps:get(details, Init)]
                        )
                    ),
                std_out => InitStdOut
            }
    end.

%%%%%%%%%%%%%%%%%% This part is similar to the one in cth_tespilot (execpt for some minor modifications
%% in representing init / main/ end testcases as {Phase, Name})

%% -----------------------------------------------------------------------------
%% Format Functions
%% -----------------------------------------------------------------------------

fmt_skip(Suite, CasePat, CaseArgs, Reason) ->
    fmt_stack(Suite, CasePat, CaseArgs, Reason, "SKIPPED").

fmt_fail(Suite, CasePat, CaseArgs, Reason) ->
    fmt_stack(Suite, CasePat, CaseArgs, Reason, "FAILED").

fmt_stack(Suite, CasePat, CaseArgs, {_Outcome, {_Suite, end_per_testcase, {'EXIT', {Reason, ST}}}}, Label) ->
    fmt_stack(Suite, CasePat, CaseArgs, {Reason, ST}, Label);
fmt_stack(Suite, CasePat, CaseArgs, {_Class, {Reason, ST}}, Label) ->
    fmt_stack(Suite, CasePat, CaseArgs, {Reason, ST}, Label);
fmt_stack(_Suite, _CasePat, _CaseArgs, Reason, _Label) ->
    Output = ct_error_printer:format_error(Reason, true),
    unicode:characters_to_list(io_lib:format("~s", [Output])).

%% -----------------------------------------------------------------------------
%% CT hooks functions
%% -----------------------------------------------------------------------------

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
-spec init(string(), proplist:proplist()) -> state().
init(Id, Opts) ->
    Output =
        case proplists:get_value(result_json, Opts, undefined) of
            undefined -> stdout;
            FN -> {file, FN}
        end,
    % IoBuffer that will catpures all the output produced by ct
    IoBuffer = whereis(cth_tpx_io_buffer),
    case IoBuffer of
        undefined ->
            ok;
        Pid ->
            unregister(user),
            unregister(cth_tpx_io_buffer),
            register(user, Pid)
    end,
    {ok, #state{
        output = Output,
        id = Id,
        starting_times = #{},
        io_buffer = IoBuffer,
        groups = []
    }}.

%% @doc Called before init_per_suite is called.
-spec pre_init_per_suite(string(), any(), state()) -> state().
pre_init_per_suite(Suite, Config, #state{} = State) ->
    initialize_stdout_capture(State),
    State1 = capture_starting_time(State, ?INIT_PER_SUITE),
    {Config, State1#state{
        suite = Suite,
        groups = [],
        tree_results = #tree_node{name = Suite},
        previous_group_failed = false
    }}.

%% @doc Called after init_per_suite.
post_init_per_suite(Suite, _Config, {skip, {failed, _Reason}} = Error, State) ->
    Desc = fmt_stack(Suite, "", [], Error, "init_per_suite FAILED"),
    {Error, add_result(?INIT_PER_SUITE, failed, Desc, State)};
post_init_per_suite(Suite, _Config, {skip, _Reason} = Error, State) ->
    % In this case the init_per_suite returns with a {skip, Reason}
    % It then passed fine.
    Desc = fmt_stack(Suite, "", [], Error, "init_per_suite SKIPPED"),
    {Error, add_result(?INIT_PER_SUITE, passed, Desc, State)};
post_init_per_suite(Suite, _Config, {fail, _Reason} = Error, State) ->
    Desc = fmt_stack(Suite, "", [], Error, "init_per_suite FAILED"),
    {Error, add_result(?INIT_PER_SUITE, failed, Desc, State)};
post_init_per_suite(Suite, _Config, Error, State) when not is_list(Error) ->
    Desc = fmt_stack(Suite, "", [], Error, "init_per_suite FAILED"),
    {Error, add_result(?INIT_PER_SUITE, failed, Desc, State)};
post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, add_result(?INIT_PER_SUITE, passed, <<"">>, State)}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite, Config, #state{} = State) ->
    initialize_stdout_capture(State),
    {Config, capture_starting_time(State, ?END_PER_SUITE)}.

%% @doc Called after end_per_suite.
post_end_per_suite(
    Suite,
    _Config,
    {skip, _Reason} = Error,
    State0
) ->
    Desc = fmt_stack(Suite, "", [], Error, "end_per_suite SKIPPED"),
    State1 = add_result(?END_PER_SUITE, skipped, Desc, State0),
    {Error, clear_suite(State1)};
post_end_per_suite(
    Suite,
    _Config,
    {fail, _Reason} = Error,
    State0
) ->
    Desc = fmt_stack(Suite, "", [], Error, "end_per_suite FAILED"),
    State1 = add_result(?END_PER_SUITE, failed, Desc, State0),
    {Error, clear_suite(State1)};
post_end_per_suite(
    Suite,
    _Config,
    {error, _Reason} = Error,
    State0
) ->
    Desc = fmt_stack(Suite, "", [], Error, "end_per_suite FAILED"),
    State1 = add_result(?END_PER_SUITE, failed, Desc, State0),
    {Error, clear_suite(State1)};
post_end_per_suite(_Suite, _Config, Return, State0) ->
    %% clean TC state
    State1 = add_result(?END_PER_SUITE, passed, <<"">>, State0),
    {Return, clear_suite(State1)}.

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

%% @doc Called before each init_per_group.
pre_init_per_group(
    _SuiteName, _Group, Config, State = #state{groups = [_ | Groups], previous_group_failed = true}
) ->
    initialize_stdout_capture(State),
    State1 = capture_starting_time(State, ?INIT_PER_GROUP),
    {Config, State1#state{groups = Groups, previous_group_failed = false}};
pre_init_per_group(_SuiteName, _Group, Config, #state{} = State) ->
    initialize_stdout_capture(State),
    {Config, capture_starting_time(State, ?INIT_PER_GROUP)}.

%% @doc Called after each init_per_group.
post_init_per_group(
    _SuiteName,
    Group,
    _Config,
    {skip, {failed, _Reason}} = Error,
    State0 = #state{suite = Suite, groups = Groups}
) ->
    State1 = State0#state{groups = [Group | Groups]},
    Desc = fmt_stack(Suite, "~s", [Group], Error, "init_per_group FAILED"),
    State2 = add_result(?INIT_PER_GROUP, failed, Desc, State1),
    {Error, fail_group(State2)};
post_init_per_group(
    _SuiteName,
    Group,
    _Config,
    {skip, _Reason} = Error,
    State0 = #state{suite = Suite, groups = Groups}
) ->
    State1 = State0#state{groups = [Group | Groups]},
    Desc = fmt_stack(Suite, "~s", [Group], Error, "init_per_group SKIPPED"),
    State2 = add_result(?INIT_PER_GROUP, skipped, Desc, State1),
    {Error, fail_group(State2)};
post_init_per_group(
    _SuiteName,
    Group,
    _Config,
    {fail, _Reason} = Error,
    State0 = #state{suite = Suite, groups = Groups}
) ->
    State1 = State0#state{groups = [Group | Groups]},
    Desc = fmt_stack(Suite, "~s", [Group], Error, "init_per_group FAILED"),
    State2 = add_result(?INIT_PER_GROUP, failed, Desc, State1),
    {Error, fail_group(State2)};
post_init_per_group(
    _SuiteName, Group, _Config, Error, State0 = #state{suite = Suite, groups = Groups}
) when
    not is_list(Error)
->
    State1 = State0#state{groups = [Group | Groups]},
    Desc = fmt_stack(Suite, "~s", [Group], Error, "init_per_group FAILED"),
    State2 = add_result(?INIT_PER_GROUP, failed, Desc, State1),
    {Error, fail_group(State2)};
post_init_per_group(_SuiteName, Group, _Config, Return, State0 = #state{groups = Groups}) ->
    State1 = State0#state{groups = [Group | Groups]},
    State2 = add_result(?INIT_PER_GROUP, passed, <<"">>, State1),
    {Return, ok_group(State2)}.

ok_group(State) ->
    State#state{previous_group_failed = false}.

fail_group(State) ->
    State#state{previous_group_failed = true}.

%% @doc Called after each end_per_group.
pre_end_per_group(_SuiteName, _Group, Config, #state{} = State) ->
    initialize_stdout_capture(State),
    {Config, capture_starting_time(State, ?END_PER_GROUP)}.

%% @doc Called after each end_per_group.
post_end_per_group(
    _SuiteName,
    Group,
    _Config,
    {skip, _Reason} = Error,
    #state{
        suite = Suite,
        groups = Groups
    } = State0
) ->
    Desc = fmt_stack(Suite, "~s", [Group], Error, "end_per_group SKIPPED"),
    State1 = add_result(?END_PER_GROUP, skipped, Desc, State0),
    {Error, State1#state{groups = tl(Groups)}};
post_end_per_group(
    _SuiteName,
    Group,
    _Config,
    {fail, _Reason} = Error,
    #state{
        suite = Suite,
        groups = Groups
    } = State0
) ->
    Desc = fmt_stack(Suite, "~s", [Group], Error, "end_per_group FAILED"),
    State1 = add_result(?END_PER_GROUP, failed, Desc, State0),
    {Error, State1#state{groups = tl(Groups)}};
post_end_per_group(
    _SuiteName,
    Group,
    _Config,
    {error, _Reason} = Error,
    #state{
        suite = Suite,
        groups = Groups
    } = State0
) ->
    Desc = fmt_stack(Suite, "~s", [Group], Error, "end_per_group FAILED"),
    State1 = add_result(?END_PER_GROUP, failed, Desc, State0),
    {Error, State1#state{groups = tl(Groups)}};
post_end_per_group(_SuiteName, _Group, _Config, Return, State0 = #state{groups = Groups}) ->
    State1 = add_result(?END_PER_GROUP, passed, <<"">>, State0),
    {Return, State1#state{groups = tl(Groups)}}.

%% @doc Called before each test case.
pre_init_per_testcase(_SuiteName, TestCase, Config, #state{} = State) ->
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
    {Config, State1}.

post_init_per_testcase(
    _SuiteName,
    TestCase,
    _Config,
    {skip, {failed, _Reason}} = Error,
    State = #state{suite = Suite}
) ->
    %% ct skip because of failed init is reported as error
    TC = io_lib:format("~p.[init_per_testcase]", [TestCase]),
    Desc = fmt_stack(Suite, "~s", [TC], Error, "init_per_testcase FAILED"),
    {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, failed, Desc, State)};
post_init_per_testcase(
    _SuiteName, TestCase, _Config, {skip, _Reason} = Error, State = #state{suite = Suite}
) ->
    %% other skips (user skip) are reported as skips
    TC = io_lib:format("~p.[init_per_testcase]", [TestCase]),
    Desc = fmt_stack(Suite, "~s", [TC], Error, "init_per_testcase SKIPPED"),
    {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, skipped, Desc, State)};
post_init_per_testcase(
    _SuiteName, TestCase, _Config, {fail, _Reason} = Error, State = #state{suite = Suite}
) ->
    %% fails are reported as errors
    TC = io_lib:format("~p.[init_per_testcase]", [TestCase]),
    Desc = fmt_stack(Suite, "~s", [TC], Error, "init_per_testcase FAILED"),
    {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, failed, Desc, State)};
post_init_per_testcase(_SuiteName, TestCase, _Config, Error, State = #state{suite = Suite}) when
    not is_list(Error) andalso ok =/= Error
->
    %% terms are reported as errors except ok (missing in CT doc)
    TC = io_lib:format("~p.[init_per_testcase]", [TestCase]),
    Desc = fmt_stack(Suite, "~s", [TC], Error, "init_per_testcase FAILED"),
    {Error, add_result({TestCase, ?INIT_PER_TESTCASE}, failed, Desc, State)};
post_init_per_testcase(_SuiteName, TestCase, _Config, Return, State) ->
    %% everything else is ok
    State1 = add_result({TestCase, ?INIT_PER_TESTCASE}, passed, <<"">>, State),
    {Return, State1}.

%% add test result to state
-spec add_result(method_id(), any(), string(), state()) -> state().
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
    NameMethod =
        case Method of
            {TestCase, Phase} -> io_lib:format("~s.~s", [TestCase, atom_to_list(Phase)]);
            NameMethod0 -> NameMethod0
        end,
    StdOut =
        case IoBuffer of
            undefined ->
                "";
            BufferPid ->
                {Io, Truncated} = io_buffer:flush(BufferPid),
                case Truncated of
                    true ->
                        StdOutLocation =
                            case os:getenv("SANDCASTLE") of
                                true ->
                                    "tab Diagnostics: Artifacts/ct_executor.stdout.txt";
                                _ ->
                                    filename:join(
                                        filename:dirname(OutputFile), "ct_executor.stdout.txt"
                                    )
                            end,
                        Io ++
                            io_lib:format(
                                "\n The std_out has been truncated, see ~s for the full suite std_out.",
                                [
                                    StdOutLocation
                                ]
                            );
                    false ->
                        Io
                end
        end,
    QualifiedName = qualifiedName(Groups, NameMethod),
    TS = second_timestamp(),
    Result0 = #{
        name => QualifiedName,
        outcome => Outcome,
        details => unicode:characters_to_list(Desc),
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
    NewTreeResults = register_result(TreeResults, Result, Groups, Method),
    State#state{starting_times = ST1, tree_results = NewTreeResults}.

pre_end_per_testcase(_SuiteName, TC, Config, #state{} = State) ->
    {Config, capture_starting_time(State, {TC, ?END_PER_TESTCASE})}.

%% @doc Called after each test case.
post_end_per_testcase(_SuiteName, TC, _Config, ok, State0) ->
    State1 = add_result({TC, ?END_PER_TESTCASE}, passed, <<"">>, State0),
    {ok, add_result({TC, ?MAIN_TESTCASE}, passed, <<"">>, State1)};
post_end_per_testcase(
    _SuiteName, TC, Config, Error, State0 = #state{suite = Suite, groups = Groups}
) ->
    NextState =
        case lists:keyfind(tc_status, 1, Config) of
            {tc_status, ok} ->
                %% Test case passed, but we still ended in an error
                %% same description as ct output
                %% first report testcase itself
                State1 = add_result({TC, ?MAIN_TESTCASE}, passed, <<"">>, State0),
                %% now report end per failure
                Desc = fmt_stack(
                    Suite,
                    "~s",
                    [format_path(TC, Groups)],
                    Error,
                    "end_per_testcase FAILED"
                ),
                add_result({TC, ?END_PER_TESTCASE}, failed, Desc, State1);
            _ ->
                %% Test case failed, in which case on_tc_fail already reports it
                add_result({TC, ?END_PER_TESTCASE}, passed, <<"">>, State0)
        end,
    {Error, NextState}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(_SuiteName, init_per_suite, _, State) ->
    State;
on_tc_fail(_SuiteName, end_per_suite, _, State) ->
    State;
on_tc_fail(_SuiteName, {init_per_group, _GroupName}, _, State) ->
    State;
on_tc_fail(_SuiteName, {end_per_group, _GroupName}, _, State) ->
    State;
on_tc_fail(_SuiteName, {TC, _Group}, Reason, State = #state{suite = Suite, groups = Groups}) ->
    Desc = fmt_fail(Suite, "~s", [format_path(TC, Groups)], Reason),
    add_result({TC, ?MAIN_TESTCASE}, failed, Desc, State);
on_tc_fail(_SuiteName, TC, Reason, State = #state{suite = Suite, groups = Groups}) ->
    Desc = fmt_fail(Suite, "~s", [format_path(TC, Groups)], Reason),
    add_result({TC, ?MAIN_TESTCASE}, failed, Desc, State).

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (>= 19.3)
on_tc_skip(_SuiteName, init_per_suite, _, State) ->
    State;
on_tc_skip(_SuiteName, end_per_suite, _, State) ->
    State;
on_tc_skip(_SuiteName, {init_per_group, _GroupName}, _, State) ->
    State;
on_tc_skip(_SuiteName, {end_per_group, _GroupName}, _, State) ->
    State;
on_tc_skip(_SuiteName, {TC, _Group}, Reason, State) ->
    handle_on_tc_skip(TC, Reason, State);
on_tc_skip(_SuiteName, TC, Reason, State) ->
    handle_on_tc_skip(TC, Reason, State).

handle_on_tc_skip(TC, {tc_auto_skip, Reason}, State = #state{suite = Suite, groups = Groups}) ->
    Desc = fmt_fail(Suite, "~s", [format_path(TC, Groups)], Reason),
    NewState = add_result({TC, ?MAIN_TESTCASE}, failed, Desc, State),
    NewState#state{suite = Suite};
handle_on_tc_skip(TC, {tc_user_skip, Reason}, State = #state{suite = Suite, groups = Groups}) ->
    Desc = fmt_skip(Suite, "~s", [format_path(TC, Groups)], Reason),
    NewState = add_result({TC, ?MAIN_TESTCASE}, skipped, Desc, State),
    NewState#state{suite = Suite}.

%% @doc Called when the scope of the CTH is done
-spec terminate(state()) -> ok | {error, _Reason}.
terminate(
    #state{output = Output, tree_results = TreeResults} =
        _State
) ->
    write_output(Output, term_to_binary(TreeResults)).

-spec write_output({file, string()} | stdout, string()) -> string().
write_output({file, FN}, JSON) ->
    io:format("Writing result file ~p", [FN]),
    ok = filelib:ensure_dir(FN),
    file:write_file(FN, JSON);
write_output(stdout, JSON) ->
    io:format(user, "~p", [JSON]).

-spec initialize_stdout_capture(#state{}) -> ok.
initialize_stdout_capture(#state{io_buffer = IoBuffer} = _State) ->
    case IoBuffer of
        undefined ->
            ok;
        Pid when erlang:is_pid(Pid) ->
            io_buffer:stop_capture(Pid),
            io_buffer:flush(Pid),
            io_buffer:start_capture(Pid)
    end.

-spec capture_starting_time(#state{}, method_id()) -> #state{}.
capture_starting_time(#state{starting_times = ST0} = State, MethodId) ->
    State#state{starting_times = ST0#{MethodId => second_timestamp()}}.

format_path(TC, Groups) ->
    lists:join([atom_to_list(P) || P <- lists:reverse([TC | Groups])], ".").
