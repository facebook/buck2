%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(cth_tpx_test_tree).
-compile([warn_missing_spec_all]).

-include("method_ids.hrl").

-export_type([
    tree/0,
    tree_node/0,
    test_leaf/0,

    name/0,

    method_result/0,
    case_result/0,
    outcome/0,

    group_path/0,
    method_id/0
]).

%% Public API
-export([
    qualified_name/2,

    new_node/1,
    new_leaf/1,

    register_result/4,
    get_result/2
]).

-import(common_util, [unicode_characters_to_list/1]).

-type tree() :: test_leaf() | tree_node().

-type option(Type) :: Type | none.

-type name() :: string() | atom().

-type tree_node() :: #{
    name := name(),
    type := node,
    init_method := option(method_result()),
    end_method := option(method_result()),
    test_cases := #{name() => test_leaf()},
    sub_groups := #{name() => tree_node()}
}.

-type test_leaf() :: #{
    name := name(),
    type := leaf,
    init_method := option(method_result()),
    end_method := option(method_result()),
    main_method := option(method_result())
}.

-type method_result() :: #{
    name := name(),
    startedTime => float(),
    endedTime => float(),
    outcome := outcome(),
    details := string(),
    std_out := string()
}.

-type outcome() ::
    passed | failed | timeout | skipped | omitted.

-type group_path() :: [atom()].

-type case_result() :: #{
    inits := [method_result()],
    main := method_result(),
    ends := [method_result()]
}.

-doc """
Gets the name for a testcase in a given group-path
The groups order expected here is [leaf_group, ...., root_group]
""".
-spec qualified_name(Groups, TestCase) -> string() when
    Groups :: group_path(),
    TestCase :: name().
qualified_name(Groups, TestCase) ->
    StringGroups = [atom_to_list(Group) || Group <- Groups],
    JoinedGroups = string:join(lists:reverse(StringGroups), ":"),
    Raw = io_lib:format("~ts.~ts", [JoinedGroups, TestCase]),
    unicode_characters_to_list(Raw).

%% Tree creation and update

-doc """
Creates a new node
""".
-spec new_node(Name :: name()) -> tree_node().
new_node(Name) ->
    #{
        name => Name,
        type => node,
        init_method => none,
        end_method => none,
        test_cases => #{},
        sub_groups => #{}
    }.

-doc """
Creates a new leaf
""".
-spec new_leaf(Name :: name()) -> test_leaf().
new_leaf(Name) ->
    #{
        name => Name,
        type => leaf,
        init_method => none,
        end_method => none,
        main_method => none
    }.

-doc """
Puts the test result inside the tree.
""".
-spec register_result(tree_node(), method_result(), group_path(), method_id()) -> tree_node().
register_result(TreeResult, Result, Groups, MethodId) ->
    insert_result(TreeResult, Result, lists:reverse(Groups), MethodId).

-doc """
Inserts the method_result inside the tree.
""".
-spec insert_result(Node, Result, ReversedPath, MethodId) -> Node when
    Node :: tree_node(),
    Result :: method_result(),
    ReversedPath :: group_path(),
    MethodId :: method_id().
insert_result(TreeNode, ResultTest, [Group | Groups], MethodId) ->
    #{sub_groups := Children} = TreeNode,
    GroupNode = maps:get(Group, Children, new_node(Group)),
    NewChildren = Children#{Group => insert_result(GroupNode, ResultTest, Groups, MethodId)},
    TreeNode#{sub_groups => NewChildren};
insert_result(TreeNode, ResultTest, [], MethodId) ->
    case MethodId of
        Init when Init =:= ?INIT_PER_SUITE; Init =:= ?INIT_PER_GROUP ->
            TreeNode#{init_method => ResultTest};
        End when End =:= ?END_PER_SUITE; End =:= ?END_PER_GROUP ->
            TreeNode#{end_method => ResultTest};
        {NameCase, Phase} ->
            #{test_cases := Cases} = TreeNode,
            TestLeaf = maps:get(NameCase, Cases, new_leaf(NameCase)),
            NewTestLeaf =
                case Phase of
                    ?INIT_PER_TESTCASE ->
                        TestLeaf#{init_method => ResultTest};
                    ?MAIN_TESTCASE ->
                        TestLeaf#{main_method => ResultTest};
                    ?END_PER_TESTCASE ->
                        TestLeaf#{end_method => ResultTest}
                end,
            TreeNode#{test_cases => Cases#{NameCase => NewTestLeaf}}
    end.

%% Collecting results

-doc """
Provides a result for the RequestedResults based on the collected results.
The format of the requested_results is a map from a list of groups to the list of test_cases that are sub-cases from the last group from the list.
""".
-spec get_result(TreeResult, RequestedResults) -> [case_result()] when
    TreeResult :: tree(),
    RequestedResults :: #{group_path() => [atom()]}.
get_result(TreeResult, RequestedResults) ->
    [
        collect_result(TreeResult, Groups, CaseRequest)
     || Groups := CasesRequests <- RequestedResults,
        CaseRequest <- CasesRequests
    ].

-doc """
Provides a result for a given specific requested_result.
""".
-spec collect_result(TreeResult, Groups, TestCase) -> case_result() when
    TreeResult :: tree(),
    Groups :: group_path(),
    TestCase :: atom().
collect_result(TreeResult, Groups, TestCase) ->
    QualifiedName = qualified_name(lists:reverse(Groups), TestCase),
    LeafResult = collect_result(TreeResult, [], [], Groups, TestCase, QualifiedName),
    #{ends := EndsResults, main := MainResult} = LeafResult,
    MainResultWithEndFailure = report_end_failure(EndsResults, MainResult),
    LeafResult#{main => MainResultWithEndFailure}.

-spec report_end_failure(MethodResults, ResultAcc) -> Result when
    MethodResults :: [method_result()],
    ResultAcc :: method_result(),
    Result :: method_result().
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
        unicode_characters_to_list([
            io_lib:format("~tp ~tp because ~tp failed with ~n", [TestName, MergedOutcome, EndName]), EndDetails
        ]),
    MergedDetails =
        case ResultOutcome of
            passed ->
                EndFailedDetails;
            _ ->
                unicode_characters_to_list(
                    lists:flatten(
                        io_lib:format("~ts~n~n~ts", [ResultDetails, EndFailedDetails])
                    )
                )
        end,
    report_end_failure(Rest, ResultAcc#{outcome => MergedOutcome, details => MergedDetails}).

-spec merge_outcome(outcome(), outcome()) -> outcome().
merge_outcome(failed, _) -> failed;
merge_outcome(_, failed) -> failed;
merge_outcome(timeout, _) -> timeout;
merge_outcome(_, timeout) -> timeout;
merge_outcome(skipped, _) -> skipped;
merge_outcome(_, skipped) -> skipped;
merge_outcome(passed, Other) -> Other.

-doc """
Collects all the inits / ends methods results linked to a requested_result.
""".
-spec collect_result(
    tree(),
    Inits :: [method_result()],
    Ends :: [method_result()],
    Groups :: group_path(),
    TestCase :: atom(),
    QualifiedName :: string()
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

-spec get_child(Node, Groups, TestCase) -> {tree(), group_path()} when
    Node :: tree(),
    Groups :: group_path(),
    TestCase :: atom().
get_child(#{sub_groups := SubGroups}, [Group | Groups], _TestCase) ->
    {maps:get(Group, SubGroups, new_node(Group)), Groups};
get_child(#{test_cases := TestCases}, [], TestCase) ->
    {maps:get(TestCase, TestCases, new_leaf(TestCase)), []}.

-doc """
Collect the results from init_testcase, end_testcase and the main testcase for a given requested result.
Proceeds with some additional logic if the result is missing or skipped.
""".
-spec collect_node(
    tree(),
    Inits :: [method_result()],
    Ends :: [method_result()],
    QualName :: string()
) ->
    {NewInits :: [method_result()], MainResult :: option(method_result()), NewEnds :: [method_result()]}.
collect_node(
    #{type := leaf} = TestLeaf,
    Inits,
    Ends,
    QualifiedName
) ->
    #{init_method := OptMethodInit, end_method := OptMethodEnd, main_method := OptMethodMain} = TestLeaf,
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
        case {InitsPassed, OptMethodMain} of
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
    #{type := node} = TreeNode,
    Inits,
    Ends,
    _QualifiedName
) ->
    #{init_method := OptMethodInit, end_method := OptMethodEnd} = TreeNode,
    {NewInits, NewEnds} = update_inits_ends(Inits, Ends, OptMethodInit, OptMethodEnd),
    {NewInits, none, NewEnds}.

-spec update_inits_ends(
    Inits :: [method_result()],
    Ends :: [method_result()],
    MethInit :: option(method_result()),
    MethEnd :: option(method_result())
) -> {NewInits :: [method_result()], NewEnds :: [method_result()]}.
update_inits_ends(Inits, Ends, OptMethodInit, OptMethodEnd) ->
    {adds_if_present(OptMethodInit, Inits), adds_if_present(OptMethodEnd, Ends)}.

-spec adds_if_present(option(X), list(X)) -> list(X).
adds_if_present(Optional, List) ->
    case Optional of
        none -> List;
        Obj -> [Obj | List]
    end.

%% Merge the StdOut from the init_per_testcase, main_testcase, and end_per_testcase
-spec merge_std_out(test_leaf()) -> string().
merge_std_out(#{type := leaf} = TestLeaf) ->
    #{init_method := OptMethodInit, main_method := OptMainMethod, end_method := OptMethodEnd} = TestLeaf,
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
    unicode_characters_to_list([InitStdOut, MainStdOut, EndStdOut]).

-doc """
Creates a method_result for a requested method for which no result was registered.
Attempts to locate if one of the inits is responsible for the missing result.
""".
-spec get_missing_result(Inits :: [method_result()], QualifiedName :: string()) -> method_result().
get_missing_result(Inits, QualifiedName) ->
    MainResult =
        #{
            name => unicode_characters_to_list(
                io_lib:format("~ts.[main_testcase]", [QualifiedName])
            ),
            outcome => failed,
            details => "no results for this test were recorded",
            std_out => ""
        },
    handle_missing_results(Inits, MainResult).

-doc """
Generates an user informative message in the case of the missing result by attempting to find the right init to blame.
""".
-spec handle_missing_results(Inits :: [method_result()], method_result()) -> method_result().
handle_missing_results([], MainResult) ->
    MainResult;
handle_missing_results([Init | Inits], MainResult) ->
    InitStdOut = unicode_characters_to_list([
        name_to_string(maps:get(name, Init)), " stdout: ", maps:get(std_out, Init)
    ]),
    case maps:get(outcome, Init) of
        failed ->
            MainResult#{
                details =>
                    unicode_characters_to_list(
                        io_lib:format(
                            "no results for this test were recorded because init ~ts failed with error message : \n ~ts",
                            [maps:get(name, Init), maps:get(details, Init)]
                        )
                    ),
                std_out => InitStdOut
            };
        timeout ->
            MainResult#{
                details => unicode_characters_to_list(
                    io_lib:format(
                        "no results for this test were recorded because init ~ts timed-out with error message : \n ~ts",
                        [maps:get(name, Init), maps:get(details, Init)]
                    )
                ),
                std_out => InitStdOut
            };
        skipped ->
            handle_skipped_result([Init | Inits], MainResult);
        omitted ->
            MainResult#{
                details => unicode_characters_to_list(
                    io_lib:format(
                        "no results for this test were recorded because init ~ts was omitted with message : \n ~ts",
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
%% In the case where it is skipped because of init failure, it is reported as failed with appropriate user message reporting
%% to the init to be blamed.
-spec handle_skipped_result(Inits :: [method_result()], MainResult :: method_result()) -> method_result().
handle_skipped_result([], MainResult) ->
    MainResult;
handle_skipped_result([Init | Inits], MainResult) ->
    InitStdOut = unicode_characters_to_list([
        name_to_string(maps:get(name, Init)), " stdout: ", maps:get(std_out, Init)
    ]),
    case maps:get(outcome, Init) of
        failed ->
            MainResult#{
                outcome => failed,
                details =>
                    unicode_characters_to_list(
                        io_lib:format(
                            "Failed because init ~ts failed, with error message : \n ~ts",
                            [maps:get(name, Init), maps:get(details, Init)]
                        )
                    ),
                std_out => InitStdOut
            };
        timeout ->
            MainResult#{
                outcome => timeout,
                details =>
                    unicode_characters_to_list(
                        io_lib:format(
                            "Timed-out because init ~ts timed-out, with error message : \n ~ts",
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
                    unicode_characters_to_list(
                        io_lib:format(
                            "Failed because init ~ts was omitted, with error message : \n ~ts",
                            [maps:get(name, Init), maps:get(details, Init)]
                        )
                    ),
                std_out => InitStdOut
            }
    end.

-spec name_to_string(Name) -> string() when
    Name :: name().
name_to_string(Name) when is_atom(Name) ->
    atom_to_list(Name);
name_to_string(Name) when is_list(Name) ->
    Name.
