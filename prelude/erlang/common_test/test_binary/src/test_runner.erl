%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(test_runner).
-compile(warn_missing_spec_all).

-include_lib("common/include/tpx_records.hrl").
-include_lib("common/include/buck_ct_records.hrl").
-include_lib("kernel/include/logger.hrl").

-export([run_tests/5, mark_success/1, mark_failure/1]).

-export([parse_test_name/2]).

-import(common_util, [unicode_characters_to_list/1, unicode_characters_to_binary/1]).

-define(DEFAULT_OUTPUT_FORMAT, json).

-spec run_tests(Tests, TestInfo, OutputDir, Listing, Timeout) -> ok when
    Tests :: [string()],
    TestInfo :: #test_info{},
    OutputDir :: file:filename_all(),
    Listing :: #test_spec_test_case{},
    Timeout :: timeout().
run_tests(Tests, #test_info{} = TestInfo, OutputDir, Listing, Timeout) ->
    check_ct_opts(TestInfo#test_info.ct_opts),
    Suite =
        case filename:basename(TestInfo#test_info.test_suite, ".beam") of
            SuiteBin when is_binary(SuiteBin) -> binary_to_atom(SuiteBin);
            SuiteStr when is_list(SuiteStr) -> list_to_atom(SuiteStr)
        end,
    StructuredTests = [parse_test_name(Test, Suite) || Test <- Tests],
    case StructuredTests of
        [] ->
            throw(no_tests_to_run);
        [_ | _] ->
            TestSpecFile = filename:join(OutputDir, "test_spec.spec"),
            OrderedTests = reorder_tests(StructuredTests, Listing),
            execute_test_suite(
                #test_env{
                    output_format = ?DEFAULT_OUTPUT_FORMAT,
                    suite = Suite,
                    tests = OrderedTests,
                    suite_path = TestInfo#test_info.test_suite,
                    output_dir = OutputDir,
                    dependencies = TestInfo#test_info.dependencies,
                    test_spec_file = TestSpecFile,
                    config_files = TestInfo#test_info.config_files,
                    providers = TestInfo#test_info.providers,
                    ct_opts = TestInfo#test_info.ct_opts,
                    common_app_env = TestInfo#test_info.common_app_env,
                    erl_cmd = TestInfo#test_info.erl_cmd,
                    extra_flags = TestInfo#test_info.extra_flags,
                    artifact_annotation_mfa = TestInfo#test_info.artifact_annotation_mfa,
                    raw_target = TestInfo#test_info.raw_target,
                    trampolines = TestInfo#test_info.trampolines,
                    timeout = Timeout
                }
            )
    end.

-doc """
Prepare the test spec and run the test.
""".
-spec execute_test_suite(TestEnv) -> ok when
    TestEnv :: #test_env{}.
execute_test_suite(TestEnv) ->
    #test_env{
        suite = Suite,
        tests = Tests,
        suite_path = SuitePath,
        output_dir = OutputDir,
        test_spec_file = TestSpecFile,
        ct_opts = CtOpts,
        timeout = Timeout
    } = TestEnv,
    TestSpec = build_test_spec(
        Suite, Tests, filename:absname(filename:dirname(SuitePath)), OutputDir, CtOpts
    ),
    FormattedSpec = [io_lib:format("~tp.~n", [Entry]) || Entry <- TestSpec],
    file:write_file(TestSpecFile, FormattedSpec, [raw, binary]),
    NewTestEnv = TestEnv#test_env{test_spec_file = TestSpecFile, ct_opts = CtOpts},
    try run_test(NewTestEnv, Timeout) of
        ok -> ok
    catch
        Class:Reason:StackTrace ->
            ErrorMsg = io_lib:format("run_test failed due to ~ts\n", [
                erl_error:format_exception(Class, Reason, StackTrace)
            ]),
            ?LOG_ERROR(ErrorMsg),
            test_run_fail(
                NewTestEnv, ErrorMsg
            )
    end.

-spec run_test(TestEnv, Timeout) -> ok when
    TestEnv :: #test_env{},
    Timeout :: timeout().
run_test(TestEnv, Timeout) ->
    register(?MODULE, self()),
    application:set_env(test_exec, test_env, TestEnv, [{persistent, true}]),
    case application:ensure_all_started(test_exec, temporary) of
        {ok, _Apps} ->
            Ref = erlang:monitor(process, test_exec_sup, []),
            receive
                {'DOWN', Ref, _Type, Object, Info} ->
                    test_run_fail(
                        TestEnv,
                        io_lib:format(
                            "unexpected exception in the buck2 Common Test runner:\n"
                            "                        application test_exec crashed (~tp ~tp) ~n",
                            [Object, Info]
                        )
                    );
                {run_succeed, Result} ->
                    ensure_test_exec_stopped(),
                    test_run_succeed(TestEnv, Result);
                {run_failed, Result} ->
                    ensure_test_exec_stopped(),
                    test_run_fail(TestEnv, Result)
            after Timeout ->
                ensure_test_exec_stopped(),
                ErrorMsg =
                    "\n***************************************************************\n"
                    "* the suite timed out, all tests will be reported as failure. *\n"
                    "***************************************************************\n",
                test_run_timeout(TestEnv, ErrorMsg)
            end;
        {error, Reason} ->
            ErrorMsg = io_lib:format("TextExec failed to start due to ~tp", [Reason]),

            ?LOG_ERROR(ErrorMsg),
            test_run_fail(
                TestEnv, ErrorMsg
            )
    end.

-spec ensure_test_exec_stopped() -> ok.
ensure_test_exec_stopped() ->
    {Pid, Monitor} = erlang:spawn_monitor(fun() -> application:stop(test_exec) end),
    receive
        {'DOWN', Monitor, process, Pid, _} -> ok
    after 5000 -> ok
    end.

-doc """
Provides result as specified by the tpx protocol when test failed to ran.
""".
-spec test_run_fail(#test_env{}, unicode:chardata()) -> ok.
test_run_fail(#test_env{} = TestEnv, Reason) ->
    provide_output_file(
        TestEnv,
        io_lib:format("Test failed to ran due to ~ts", [Reason]),
        failed
    ).

-spec test_run_timeout(#test_env{}, string()) -> ok.
test_run_timeout(#test_env{} = TestEnv, Reason) ->
    provide_output_file(
        TestEnv, Reason, timeout
    ).

-doc """
Provides result as specified by the tpx protocol when test succeed to ran.
""".
-spec test_run_succeed(#test_env{}, string()) -> ok.
test_run_succeed(#test_env{} = TestEnv, Reason) ->
    provide_output_file(TestEnv, Reason, passed).

-doc """
Provides result as specified by the tpx protocol.
""".
-spec provide_output_file(#test_env{}, unicode:chardata(), failed | passed | timeout) -> ok.
provide_output_file(
    #test_env{
        output_dir = OutputDir,
        tests = Tests,
        suite = Suite,
        artifact_annotation_mfa = ArtifactAnnotationFunction
    },
    ResultExec,
    Status
) ->
    LogFile = test_logger:get_log_file(OutputDir, ct_executor),
    Log = trimmed_content_file(LogFile),
    StdOutFile = test_logger:get_std_out(OutputDir, ct_executor),
    StdOut = trimmed_content_file(StdOutFile),
    OutLog = io_lib:format("ct_executor_log: ~ts ~nct_executor_stdout: ~ts", [Log, StdOut]),
    ResultsFile = filename:join(OutputDir, "result.json"),
    Results =
        case Status of
            failed ->
                collect_results_broken_run(
                    Tests, Suite, "internal crash", ResultExec, OutLog
                );
            timeout ->
                collect_results_broken_run(Tests, Suite, "", ResultExec, StdOut);
            passed ->
                % Here we either passed or timeout.
                case file:read_file(ResultsFile, [raw]) of
                    {ok, JsonFile} ->
                        TreeResults = decode_erlang_term(JsonFile),
                        case TreeResults of
                            undefined ->
                                ErrorMsg =
                                    io_lib:format(
                                        "ct failed to produced results valid file ~tp", [
                                            ResultsFile
                                        ]
                                    ),
                                collect_results_broken_run(
                                    Tests, Suite, ErrorMsg, ResultExec, OutLog
                                );
                            _ ->
                                collect_results_fine_run(TreeResults, Tests)
                        end;
                    {error, _Reason} ->
                        ErrorMsg = io_lib:format("ct failed to produced results file ~tp", [
                            ResultsFile
                        ]),
                        collect_results_broken_run(Tests, Suite, ErrorMsg, ResultExec, OutLog)
                end
        end,
    {ok, _ResultOuptuFile} = json_interfacer:write_json_output(OutputDir, Results),
    test_artifact_directory:link_to_artifact_dir(
        test_logger:get_std_out(OutputDir, ct_executor), OutputDir, ArtifactAnnotationFunction
    ),
    test_artifact_directory:link_to_artifact_dir(
        test_logger:get_std_out(OutputDir, test_runner), OutputDir, ArtifactAnnotationFunction
    ),
    test_artifact_directory:prepare(OutputDir, ArtifactAnnotationFunction).

-spec decode_erlang_term(Bin :: binary()) -> dynamic().
decode_erlang_term(Bin) ->
    binary_to_term(Bin).

-spec trimmed_content_file(File) -> unicode:chardata() when
    File :: file:filename_all().
trimmed_content_file(File) ->
    case file:open(File, [read, binary]) of
        {error, Reason} ->
            io_lib:format("No ~tp file found, reason ~tp ", [filename:basename(File), Reason]);
        {ok, IoDevice} ->
            try
                case file:pread(IoDevice, {eof, -5000}, 5000) of
                    {error, _} ->
                        case file:pread(IoDevice, bof, 5000) of
                            {ok, Data} -> Data;
                            eof -> io_lib:format("nothing to read from ~ts", [File])
                        end;
                    {ok, EndOfFile} ->
                        io_lib:format("~ts~nFile truncated, see ~tp for full output", [
                            EndOfFile,
                            filename:basename(File)
                        ])
                end
            of
                Content -> Content
            after
                file:close(IoDevice)
            end
    end.

-doc """
Provide tpx with a result when CT failed to provide results for tests.
""".
-spec collect_results_broken_run([#ct_test{}], atom(), unicode:chardata(), term(), unicode:chardata()) ->
    [cth_tpx_test_tree:case_result()].
collect_results_broken_run(Tests, _Suite, ErrorMsg, ResultExec, StdOut) ->
    FormattedErrorMsg = io_lib:format("~ts~n", [ErrorMsg]),
    [
        #{
            ends => [],
            inits => [],
            main => #{
                name => lists:flatten(
                    io_lib:format("~ts.[main_testcase]", [
                        % We need to reverse the list of groups as the method cth_tpx_test_tree:qualified_name expects them
                        % in the reverse order (as it is designed to be called when exploring the tree of results
                        % where we push at each time the group we are in, leading to them being in reverse order).
                        cth_tpx_test_tree:qualified_name(
                            lists:reverse(Test#ct_test.groups),
                            Test#ct_test.test_name
                        )
                    ])
                ),
                details =>
                    unicode_characters_to_list(
                        io_lib:format(
                            "~ts~ts ~n",
                            [FormattedErrorMsg, ResultExec]
                        )
                    ),
                startedTime => 0.0,
                endedTime => 0.0,
                outcome => failed,
                std_out => unicode_characters_to_list(StdOut)
            }
        }
     || Test <- Tests
    ].

-doc """
Provide the results from the tests as specified by tpx protocol, from the json file
provided by ct displaying results of all the tests ran.
""".
-spec collect_results_fine_run(cth_tpx_test_tree:tree_node(), [#ct_test{}]) -> [cth_tpx_test_tree:case_result()].
collect_results_fine_run(TreeResults, Tests) ->
    cth_tpx_test_tree:get_result(TreeResults, maps:from_list(get_requested_tests(Tests))).

-doc """
Returns a list of the tests by classifying from the (sequence) of groups they belong.
The list is [{[sequence of groups] => [list of tests belonging to this sequence]}].
We make sure to respect the group / test insertion order. That is, if the sequence is
g1.t1, g2.t2, g1.t2, g1.t3, g2.t2, we produce:
[g1.[t1,t2,t3], g2.[t1,t2]]
""".
-spec get_requested_tests([#ct_test{}]) -> [{[atom()], [atom()]}].
get_requested_tests(Tests) ->
    lists:foldl(
        fun(Test, List) ->
            Groups = Test#ct_test.groups,
            add_or_append(List, {Groups, Test#ct_test.test_name})
        end,
        [],
        Tests
    ).

-spec add_or_append(list({K, list(V)}), {K, V}) -> list({K, list(V)}).
add_or_append(List, {Key, Value}) ->
    List0 = lists:map(
        fun
            ({Key0, Value0}) when Key0 =:= Key -> {Key0, lists:append(Value0, [Value])};
            (Other) -> Other
        end,
        List
    ),
    case List0 =:= List of
        true -> lists:append(List0, [{Key, [Value]}]);
        false -> List0
    end.

-doc """
Built the test_spec selecting the requested tests and
specifying the result output.
""".
-spec build_test_spec(Suite, Tests, TestDir, OutputDir, CtOpts) -> [term()] when
    Suite :: module(),
    Tests :: [#ct_test{}],
    TestDir :: file:filename_all(),
    OutputDir :: file:filename_all(),
    CtOpts :: [term()].
build_test_spec(Suite, Tests, TestDir0, OutputDir, CtOpts) ->
    TestDir = unicode_characters_to_list(TestDir0),
    ListGroupTest = get_requested_tests(Tests),
    SpecTests = lists:map(
        fun
            ({[], TopTests}) ->
                {cases, TestDir, Suite, TopTests};
            ({Groups, GroupTests}) ->
                GroupPath = [[{Group, []} || Group <- Groups]],
                {groups, TestDir, Suite, GroupPath, {cases, GroupTests}}
        end,
        ListGroupTest
    ),
    ResultOutput = filename:join(OutputDir, "result.json"),
    {TpxCtHook, CtOpts1} = getCtHook(CtOpts, ResultOutput),
    LogDir = set_up_log_dir(OutputDir),
    CtOpts2 = add_spec_if_absent(
        {auto_compile, false}, add_spec_if_absent({logdir, LogDir}, CtOpts1)
    ),
    SpecTests ++ [TpxCtHook] ++ CtOpts2.

-doc """
Create a ct_hook for the test spec by plugging together
""".
-spec getCtHook([term()], string()) -> {term(), [term()]}.
getCtHook(CtOpts, ResultOutput) ->
    {NewOpts, Hooks} = addOptsHook(CtOpts, []),
    CthTpxHooks = [
        {cth_tpx, #{role => top, result_json => ResultOutput}},
        {cth_tpx, #{role => bot}}
    ],
    CtHookHandle = {ct_hooks, CthTpxHooks ++ lists:reverse(Hooks)},
    {CtHookHandle, NewOpts}.

-spec addOptsHook([term()], [term()]) -> {[term()], [term()]}.
addOptsHook(CtOpts, Hooks) ->
    case lists:keyfind(ct_hooks, 1, CtOpts) of
        false ->
            {CtOpts, Hooks};
        {ct_hooks, NewHooks} when is_list(NewHooks) ->
            addOptsHook(lists:keydelete(ct_hooks, 1, CtOpts), NewHooks ++ Hooks)
    end.

-doc """
Add a spec tuple to the list of ct_options if a tuple defining the property isn't present yet.
""".
-spec add_spec_if_absent({atom(), term()}, [term()]) -> [term()].
add_spec_if_absent({Key, Value}, CtOpts) ->
    case lists:keyfind(Key, 1, CtOpts) of
        false -> [{Key, Value} | CtOpts];
        _ -> CtOpts
    end.

-doc """
Parse the test name, and decompose it into the test, group and suite atoms
""".
-spec parse_test_name(string(), atom()) -> #ct_test{}.
parse_test_name(Test, Suite) ->
    [Groups0, TestName] = string:split(Test, ".", all),
    Groups1 =
        case Groups0 of
            [] -> [];
            _ -> string:split(Groups0, ":", all)
        end,
    Groups = lists:map(fun(GroupStr) -> list_to_atom(GroupStr) end, Groups1),
    #ct_test{
        suite = Suite,
        groups = Groups,
        test_name = list_to_atom(TestName),
        canonical_name = Test
    }.

-spec reorder_tests(list(#ct_test{}), #test_spec_test_case{}) -> list(#ct_test{}).
reorder_tests(Tests, #test_spec_test_case{testcases = TestCases}) ->
    % This is the ordered lists of test from the suite as
    % binary strings.
    MapNameToTests = lists:foldl(
        fun(#ct_test{canonical_name = Name} = Test, Map) -> Map#{unicode_characters_to_binary(Name) => Test} end,
        maps:new(),
        Tests
    ),
    lists:foldr(
        fun(#test_spec_test_info{name = TestName}, ListOrdered) ->
            case MapNameToTests of
                #{TestName := Test} -> [Test | ListOrdered];
                _ -> ListOrdered
            end
        end,
        [],
        TestCases
    ).

-doc """
LogDir is the directory where ct will log to.
Make sure it exists and returns it.
""".
-spec set_up_log_dir(file:filename_all()) -> file:filename_all().
set_up_log_dir(OutputDir) ->
    LogDir = filename:join(OutputDir, "log_dir"),
    ok = filelib:ensure_path(LogDir),
    LogDir.

-doc """
Informs the test runner of a successful test run.
""".
-spec mark_success(unicode:chardata()) -> ok.
mark_success(Result) ->
    ?MODULE ! {run_succeed, Result},
    ok.

-doc """
Informs the test runner of a fataled test run.
""".
-spec mark_failure(unicode:chardata()) -> ok.
mark_failure(Error) ->
    ?MODULE ! {run_failed, Error},
    ok.

-doc """
CtOpts must be tuple as defined here:
https://www.erlang.org/doc/apps/common_test/run_test_chapter.html#test-specification-syntax
that will be inserted to the test specification.
We do not check here that those are valid, but that they do not conflict with those
created here by the runner.
""".
-spec check_ct_opts([term()]) -> ok.
check_ct_opts(CtOpts) ->
    ProblematicsOpts = [suites, groups, cases, skip_suites, skip_groups, skip_cases],
    lists:foreach(
        fun(Opt) ->
            case lists:keyfind(Opt, 1, CtOpts) of
                false ->
                    ok;
                _ ->
                    ?LOG_ERROR("Option ~tp is not supported by test runner", [Opt]),
                    throw({non_valid_ct_opt, Opt})
            end
        end,
        ProblematicsOpts
    ).
