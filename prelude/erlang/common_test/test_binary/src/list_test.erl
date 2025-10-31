%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(list_test).
-compile(warn_missing_spec_all).

-include_lib("common/include/tpx_records.hrl").

-export([
    list_tests/2,
    list_test_spec/1, list_test_spec/2
]).

-import(common_util, [unicode_characters_to_binary/1]).

%% Fallback oncall
-define(FALLBACK_ONCALL, <<"fallback_oncall">>).

-type ct_groupname() :: ct_suite:ct_groupname().
-type ct_testname() :: ct_suite:ct_testname().
-type suite() :: module().

%% coming from the output of the group/0 method.
%% See https://www.erlang.org/doc/man/ct_suite.html#Module:groups-0 for the upstream type.
-type groups_output() :: [ct_group_def()].
-type ct_group_def() ::
    {ct_groupname(), ct_group_props(), [ct_group_content()]}
    | {ct_groupname(), [ct_group_content()]}.
-type ct_group_content() :: ct_testname() | ct_group_def() | {group, ct_groupname()} | ct_testcase_ref().

%% coming from the output of the all/0 method.
%% See https://www.erlang.org/doc/man/ct_suite.html#Module:all-0 for the upstream type.
-type all_output() :: [ct_test_def()].
-type ct_test_def() :: ct_testname() | ct_group_ref() | ct_testcase_ref().
-type ct_testcase_ref() :: {testcase, ct_testname(), ct_testcase_repeat_prop()}.
-type ct_testcase_repeat_prop() :: [
    {repeat, ct_test_repeat()}
    | {repeat_until_ok, ct_test_repeat()}
    | {repeat_until_fail, ct_test_repeat()}
].
-type ct_group_ref() ::
    {group, ct_groupname()}
    | {group, ct_groupname(), ct_group_props_ref()}
    | {group, ct_groupname(), ct_group_props_ref(), ct_subgroups_def()}.
-type ct_group_props_ref() :: ct_group_props() | default.
-type ct_group_props() ::
    [
        parallel
        | sequence
        | shuffle
        | {shuffle, Seed :: {integer(), integer(), integer()}}
        | {ct_group_repeat_type(), ct_test_repeat()}
    ].
-type ct_group_repeat_type() ::
    repeat
    | repeat_until_all_ok
    | repeat_until_all_fail
    | repeat_until_any_ok
    | repeat_until_any_fail.
-type ct_test_repeat() :: integer() | forever.
-type ct_subgroups_def() ::
    {ct_groupname(), ct_group_props_ref()}
    | {ct_groupname(), ct_group_props_ref(), ct_subgroups_def()}.

%% ------ Public Function --------

-doc """
Outputs a string representation
of the tests in the suite, as a XML
as defined by tpx-buck2 specifications
(see https://www.internalfb.com/code/fbsource/fbcode/buck2/docs/test_execution.md#test-spec-integration-with-tpx)
""".
-spec list_tests(suite(), [module()]) -> #test_spec_test_case{}.
list_tests(Suite, Hooks) ->
    TestNames = list_test_spec(Suite, Hooks),
    throw_if_duplicate(TestNames),
    listing_interfacer:test_case_constructor(Suite, TestNames).

%% -------------- Internal functions ----------------
%%
%%
-spec throw_if_duplicate(list(binary())) -> ok.
throw_if_duplicate(TestNames) ->
    throw_if_duplicate(sets:new([{version, 2}]), TestNames).

-spec throw_if_duplicate(sets:set(binary()), list(binary())) -> ok.
throw_if_duplicate(_, []) ->
    ok;
throw_if_duplicate(TestNameSet, [TestName | Tail]) ->
    case sets:is_element(TestName, TestNameSet) of
        true ->
            throw({found_duplicate_test, TestName});
        false ->
            throw_if_duplicate(sets:add_element(TestName, TestNameSet), Tail)
    end.

-doc """
Test that all the tests in the list are exported.
""".
-spec test_exported_test(suite(), ct_testname()) -> error | ok.
test_exported_test(Suite, Test) ->
    case erlang:function_exported(Suite, Test, 1) of
        false ->
            case erlang:function_exported(Suite, '$handle_undefined_function', 2) of
                true ->
                    ok;
                false ->
                    error(
                        {invalid_test,
                            io_lib:format(
                                "The test ~ts has been discovered while recursively exploring all/0, " ++
                                    "groups/0 but is not an exported method of arity 1",
                                [Test]
                            )}
                    )
            end;
        true ->
            ok
    end.

-spec load_hooks([module()]) -> ok.
load_hooks(Hooks) ->
    ok = code:ensure_modules_loaded(Hooks).

%% We extract the call to the groups() method so that we can type it.
-spec suite_groups(suite(), [module()]) -> groups_output().
suite_groups(Suite, Hooks) ->
    GroupDef =
        case erlang:function_exported(Suite, groups, 0) of
            true -> Suite:groups();
            false -> []
        end,
    lists:foldl(
        fun(Hook, CurrGroupDef) ->
            case erlang:function_exported(Hook, post_groups, 2) of
                true ->
                    Hook:post_groups(Suite, CurrGroupDef);
                false ->
                    CurrGroupDef
            end
        end,
        GroupDef,
        Hooks
    ).

-spec suite_all(suite(), [module()], groups_output()) -> all_output().
suite_all(Suite, Hooks, GroupsDef) ->
    TestsDef = Suite:all(),
    lists:foldl(
        fun(Hook, CurrTestsDef) ->
            case erlang:function_exported(Hook, post_all, 3) of
                true ->
                    Hook:post_all(Suite, CurrTestsDef, GroupsDef);
                false ->
                    CurrTestsDef
            end
        end,
        TestsDef,
        Hooks
    ).

-spec list_test([ct_test_def() | ct_group_content()], [ct_groupname()], groups_output(), suite()) -> [binary()].
list_test(Node, Groups, SuiteGroups, Suite) ->
    lists:foldl(
        fun
            (Test, ListTestsAcc) when is_atom(Test) ->
                [test_format(Suite, Groups, Test) | ListTestsAcc];
            ({testcase, Test}, ListTestsAcc) when is_atom(Test) ->
                [test_format(Suite, Groups, Test) | ListTestsAcc];
            ({testcase, TestName, _Properties}, ListTestsAcc) when is_atom(TestName) ->
                [test_format(Suite, Groups, TestName) | ListTestsAcc];
            (Group, ListTestsAcc) ->
                lists:append(list_group(Group, Groups, SuiteGroups, Suite), ListTestsAcc)
        end,
        [],
        Node
    ).

%% case where the format of the group is {group, GroupName}, then we need to
%% look for the specifications of the group from the groups() method.
-spec list_group(ct_group_ref() | ct_group_def(), [ct_groupname()], groups_output(), suite()) -> [binary()].
list_group({group, Group}, Groups, SuiteGroups, Suite) when is_atom(Group) ->
    list_sub_group(Group, Groups, SuiteGroups, Suite);
%% case {group, GroupName, Properties}, similar as above
list_group({group, Group, _}, Groups, SuiteGroups, Suite) when is_atom(Group) ->
    list_sub_group(Group, Groups, SuiteGroups, Suite);
%% case {group, GroupName, Properties, SubGroupProperties},
%% similar_as_above.
list_group({group, Group, _, _}, Groups, SuiteGroups, Suite) ->
    list_sub_group(Group, Groups, SuiteGroups, Suite);
list_group(GroupDef, Groups, SuiteGroups, Suite) ->
    list_group_def(GroupDef, Groups, SuiteGroups, Suite).

-spec list_group_def(ct_group_def(), [ct_groupname()], groups_output(), suite()) -> [binary()].
%% case {GroupName, SubGroupTests}, then we need to look for the specification of the group
%% from the groups() method as above
list_group_def({Group, SubGroupTests}, Groups, SuiteGroups, Suite) ->
    Groups1 = lists:append(Groups, [Group]),
    list_test(SubGroupTests, Groups1, SuiteGroups, Suite);
%% case {GroupName, Properties, SubGroupsAndTests},
%% then in this case we explore the SubGroupsAndTests
list_group_def({Group, _, SubGroupTests}, Groups, SuiteGroups, Suite) ->
    Groups1 = lists:append(Groups, [Group]),
    list_test(SubGroupTests, Groups1, SuiteGroups, Suite).

-doc """
Makes use of the output from the groups/0 method to get the tests and subgroups
of the group name given as input
""".
-spec list_sub_group(ct_groupname(), [ct_groupname()], groups_output(), suite()) -> [binary()].
list_sub_group(Group, Groups, SuiteGroups, Suite) when is_list(SuiteGroups) ->
    TestsAndGroups =
        case lists:keyfind(Group, 1, SuiteGroups) of
            {Group, TestsDef} when is_list(TestsDef) -> TestsDef;
            {Group, _, TestsDef} when is_list(TestsDef) -> TestsDef;
            false -> error({invalid_group, Suite, Group});
            GroupSpec -> error({bad_group_spec, GroupSpec})
        end,
    Groups1 = lists:append(Groups, [Group]),
    list_test(TestsAndGroups, Groups1, SuiteGroups, Suite).

-doc """
Given a test that belongs to a common test suite,
prints it as follows:
name_of_suite.group1:group2:...:groupn.test_name
""".
-spec test_format(suite(), [ct_groupname()], ct_testname()) -> binary().
test_format(Suite, Groups, Test) ->
    ok = test_exported_test(Suite, Test),
    ListPeriodGroups = lists:join(":", lists:map(fun(Group) -> atom_to_list(Group) end, Groups)),
    GroupString = lists:foldl(
        fun(Element, Acc) -> string:concat(Acc, Element) end,
        "",
        ListPeriodGroups
    ),
    unicode_characters_to_binary(io_lib:format("~ts.~ts", [GroupString, Test])).

-spec list_test_spec(suite()) -> [binary()].
list_test_spec(Suite) ->
    list_test_spec(Suite, []).

-doc """
Creates a Xml representation of all the group / tests
of the suite by exploring the suite
""".
-spec list_test_spec(suite(), [module()]) -> [binary()].
list_test_spec(Suite, Hooks) ->
    ok = load_hooks(Hooks),
    _Contacts = get_contacts(Suite),
    GroupsDef = suite_groups(Suite, Hooks),
    AllResult = suite_all(Suite, Hooks, GroupsDef),
    lists:reverse(list_test(AllResult, [], GroupsDef, Suite)).

-spec get_contacts(suite()) -> [binary()].
get_contacts(Suite) ->
    try
        SuiteSource = proplists:get_value(source, Suite:module_info(compile)),
        {ok, Forms} = epp_dodger:parse_file(SuiteSource),
        Oncalls = extract_attribute(oncall, Forms),
        Authors = extract_attribute(author, Forms),
        case lists:append(Oncalls, Authors) of
            [] -> [?FALLBACK_ONCALL];
            Contacts -> Contacts
        end
    catch
        % the suite module is for some reason not accessible
        _:_:_ -> [?FALLBACK_ONCALL]
    end.

-spec extract_attribute(atom(), erl_syntax:forms()) -> [binary()].
extract_attribute(_, []) ->
    [];
extract_attribute(Attribute, [Form | Forms]) ->
    case erl_syntax:type(Form) of
        attribute ->
            AttrName = erl_syntax:attribute_name(Form),
            FoundHere =
                case erl_syntax:is_atom(AttrName, Attribute) of
                    false ->
                        [];
                    true ->
                        case erl_syntax:attribute_arguments(Form) of
                            [AttrArg] ->
                                case erl_syntax:type(AttrArg) of
                                    string ->
                                        [unicode_characters_to_binary(erl_syntax:string_value(AttrArg))];
                                    list ->
                                        [
                                            unicode_characters_to_binary(erl_syntax:string_value(S))
                                         || S <- erl_syntax:list_elements(AttrArg), erl_syntax:type(S) =:= string
                                        ];
                                    _ ->
                                        []
                                end;
                            _ ->
                                []
                        end
                end,
            FoundHere ++ extract_attribute(Attribute, Forms);
        _ ->
            extract_attribute(Attribute, Forms)
    end.
