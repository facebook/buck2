%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(listing_interfacer).
-moduledoc """
This module interfaces with the tpx listing protocol, presented in https://www.internalfb.com/code/fbsource/[51311877d966]/fbcode/buck2/docs/test_execution.md?lines=50
for high level, see https://www.internalfb.com/code/fbsource/[0101a07bcb98bf8dbed51f55b7b5e4ab8346130f]/fbcode/testinfra/tpx/tpx-buck/src/listing/test_xml.rs?lines=39-55). for
code implementation.
""".
-compile(warn_missing_spec_all).

-include_lib("common/include/tpx_records.hrl").
-export([produce_xml_file/2, test_case_constructor/2]).

%% Copy-and-paste of `xmerl:simple_element`, that is not currently exported by xmerl
-type 'xmerl:simple_element'() ::
    {
        Tag :: atom(),
        Attributes :: [{Name :: atom(), Value :: iolist() | atom() | integer()}],
        Content :: ['xmerl:simple_element'()]
    }
    | {Tag :: atom(), Content :: ['xmerl:simple_element'()]}
    | Tag :: atom() | IOString :: iolist() | xmerl:element().

-spec test_case_to_xml(TestCase) -> 'xmerl:simple_element'() when
    TestCase :: #test_spec_test_case{}.
test_case_to_xml(#test_spec_test_case{suite = Suite, testcases = TestInfos} = _TestCase) ->
    TestElementsXml = [test_info_to_xml(TestInfo) || TestInfo <- TestInfos],
    {testcase, [{suite, binary_to_atom(Suite)}], TestElementsXml}.

-spec test_case_constructor(atom(), [binary()]) -> #test_spec_test_case{}.
test_case_constructor(Suite, Tests) ->
    #test_spec_test_case{
        suite = atom_to_binary(Suite),
        testcases = lists:map(
            fun(TestName) -> #test_spec_test_info{name = TestName, filter = TestName} end, Tests
        )
    }.

-spec test_info_to_xml(#test_spec_test_info{}) -> 'xmerl:simple_element'().
test_info_to_xml(#test_spec_test_info{name = TestName, filter = TestName}) ->
    {test, [{name, [TestName]}, {filter, [TestName]}], []}.

-spec produce_xml_file(file:filename_all(), #test_spec_test_case{}) -> ok.
produce_xml_file(OutputDir, TestCase) ->
    XmlString = xmerl:export_simple([test_case_to_xml(TestCase)], xmerl_xml),
    ok = file:write_file(filename:join(OutputDir, result), XmlString, [append, raw, binary]).
