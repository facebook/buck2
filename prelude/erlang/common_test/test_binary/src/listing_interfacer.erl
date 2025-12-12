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
-export([produce_json_file/2]).

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

-spec produce_json_file(OutputDir, TestCase) -> ok when
    OutputDir :: file:filename_all(),
    TestCase :: #test_spec_test_case{}.
produce_json_file(OutputDir, TestCase) ->
    Filename = filename:join(OutputDir, ~"result"),
    {ok, Fd} = file:open(Filename, [write, raw, binary]),
    try
        lists:foreach(
            fun(TestInfo) ->
                TestInfoJson = test_info_to_json_line(TestInfo),
                ok = file:write(Fd, [TestInfoJson, ~"\n"])
            end,
            TestCase#test_spec_test_case.testcases
        )
    after
        file:close(Fd)
    end.

-spec test_info_to_json_line(TestInfo) -> iodata() when
    TestInfo :: #test_spec_test_info{}.
test_info_to_json_line(TestInfo) ->
    % NB. json:encode() guarantees the output is in one line
    json:encode(#{
        testcase => TestInfo#test_spec_test_info.name,
        filter => TestInfo#test_spec_test_info.filter
    }).
