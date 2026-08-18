%% % @format
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

-module(json_interfacer).
-moduledoc """
Methods used for serialization to the type
defined in tpx [here](https://www.internalfb.com/code/fbsource/[bb9e81daacad]/fbcode/testinfra/tpx/tpx-output/src/erl_parser.rs).
""".
-compile(warn_missing_spec_all).

-export([write_json_output/4, format_json/3, status_name/1]).

-import(common_util, [unicode_characters_to_binary/1]).

-define(RESULT_EXEC_VERSION, 1).

-define(PASSED, <<"PASSED">>).
-define(FAILED, <<"FAILED">>).
-define(SKIPPED, <<"SKIPPED">>).
-define(TIMEOUT, <<"TIMEOUT">>).
-define(OMITTED, <<"OMITTED">>).
-define(INFRA_FAILURE, <<"INFRA_FAILURE">>).

-type status() :: passed | failed | skipped | timeout | omitted | infra_failure.

%% The integers are the discriminants of `TestStatusErl` in erl_parser.rs. 4 is FATAL
%% and 6 is unused there; do not reuse them for anything else.
-spec status(status()) -> integer().
status(passed) -> 1;
status(failed) -> 2;
status(skipped) -> 3;
status(timeout) -> 5;
status(omitted) -> 7;
status(infra_failure) -> 8.

-spec status_name(integer()) -> status().
status_name(1) -> passed;
status_name(2) -> failed;
status_name(3) -> skipped;
status_name(5) -> timeout;
status_name(7) -> omitted;
status_name(8) -> infra_failure.

-spec summary(status()) -> binary().
summary(passed) -> ?PASSED;
summary(failed) -> ?FAILED;
summary(skipped) -> ?SKIPPED;
summary(timeout) -> ?TIMEOUT;
summary(omitted) -> ?OMITTED;
summary(infra_failure) -> ?INFRA_FAILURE.

-type formatted_result() ::
    #{
        name := binary(),
        status := integer(),
        summary := binary(),
        details := binary(),
        durationSecs := float(),
        std_out := binary(),
        endedTime => integer()
    }.

-type collected_result() :: cth_tpx_test_tree:collected_result().

-type formatted_case_result() ::
    #{
        inits := [formatted_result()],
        main := formatted_result(),
        ends := [formatted_result()]
    }.

-type formatted_output() ::
    #{
        version := pos_integer(),
        status := integer(),
        summary := binary(),
        details := binary(),
        test_results := [formatted_case_result()]
    }.

-spec write_json_output(file:filename_all(), status(), unicode:chardata(), [collected_result()]) ->
    {ok, file:filename_all()}.
write_json_output(OutputDir, SuiteStatus, SuiteDetails, TpxResults) ->
    OuptputFile = filename:join(OutputDir, "result_exec.json"),
    file:write_file(
        OuptputFile, format_json(SuiteStatus, SuiteDetails, TpxResults), [raw, binary]
    ),
    {ok, OuptputFile}.

-spec format_json(status(), unicode:chardata(), [collected_result()]) -> iodata().
format_json(SuiteStatus, SuiteDetails, TpxResults) ->
    json:encode(format_output(SuiteStatus, SuiteDetails, TpxResults)).

-spec format_output(status(), unicode:chardata(), [collected_result()]) -> formatted_output().
format_output(SuiteStatus, SuiteDetails, TpxResults) ->
    #{
        version => ?RESULT_EXEC_VERSION,
        status => status(SuiteStatus),
        summary => summary(SuiteStatus),
        details => unicode_characters_to_binary(SuiteDetails),
        test_results => [format_case(CaseResult) || CaseResult <- TpxResults]
    }.

-spec format_case(collected_result()) -> formatted_case_result().
format_case(
    #{
        inits := Inits,
        main := Main,
        ends := Ends
    } = _CaseResult
) ->
    #{
        inits => [format_method_result(Elem) || Elem <:- Inits],
        main => format_method_result(Main),
        ends => [format_method_result(Elem) || Elem <:- Ends]
    }.

-spec format_method_result(cth_tpx_test_tree:collected_method_result()) -> formatted_result().
format_method_result(
    #{
        name := Name,
        start_time := Start,
        end_time := End,
        outcome := Outcome,
        details := Details,
        std_out := StdOut
    } = _TestResult
) ->
    #{
        name => name_to_binary(Name),
        endedTime => trunc(End),
        durationSecs => End - Start,
        status => status(Outcome),
        summary => summary(Outcome),
        details => unicode_characters_to_binary(Details),
        std_out => unicode_characters_to_binary(StdOut)
    };
format_method_result(
    #{
        name := Name,
        outcome := Outcome,
        details := Details,
        std_out := StdOut
    } = _TestResult
) ->
    #{
        name => name_to_binary(Name),
        status => status(Outcome),
        summary => summary(Outcome),
        details => unicode_characters_to_binary(Details),
        std_out => unicode_characters_to_binary(StdOut),
        durationSecs => 0.0
    }.

-spec name_to_binary(Name) -> binary() when
    Name :: cth_tpx_test_tree:name().
name_to_binary(Name) when is_atom(Name) ->
    atom_to_binary(Name);
name_to_binary(Name) when is_list(Name) ->
    unicode_characters_to_binary(Name).
