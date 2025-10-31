%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(ct_daemon_printer).
-moduledoc """
Abstractions for pretty printing test results
""".
-compile(warn_missing_spec_all).

%% Public API
-export([print_result/2, print_summary/3]).

-define(CHECK_MARK, "✓").
-define(CROSS_MARK, "✗").
-define(SKIP_MARK, "∅").
-define(SUM_MARK, "∑").

-spec print_summary(Count, Count, Count) -> ok when Count :: non_neg_integer().
print_summary(Total, Passed, FailedOrSkipped) ->
    io:format("------------------------------~n"),
    io:format("~ts   Total: ~b~n", [?SUM_MARK, Total]),
    io:format("~ts   Passed: ~b~n", [?CHECK_MARK, Passed]),
    io:format("~ts/~ts Failed or Skipped: ~b~n", [?CROSS_MARK, ?SKIP_MARK, FailedOrSkipped]).

-spec print_result(string(), ct_daemon_core:run_result() | {error, ct_daemon_runner:discover_error()}) ->
    ok | fail | skip.
print_result(Name, pass_result) ->
    io:format("~ts ~ts~n", [?CHECK_MARK, Name]);
print_result(Name, {fail, {TestId, {end_per_testcase, E}}}) ->
    print_result(Name ++ " [end_per_testcase]", {fail, {TestId, E}});
print_result(Name, {fail, {_TestId, {thrown, {Reason, Stacktrace}}}}) ->
    print_error(Name, throw, Reason, Stacktrace);
print_result(Name, {fail, {_TestId, {Reason, Stacktrace}}}) ->
    print_error(Name, error, Reason, Stacktrace);
print_result(Name, {skip, Where, {error, {Reason, Stacktrace}}}) ->
    print_skip_error(Name, Where, error, Reason, Stacktrace);
print_result(Name, {skip, Where, {error, {thrown, Reason, Stacktrace}}}) ->
    print_skip_error(Name, Where, throw, Reason, Stacktrace);
print_result(Name, {skip, Where, Reason}) ->
    io:format("~ts ~ts~n", [?SKIP_MARK, Name]),
    io:format("skipped at ~ts because of ~tp~n", [print_skip_location(Where), Reason]),
    skip;
print_result(Name, {fail, {TestId, Reason}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("~ts failed:~n", [TestId]),
    io:format("~tp~n", [Reason]),
    fail;
print_result(Name, {error, {_TestId, {'ct_daemon_core$sentinel_crash', Info}}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("Test process received EXIT signal with reason: ~tp~n", [Info]),
    fail;
print_result(Name, {error, {_TestId, {timetrap, TimeoutValue}}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("Test timed out after ~tp ms~n", [TimeoutValue]),
    fail;
print_result(Name, {error, {_TestId, {Reason, Stacktrace}}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    print_error(Name, error, Reason, Stacktrace);
print_result(Name, Unstructured) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("unable to format failure reason, please report.~n"),
    io:format("~tp~n", [Unstructured]),
    fail.

-spec print_error(Name, Type, Reason, Stacktrace) -> fail when
    Name :: unicode:chardata(),
    Type :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: erlang:stacktrace() | term().
print_error(Name, Type, Reason, Stacktrace) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("failed with ~tp:~n", [Type]),
    Output = ct_error_printer:format_error(Type, {Reason, chop_stack(Stacktrace)}, true),
    io:format("~ts~n", [Output]),
    fail.

-spec print_skip_error(Name, Where, Type, Reason, Stacktrace) -> skip when
    Name :: unicode:chardata(),
    Where :: term(),
    Type :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: erlang:stacktrace() | term().
print_skip_error(Name, Where, Type, Reason, Stacktrace) ->
    io:format("~ts ~ts~n", [?SKIP_MARK, Name]),
    io:format("skipped at ~ts because of~n", [print_skip_location(Where)]),
    Output = ct_error_printer:format_error(Type, {Reason, chop_stack(Stacktrace)}, true),
    io:format("~ts~n", [Output]),
    skip.

-spec print_skip_location(Location) -> unicode:chardata() when
    Location :: {_, atom()} | term().
print_skip_location({_, GroupOrSuite}) when is_atom(GroupOrSuite) ->
    case re:run(atom_to_list(GroupOrSuite), "SUITE$", [unicode]) of
        nomatch -> io_lib:format("init_per_group of ~ts", [GroupOrSuite]);
        _ -> "init_per_suite"
    end;
print_skip_location(Other) ->
    io_lib:format("~ts", [Other]).

-spec chop_stack(term()) -> term().
chop_stack(E = {failed, _}) ->
    E;
chop_stack(Stacktrace) when is_list(Stacktrace) ->
    lists:takewhile(
        fun({Module, _, _, _}) -> not lists:member(Module, [ct_daemon_core, ct_daemon_hooks]) end, Stacktrace
    );
chop_stack(Other) ->
    Other.
