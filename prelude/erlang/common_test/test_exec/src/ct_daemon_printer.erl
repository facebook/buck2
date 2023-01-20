%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%%%-------------------------------------------------------------------
%%% @doc
%%% Abstractions for pretty printing test results
%%% @end
%%% % @format

-module(ct_daemon_printer).
-typing(eqwalizer).

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

-spec print_result(string(), ct_daemon_core:run_result() | ct_daemon_runner:discover_error()) ->
    ok | fail.
print_result(Name, pass_result) ->
    io:format("~ts ~ts~n", [?CHECK_MARK, Name]);
print_result(Name, {error, {TestId, {ErrType, Reason}}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("~ts failed with ~p:~n", [TestId, ErrType]),
    Output = ct_error_printer:format_error(ErrType, Reason, true),
    io:format("~ts~n", [Output]),
    fail;
print_result(Name, {error, {TestId, {Class, Reason, StackTrace}}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("~ts failed:~n", [TestId]),
    io:format("~s~n", [erl_error:format_exception(Class, Reason, StackTrace)]),
    fail;
print_result(Name, {error, {TestId, UnstructuredReason}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("~ts failed:~n", [TestId]),
    io:format("~p~n", [UnstructuredReason]),
    fail;
print_result(Name, {error, {Error, Reason, Stacktrace}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("~nrun failed:~n", []),
    io:format("~ts~n", [erl_error:format_exception(Error, Reason, Stacktrace)]),
    fail;
print_result(Name, {skip, Where, {Error, Reason, Stacktrace}}) ->
    io:format("~ts ~ts~n", [?SKIP_MARK, Name]),
    io:format("skipped at ~s because of ~n~ts~n", [
        print_skip_location(Where), erl_error:format_exception(Error, Reason, Stacktrace)
    ]),
    skip;
print_result(Name, {skip, Where, Reason}) ->
    io:format("~ts ~ts~n", [?SKIP_MARK, Name]),
    io:format("skipped at ~s because of ~p~n", [print_skip_location(Where), Reason]),
    skip;
print_result(Name, {fail, {TestId, Reason}}) ->
    io:format("~ts ~ts~n", [?CROSS_MARK, Name]),
    io:format("~ts failed:~n", [TestId]),
    io:format("~p~n", [Reason]).

print_skip_location({init_per_suite, _Suite}) ->
    "init_per_suite";
print_skip_location({_, Group}) ->
    io_lib:format("init_per_group of ~s", [Group]);
print_skip_location(Other) ->
    Other.
