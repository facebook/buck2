%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(ct_stdout).
-moduledoc """
Functions to work with the stdout of a Common Test, for example
to delimit the output of each test-case.
""".
-compile(warn_missing_spec_all).

-export([emit_progress/3]).

-import(common_util, [unicode_characters_to_binary/1]).

%% ---------------------------------------------------------------------------
%% Types
%% ---------------------------------------------------------------------------

-export_type([progress/0, callback/0, progress_line/0]).

-type progress() :: started | finished.
-type callback() :: init_per | end_per.
-type progress_line() :: binary().

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

-doc """
Emit a unique line to stdout to mark the start/end of a ct_suite callback.

The logged line is returned so it can later be used to find the marker
in a stdout log.

NB. `UserProcess` is expected to be the `user` process, but we demand an
explicit (cached) version, in case testcases mess with this process
""".
-spec emit_progress(UserProcess, What, Progress) -> progress_line() when
    UserProcess :: pid(),
    What :: unicode:chardata(),
    Progress :: progress().
emit_progress(UserProcess, What, Progress) ->
    ProgressStr =
        case Progress of
            started -> ~"START";
            finished -> ~"END"
        end,
    Uniq = erlang:unique_integer([positive]),
    ProgressLine = unicode_characters_to_binary(
        io_lib:format(~"[~ts] ~ts ~ts [~tp]~n", [timestamp_str(), ProgressStr, What, Uniq])
    ),
    io:format(UserProcess, ProgressLine, []),
    ProgressLine.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

-spec timestamp_str() -> unicode:chardata().
timestamp_str() ->
    T = erlang:system_time(millisecond),
    calendar:system_time_to_rfc3339(T, [{unit, millisecond}, {time_designator, $\s}]).
