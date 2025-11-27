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

-export([filename/0, filename/1]).
-export([emit_progress/3]).
-export([process_raw_stdout_log/4]).

-import(common_util, [unicode_characters_to_binary/1]).

%% ---------------------------------------------------------------------------
%% Types
%% ---------------------------------------------------------------------------

-export_type([progress/0, callback/0, progress_line/0, collected_stdout/0]).

-type progress() :: started | finished.
-type callback() :: init_per | end_per.
-type progress_line() :: binary().

-doc """
Mapping from a "start" marker to the offsets in the stdout log with
the stdout for that marker.
""".
-type stdout_file_offsets() :: #{
    progress_line() => no_stdout | #{first => integer(), last => integer()}
}.

-doc """
Mapping from a "start" marker to the stdout contents associated to that marker.
""".
-type collected_stdout() :: #{
    progress_line() => binary()
}.

%% ---------------------------------------------------------------------------
%% Public API
%% ---------------------------------------------------------------------------

-doc """
Filename of the CT stdout log file that will be returned to the user.
""".
-spec filename() -> file:filename_all().
filename() ->
    ~"ct_stdout.txt".

-doc """
Like `m:filename/0` but will prefix the given `OutputDir`.
""".
-spec filename(OutputDir) -> file:filename_all() when
    OutputDir :: file:filename_all().
filename(OutputDir) ->
    filename:join(OutputDir, filename()).

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

    % NB. We emit a leading newling in case the last print statement
    % didn't finish the line. We'll manually remove it, if necessary when
    % processing the markers
    io:format(UserProcess, "~n~s", [ProgressLine]),

    ProgressLine.

-doc """
Writes to `OutputFile` a copy of `RawStdoutFile` where all progress markers
mantioned in `TreeResults` have been removed.

The returned value contains the collected stdout for each "start" marker, capped to
the last `MaxPerCollected` bytes.
""".
-spec process_raw_stdout_log(RawStdoutFile, OutputFile, TreeResults, MaxPerCollected) -> {ok, collected_stdout()} when
    RawStdoutFile :: file:filename_all(),
    OutputFile :: file:filename_all(),
    TreeResults :: cth_tpx_test_tree:tree_node(),
    MaxPerCollected :: non_neg_integer().
process_raw_stdout_log(RawStdoutFile, OutputFile, TreeResults, MaxPerCollected) ->
    MethodResults = get_method_results(TreeResults),
    ProgressMarkers = lists:foldl(fun add_progress_markers/2, #{}, MethodResults),
    {ok, InHandle} = file:open(RawStdoutFile, [raw, binary, read, read_ahead]),
    {ok, OutHandle} = file:open(OutputFile, [raw, write, delayed_write]),
    Offsets =
        try
            process_raw_stdout_log_loop(InHandle, OutHandle, ProgressMarkers, false, #{})
        after
            file:close(OutHandle),
            file:close(InHandle)
        end,
    {ok, RereadHandle} = file:open(OutputFile, [raw, binary, read, read_ahead]),
    try
        {ok, #{K => collect_stdout(RereadHandle, V, MaxPerCollected) || K := V <- Offsets}}
    after
        file:close(RereadHandle)
    end.

%% ---------------------------------------------------------------------------
%% Helpers
%% ---------------------------------------------------------------------------

-spec timestamp_str() -> unicode:chardata().
timestamp_str() ->
    T = erlang:system_time(millisecond),
    calendar:system_time_to_rfc3339(T, [{unit, millisecond}, {time_designator, $\s}]).

-spec get_method_results(TreeResults) -> [cth_tpx_test_tree:method_result()] when
    TreeResults :: cth_tpx_test_tree:tree().
get_method_results(TreeResults) ->
    lists:flatten(get_method_results(TreeResults, [])).

-spec get_method_results(TreeResults, Acc) -> Acc when
    TreeResults :: cth_tpx_test_tree:tree() | [cth_tpx_test_tree:tree()],
    Acc :: [[cth_tpx_test_tree:method_result()]].
get_method_results([], Acc) ->
    Acc;
get_method_results([First | Rest], Acc0) ->
    Acc1 = get_method_results(First, Acc0),
    get_method_results(Rest, Acc1);
get_method_results(Leaf = #{type := leaf}, Acc) ->
    #{init_method := Init, main_method := Main, end_method := End} = Leaf,
    [filter_none([Init, Main, End]) | Acc];
get_method_results(Node = #{type := node}, Acc0) ->
    #{init_method := Init, end_method := End, test_cases := TestCases, sub_groups := SubGroups} = Node,
    Acc1 = [filter_none([Init, End]) | Acc0],
    Acc2 = get_method_results(maps:values(TestCases), Acc1),
    Acc3 = get_method_results(maps:values(SubGroups), Acc2),
    Acc3.

-spec add_progress_markers(MethodResult, Acc) -> Acc when
    MethodResult :: cth_tpx_test_tree:method_result(),
    Acc :: #{progress_line() => progress_line()}.
add_progress_markers(#{start_progress_marker := StartMarker, end_progress_marker := EndMarker}, Acc) ->
    case Acc of
        #{StartMarker := EndMarker} ->
            Acc;
        #{StartMarker := OtherEndMarker} ->
            % Sanity-check
            error({inconsistent_start_markers, StartMarker, EndMarker, OtherEndMarker});
        _ ->
            Acc#{StartMarker => EndMarker}
    end;
add_progress_markers(_MethodResult, Acc) ->
    Acc.

-spec filter_none([none | A]) -> [A].
filter_none([]) -> [];
filter_none([none | As]) -> filter_none(As);
filter_none([A | As]) when A /= none -> [A | filter_none(As)].

-spec process_raw_stdout_log_loop(InH, OutH, PendingMarkers, HasPendingNewLine, Acc) -> Acc when
    InH :: file:fd(),
    OutH :: file:fd(),
    PendingMarkers :: #{progress_line() => progress_line() | {progress_line(), integer()}},
    HasPendingNewLine :: boolean(),
    Acc :: stdout_file_offsets().
process_raw_stdout_log_loop(InH, OutH, PendingMarkers, HasPendingNewLine, Acc) ->
    case file:read_line(InH) of
        eof ->
            case maps:size(PendingMarkers) of
                0 ->
                    not HasPendingNewLine orelse file:write(OutH, ~"\n"),
                    Acc;
                _ ->
                    error({stdout_markers_not_found, PendingMarkers})
            end;
        {ok, Line} when is_binary(Line) ->
            case maps:take(Line, PendingMarkers) of
                {EndMarker, PendingMarkers1} when is_binary(EndMarker) ->
                    {ok, StartOffset} = file:position(OutH, cur),
                    PendingMarkers2 = PendingMarkers1#{EndMarker => {Line, StartOffset}},
                    process_raw_stdout_log_loop(InH, OutH, PendingMarkers2, _HasPendingNewLine = false, Acc);
                {{StartMarker, StartOffset}, PendingMarkers1} when is_binary(StartMarker), is_integer(StartOffset) ->
                    {ok, EndOffset} = file:position(OutH, cur),
                    Offsets =
                        case StartOffset =:= EndOffset of
                            true -> no_stdout;
                            false -> #{first => StartOffset, last => EndOffset - 1}
                        end,
                    Acc1 = Acc#{StartMarker => Offsets},
                    process_raw_stdout_log_loop(InH, OutH, PendingMarkers1, _HasPendingNewLine = false, Acc1);
                error ->
                    % No marker found, copy the stdout log
                    not HasPendingNewLine orelse file:write(OutH, ~"\n"),
                    case Line of
                        <<"\n">> ->
                            % This may be the leading newline of a marker, so postpone until we are sure
                            process_raw_stdout_log_loop(InH, OutH, PendingMarkers, _HasPendingNewLine = true, Acc);
                        _ ->
                            file:write(OutH, Line),
                            process_raw_stdout_log_loop(InH, OutH, PendingMarkers, _HasPendingNewLine = false, Acc)
                    end
            end
    end.

-spec collect_stdout(InH, What, Max) -> binary() when
    InH :: file:fd(),
    What :: no_stdout | #{first => integer(), last => integer()},
    Max :: non_neg_integer().
collect_stdout(_InH, no_stdout, _Max) ->
    ~"";
collect_stdout(InH, #{first := First, last := Last}, Max) ->
    Count = Last - First + 1,
    case Count =< Max of
        true ->
            case file:pread(InH, First, Count) of
                {ok, Result} when is_binary(Result) -> Result
            end;
        false ->
            First1 = Last - Max + 1,
            Count1 = Max,
            case file:pread(InH, First1, Count1) of
                {ok, Result} when is_binary(Result) ->
                    % utf-8 chars take 1-3 bytes. If by truncating we end
                    % in the middle of one of the chars taking 2-3 bytes,
                    % then we get an invalid binary, which we can fix by
                    % dropping the first 1 or 2 bytes
                    fix_unicode_encoding(Result)
            end
    end.

-spec fix_unicode_encoding(binary()) -> binary().
fix_unicode_encoding(Bin) ->
    case unicode:characters_to_binary(Bin) of
        Ok when is_binary(Ok) -> Ok;
        {error, ~"", Bin} ->
            <<_:8, Bin1/binary>> = Bin,
            case unicode:characters_to_binary(Bin1) of
                Ok when is_binary(Ok) -> Ok;
                {error, ~"", Bin1} ->
                    <<_:8, Bin2/binary>> = Bin1,
                    case unicode:characters_to_binary(Bin2) of
                        Ok when is_binary(Ok) -> Ok;
                        _ -> error({unfixable_utf8, Bin})
                    end
            end
    end.
