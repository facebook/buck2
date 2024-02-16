%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%%% % @format
%%%-------------------------------------------------------------------
%%% @doc
%%% Utilities method to parse string args given to the test binary
%%% via user input.
%%% @end

-module(buck_ct_parser).
-compile(warn_missing_spec).

%% Public API
-export([parse_str/1]).

-spec parse_str(string()) -> term().
parse_str("") ->
    [];
parse_str(StrArgs) ->
    try
        {ok, Tokens, _} = erl_scan:string(StrArgs ++ "."),
        erl_parse:parse_term(Tokens)
    of
        {ok, Term} ->
            Term;
        {error, Reason} ->
            error(lists:flatten(io_lib:format("Error parsing StrArgs ~p, Reason: ~p", [StrArgs, Reason])))
    catch
        E:R:S ->
            error(
                lists:flatten(
                    io_lib:format("Error parsing StrArgs ~p, error ~p", [StrArgs, erl_error:format_exception(E, R, S)])
                )
            )
    end.
