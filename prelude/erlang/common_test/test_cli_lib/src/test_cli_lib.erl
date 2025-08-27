%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(test_cli_lib).
-moduledoc """
Entrypoint for the interactive Common Test shell.
""".
-typing([eqwalizer]).
-compile(warn_missing_spec_all).

%% Public API
-export([start/0]).

-spec start() -> ok.
start() ->
    case application:load(test_cli_lib) of
        ok -> ok;
        {error, {already_loaded, test_cli_lib}} -> ok
    end,

    case application:get_env(test_cli_lib, preamble) of
        undefined ->
            standard_preamble();
        {ok, Preamble} when is_binary(Preamble) ->
            case try_execute_preamble(Preamble) of
                ok ->
                    ok;
                error ->
                    erlang:halt(1)
            end
    end.

-spec standard_preamble() -> ok.
standard_preamble() ->
    test:info(),
    test:ensure_initialized(),
    test:start_shell(),
    ok.

-spec try_execute_preamble(binary()) -> ok | error.
try_execute_preamble(Preamble) ->
    try
        Tokens =
            case erl_scan:string(binary_to_list(Preamble)) of
                {ok, Toks, _} ->
                    Toks;
                {error, {Line1, Module1, Reason1}, _} ->
                    throw({bad_preamble, "Lexical error at line ~p: ~s~n", [Line1, Module1:format_error(Reason1)]})
            end,
        Exprs =
            case erl_parse:parse_exprs(Tokens) of
                {ok, Es} ->
                    Es;
                {error, {Line2, Module2, Reason2}} ->
                    throw({bad_preamble, "Parse error at line ~p: ~s~n", [Line2, Module2:format_error(Reason2)]})
            end,
        {value, _Result, _Bindings} = erl_eval:exprs(Exprs, []),
        ok
    catch
        throw:{bad_preamble, ErrorFmt, Args} ->
            io:format(ErrorFmt, Args),
            error
    end.
