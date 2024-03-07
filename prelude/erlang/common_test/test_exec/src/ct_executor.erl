%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%%% % @format
%%@doc
%% Simple trampoline for ct_run.
%% Notably allows us to call post/pre method on the node if needed, e.g for coverage.

-module(ct_executor).
-include_lib("kernel/include/logger.hrl").
-include_lib("common/include/buck_ct_records.hrl").
-compile(warn_missing_spec_all).

-export([run/1]).

%% `ct_run_arg()` represents an option accepted by ct:run_test/1, such as
%% `multiply_timetraps` or `ct_hooks`.
%% For all the options, see https://www.erlang.org/doc/man/ct#run_test-1
-type ct_run_arg() :: {atom(), term()}.
-type ct_exec_arg() :: {output_dir | suite | providers, term()}.

% For testing
-export([split_args/1]).

-define(STDOUT_MAX_LINES, 1000).
-define(STDOUT_MAX_LINE_LENGTH, 10000).

-spec run([string()]) -> no_return().
run(Args) when is_list(Args) ->
    ExitCode =
        try
            {CtExecutorArgs, CtRunArgs} = parse_arguments(Args),
            debug_print("~p", [#{ct_exec_args => CtExecutorArgs, ct_run_args => CtRunArgs}]),
            {_, OutputDir} = lists:keyfind(output_dir, 1, CtExecutorArgs),
            ok = test_logger:set_up_logger(OutputDir, ?MODULE),

            %% log arguments into ct_executor.log
            ?LOG_INFO("raw args: ~p", [Args]),
            ?LOG_INFO("executor args: ~p", [CtExecutorArgs]),
            ?LOG_INFO("CtRunArgs: ~p", [CtRunArgs]),

            % Until this point the logger is not set up so we cannot log.
            % Therefore we used io:format to forward information to the
            % process calling it (ct_runner).
            try
                % We need to load the 'common' application to be able to configure
                % it via the `common_app_env` arguments
                application:load(common),
                % We consult all the .app files to load the atoms.
                % This solution is less than optimal and should be addressed
                % T120903856
                PotentialDotApp = [
                    filename:join(Dep, filename:basename(filename:dirname(Dep)) ++ ".app")
                 || Dep <- code:get_path()
                ],
                [file:consult(DotApp) || DotApp <- PotentialDotApp, filelib:is_regular(DotApp)],
                {_, Suite} = lists:keyfind(suite, 1, CtExecutorArgs),
                ProviderInitState = #init_provider_state{output_dir = OutputDir, suite = Suite},
                Providers0 =
                    case lists:keyfind(providers, 1, CtExecutorArgs) of
                        false ->
                            [];
                        {_, Providers} ->
                            [
                                buck_ct_provider:do_init(Provider, ProviderInitState)
                             || Provider <- Providers
                            ]
                    end,
                %% get longer stack traces
                erlang:system_flag(backtrace_depth, 20),
                ?LOG_DEBUG("ct_run called with arguments ~p ~n", [CtRunArgs]),
                Providers1 = [buck_ct_provider:do_pre_running(Provider) || Provider <- Providers0],
                {ok, IoBuffer} = io_buffer:start_link(#{
                    passthrough => true, max_elements => ?STDOUT_MAX_LINES, max_length => ?STDOUT_MAX_LINE_LENGTH
                }),
                register(cth_tpx_io_buffer, IoBuffer),
                %% set global timeout
                Result = ct:run_test(CtRunArgs),
                ?LOG_DEBUG("ct_run finished with result ~p ~n", [Result]),
                Providers2 = [buck_ct_provider:do_post_running(Provider) || Provider <- Providers1],
                [buck_ct_provider:do_terminate(Provider) || Provider <- Providers2],
                0
            catch
                Class:Reason:Stack ->
                    ?LOG_ERROR("ct executor failed due to ~ts\n", [
                        erl_error:format_exception(Class, Reason, Stack)
                    ]),
                    2
            after
                test_logger:flush()
            end
        catch
            % Catch an exception that happens before logging is set up.
            % Will forward the exception to the process that opened the port (ct_runner).
            Class1:Reason1:Stack1 ->
                io:format("~ts\n", [erl_error:format_exception(Class1, Reason1, Stack1)]),
                1
        end,
    erlang:halt(ExitCode).

-spec parse_arguments([string()]) -> {[ct_exec_arg()], [ct_run_arg()]}.
parse_arguments(Args) ->
    % The logger is not set up yet.
    % This will be sent to the program executing it (ct_runner),
    % that will log it in its own log.
    debug_print("CT executor called with ~p~n", [Args]),
    ParsedArgs = lists:map(
        fun(StrArgs) ->
            buck_ct_parser:parse_str(StrArgs)
        end,
        Args
    ),
    debug_print("Parsed arguments ~p~n", [ParsedArgs]),
    % We split the arguments between those that go to ct_run and those that are for
    % ct_executor
    % the args passed to ct are to be found after the --ct-args
    split_args(ParsedArgs).

% @doc Splits the argument before those that happens
% before ct_args (the executor args) and those after
% (the args for ct_run). ct_args will always be
% present in the list
-spec split_args([term()]) -> {[ct_exec_arg()], [ct_run_arg()]}.
split_args(Args) ->
    {CtExecutorArgs, [ct_args | CtRunArgs]} = lists:splitwith(fun(Arg) -> Arg =/= ct_args end, Args),
    {parse_ct_exec_args(CtExecutorArgs), parse_ct_run_args(CtRunArgs)}.

-spec parse_ct_run_args([term()]) -> [ct_run_arg()].
parse_ct_run_args([]) ->
    [];
parse_ct_run_args([{Key, _Value} = Arg | Args]) when is_atom(Key) ->
    [Arg | parse_ct_run_args(Args)].

-spec parse_ct_exec_args([term()]) -> [ct_exec_arg()].
parse_ct_exec_args([]) ->
    [];
parse_ct_exec_args([{Key, _Value} = Arg | Args]) when Key =:= output_dir; Key =:= suite; Key =:= providers ->
    [Arg | parse_ct_exec_args(Args)].

-spec debug_print(string(), [term()]) -> ok.
debug_print(Fmt, Args) ->
    case os:getenv("ERLANG_BUCK_DEBUG_PRINT") of
        false -> io:format(Fmt, Args);
        "disabled" -> ok;
        _ -> io:format(Fmt, Args)
    end.
