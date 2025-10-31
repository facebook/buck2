%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(ct_runner).
-moduledoc """
Simple gen_server that will run the the test and
communicates the result to the test runner.
""".
-compile(warn_missing_spec_all).

-behavior(gen_server).

-export([start_link/1]).
-include_lib("common/include/buck_ct_records.hrl").
-include_lib("kernel/include/logger.hrl").
-define(raw_file_access, prim_file).

-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-export([
    start_test_node/6,
    start_test_node/8,
    cookie/0,
    generate_arg_tuple/2,
    project_root/0
]).

-import(common_util, [unicode_characters_to_binary/1, filename_all_to_filename/1]).

-type opt() ::
    {packet, N :: 1 | 2 | 4}
    | stream
    | {line, L :: non_neg_integer()}
    | {cd, Dir :: string() | binary()}
    | {env, Env :: [{Name :: os:env_var_name(), Val :: os:env_var_value() | false}]}
    | {args, [string() | binary()]}
    | {arg0, string() | binary()}
    | exit_status
    | use_stdio
    | nouse_stdio
    | stderr_to_stdout
    | in
    | out
    | binary
    | eof
    | {parallelism, Boolean :: boolean()}
    | hide
    | {busy_limits_port, {non_neg_integer(), non_neg_integer()} | disabled}
    | {busy_limits_msgq, {non_neg_integer(), non_neg_integer()} | disabled}.

-type port_settings() :: [opt()].

-export_type([port_settings/0]).

-define(DEFAULT_LAUNCH_PORT_OPTIONS, [exit_status, nouse_stdio]).

-type initial_state() :: #{
    test_env := #test_env{}
}.
-type state() ::
    #{
        test_env := #test_env{},
        std_out := [string()],
        port := erlang:port()
    }.

%% Starts and monitor (through an erlang port) a ct_run.
%% Reports the result of the execution to the test runner.
-spec start_link(#test_env{}) -> gen_server:start_ret().
start_link(#test_env{} = TestEnv) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, TestEnv, []).

-spec init(TestEnv) -> {ok, initial_state(), {continue, {run, PortEpmd}}} when
    TestEnv :: #test_env{},
    PortEpmd :: inet:port_number().
init(#test_env{} = TestEnv) ->
    process_flag(trap_exit, true),
    PortEpmd = epmd_manager:get_port(),
    {ok, #{test_env => TestEnv}, {continue, {run, PortEpmd}}}.

-spec handle_continue({run, PortEpmd}, State0) -> {noreply, State1} | {stop, Reason, State0} when
    PortEpmd :: inet:port_number(),
    State0 :: initial_state(),
    State1 :: state(),
    Reason :: ct_runner_failed.
handle_continue({run, PortEpmd}, #{test_env := TestEnv} = State0) ->
    try run_test(TestEnv, PortEpmd) of
        Port ->
            State1 = State0#{std_out => [], port => Port},
            {noreply, State1}
    catch
        Class:Reason:Stack ->
            ErrorMsg = io_lib:format("Ct Runner failed to launch test due to ~ts\n", [
                erl_error:format_exception(Class, Reason, Stack)
            ]),
            ?LOG_ERROR(ErrorMsg),
            test_runner:mark_failure(ErrorMsg),
            {stop, ct_runner_failed, State0}
    end.

-spec handle_info
    ({Port, {exit_status, ExitStatus}}, state()) ->
        {stop, {ct_run_finished, ExitStatus}, state()} | {noreply, state()}
    when
        Port :: erlang:port(),
        ExitStatus :: integer();
    ({Port, {data, Data}}, state()) -> {noreply, state()} when
        Port :: erlang:port(),
        Data :: string();
    ({Port, closed}, state()) -> {stop, ct_port_closed, state()} | {noreply, state()} when
        Port :: erlang:port();
    ({'EXIT', Port, Reason}, state()) -> {stop, {ct_port_exit, Reason}, state()} | {noreply, state()} when
        Port :: erlang:port(),
        Reason :: term().

handle_info(
    {Port, {exit_status, ExitStatus}},
    #{port := Port} = State
) ->
    case ExitStatus of
        0 ->
            ResultMsg = "ct_runner finished successfully with exit status 0",
            ?LOG_DEBUG(ResultMsg),
            test_runner:mark_success(ResultMsg);
        _ ->
            ErrorMsg =
                case ExitStatus of
                    N when N == 137 orelse N == 143 ->
                        io_lib:format(
                            ("ct runner killed by SIGKILL (exit code ~b), likely due to running out of memory."
                            " Check https://fburl.com/wiki/01s5fnom for information about memory limits for tests"),
                            [ExitStatus]
                        );
                    _ ->
                        io_lib:format("ct run exited with status exit ~tp", [
                            ExitStatus
                        ])
                end,
            ?LOG_ERROR(ErrorMsg),
            test_runner:mark_failure(ErrorMsg)
    end,
    {stop, {ct_run_finished, ExitStatus}, State};
handle_info({_Port, {data, Data}}, #{std_out := StdOut} = State) ->
    ?LOG_DEBUG("~ts", [Data]),
    {noreply, State#{std_out => [Data | StdOut]}};
handle_info({Port, closed}, #{port := Port} = State) ->
    {stop, ct_port_closed, State};
handle_info({'EXIT', Port, Reason}, #{port := Port} = State) ->
    {stop, {ct_port_exit, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_call(term(), gen_server:from(), state()) -> no_return().
handle_call(_Request, _From, _State) -> error(not_implemented).

-spec handle_cast(term(), state()) -> no_return().
handle_cast(_Request, _State) -> error(not_implemented).

-spec terminate(Reason, State) -> ok when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: initial_state() | state().
terminate(_Reason, #{port := Port}) ->
    test_exec:kill_process(Port);
terminate(_Reason, _State) ->
    ok.

-doc """
Executes the test in a new node by launching ct_run.
""".
-spec run_test(#test_env{}, integer()) -> port().
run_test(
    #test_env{
        test_spec_file = TestSpecFile,
        output_dir = OutputDir,
        config_files = ConfigFiles,
        dependencies = Dependencies,
        suite_path = SuitePath,
        providers = Providers,
        suite = Suite,
        erl_cmd = ErlCmd,
        extra_flags = ExtraFlags,
        common_app_env = CommonAppEnv0,
        raw_target = RawTarget,
        trampolines = Trampolines
    } = _TestEnv,
    PortEpmd
) ->
    % We create the arguments for the ct_run, adding the ebin folder
    % where the suite is as part of the dependencies.
    SuiteFolder = filename:dirname(filename:absname(SuitePath)),
    CodePath = [SuiteFolder | Dependencies],
    CommonAppEnv1 = CommonAppEnv0#{~"raw_target" => unicode_characters_to_binary(io_lib:format("~0p", [RawTarget]))},
    {ok, ServerPort} = ct_executor_watchdog:start_server(),

    Args = build_run_args(OutputDir, ServerPort, Providers, Suite, TestSpecFile, CommonAppEnv1),

    {ok, ProjectRoot} = file:get_cwd(),

    start_test_node(
        ErlCmd,
        Trampolines,
        ExtraFlags,
        CodePath,
        ConfigFiles,
        OutputDir,
        [
            {args, Args},
            {env, [
                {"ERL_EPMD_PORT", integer_to_list(PortEpmd)},
                {"PROJECT_ROOT", ProjectRoot}
            ]}
        ],
        false
    ).

-spec build_common_args(
    CodePath :: [file:filename_all()],
    ConfigFiles :: [file:filename_all()]
) -> [binary() | string()].
build_common_args(CodePath, ConfigFiles) ->
    lists:append([
        [~"-noinput"],
        [~"-pa"],
        CodePath,
        config_arg(ConfigFiles)
    ]).

-spec build_run_args(
    OutputDir :: file:filename_all(),
    ServerPort :: number(),
    Providers :: [{module(), [term()]}],
    Suite :: module(),
    TestSpecFile :: file:filename_all(),
    CommonAppEnv :: #{binary() => binary()}
) -> [binary()].
build_run_args(OutputDir, ServerPort, Providers, Suite, TestSpecFile, CommonAppEnv) ->
    lists:append(
        [
            [~"-run", ~"ct_executor", ~"run"],
            generate_arg_tuple(output_dir, OutputDir),
            generate_arg_tuple(server_port, ServerPort),
            generate_arg_tuple(providers, Providers),
            generate_arg_tuple(suite, Suite),
            [~"ct_args"],
            generate_arg_tuple(spec, TestSpecFile),
            common_app_env_args(CommonAppEnv)
        ]
    ).

-spec common_app_env_args(Env :: #{binary() => binary()}) -> [binary()].
common_app_env_args(Env) ->
    lists:append([[~"-common", Key, Value] || Key := Value <- Env]).

-spec start_test_node(
    Erl :: [binary()],
    ExtraFlags :: [binary()],
    CodePath :: [file:filename_all()],
    ConfigFiles :: [file:filename_all()],
    OutputDir :: file:filename_all(),
    PortSettings :: port_settings()
) -> port().
start_test_node(
    ErlCmd,
    ExtraFlags,
    CodePath,
    ConfigFiles,
    OutputDir,
    PortSettings0
) ->
    start_test_node(
        ErlCmd,
        [],
        ExtraFlags,
        CodePath,
        ConfigFiles,
        OutputDir,
        PortSettings0,
        false
    ).

-spec start_test_node(
    Erl :: [binary()],
    Trampolines :: [file:filename_all()],
    ExtraFlags :: [binary()],
    CodePath :: [file:filename_all()],
    ConfigFiles :: [file:filename_all()],
    OutputDir :: file:filename_all(),
    PortSettings :: port_settings(),
    ReplayIo :: boolean()
) -> port().
start_test_node(
    ErlCmd,
    Trampolines,
    ExtraFlags,
    CodePath,
    ConfigFiles,
    OutputDir,
    PortSettings0,
    ReplayIo
) ->
    % we handle ErlCmd and Trampolines as one and execute the first
    % executale in the chain
    [Executable | ExecutableArgs] = Trampolines ++ ErlCmd,

    % HomeDir is the execution directory.
    HomeDir = set_home_dir(OutputDir),

    %% merge args, enc, cd settings
    LaunchArgs =
        ExecutableArgs ++ ExtraFlags ++
            build_common_args(CodePath, ConfigFiles) ++
            proplists:get_value(args, PortSettings0, []),

    Env = proplists:get_value(env, PortSettings0, []),
    LaunchEnv = [{"HOME", filename_all_to_filename(HomeDir)} | Env],

    LaunchCD = proplists:get_value(cd, PortSettings0, HomeDir),

    %% prepare launch settings
    PortSettings1 = lists:foldl(
        fun(Key, Settings) ->
            lists:keydelete(Key, 1, Settings)
        end,
        PortSettings0,
        [args, env, cd]
    ),

    DefaultOptions =
        case ReplayIo of
            true -> [stderr_to_stdout, exit_status, {line, 1024}];
            false -> ?DEFAULT_LAUNCH_PORT_OPTIONS
        end,
    ?LOG_DEBUG("default options ~tp", [DefaultOptions]),
    LaunchSettings = [
        {args, LaunchArgs},
        {env, LaunchEnv},
        {cd, LaunchCD}
        | PortSettings1 ++ DefaultOptions
    ],

    %% start the node
    ?LOG_DEBUG(
        io_lib:format("Launching ~tp ~tp ~n with env variables ~tp ~n", [
            Executable,
            LaunchArgs,
            LaunchEnv
        ])
    ),

    erlang:open_port({spawn_executable, Executable}, LaunchSettings).

-spec generate_arg_tuple(atom(), [] | term()) -> [binary()].
generate_arg_tuple(_Prop, []) ->
    [];
generate_arg_tuple(Prop, ConfigFiles) ->
    [unicode_characters_to_binary(io_lib:format("~tp", [{Prop, ConfigFiles}]))].

-spec config_arg(ConfigFiles) -> [string() | binary()] when
    ConfigFiles :: [binary() | string()].
config_arg([]) -> [];
config_arg(ConfigFiles) -> [~"-config" | ConfigFiles].

-doc """
Create a set up a home dir in the output directory.
Each test execution will have a separate home dir with a
erlang default cookie file, setting the default cookie to
buck2-test-runner-cookie
""".
-spec set_home_dir(file:filename_all()) -> file:filename_all().
set_home_dir(OutputDir) ->
    HomeDir = filename:join(OutputDir, "HOME"),
    ErlangCookieFile = filename:join(HomeDir, ".erlang.cookie"),
    ok = filelib:ensure_dir(ErlangCookieFile),
    ok = file:write_file(ErlangCookieFile, atom_to_list(cookie()), [raw, binary]),
    ok = file:change_mode(ErlangCookieFile, 8#00400),

    % In case the system is using dotslash, we leave a symlink to
    % the real dotslash cache, otherwise erl could be re-downloaded, etc
    try_setup_dotslash_cache(HomeDir),

    HomeDir.

-spec try_setup_dotslash_cache(FakeHomeDir :: file:filename_all()) -> ok.
try_setup_dotslash_cache(FakeHomeDir) ->
    case init:get_argument(home) of
        {ok, [[RealHomeDir]]} ->
            RealDotslashCacheDir = filename:basedir(user_cache, "dotslash"),

            case filelib:is_file(RealDotslashCacheDir, ?raw_file_access) of
                false ->
                    ok;
                true ->
                    RealHomeDirParts = filename:split(RealHomeDir),
                    RealDotslashCacheDirParts = filename:split(RealDotslashCacheDir),

                    case lists:split(length(RealHomeDirParts), RealDotslashCacheDirParts) of
                        {RealHomeDirParts, GenDotslashCacheDirParts} ->
                            FakeHomeDotslashCacheDir = filename:join([FakeHomeDir | GenDotslashCacheDirParts]),
                            ok = filelib:ensure_path(filename:dirname(FakeHomeDotslashCacheDir)),
                            ok = file:make_symlink(RealDotslashCacheDir, FakeHomeDotslashCacheDir),
                            ok;
                        _ ->
                            ok
                    end
            end;
        _ ->
            ok
    end.

-spec cookie() -> atom().
cookie() ->
    'buck2-test-runner-cookie'.

-spec project_root() -> file:filename().
project_root() ->
    {ok, CWD} = file:get_cwd(),
    Command = "buck2 root --kind=project",
    Dir = string:trim(os:cmd(Command)),
    ?LOG_INFO(#{command => Command, result => Dir, cwd => CWD}),
    case filelib:is_dir(Dir, ?raw_file_access) of
        true ->
            Dir;
        false ->
            {ok, FileInfo} = file:read_file_info(Dir),
            ?LOG_ERROR(#{directory => Dir, stat => FileInfo}),
            error({project_root_not_found, Dir})
    end.
