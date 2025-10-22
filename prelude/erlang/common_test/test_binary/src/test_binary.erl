%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(test_binary).
-compile(warn_missing_spec_all).

-export([main/0]).

-include_lib("common/include/buck_ct_records.hrl").
-include_lib("common/include/tpx_records.hrl").
-include_lib("kernel/include/logger.hrl").

-spec main() -> no_return().
main() ->
    Args = init:get_plain_arguments(),
    run_action_and_halt(fun() -> argparse:run(Args, cli(), #{}) end).

-spec cli() -> argparse:command().
cli() ->
    #{
        handler => fun handle_list_and_run/1,
        arguments => [
            #{
                name => test_info_file,
                long => "-test-info-file",
                type => string,
                required => true
            },
            #{
                name => edb_code,
                long => "-edb-code",
                type => string,
                default => none,
                help =>
                    "EDB code to inject for debugging. Can be actual code or the environment variable that contains the code."
            }
        ],
        commands => #{
            "list" => #{
                handler => fun handle_list/1,
                arguments => [
                    #{name => output_dir, long => "-output-dir", type => string, required => true}
                ]
            },
            "run" => #{
                handler => fun handle_run/1,
                arguments => [
                    #{name => output_dir, long => "-output-dir", type => string, required => true},
                    #{name => tests, type => string, nargs => list, default => []}
                ]
            }
        }
    }.

-type list_args() :: #{
    test_info_file := file:filename(),
    output_dir := file:filename()
}.

-type run_args() :: #{
    test_info_file := file:filename(),
    output_dir := file:filename(),
    tests := [string()],
    edb_code := string() | none
}.

-type list_and_run_args() :: #{
    test_info_file := file:filename(),
    edb_code := string() | none
}.

-spec handle_list(Args) -> ok when
    Args :: list_args().
handle_list(Args) ->
    #{output_dir := OutputDir} = Args,
    test_logger:set_up_logger(OutputDir, test_listing),
    ok = listing(Args),
    ?LOG_DEBUG("Listing done"),
    ok.

-spec handle_run(Args) -> ok when
    Args :: run_args().
handle_run(Args) ->
    #{output_dir := OutputDir} = Args,
    test_logger:set_up_logger(OutputDir, test_runner),
    ok = running(Args),
    ?LOG_DEBUG("Running done"),
    ok.

-spec handle_list_and_run(Args) -> ok | {exit_code, 1} when
    Args :: list_and_run_args().
handle_list_and_run(Args) ->
    %% without test runner support we run all tests and need to create our own test dir
    OutputDir = string:trim(os:cmd("mktemp -d")),
    test_logger:set_up_logger(OutputDir, test_runner, true),
    case list_and_run(Args, OutputDir) of
        true ->
            io:format("~nAt least one test didn't pass!~nYou can find the test output directory here: ~ts~n", [
                OutputDir
            ]),
            {exit_code, 1};
        false ->
            ok
    end.

-spec run_action_and_halt(Action) -> no_return() when
    Action :: fun(() -> ok | {exit_code, non_neg_integer()}).
run_action_and_halt(Action) ->
    ExitCode =
        try Action() of
            ok -> 0;
            {exit_code, N} when is_integer(N) -> N
        catch
            Class:Reason:StackTrace ->
                ?LOG_ERROR(erl_error:format_exception(Class, Reason, StackTrace)),
                1
        after
            test_logger:flush()
        end,
    erlang:halt(ExitCode).

-spec load_suite(binary()) -> atom().
load_suite(SuitePath) ->
    Path = filename:rootname(filename:absname(SuitePath)),
    {module, Module} = code:load_abs(filename_all_to_filename(Path)),
    Module.

-spec get_hooks(#test_info{}) -> [module()].
get_hooks(TestInfo) ->
    Hooks = lists:append(proplists:get_all_values(ct_hooks, TestInfo#test_info.ct_opts)),
    [
        case HookSpec of
            {HookModule, _InitArguments} when is_atom(HookModule) -> HookModule;
            {HookModule, _InitArguments, Priority} when is_atom(HookModule), is_integer(Priority) -> HookModule;
            HookModule when is_atom(HookModule) -> HookModule
        end
     || HookSpec <- Hooks
    ].

-spec listing(Args) -> ok when
    Args :: list_args().
listing(Args) ->
    #{test_info_file := TestInfoFile, output_dir := OutputDir} = Args,
    TestInfo = test_info:load_from_file(TestInfoFile),
    Listing = get_listing(TestInfo, OutputDir),
    listing_interfacer:produce_xml_file(OutputDir, Listing).

-spec running(Args) -> ok when
    Args :: run_args().
running(Args) ->
    #{
        test_info_file := TestInfoFile,
        output_dir := OutputDir,
        tests := Tests,
        edb_code := EdbCode
    } = Args,
    AbsOutputDir = filename:absname(OutputDir),
    TestInfo0 = test_info:load_from_file(TestInfoFile),
    Listing = get_listing(TestInfo0, AbsOutputDir),
    ExtraEmuFlags = edb_extra_emu_flags(EdbCode),
    TestInfo1 = TestInfo0#test_info{extra_flags = ExtraEmuFlags ++ TestInfo0#test_info.extra_flags},
    test_runner:run_tests(Tests, TestInfo1, AbsOutputDir, Listing).

-spec get_listing(TestInfo, OutputDir) -> #test_spec_test_case{} when
    TestInfo :: #test_info{},
    OutputDir :: file:filename_all().
get_listing(TestInfo, OutputDir) ->
    code:add_paths(TestInfo#test_info.dependencies),
    Suite = load_suite(TestInfo#test_info.test_suite),

    {ok, ProjectRoot} = file:get_cwd(),
    true = os:putenv("PROJECT_ROOT", ProjectRoot),

    InitProviderState = #init_provider_state{
        output_dir = filename_all_to_filename(OutputDir),
        suite = Suite,
        raw_target = TestInfo#test_info.raw_target
    },
    Providers0 = [
        buck_ct_provider:do_init(Provider, InitProviderState)
     || Provider <- TestInfo#test_info.providers
    ],
    HookModules = get_hooks(TestInfo),
    Providers1 = [buck_ct_provider:do_pre_listing(Provider) || Provider <- Providers0],
    Listing = list_test:list_tests(Suite, HookModules),
    Providers2 = [buck_ct_provider:do_post_listing(Provider) || Provider <- Providers1],
    [buck_ct_provider:do_terminate(Provider) || Provider <- Providers2],
    Listing.

%% rudimantary implementation for running tests with buck2 open-sourced test runner

-spec list_and_run(Args, OutputDir) -> boolean() when
    Args :: list_and_run_args(),
    OutputDir :: file:filename().
list_and_run(Args, OutputDir) ->
    os:putenv("ERLANG_BUCK_DEBUG_PRINT", "disabled"),
    #{test_info_file := TestInfoFile} = Args,
    TestInfo = test_info:load_from_file(TestInfoFile),
    Listing = get_listing(TestInfo, OutputDir),
    Tests = listing_to_testnames(Listing),
    running(Args#{output_dir => OutputDir, tests => Tests}),
    ResultsFile = filename:join(OutputDir, "result_exec.json"),
    print_results(ResultsFile).

-spec listing_to_testnames(#test_spec_test_case{}) -> [string()].
listing_to_testnames(Listing) ->
    [
        case unicode:characters_to_list(TestCase#test_spec_test_info.name) of
            Name when is_list(Name) -> Name
        end
     || TestCase <- Listing#test_spec_test_case.testcases
    ].

-spec edb_extra_emu_flags(EdbCode) -> [binary()] when
    EdbCode :: string() | none.
edb_extra_emu_flags(none) ->
    [];
edb_extra_emu_flags(EdbCode) ->
    CodeToInject =
        case os:getenv(EdbCode) of
            false -> unicode_characters_to_binary(EdbCode);
            Value -> unicode_characters_to_binary(Value)
        end,
    [~"-eval", CodeToInject].

-spec print_results(file:filename()) -> boolean().
print_results(ResultsFile) ->
    {ok, Data} = file:read_file(ResultsFile, [raw]),
    Results = json:decode(Data),
    {Summary, AnyFailure} = lists:foldl(fun print_individual_results/2, {#{}, false}, Results),
    io:format("~n~10s: ~b~n~n", ["TOTAL", lists:sum(maps:values(Summary))]),
    [
        io:format("~10ts: ~b~n", [json_interfacer:status_name(Result), Amount])
     || Result := Amount <- Summary
    ],
    AnyFailure.

-spec print_individual_results(map(), Acc) -> Acc when Acc :: {#{non_neg_integer() => non_neg_integer()}, boolean()}.
print_individual_results(Result, {Summary, AnyFailure}) ->
    #{<<"main">> := #{<<"details">> := Details, <<"status">> := Status, <<"std_out">> := StdOut}} = Result,
    NewAnyFailure =
        case json_interfacer:status_name(Status) of
            passed ->
                AnyFailure;
            skipped ->
                print_details(StdOut, Details),
                AnyFailure;
            omitted ->
                AnyFailure;
            _NotPassed ->
                print_details(StdOut, Details),
                true
        end,
    {Summary#{Status => maps:get(Status, Summary, 0) + 1}, NewAnyFailure}.

-spec print_details(StdOut, Details) -> ok when
    StdOut :: unicode:chardata(),
    Details :: unicode:chardata().
print_details(StdOut, Details) ->
    io:format("~ts~n", [StdOut]),
    io:format("~ts~n", [Details]),
    io:format("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -~n"),
    io:format("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -~n").

-spec filename_all_to_filename(file:filename_all()) -> file:filename().
filename_all_to_filename(Filename) when is_binary(Filename) ->
    case unicode:characters_to_list(Filename) of
        FilenameStr when is_list(FilenameStr) -> FilenameStr
    end;
filename_all_to_filename(Filename) ->
    Filename.

-spec unicode_characters_to_binary(unicode:chardata()) -> binary().
unicode_characters_to_binary(Chars) ->
    case unicode:characters_to_binary(Chars) of
        Bin when is_binary(Bin) -> Bin
    end.
