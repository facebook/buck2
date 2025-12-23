%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.
%%% % @format
-module(test_cli_e2e_SUITE).
-typing([eqwalizer]).

% elp:ignore WA003 (better_assertions) - Open Source
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("common/include/buck_ct_records.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

-export([
    test_list/1
]).

suite() ->
    [{appatic, #{enable_autoclean => true}}].

all() ->
    [test_list].

init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    TestInfoFile = filename:join(PrivDir, <<"test_info">>),

    {ok, [ErlCmd]} = init:get_argument(progname),

    TestSuitePath = [_ | _] = code:which(test_list_SUITE),
    test_info:write_to_file(TestInfoFile, #test_info{
        common_app_env = #{},
        raw_target = ~"raw_target",
        dependencies = [],
        test_suite = list_to_binary(TestSuitePath),
        config_files = [],
        providers = [],
        ct_opts = [],
        erl_cmd = lists:map(fun list_to_binary/1, ErlCmd),
        extra_flags = [],
        artifact_annotation_mfa = fun artifact_annotations:default_annotation/1,
        trampolines = []
    }),

    application:set_env(test_cli_lib, test_info_file, TestInfoFile),

    Config.

end_per_suite(_Config) ->
    ok.

test_list(_Config) ->
    Expected =
        "test_cli_e2e_SUITE:\n"
        "test_list_SUITE:\n"
        "\t1 - test_list_SUITE - .test_pass\n"
        "\t2 - test_list_SUITE - default.test_fail\n"
        "\t3 - test_list_SUITE - .test_extended_ascii_£\n"
        "\t4 - test_list_SUITE - .test_unicode_🫠\n",
    ?assertEqual({ok, Expected}, test:list_impl("test_list_SUITE")),

    EmojiExpected =
        "test_cli_e2e_SUITE:\n"
        "test_list_SUITE:\n"
        "\t4 - test_list_SUITE - .test_unicode_🫠\n",
    ?assertEqual({ok, EmojiExpected}, test:list_impl("_🫠")),

    ?assertMatch({error, {invalid_regex, _}}, test:list_impl("^[a")),

    EmptyExpected =
        "test_cli_e2e_SUITE:\n"
        "test_list_SUITE:\n",
    ?assertEqual({ok, EmptyExpected}, test:list_impl("does_not_exist_SUITE")).
