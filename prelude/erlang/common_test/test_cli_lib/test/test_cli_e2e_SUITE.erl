%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.
%%% % @format
-module(test_cli_e2e_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([
    test_list/1
]).

all() ->
    [test_list].

test_list(_Config) ->
    Expected =
        "test_cli_e2e_SUITE:\n"
        "test_list_SUITE:\n"
        "\t1 - test_list_SUITE - .test_pass\n"
        "\t2 - test_list_SUITE - default.test_fail\n",
    ?assertEqual({ok, Expected}, test:list_impl("test_list_SUITE")),

    ?assertMatch({error, {invalid_regex, _}}, test:list_impl("^[a")),

    EmptyExpected =
        "test_cli_e2e_SUITE:\n"
        "test_list_SUITE:\n",
    ?assertEqual({ok, EmptyExpected}, test:list_impl("does_not_exist_SUITE")).
