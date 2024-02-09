%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.
%%% % @format
-module(ct_executor_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).

-export([
    test_split_args/1
]).

all() ->
    [test_split_args].

test_split_args(_Config) ->
    ?assertEqual(
        {[{output_dir, ""}, {providers, [something]}, {suite, a_suite}], [{dir, ""}, {suite, a_suite}, {group, a_group}]},
        ct_executor:split_args([
            {output_dir, ""},
            {providers, [something]},
            {suite, a_suite},
            ct_args,
            {dir, ""},
            {suite, a_suite},
            {group, a_group}
        ])
    ),
    ?assertEqual(
        {[{output_dir, ""}, {providers, [something]}, {suite, a_suite}], []},
        ct_executor:split_args([{output_dir, ""}, {providers, [something]}, {suite, a_suite}, ct_args])
    ),
    ?assertEqual(
        {[], [{dir, ""}, {suite, a_suite}, {group, a_group}]},
        ct_executor:split_args([ct_args, {dir, ""}, {suite, a_suite}, {group, a_group}])
    ),
    ?assertEqual({[], []}, ct_executor:split_args([ct_args])).
