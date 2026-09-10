%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(cth_skip_cases).
-compile(warn_missing_spec_all).

%% Callbacks
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([pre_init_per_group/4]).
-export([post_init_per_group/5]).
-export([post_end_per_group/5]).

-export([pre_init_per_testcase/4]).

-type ct_suite() :: module().
-type ct_groupname() :: ct_suite:ct_groupname().
-type ct_testname() :: ct_suite:ct_testname().
-type ct_config() :: ct_suite:ct_config().
-type ct_config_or_skip_or_fail() ::
    ct_config() | {skip, term()} | {fail, term()}.
-type ct_config_or_skip_or_fail_or_term() ::
    ct_config_or_skip_or_fail() | ok | term().

-type qualified_name() :: string().
-type reason() :: unicode:chardata().
-type skip_spec() :: {qualified_name(), reason()}.

-type state() :: #{
    skips := [skip_spec()],
    groups := cth_tpx_test_tree:group_path()
}.

-doc """
Skips suite, group, or testcase callbacks whose qualified name matches one of
the configured regular expressions, each with its own reason.

`pre_init_per_suite` matches against `""`, `pre_init_per_group` matches the
qualified group name with an empty testcase, and `pre_init_per_testcase`
matches the fully-qualified testcase name that
`cth_tpx_test_tree:qualified_name/2` reports. The first matching pattern wins.
""".
-spec init(Id, Opts) -> {ok, state()} when
    Id :: term(),
    Opts :: [skip_spec()].
init(_Id, Opts) ->
    {ok, #{skips => Opts, groups => []}}.

-spec pre_init_per_suite(_Suite, Config, State) -> {ct_config_or_skip_or_fail(), state()} when
    _Suite :: ct_suite(),
    Config :: ct_config(),
    State :: state().
pre_init_per_suite(_Suite, Config, State = #{skips := Skips}) ->
    case find_skip("", Skips) of
        {ok, Reason} ->
            {{skip, Reason}, State};
        error ->
            {Config, State}
    end.

-spec pre_init_per_group(_Suite, Group, Config, State) -> {ct_config_or_skip_or_fail(), state()} when
    _Suite :: ct_suite(),
    Group :: ct_groupname(),
    Config :: ct_config(),
    State :: state().
pre_init_per_group(_Suite, Group, Config, State = #{skips := Skips, groups := Groups}) ->
    State1 = State#{groups := [Group | Groups]},
    case find_skip(cth_tpx_test_tree:qualified_name([Group | Groups], ""), Skips) of
        {ok, Reason} ->
            {{skip, Reason}, State1};
        error ->
            {Config, State1}
    end.

-spec post_init_per_group(
    ct_suite(), ct_groupname(), ct_config(), ct_config_or_skip_or_fail_or_term(), state()
) ->
    {ct_config_or_skip_or_fail_or_term(), state()}.
post_init_per_group(_Suite, Group, _Config, Return, State) when not is_list(Return) ->
    {Return, pop_group(Group, State)};
post_init_per_group(_Suite, _Group, _Config, Return, State) ->
    {Return, State}.

-spec post_end_per_group(
    ct_suite(), ct_groupname(), ct_config(), ct_config_or_skip_or_fail_or_term(), state()
) ->
    {ct_config_or_skip_or_fail_or_term(), state()}.
post_end_per_group(_Suite, Group, _Config, Return, State) ->
    {Return, pop_group(Group, State)}.

-spec pre_init_per_testcase
    (_Suite, _TestCase, SkipOrFail, State) -> {ct_config_or_skip_or_fail(), state()} when
        _Suite :: ct_suite(),
        _TestCase :: ct_testname(),
        SkipOrFail :: ct_config_or_skip_or_fail(),
        State :: state();
    (_Suite, TestCase, Config, State) -> {ct_config_or_skip_or_fail(), state()} when
        _Suite :: ct_suite(),
        TestCase :: ct_testname(),
        Config :: ct_config(),
        State :: state().
pre_init_per_testcase(_Suite, _TestCase, {Tag, _Reason} = SkipOrFail, State) when
    Tag =:= skip; Tag =:= fail
->
    {SkipOrFail, State};
pre_init_per_testcase(_Suite, TestCase, Config, State = #{skips := Skips, groups := Groups}) ->
    case find_skip(cth_tpx_test_tree:qualified_name(Groups, TestCase), Skips) of
        {ok, Reason} ->
            {{skip, Reason}, State};
        error ->
            {Config, State}
    end.

-spec find_skip(Name, Skips) -> {ok, Reason} | error when
    Name :: qualified_name(),
    Skips :: [skip_spec()],
    Reason :: reason().
find_skip(_Name, []) ->
    error;
find_skip(Name, [{Pattern, Reason} | Rest]) ->
    case re:run(Name, Pattern, [{capture, none}]) of
        match -> {ok, Reason};
        nomatch -> find_skip(Name, Rest)
    end.

-spec pop_group(ct_groupname(), state()) -> state().
pop_group(Group, State = #{groups := [Group | Groups]}) ->
    State#{groups := Groups};
pop_group(_Group, State) ->
    State.
