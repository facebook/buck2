%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format

-record(test_info, {
    dependencies :: [file:filename()],
    test_suite :: binary(),
    config_files :: [binary()],
    providers :: [{atom(), [term()]}],
    ct_opts :: [term()],
    erl_cmd :: [binary()],
    extra_flags :: [string()],
    common_app_env :: #{string() => string()},
    artifact_annotation_mfa :: artifact_annotations:annotation_function(),
    raw_target :: binary()
}).

-record(ct_test, {suite, groups, test_name, canonical_name}).

-record(test_env, {
    suite :: module(),
    tests :: [#ct_test{}],
    suite_path :: file:filename_all(),
    output_dir :: file:filename_all(),
    dependencies :: [file:filename_all()],
    test_spec_file :: file:filename_all() | undefined,
    output_format :: xml | json,
    config_files :: [file:filename_all()],
    providers :: [{module(), [term()]}],
    ct_opts :: [term()],
    common_app_env :: #{string() => string()},
    erl_cmd :: [binary()],
    extra_flags :: [string()],
    artifact_annotation_mfa :: artifact_annotations:annotation_function(),
    raw_target :: binary() | undefined
}).

-record(run_specs, {
    mfa :: {function(), [term()]} | {atom(), atom(), [term()]},
    timeout :: integer(),
    schedulers :: integer(),
    code_path = [] :: [file:filename()]
}).

-record(init_provider_state, {output_dir :: file:filename(), suite :: module(), raw_target :: binary()}).
