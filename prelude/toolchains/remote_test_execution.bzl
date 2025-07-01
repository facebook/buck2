# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//decls:re_test_common.bzl", "re_test_common")
load("@prelude//tests:remote_test_execution_toolchain.bzl", "RemoteTestExecutionToolchainInfo")
load("@prelude//utils:utils.bzl", "map_val")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    default_profile = map_val(ctx.attrs.profiles.get, ctx.attrs.default_profile)
    if ctx.attrs.default_run_as_bundle != None:
        default_run_as_bundle = ctx.attrs.default_run_as_bundle
    else:
        default_run_as_bundle = bool(default_profile)

    return [
        DefaultInfo(),
        RemoteTestExecutionToolchainInfo(
            default_profile = default_profile,
            default_run_as_bundle = default_run_as_bundle,
            profiles = ctx.attrs.profiles,
        ),
    ]

remote_test_execution_toolchain = rule(
    impl = _impl,
    is_toolchain_rule = True,
    attrs = {
        "default_profile": attrs.option(attrs.string(), default = None),
        "default_run_as_bundle": attrs.option(attrs.bool(), default = None),
        "profiles": attrs.dict(
            key = attrs.string(),
            value = attrs.option(re_test_common.opts_for_tests_arg()),
            default = {},
        ),
    },
)
