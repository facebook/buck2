# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _rule_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        LocalResourceInfo(
            setup = cmd_args([ctx.attrs.broker[RunInfo], "--simulator-manager", ctx.attrs.idb_targets[RunInfo]] + ctx.attrs.args),
            resource_env_vars = {
                "DEVICE_SET_PATH": "device_set_path",
                "DEVICE_UDID": "udid",
            },
            setup_timeout_seconds = ctx.attrs.setup_timeout_seconds,
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "apple_simulators",
    impl = _rule_impl,
    attrs = {
        "args": attrs.list(attrs.string(), default = []),
        "broker": attrs.exec_dep(providers = [RunInfo]),
        "idb_targets": attrs.exec_dep(providers = [RunInfo]),
        "setup_timeout_seconds": attrs.option(attrs.int(), default = None),
    },
)
