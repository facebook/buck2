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
                "IDB_COMPANION": "socket_address",
            },
            setup_timeout_seconds = ctx.attrs.setup_timeout_seconds,
        ),
    ]

# We don't want `apple_simulators` target to be configured differently and handled as a different resource broker by buck2 core.
# By nuking a platform we make sure there is only a single configured target for a resource broker which manages resources of certain type.
def _transition_impl(platform: PlatformInfo, refs: struct) -> PlatformInfo:
    # buildifier: disable=unused-variable
    _ = (platform, refs)
    return PlatformInfo(
        label = "apple_simulators",
        configuration = ConfigurationInfo(
            constraints = {},
            values = {},
        ),
    )

apple_simulators_transition = transition(
    impl = _transition_impl,
    refs = {},
)

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
