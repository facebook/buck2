# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        LocalResourceInfo(
            setup = cmd_args([ctx.attrs.broker[RunInfo]] + ctx.attrs.args),
            resource_env_vars = {
                "ANDROID_SERIAL": "serial_number",
            },
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "android_emulators",
    impl = _impl,
    attrs = {
        "args": attrs.list(attrs.arg(), default = []),
        "broker": attrs.exec_dep(providers = [RunInfo]),
    },
)
