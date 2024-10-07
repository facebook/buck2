# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(_ctx):
    fail()

suite = rule(
    impl = _impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "toolchain": attrs.option(attrs.toolchain_dep(), default = None),
    },
)

toolchain = rule(
    impl = _impl,
    is_toolchain_rule = True,
    attrs = {
        "exec_deps": attrs.list(attrs.exec_dep()),
    },
)

def exec_platforms_impl(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [
                ExecutionPlatformInfo(
                    label = ctx.label.raw_target(),
                    configuration = ConfigurationInfo(constraints = {}, values = {}),
                    executor_config = CommandExecutorConfig(local_enabled = True, remote_enabled = False),
                ),
            ],
        ),
    ]

execution_platforms = rule(
    impl = exec_platforms_impl,
    attrs = {},
)
