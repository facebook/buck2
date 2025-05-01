# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _execution_platform(ctx):
    constraints = {}
    values = {}
    for x in ctx.attrs.configuration:
        constraints |= x[ConfigurationInfo].constraints
        values |= x[ConfigurationInfo].values
    cfg = ConfigurationInfo(constraints = constraints, values = values)

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = cfg,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = False,
        ),
    )

    return [DefaultInfo(), platform]

execution_platform = rule(
    impl = _execution_platform,
    is_configuration_rule = True,
    attrs = {"configuration": attrs.list(attrs.dep(providers = [ConfigurationInfo]))},
)

def _execution_platforms(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [x[ExecutionPlatformInfo] for x in ctx.attrs.platforms],
        ),
    ]

execution_platforms = rule(
    impl = _execution_platforms,
    is_configuration_rule = True,
    attrs = {"platforms": attrs.list(attrs.dep(providers = [ExecutionPlatformInfo]))},
)
