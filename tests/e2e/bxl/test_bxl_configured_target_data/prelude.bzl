# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _platform(ctx):
    constraints = {}
    values = {}
    for x in ctx.attrs.constraint_values:
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

    return [DefaultInfo(), platform, PlatformInfo(label = str(ctx.label.raw_target()), configuration = cfg)]

platform = rule(
    impl = _platform,
    attrs = {"constraint_values": attrs.list(attrs.dep(providers = [ConfigurationInfo]))},
)
