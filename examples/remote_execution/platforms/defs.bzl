# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _platforms(ctx):
    configuration = ConfigurationInfo(
        constraints = {},
        values = {},
    )

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = configuration,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = True,
            use_limited_hybrid = True,
            remote_execution_properties = {
                "platform": "linux-remote-execution",
            },
            remote_execution_use_case = "buck2-default",
            remote_output_paths = "strict",
        ),
    )

    return [DefaultInfo(), ExecutionPlatformRegistrationInfo(platforms = [platform])]

platforms = rule(attrs = {}, impl = _platforms)
