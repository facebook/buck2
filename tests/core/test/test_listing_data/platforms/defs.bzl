# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _execution_platforms(ctx):
    configuration = ConfigurationInfo(
        constraints = {
        },
        values = {},
    )

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = configuration,
        executor_config = CommandExecutorConfig(
            local_enabled = ctx.attrs.local_enabled,
            remote_enabled = ctx.attrs.remote_enabled,
            remote_cache_enabled = ctx.attrs.remote_cache_enabled,
            remote_execution_properties = {
                "platform": "linux-remote-execution",
            },
            remote_execution_use_case = "buck2-testing",
            allow_cache_uploads = ctx.attrs.allow_cache_uploads,
            max_cache_upload_mebibytes = 1,
        ),
    )

    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(platforms = [platform]),
    ]

execution_platforms = rule(attrs = {
    "allow_cache_uploads": attrs.bool(),
    "local_enabled": attrs.bool(),
    "remote_cache_enabled": attrs.option(attrs.bool(), default = None),
    "remote_enabled": attrs.bool(),
}, impl = _execution_platforms)
