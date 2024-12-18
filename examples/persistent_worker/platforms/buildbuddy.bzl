# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _platforms(ctx):
    constraints = dict()
    constraints.update(ctx.attrs.cpu_configuration[ConfigurationInfo].constraints)
    constraints.update(ctx.attrs.os_configuration[ConfigurationInfo].constraints)
    configuration = ConfigurationInfo(constraints = constraints, values = {})

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = configuration,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = True,
            remote_cache_enabled = True,
            allow_cache_uploads = True,
            use_limited_hybrid = True,
            use_persistent_workers = ctx.attrs.use_persistent_workers,
            use_remote_persistent_workers = ctx.attrs.use_persistent_workers,
            remote_execution_properties = {
                "OSFamily": "Linux",
                "nonroot-workspace": True,
                "recycle-runner": True,  # required for remote persistent workers
            },
            remote_execution_use_case = "buck2-default",
            remote_output_paths = "output_paths",
        ),
    )

    return [DefaultInfo(), ExecutionPlatformRegistrationInfo(platforms = [platform])]

buildbuddy = rule(
    attrs = {
        "cpu_configuration": attrs.dep(providers = [ConfigurationInfo]),
        "os_configuration": attrs.dep(providers = [ConfigurationInfo]),
        "use_persistent_workers": attrs.bool(default = False),
    },
    impl = _platforms,
)
