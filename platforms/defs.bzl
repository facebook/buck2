# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# See //:Dockerfile for more information
_DEFAULT_IMAGE = "docker://ghcr.io/sluongng/buck2-toolchains@sha256:51dbccd962018fe862322362d884b340e7386c6485d437e562b5f3c5f0e7cf80"

def _platforms(ctx):
    constraints = dict()
    constraints.update(ctx.attrs.cpu_configuration[ConfigurationInfo].constraints)
    constraints.update(ctx.attrs.os_configuration[ConfigurationInfo].constraints)
    cfg = ConfigurationInfo(constraints = constraints, values = {})
    name = ctx.label.raw_target()

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = cfg,
        executor_config = CommandExecutorConfig(
            # Note: There are 3 git_fetch targets with `local_only = True` set in their actions.
            # Currently we patch prelude and build a new buck2 binary with cargo to make them
            # remote-compatible. However, that does require external network access in the RBE
            # worker. In BuildBuddy, that's controlled by the `dockerNetwork` exec property below.
            #
            # If the RBE worker does not have the ability to access external network, we will
            # need to run the git_fetch targets locally.
            local_enabled = False,
            remote_enabled = True,
            use_limited_hybrid = False,
            remote_cache_enabled = True,
            allow_cache_uploads = True,
            remote_execution_properties = {
                "OSFamily": "Linux",
                "Arch": "amd64",
                "container-image": ctx.attrs.container_image,
                # Prefer BuildBuddy-managed executors
                "use-self-hosted-executors": "false",
                # Typically we disable external network access with "off" value for a performance boost.
                # However, we want to enable external network access here so that git_fetch action
                # can run remotely.
                "dockerNetwork": "bridge",
            },
            remote_execution_use_case = "buck2-default",
            remote_output_paths = "output_paths",
        ),
    )

    return [
        DefaultInfo(),
        platform,
        PlatformInfo(label = str(name), configuration = cfg),
        ExecutionPlatformRegistrationInfo(platforms = [platform]),
    ]

platforms = rule(
    impl = _platforms,
    attrs = {
        "container_image": attrs.string(default = _DEFAULT_IMAGE),
        "cpu_configuration": attrs.dep(providers = [ConfigurationInfo], default = "prelude//cpu:x86_64"),
        "os_configuration": attrs.dep(providers = [ConfigurationInfo], default = "prelude//os:linux"),
    },
)
