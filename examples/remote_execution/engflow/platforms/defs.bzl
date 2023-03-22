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

    # This could be any Docker image.
    # This one is Ubuntu 22.04 (i.e. its current LTS release): https://hub.docker.com/_/ubuntu
    image = "docker://ubuntu@sha256:b2175cd4cfdd5cdb1740b0e6ec6bbb4ea4892801c0ad5101a81f694152b6c559"
    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = configuration,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = True,
            use_limited_hybrid = True,
            remote_execution_properties = {
                "container-image": image,
            },
            remote_execution_use_case = "buck2-default",
            # TODO: Use output_paths
            remote_output_paths = "strict",
        ),
    )

    return [DefaultInfo(), ExecutionPlatformRegistrationInfo(platforms = [platform])]

platforms = rule(attrs = {}, impl = _platforms)
