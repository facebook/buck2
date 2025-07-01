# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def apple_simulators_impl(ctx: AnalysisContext) -> list[Provider]:
    cmd_args_list = [ctx.attrs.broker[RunInfo], "--simulator-manager", ctx.attrs.idb_targets[RunInfo]] + ctx.attrs.args
    simulator_os_version = ctx.attrs._simulator_os_version
    simulator_device = ctx.attrs._simulator_device
    if simulator_os_version:
        cmd_args_list += ["--os_version", simulator_os_version]
    if simulator_device:
        cmd_args_list += ["--device", simulator_device]
    return [
        DefaultInfo(),
        LocalResourceInfo(
            setup = cmd_args(cmd_args_list),
            resource_env_vars = {
                "DEVICE_SET_PATH": "device_set_path",
                "DEVICE_UDID": "udid",
            },
            setup_timeout_seconds = ctx.attrs.setup_timeout_seconds,
        ),
    ]
