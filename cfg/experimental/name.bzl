# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

NAMED_CONSTRAINT_SETTINGS = [
    # TODO(scottcao): Add OSS constraints as well
    "ovr_config//os/constraints:os",
    "ovr_config//cpu/constraints:cpu",
    "ovr_config//runtime/constraints:runtime",
    "ovr_config//runtime/constraints:runtime_version",
    "ovr_config//os/sdk/apple/constraints:_",
    "ovr_config//os/sdk/android/ndk/constraints:version",
    "ovr_config//build_mode/constraints:san",
]

def cfg_name(cfg: ConfigurationInfo) -> str:
    """Derives a reasonable name for a ConfigurationInfo"""

    name_list = []
    constraints = {str(key): value for key, value in cfg.constraints.items()}
    for constraint_setting in NAMED_CONSTRAINT_SETTINGS:
        if constraint_setting in constraints:
            name_list.append(constraints[constraint_setting].label.name)
    return "-".join(name_list)
