# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _is_universal(platform: PlatformInfo, refs: struct) -> bool:
    universal = platform.configuration.constraints.get(refs.universal[ConstraintSettingInfo].label)
    return universal.label == refs.universal_enabled[ConstraintValueInfo].label if universal != None else False

def _impl(platform: PlatformInfo, refs: struct, attrs: struct) -> PlatformInfo:
    if attrs.skip_universal_resource_dedupe or not _is_universal(platform, refs):
        return platform
    else:
        cpu_constraint_label = refs.cpu[ConstraintSettingInfo].label
        universal_constraint_label = refs.universal[ConstraintSettingInfo].label
        filtered_constraints = {
            constraint_setting_label: constraint_setting_value
            for (constraint_setting_label, constraint_setting_value) in platform.configuration.constraints.items()
            if constraint_setting_label != cpu_constraint_label and constraint_setting_label != universal_constraint_label
        }
        return PlatformInfo(
            label = "apple_universal_deduped_resource",
            configuration = ConfigurationInfo(
                constraints = filtered_constraints,
                values = platform.configuration.values,
            ),
        )

apple_resource_transition = transition(
    impl = _impl,
    refs = {
        "cpu": "config//cpu/constraints:cpu",
        "universal": "config//build_mode/apple/constraints:universal",
        "universal_enabled": "config//build_mode/apple/constraints:universal-enabled",
    },
    attrs = [
        "skip_universal_resource_dedupe",
    ],
)
