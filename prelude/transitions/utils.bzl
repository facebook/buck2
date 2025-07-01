# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def filtered_platform_constraints(platform: PlatformInfo, constraint_settings_labels_to_remove: list[TargetLabel]) -> dict[TargetLabel, ConstraintValueInfo]:
    return {
        constraint_setting_label: constraint_setting_value
        for (constraint_setting_label, constraint_setting_value) in platform.configuration.constraints.items()
        if constraint_setting_label not in constraint_settings_labels_to_remove
    }

def get_constraint_value(platform: PlatformInfo, constraint: ConstraintSettingInfo) -> [None, ConstraintValueInfo]:
    return platform.configuration.constraints.get(constraint.label)

utils = {
    "filtered_platform_constraints": filtered_platform_constraints,
    "get_constraint_value": get_constraint_value,
}
