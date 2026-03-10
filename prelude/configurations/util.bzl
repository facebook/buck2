# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _configuration_info_union(infos):
    """Merge multiple ConfigurationInfo into one.

    When the same constraint_setting appears in multiple infos, later values
    override earlier ones. This is intentional behavior that allows platforms
    to specialize constraints from their dependencies.

    Args:
        infos: A list of ConfigurationInfo to merge.

    Returns:
        A merged ConfigurationInfo.
    """
    if len(infos) == 0:
        return ConfigurationInfo(
            constraints = {},
            values = {},
        )
    if len(infos) == 1:
        return infos[0]

    constraints = {k: v for info in infos for (k, v) in info.constraints.items()}
    values = {k: v for info in infos for (k, v) in info.values.items()}
    return ConfigurationInfo(
        constraints = constraints,
        values = values,
    )

def _constraint_values_to_configuration(values, fail_on_duplicates = False):
    """Convert a list of constraint_value deps to a ConfigurationInfo.

    Args:
        values: A list of targets that provide ConstraintValueInfo.
        fail_on_duplicates: If True, fail() when multiple constraint_values
            target the same constraint_setting. This catches bugs where the
            same constraint_setting has multiple conflicting values in a
            single list (which is always a mistake).

    Returns:
        A ConfigurationInfo containing the constraints from the values.
    """
    if fail_on_duplicates:
        seen_settings = {}  # constraint_setting label -> ConstraintValueInfo
        for cv_target in values:
            cv_info = cv_target[ConstraintValueInfo]
            setting_label = cv_info.setting.label
            if setting_label in seen_settings:
                existing_cv = seen_settings[setting_label]
                if existing_cv.label != cv_info.label:
                    fail("Conflicting constraint_values for constraint_setting {}:\n  {}\n  {}\nMultiple constraint_values for the same constraint_setting are not allowed.".format(
                        setting_label,
                        existing_cv.label,
                        cv_info.label,
                    ))
            seen_settings[setting_label] = cv_info

    return ConfigurationInfo(constraints = {
        info[ConstraintValueInfo].setting.label: info[ConstraintValueInfo]
        for info in values
    }, values = {})

util = struct(
    configuration_info_union = _configuration_info_union,
    constraint_values_to_configuration = _constraint_values_to_configuration,
)
