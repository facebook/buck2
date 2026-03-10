# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _constraint_setting(ctx):
    return [DefaultInfo(), ConstraintSettingInfo(label = ctx.label.raw_target())]

constraint_setting = rule(
    impl = _constraint_setting,
    is_configuration_rule = True,
    attrs = {},
)

def _constraint_value(ctx):
    constraint_value = ConstraintValueInfo(
        setting = ctx.attrs.constraint_setting[ConstraintSettingInfo],
        label = ctx.label.raw_target(),
    )
    return [
        DefaultInfo(),
        constraint_value,
        # Provide `ConfigurationInfo` from `constraint_value` so it could be used as select key.
        ConfigurationInfo(constraints = {
            constraint_value.setting.label: constraint_value,
        }, values = {}),
    ]

constraint_value = rule(
    impl = _constraint_value,
    is_configuration_rule = True,
    attrs = {"constraint_setting": attrs.dep(providers = [ConstraintSettingInfo])},
)

def _platform_impl(ctx):
    # Check for duplicates within the platform's own constraint_values attribute.
    # This catches bugs where the same constraint_setting has multiple values.
    seen_settings = {}
    for cv in ctx.attrs.constraint_values:
        cv_info = cv[ConstraintValueInfo]
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

    constraints = {cv[ConstraintValueInfo].setting.label: cv[ConstraintValueInfo] for cv in ctx.attrs.constraint_values}
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            configuration = ConfigurationInfo(
                constraints = constraints,
                values = {},
            ),
        ),
    ]

platform = rule(
    impl = _platform_impl,
    is_configuration_rule = True,
    attrs = {"constraint_values": attrs.list(attrs.dep(providers = [ConstraintValueInfo]), default = [])},
)

def _config_setting_impl(ctx):
    constraints = {cv[ConstraintValueInfo].setting.label: cv[ConstraintValueInfo] for cv in ctx.attrs.constraint_values}
    return [
        DefaultInfo(),
        ConfigurationInfo(
            constraints = constraints,
            values = {},
        ),
    ]

config_setting = rule(
    impl = _config_setting_impl,
    is_configuration_rule = True,
    attrs = {"constraint_values": attrs.list(attrs.dep(providers = [ConstraintValueInfo]), default = [])},
)
