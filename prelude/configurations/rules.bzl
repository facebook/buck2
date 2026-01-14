# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cfg/modifier:types.bzl", "ConditionalModifierInfo")
load(":util.bzl", "util")

def config_setting_impl(ctx):
    subinfos = [util.constraint_values_to_configuration(ctx.attrs.constraint_values)]
    subinfos.append(ConfigurationInfo(constraints = {}, values = ctx.attrs.values))
    return [DefaultInfo(), util.configuration_info_union(subinfos)]

def constraint_setting_impl(ctx):
    return [DefaultInfo(), ConstraintSettingInfo(label = ctx.label.raw_target())]

def constraint_value_impl(ctx):
    constraint_value = ConstraintValueInfo(
        setting = ctx.attrs.constraint_setting[ConstraintSettingInfo],
        label = ctx.label.raw_target(),
    )
    return [
        DefaultInfo(),
        constraint_value,
        # Provide `ConfigurationInfo` from `constraint_value` so it could be used as select key.
        ConfigurationInfo(
            constraints = {
                constraint_value.setting.label: constraint_value,
            },
            values = {},
        ),
        ConditionalModifierInfo(
            inner = constraint_value,
            key = constraint_value.setting.label,
        ),
    ]

def constraint_impl(ctx):
    # Validate values are unique and non-empty
    values = ctx.attrs.values
    if len(values) <= 1:
        fail("constraint() rule must have at least two values: one for the default and at least one alternative to provide constraint choices. Example: values = ['disable', 'enable']")

    # Reserved keywords that cannot be used as values
    # - 'default': Reserved for aliasing to the actual default value (e.g., :os[default] -> :os[none])
    # - 'DEFAULT': Reserved to avoid confusion with :os[default]
    reserved_keywords = ["default", "DEFAULT"]

    seen = set()
    for v in values:
        if v in reserved_keywords:
            fail("'{}' is a reserved keyword and cannot be used as a constraint value. Use a different name to avoid confusion.".format(v))
        if v in seen:
            fail("Duplicate value '{}' in constraint()".format(v))
        seen.add(v)

    # Validate default if provided
    default = ctx.attrs.default
    if default not in seen:
        fail("default value '{}' must be one of the declared values: {}".format(default, values))

    # Main target provides the constraint setting
    main_label = ctx.label.raw_target()
    constraint_setting = ConstraintSettingInfo(
        label = main_label,
        default = main_label.with_sub_target(default),
    )

    # Create subtargets for each value
    sub_targets = {}
    for value_name in values:
        # Create a constraint value for this subtarget
        constraint_value = ConstraintValueInfo(
            setting = constraint_setting,
            label = main_label.with_sub_target(value_name),
        )

        sub_targets[value_name] = [
            DefaultInfo(),
            constraint_value,
            ConfigurationInfo(
                constraints = {
                    main_label: constraint_value,
                },
                values = {},
            ),
            ConditionalModifierInfo(
                inner = constraint_value,
                key = main_label,
            ),
        ]

    # Add 'default' subtarget that aliases to the actual default value
    sub_targets["default"] = sub_targets[default]

    return [
        DefaultInfo(sub_targets = sub_targets),
        constraint_setting,
    ]

def platform_impl(ctx):
    subinfos = (
        [dep[PlatformInfo].configuration for dep in ctx.attrs.deps] +
        [util.constraint_values_to_configuration(ctx.attrs.constraint_values)]
    )
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            # TODO(nga): current behavior is the last constraint value for constraint setting wins.
            #   This allows overriding constraint values from dependencies, and moreover,
            #   it allows overriding constraint values from constraint values listed
            #   in the same `constraint_values` attribute earlier.
            #   If this is intentional, state it explicitly.
            #   Otherwise, fix it.
            configuration = util.configuration_info_union(subinfos),
        ),
    ]

def configuration_alias_impl(ctx: AnalysisContext) -> list[Provider]:
    return ctx.attrs.actual.providers

# TODO(cjhopman): Update the attributes for these ruletypes to declare the types of providers that they expect in their references.
extra_attributes = {
    "platform": {
        "constraint_values": attrs.list(attrs.dep(providers = [ConstraintValueInfo]), default = []),
    },
}

implemented_rules = {
    "config_setting": config_setting_impl,
    "configuration_alias": configuration_alias_impl,
    "constraint": constraint_impl,
    "constraint_setting": constraint_setting_impl,
    "constraint_value": constraint_value_impl,
    "platform": platform_impl,
}
