# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cfg/modifier:constraint_modifier_info.bzl", "make_constraint_modifier_info")
load("@prelude//cfg/modifier:types.bzl", "ConditionalModifierInfo")
load(":util.bzl", "util")

_ExecutionModifierInfo = provider(
    fields = {
        "execution_modifier": bool,
    }
)

def config_setting_impl(ctx):
    subinfos = [util.constraint_values_to_configuration(ctx.attrs.constraint_values)]
    subinfos.append(ConfigurationInfo(constraints = {}, values = ctx.attrs.values))
    cfg_info = util.configuration_info_union(subinfos)
    providers = [DefaultInfo(), cfg_info]
    if len(ctx.attrs.constraint_values) == 1:
        # When only have one constraint, we return an additional ConditionalModifierInfo provider for
        # conditional modifier
        conditional_modifier_info = ctx.attrs.constraint_values[0][ConditionalModifierInfo]
        providers.append(conditional_modifier_info)
    return providers

def constraint_setting_impl(ctx):
    return [
        DefaultInfo(),
        ConstraintSettingInfo(label = ctx.label.raw_target()),
        # In order for the constraint_value to access the execution modifier info, we need to provide an additional provider here.
        _ExecutionModifierInfo(execution_modifier = ctx.attrs.execution_modifier),
    ]

def constraint_value_impl(ctx):
    constraint_value = ConstraintValueInfo(
        setting = ctx.attrs.constraint_setting[ConstraintSettingInfo],
        label = ctx.label.raw_target(),
    )
    execution_modifier_info = ctx.attrs.constraint_setting[_ExecutionModifierInfo]

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
        make_constraint_modifier_info(
            constraint_value = constraint_value,
            key = constraint_value.setting.label,
            execution_modifier = execution_modifier_info.execution_modifier,
            exec_platform_marker_configuration_info = ctx.attrs._exec_platform_marker[ConfigurationInfo],
        ),
    ]

def constraint_impl(ctx):
    # Validate values are unique and non-empty
    values = ctx.attrs.values

    min_values = 1 if ctx.attrs.allow_trivial_constraint else 2
    if len(values) < min_values:
        if ctx.attrs.allow_trivial_constraint:
            fail("constraint() rule must have at least one value.")
        else:
            fail(
                "constraint() rule must have at least two values. "
                + "A single-value constraint has no selectivity: it can't express a choice between alternatives "
                + "(e.g., 'linux' vs 'macos', 'enabled' vs 'disabled'), which is the whole point of a constraint. "
                + "If you don't need to select between alternatives, you likely don't need a constraint at all.\n\n"
                + "Set allow_trivial_constraint = True only if your constraint legitimately has just one value "
                + "most of the time and may temporarily gain alternatives (e.g., versioned packages "
                + "that pick up an extra value during an upgrade and drop it once the upgrade lands).",
            )

    default = ctx.attrs.default

    # Value-name reservations:
    # - 'DEFAULT' is always reserved, to avoid visual confusion with the special `[default]` subtarget.
    # - 'default' is allowed only when it is itself the default value, in which case it coincides with the
    #   `[default]` subtarget added below. Otherwise `[default]` would be ambiguous, so it is rejected.
    seen = set()
    for v in values:
        if v == "DEFAULT":
            fail("'DEFAULT' is a reserved keyword and cannot be used as a constraint value. Use a different name to avoid confusion.")
        if v == "default" and default != "default":
            fail(
                "'default' can be used as a constraint value only when it is also the default value "
                + "(i.e. default = \"default\"); otherwise ':{}[default]' is ambiguous between the value ".format(ctx.label.name)
                + "named 'default' and the default value '{}'.".format(default),
            )
        if v in seen:
            fail("Duplicate value '{}' in constraint()".format(v))
        seen.add(v)

    # Validate default is one of the declared values.
    if default not in seen:
        fail("default value '{}' must be one of the declared values: {}".format(default, values))

    # Main target provides the constraint setting
    main_label = ctx.label.raw_target()
    constraint_setting = ConstraintSettingInfo(
        label = main_label,
        default = main_label.with_sub_target(default),
    )

    # exec_platform_marker_constraint() and constraint() rule both use this impl,
    # but only constraint() has this attribute.
    exec_platform_marker_configuration_info = None
    if hasattr(ctx.attrs, "_exec_platform_marker"):
        exec_platform_marker_configuration_info = ctx.attrs._exec_platform_marker[ConfigurationInfo]

    execution_modifier = False
    if hasattr(ctx.attrs, "execution_modifier"):
        execution_modifier = ctx.attrs.execution_modifier

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
            make_constraint_modifier_info(
                constraint_value = constraint_value,
                key = main_label,
                execution_modifier = execution_modifier,
                exec_platform_marker_configuration_info = exec_platform_marker_configuration_info,
            ),
        ]

    # Add 'default' subtarget that aliases to the actual default value
    sub_targets["default"] = sub_targets[default]

    # Validate and register user-defined aliases.
    # Each alias becomes an additional subtarget pointing to an existing value's providers.
    reserved_keywords = ["default", "DEFAULT"]
    aliases = ctx.attrs.aliases
    for alias_name, alias_value in aliases.items():
        if alias_name in reserved_keywords:
            fail("alias '{}' is a reserved keyword and cannot be used as a constraint alias.".format(alias_name))
        if alias_name in seen:
            fail("alias '{}' conflicts with a declared constraint value.".format(alias_name))
        if alias_value not in seen:
            fail("alias '{}' targets value '{}' which is not declared in values: {}".format(alias_name, alias_value, values))
        sub_targets[alias_name] = sub_targets[alias_value]

    return [
        DefaultInfo(sub_targets = sub_targets),
        constraint_setting,
    ]

def platform_impl(ctx):
    subinfos = [dep[PlatformInfo].configuration for dep in ctx.attrs.deps] + [util.constraint_values_to_configuration(ctx.attrs.constraint_values)]
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
    "exec_platform_marker_constraint": constraint_impl,
    "platform": platform_impl,
}
