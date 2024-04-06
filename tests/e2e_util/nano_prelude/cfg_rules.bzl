# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# This is copy-paste from `prelude/configurations/util.bzl`

def _configuration_info_union(infos):
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

def _constraint_values_to_configuration(values):
    return ConfigurationInfo(constraints = {
        info[ConstraintValueInfo].setting.label: info[ConstraintValueInfo]
        for info in values
    }, values = {})

# This is copy-paste from `prelude/configurations/rules.bzl`

def _constraint_setting_impl(ctx):
    return [DefaultInfo(), ConstraintSettingInfo(label = ctx.label.raw_target())]

constraint_setting = rule(
    impl = _constraint_setting_impl,
    is_configuration_rule = True,
    attrs = {},
)

def _constraint_value_impl(ctx):
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
    impl = _constraint_value_impl,
    is_configuration_rule = True,
    attrs = {
        "constraint_setting": attrs.dep(providers = [ConstraintSettingInfo]),
    },
)

def _platform_impl(ctx):
    subinfos = (
        [dep[PlatformInfo].configuration for dep in ctx.attrs.deps] +
        [_constraint_values_to_configuration(ctx.attrs.constraint_values)]
    )
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            configuration = _configuration_info_union(subinfos),
        ),
    ]

platform = rule(
    impl = _platform_impl,
    is_configuration_rule = True,
    attrs = {
        "constraint_values": attrs.list(attrs.dep(providers = [ConfigurationInfo]), default = []),
        "deps": attrs.list(attrs.dep(providers = [PlatformInfo]), default = []),
    },
)

def _config_setting_impl(ctx):
    subinfos = [_constraint_values_to_configuration(ctx.attrs.constraint_values)]
    subinfos.append(ConfigurationInfo(constraints = {}, values = ctx.attrs.values))
    return [DefaultInfo(), _configuration_info_union(subinfos)]

config_setting = rule(
    impl = _config_setting_impl,
    is_configuration_rule = True,
    attrs = {
        "constraint_values": attrs.list(attrs.dep(providers = [ConstraintValueInfo]), default = []),
        "values": attrs.dict(attrs.string(), attrs.string(), default = {}),
    },
)
