# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    ":common.bzl",
    "get_modifier_info",
    "modifier_key_to_constraint_setting",
    "modifier_to_refs",
    "resolve_modifier",
)
load(":name.bzl", "cfg_name")
load(
    ":types.bzl",
    "CfgModifierCliLocation",
    "CfgModifierInfoWithLocation",
    "CfgModifierWithLocation",
)

PostConstraintAnalysisParams = record(
    legacy_platform = PlatformInfo | None,
    # Merged modifier dictionaries from PACKAGE and target modifiers.
    # If a key exists in both PACKAGE and target modifiers, the target modifier
    # will override PACKAGE modifier for that key.
    package_and_target_modifiers = dict[str, CfgModifierWithLocation],
    cli_modifiers = list[str],
)

def cfg_constructor_pre_constraint_analysis(
        *,
        legacy_platform: PlatformInfo | None,
        package_modifiers: dict[str, CfgModifierWithLocation],
        target_modifiers: dict[str, CfgModifierWithLocation],
        cli_modifiers: list[str]) -> (list[str], PostConstraintAnalysisParams):
    """
    First stage of cfg constructor for modifiers.

    Args:
        legacy_platform:
            PlatformInfo from legacy target platform resolution, if one is specified
        package_modifiers:
            Modifiers specified from all parent PACKAGE files aggregated into a single dictionary.
            Key of dictionary is the modifier key converted from constraint setting.
            Value of dictionary is the modifier for that constraint setting.
        target_modifier:
            Modifiers specified from buildfile via `metadata` attribute.
            Key of dictionary is the modifier key converted from constraint setting.
            Value of dictionary is the modifier for that constraint setting.
        cli_modifiers:
            modifiers specified from `--modifier` flag, `?modifier`, or BXL

    Returns `(refs, PostConstraintAnalysisParams)`, where `refs` is a list of fully qualified configuration
    targets we need providers for.
    """

    # Merge PACKAGE and target modifiers into one dictionary
    package_and_target_modifiers = {
        modifier_key_to_constraint_setting(modifier_key): modifier_with_loc
        # Target modifier always overrides PACKAGE modifier of the same key
        for modifiers in (package_modifiers, target_modifiers)
        for modifier_key, modifier_with_loc in modifiers.items()
    }

    refs = []
    for constraint_setting, modifier_with_loc in package_and_target_modifiers.items():
        refs.append(constraint_setting)
        refs.extend(modifier_to_refs(modifier_with_loc.modifier, constraint_setting, modifier_with_loc.location))
    refs.extend(cli_modifiers)

    return refs, PostConstraintAnalysisParams(
        legacy_platform = legacy_platform,
        package_and_target_modifiers = package_and_target_modifiers,
        cli_modifiers = cli_modifiers,
    )

def _get_constraint_setting_and_modifier_info(
        refs: dict[str, ProviderCollection],
        constraint_setting: str,
        modifier_with_loc: CfgModifierWithLocation) -> (TargetLabel, CfgModifierInfoWithLocation):
    modifier_info = get_modifier_info(
        refs = refs,
        modifier = modifier_with_loc.modifier,
        constraint_setting = constraint_setting,
        location = modifier_with_loc.location,
    )
    constraint_setting = refs[constraint_setting][ConstraintSettingInfo]
    return constraint_setting.label, CfgModifierInfoWithLocation(
        modifier_info = modifier_info,
        setting = constraint_setting,
        location = modifier_with_loc.location,
    )

def cfg_constructor_post_constraint_analysis(
        *,
        refs: dict[str, ProviderCollection],
        params: PostConstraintAnalysisParams) -> PlatformInfo:
    """
    Second stage of cfg constructor for modifiers.

    Args:
        refs: a dictionary of fully qualified target labels for configuration targets with their providers
        params: `PostConstraintAnalysisParams` returned from first stage of cfg constructor

    Returns a PlatformInfo
    """

    modifier_infos_with_loc = {}

    for constraint_setting, modifier_with_loc in params.package_and_target_modifiers.items():
        constraint_setting_label, modifier_info_with_loc = _get_constraint_setting_and_modifier_info(
            refs = refs,
            constraint_setting = constraint_setting,
            modifier_with_loc = modifier_with_loc,
        )
        modifier_infos_with_loc[constraint_setting_label] = modifier_info_with_loc

    for modifier in params.cli_modifiers:
        constraint_value_info = refs[modifier][ConstraintValueInfo]
        modifier_infos_with_loc[constraint_value_info.setting.label] = CfgModifierInfoWithLocation(
            modifier_info = constraint_value_info,
            setting = constraint_value_info.setting,
            location = CfgModifierCliLocation(),
        )

    if not modifier_infos_with_loc:
        # If there is no modifier and legacy platform is specified,
        # then return the legacy platform as is without changing the label or
        # configuration.
        return params.legacy_platform or PlatformInfo(
            # Empty configuration
            label = "",
            configuration = ConfigurationInfo(
                constraints = {},
                values = {},
            ),
        )

    cfg = ConfigurationInfo(
        constraints = {},
        values = {},
    )
    for constraint_setting, modifier_info_with_loc in modifier_infos_with_loc.items():
        constraint_value = resolve_modifier(cfg, modifier_info_with_loc.modifier_info)
        if constraint_value:
            cfg.constraints[constraint_setting] = constraint_value

    if params.legacy_platform:
        # For backwards compatibility with legacy target platform, any constraint setting
        # from legacy target platform not covered by modifiers will be added to the configuration
        for key, value in params.legacy_platform.configuration.constraints.items():
            if key not in cfg.constraints:
                cfg.constraints[key] = value

    name = cfg_name(cfg)
    return PlatformInfo(
        label = name,
        configuration = cfg,
    )
