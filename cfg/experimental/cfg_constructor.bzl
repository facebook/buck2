# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    ":common.bzl",
    "get_modifier_info",
    "merge_modifiers",
    "modifier_to_refs",
    "resolve_modifier",
)
load(":name.bzl", "cfg_name")
load(
    ":types.bzl",
    "ModifierCliLocation",
    "ModifierLegacyPlatformLocation",
    "TaggedModifier",
    "TaggedModifierInfo",
)

PostConstraintAnalysisParams = record(
    legacy_platform = PlatformInfo | None,
    # Merged modifier dictionaries from PACKAGE and target modifiers.
    # If a key exists in both PACKAGE and target modifiers, the target modifier
    # will override PACKAGE modifier for that key.
    package_and_target_modifiers = dict[str, list[TaggedModifier]],
    cli_modifiers = list[str],
)

# List of constraint settings that can be selected on via `modifier_select`.
# Modifiers for these constraints are resolved first before other modifiers,
# if they exist. `modifier_select` keying on other constraint settings will
# fail
# TODO(scottcao): Find a better place to set this so that OSS users can add
# their own constraints.
CONSTRAINT_SETTING_ORDER = [
    "ovr_config//build_mode/constraints:build_mode",
    "ovr_config//os/constraints:os",
    "ovr_config//cpu/constraints:cpu",
    "ovr_config//build_mode/constraints:lto",
]

def cfg_constructor_pre_constraint_analysis(
        *,
        legacy_platform: PlatformInfo | None,
        package_modifiers: dict[str, list[TaggedModifier]] | None,
        target_modifiers: dict[str, TaggedModifier] | None,
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
    package_modifiers = package_modifiers or {}
    target_modifiers = target_modifiers or {}

    # Merge PACKAGE and target modifiers into one dictionary
    package_and_target_modifiers = package_modifiers
    for modifier_key, modifier_with_loc in target_modifiers.items():
        package_and_target_modifiers[modifier_key] = merge_modifiers(
            package_and_target_modifiers.get(modifier_key),
            modifier_with_loc,
        )

    refs = list(CONSTRAINT_SETTING_ORDER)
    for constraint_setting, modifiers in package_and_target_modifiers.items():
        refs.append(constraint_setting)
        for modifier_with_loc in modifiers:
            refs.extend(modifier_to_refs(modifier_with_loc.modifier, constraint_setting, modifier_with_loc.location))
    refs.extend(cli_modifiers)

    return refs, PostConstraintAnalysisParams(
        legacy_platform = legacy_platform,
        package_and_target_modifiers = package_and_target_modifiers,
        cli_modifiers = cli_modifiers,
    )

def get_constraint_setting_order(refs: dict[str, ProviderCollection]) -> list[TargetLabel]:
    return [refs[constraint_setting][ConstraintSettingInfo].label for constraint_setting in CONSTRAINT_SETTING_ORDER]

def _get_constraint_setting_and_tagged_modifier_infos(
        refs: dict[str, ProviderCollection],
        constraint_setting: str,
        modifiers: list[TaggedModifier],
        constraint_setting_order: list[TargetLabel]) -> (TargetLabel, list[TaggedModifierInfo]):
    constraint_setting_info = refs[constraint_setting][ConstraintSettingInfo]
    modifier_infos = [TaggedModifierInfo(
        modifier_info = get_modifier_info(
            refs = refs,
            modifier = modifier_with_loc.modifier,
            constraint_setting = constraint_setting,
            location = modifier_with_loc.location,
            constraint_setting_order = constraint_setting_order,
        ),
        location = modifier_with_loc.location,
    ) for modifier_with_loc in modifiers]
    return constraint_setting_info.label, modifier_infos

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

    if not params.package_and_target_modifiers and not params.cli_modifiers:
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

    constraint_setting_to_tagged_modifier_infos = {}
    constraint_setting_order = get_constraint_setting_order(refs)

    if params.legacy_platform:
        for constraint_setting, constraint_value_info in params.legacy_platform.configuration.constraints.items():
            constraint_setting_to_tagged_modifier_infos[constraint_setting] = [TaggedModifierInfo(
                modifier_info = constraint_value_info,
                location = ModifierLegacyPlatformLocation(),
            )]

    for constraint_setting, modifiers in params.package_and_target_modifiers.items():
        constraint_setting_label, tagged_modifier_infos = _get_constraint_setting_and_tagged_modifier_infos(
            refs = refs,
            constraint_setting = constraint_setting,
            modifiers = modifiers,
            constraint_setting_order = constraint_setting_order,
        )
        constraint_setting_to_tagged_modifier_infos[constraint_setting_label] = tagged_modifier_infos

    for modifier in params.cli_modifiers:
        constraint_value_info = refs[modifier][ConstraintValueInfo]
        constraint_setting_to_tagged_modifier_infos[constraint_value_info.setting.label] = [TaggedModifierInfo(
            modifier_info = constraint_value_info,
            location = ModifierCliLocation(),
        )]

    cfg = ConfigurationInfo(
        constraints = {},
        values = {},
    )

    # Order modifiers by CONSTRAINT_SETTING_ORDER, then the rest of modifiers can go in any order
    constraint_setting_order = [
        constraint_setting
        for constraint_setting in constraint_setting_order
        if constraint_setting in constraint_setting_to_tagged_modifier_infos
    ] + [
        constraint_setting
        for constraint_setting in constraint_setting_to_tagged_modifier_infos
        if constraint_setting not in constraint_setting_order
    ]
    for constraint_setting in constraint_setting_order:
        for tagged_modifier_info in constraint_setting_to_tagged_modifier_infos[constraint_setting]:
            constraint_value = resolve_modifier(cfg, tagged_modifier_info.modifier_info)
            if constraint_value:
                cfg.constraints[constraint_setting] = constraint_value

    name = cfg_name(cfg)
    return PlatformInfo(
        label = name,
        configuration = cfg,
    )
