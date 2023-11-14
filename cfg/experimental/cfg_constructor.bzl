# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    ":common.bzl",
    "get_modifier_info",
    "json_to_tagged_modifier",
    "modifier_to_refs",
    "resolve_modifier",
)
load(":name.bzl", "cfg_name")
load(":order.bzl", "CONSTRAINT_SETTING_ORDER", "get_constraint_setting_order")
load(
    ":types.bzl",
    "ModifierCliLocation",
    "ModifierInfo",  # @unused
    "TaggedModifier",
)

PostConstraintAnalysisParams = record(
    legacy_platform = PlatformInfo | None,
    # Merged modifier from PACKAGE, target, and cli modifiers.
    merged_modifiers = list[TaggedModifier],
)

def cfg_constructor_pre_constraint_analysis(
        *,
        legacy_platform: PlatformInfo | None,
        # dict[str, typing.Any] is JSON dictionary form of `TaggedModifier` passed from buck2 core
        package_modifiers: list[dict[str, typing.Any]] | None,
        target_modifiers: list[dict[str, typing.Any]] | None,
        cli_modifiers: list[str]) -> (list[str], PostConstraintAnalysisParams):
    """
    First stage of cfg constructor for modifiers.

    Args:
        legacy_platform:
            PlatformInfo from legacy target platform resolution, if one is specified
        package_modifiers:
            A list of modifiers specified from all parent PACKAGE files
        target_modifier:
            A list of modifiers specified from buildfile via `metadata` attribute.
        cli_modifiers:
            modifiers specified from `--modifier` flag, `?modifier`, or BXL

    Returns `(refs, PostConstraintAnalysisParams)`, where `refs` is a list of fully qualified configuration
    targets we need providers for.
    """
    package_modifiers = package_modifiers or []
    target_modifiers = target_modifiers or []

    # Convert JSONs back to TaggedModifier
    package_modifiers = [json_to_tagged_modifier(modifier_json) for modifier_json in package_modifiers]
    target_modifiers = [json_to_tagged_modifier(modifier_json) for modifier_json in target_modifiers]

    # Convert CLI modifiers to `TaggedModifier`
    cli_modifiers = [TaggedModifier(modifier = modifier, location = ModifierCliLocation()) for modifier in cli_modifiers]

    merged_modifiers = package_modifiers + target_modifiers + cli_modifiers

    refs = list(CONSTRAINT_SETTING_ORDER)
    for tagged_modifier in merged_modifiers:
        refs.extend(modifier_to_refs(tagged_modifier.modifier, tagged_modifier.location))

    return refs, PostConstraintAnalysisParams(
        legacy_platform = legacy_platform,
        merged_modifiers = merged_modifiers,
    )

def _get_constraint_setting_and_modifier_info(
        refs: dict[str, ProviderCollection],
        tagged_modifier: TaggedModifier,
        constraint_setting_order: list[TargetLabel]) -> (TargetLabel, ModifierInfo):
    return get_modifier_info(
        refs = refs,
        modifier = tagged_modifier.modifier,
        location = tagged_modifier.location,
        constraint_setting_order = constraint_setting_order,
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

    if not params.merged_modifiers:
        # If there is no modifier and legacy platform is specified,
        # then return the legacy platform as is without changing the label or
        # configuration.
        return params.legacy_platform or PlatformInfo(
            # Empty configuration
            label = "<empty>",
            configuration = ConfigurationInfo(
                constraints = {},
                values = {},
            ),
        )

    constraint_setting_to_modifier_infos = {}
    constraint_setting_order = get_constraint_setting_order(refs)

    if params.legacy_platform:
        for constraint_setting, constraint_value_info in params.legacy_platform.configuration.constraints.items():
            constraint_setting_to_modifier_infos[constraint_setting] = [constraint_value_info]

    for tagged_modifier in params.merged_modifiers:
        constraint_setting_label, modifier_info = _get_constraint_setting_and_modifier_info(
            refs = refs,
            tagged_modifier = tagged_modifier,
            constraint_setting_order = constraint_setting_order,
        )
        modifier_infos = constraint_setting_to_modifier_infos.get(constraint_setting_label) or []
        modifier_infos.append(modifier_info)
        constraint_setting_to_modifier_infos[constraint_setting_label] = modifier_infos

    cfg = ConfigurationInfo(
        constraints = {},
        values = {},
    )

    # Order modifiers by CONSTRAINT_SETTING_ORDER, then the rest of modifiers can go in any order
    constraint_setting_order = [
        constraint_setting
        for constraint_setting in constraint_setting_order
        if constraint_setting in constraint_setting_to_modifier_infos
    ] + [
        constraint_setting
        for constraint_setting in constraint_setting_to_modifier_infos
        if constraint_setting not in constraint_setting_order
    ]
    for constraint_setting in constraint_setting_order:
        for modifier_info in constraint_setting_to_modifier_infos[constraint_setting]:
            constraint_value = resolve_modifier(cfg, modifier_info)
            if constraint_value:
                cfg.constraints[constraint_setting] = constraint_value

    name = cfg_name(cfg)
    return PlatformInfo(
        label = name,
        configuration = cfg,
    )
