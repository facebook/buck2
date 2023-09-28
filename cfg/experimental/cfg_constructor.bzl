# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":name.bzl", "cfg_name")

PostConstraintAnalysisParams = record(
    legacy_platform = PlatformInfo | None,
    cli_modifiers = list[str],
)

def cfg_constructor_pre_constraint_analysis(
        *,
        legacy_platform: PlatformInfo | None,
        cli_modifiers: list[str]) -> (list[str], PostConstraintAnalysisParams):
    """
    First stage of cfg constructor for modifiers.

    Args:
        legacy_platform: PlatformInfo from legacy target platform resolution, if one is specified
        cli_modifiers: modifiers specified from `--modifier` flag, `?modifier`, or BXL

    Returns `(refs, PostConstraintAnalysisParams)`, where `refs` is a list of fully qualified configuration
    targets we need providers for.
    """

    refs = cli_modifiers
    return refs, PostConstraintAnalysisParams(
        legacy_platform = legacy_platform,
        cli_modifiers = cli_modifiers,
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

    modifiers = params.cli_modifiers

    if not modifiers:
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

    constraints = {}
    for modifier in modifiers:
        constraint_value = refs[modifier][ConstraintValueInfo]
        constraints[constraint_value.setting.label] = constraint_value

    if params.legacy_platform:
        # For backwards compatibility with legacy target platform, any constraint setting
        # from legacy target platform not covered by modifiers will be added to the configuration
        for key, value in params.legacy_platform.configuration.constraints.items():
            if key not in constraints:
                constraints[key] = value

    cfg = ConfigurationInfo(
        constraints = constraints,
        values = {},
    )
    name = cfg_name(cfg)
    return PlatformInfo(
        label = name,
        configuration = cfg,
    )
