# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

PostConstraintAnalysisParams = record(
    legacy_platform = PlatformInfo | None,
)

def cfg_constructor_pre_constraint_analysis(
        *,
        legacy_platform: PlatformInfo | None) -> (list[str], PostConstraintAnalysisParams):
    """
    First stage of cfg constructor for modifiers.

    Args:
        legacy_platform: PlatformInfo from legacy target platform resolution, if one is specified

    Returns `(refs, PostConstraintAnalysisParams)`, where `refs` is a list of fully qualified configuration
    targets we need providers for.
    """

    return [], PostConstraintAnalysisParams(legacy_platform = legacy_platform)

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

    _unused = refs  # buildifier: disable=unused-variable

    return params.legacy_platform or PlatformInfo(
        # Empty configuration if there is no modifier or legacy platform.
        label = "",
        configuration = ConfigurationInfo(
            constraints = {},
            values = {},
        ),
    )
