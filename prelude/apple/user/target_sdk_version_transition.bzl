# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Apply a constraint if the propagated_target_sdk_version attribute is set.
This overrides any existing target_sdk_version select.
"""

load("@prelude//apple:versions.bzl", "TARGET_SDK_VERSIONS")

def _impl(platform: PlatformInfo, refs: struct, attrs: struct) -> PlatformInfo:
    target_sdk_version = attrs.propagated_target_sdk_version
    if not target_sdk_version:
        return platform

    constraint_label = refs.version[ConstraintSettingInfo].label
    constraint_value = platform.configuration.constraints.get(constraint_label)
    version_provider = getattr(refs, target_sdk_version, None)
    if version_provider == None:
        fail("target sdk version {} is missing constraints".format(target_sdk_version))

    version_constraint = version_provider[ConstraintValueInfo]
    if constraint_value == version_constraint:
        return platform

    updated_constraints = platform.configuration.constraints
    updated_constraints[constraint_label] = version_constraint
    new_cfg = ConfigurationInfo(
        constraints = updated_constraints,
        values = platform.configuration.values,
    )
    return PlatformInfo(
        label = platform.label + "_target_sdk_version_" + target_sdk_version,
        configuration = new_cfg,
    )

target_sdk_version_transition = transition(
    impl = _impl,
    refs = dict(
        [("version", "@config//version:constraint-setting-target-sdk-version")] + {
            version: "@config//version:constraint-value-target-sdk-version-" + version
            for version in TARGET_SDK_VERSIONS
        }.items(),
    ),
    attrs = ["propagated_target_sdk_version"],
)
