# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//transitions:utils.bzl", "filtered_platform_constraints", "get_constraint_value")
load("@prelude//utils:expect.bzl", "expect")

"""
Transition from catalyst SDK to macOS SDK. Used for AppKit extension bundle rules.
Transforms both OS and SDK constraints.
Only sanity check for source configuration is done.
"""

def _macos_transition_impl(platform: PlatformInfo, refs: struct) -> PlatformInfo:
    # This functions operates in the following way:
    #  - Start with all the constraints from the platform and filter out the constraints for OS and SDK.
    #  - Always set the new OS constraint to macOS.
    #  - If the old SDK constraint was macCatalyst, replace with the equivalent macOS constraint.
    #  - Return a new platform with the updated constraints.
    updated_constraints = filtered_platform_constraints(platform, [refs.os[ConstraintSettingInfo].label, refs.sdk[ConstraintSettingInfo].label])

    macos = refs.macos[ConstraintValueInfo]
    updated_constraints[refs.os[ConstraintSettingInfo].label] = macos

    # Update SDK constraint
    old_sdk = get_constraint_value(platform, refs.sdk[ConstraintSettingInfo])
    maccatalyst_sdk = refs.maccatalyst_sdk[ConstraintValueInfo]
    macosx_sdk = refs.macos_sdk[ConstraintValueInfo]

    if old_sdk != None:
        expect(old_sdk.label in [macosx_sdk.label, maccatalyst_sdk.label], "If present, SDK transitioned non-identically to macOS should be `macCatalyst`, got {}".format(old_sdk.label))
    updated_constraints[refs.sdk[ConstraintSettingInfo].label] = macosx_sdk

    new_cfg = ConfigurationInfo(
        constraints = updated_constraints,
        values = platform.configuration.values,
    )
    return PlatformInfo(
        label = "macos_transition",
        configuration = new_cfg,
    )

macos_transition = transition(impl = _macos_transition_impl, refs = {
    "maccatalyst_sdk": "config//os/sdk/apple/constraints:maccatalyst",
    "macos": "config//os/constraints:macos",
    "macos_sdk": "config//os/sdk/apple/constraints:macosx",
    "os": "config//os/constraints:os",
    "sdk": "config//os/sdk/apple/constraints:_",
})
