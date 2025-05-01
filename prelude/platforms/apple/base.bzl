# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:is_buck2.bzl", "is_buck2") # @oss-enable
load(
    "@prelude//platforms/apple:build_mode.bzl",
    "APPLE_BUILD_MODES",
    "CONSTRAINT_PACKAGE",
    "REMAPPED_BUILD_MODES",
    "get_build_mode",
)
load(
    "@prelude//platforms/apple:constants.bzl",
    "ios_platforms",
    "mac_catalyst_platforms",
    "mac_platforms",
    "watch_platforms",
)
# @oss-disable[end= ]: load("@prelude//platforms/apple/meta_only:build_mode.bzl", "BUILD_MODE_CONSTRAINTS")

BUILD_MODE_TO_CONSTRAINTS_MAP = {
    build_mode: ["{}:{}".format(CONSTRAINT_PACKAGE, build_mode)] +
                # @oss-disable[end= ]: BUILD_MODE_CONSTRAINTS[build_mode]
    [] # @oss-enable
    for build_mode in APPLE_BUILD_MODES + REMAPPED_BUILD_MODES.keys()
}

_MOBILE_PLATFORMS = [
    ios_platforms.IPHONEOS_ARM64,
    ios_platforms.IPHONESIMULATOR_ARM64,
    ios_platforms.IPHONESIMULATOR_X86_64,
    watch_platforms.WATCHOS_ARM64,
    watch_platforms.WATCHOS_ARM64_32,
    watch_platforms.WATCHSIMULATOR_ARM64,
    watch_platforms.WATCHSIMULATOR_X86_64,
]

_MAC_PLATFORMS = [
    mac_platforms.MACOS_ARM64,
    mac_platforms.MACOS_X86_64,
    mac_platforms.MACOS_UNIVERSAL,
    mac_catalyst_platforms.MACCATALYST_ARM64,
    mac_catalyst_platforms.MACCATALYST_X86_64,
]

# TODO: Drop the platform_rule when we're not longer attempting to support buck1.
def apple_generated_platforms(name, constraint_values, deps, platform_rule, platform = None, supported_build_modes = APPLE_BUILD_MODES):
    # By convention, the cxx.default_platform is typically the same as the platform being defined.
    # This is not the case for all watch platforms, so provide an override.
    platform = platform if platform else name
    if is_mobile_platform(platform) or is_buck2_mac_platform(platform):
        for build_mode in supported_build_modes:
            platform_rule(
                name = _get_generated_name(name, platform, build_mode),
                constraint_values = constraint_values + BUILD_MODE_TO_CONSTRAINTS_MAP.get(build_mode),
                visibility = ["PUBLIC"],
                deps = deps,
            )

    # Create a platform without the build mode to support backwards compatibility of hardcoded platforms
    # and with buck1 cxx platform setup.
    # TODO(chatatap): Look to remove all hardcoded references and get rid of these
    platform_rule(
        name = name,
        constraint_values = constraint_values,
        visibility = ["PUBLIC"],
        deps = deps,
    )

def apple_build_mode_backed_platform(name, platform, build_mode = None):
    build_mode = get_build_mode() if build_mode == None else build_mode
    return _get_generated_name(name, platform, build_mode)

def is_mobile_platform(platform):
    # These builds modes are primarily used in mobile code. MacOS builds in fbcode/arvr use different
    # modes to represent dev/opt variants.
    return platform in _MOBILE_PLATFORMS

def is_buck2_mac_platform(platform):
    return platform in _MAC_PLATFORMS

def _get_generated_name(name, platform, build_mode):
    if is_mobile_platform(platform) or is_buck2_mac_platform(platform):
        return "{}-{}".format(name, build_mode)
    else:
        return name
