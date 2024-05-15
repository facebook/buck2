# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbsource//tools/build_defs/buck2:is_buck2.bzl", "is_buck2")
load("@prelude//apple:apple_platforms.bzl", "APPLE_PLATFORMS_KEY")
load("@prelude//platforms/apple:base.bzl", "BUILD_MODE_TO_CONSTRAINTS_MAP", "apple_build_mode_backed_platform", "is_buck2_mac_platform", "is_mobile_platform")
load(
    "@prelude//platforms/apple:build_mode.bzl",
    "APPLE_BUILD_MODES",
    "get_build_mode",
    "get_build_mode_debug",
)
load(
    "@prelude//platforms/apple:constants.bzl",
    "ios_platforms",
    "mac_catalyst_platforms",
    "mac_platforms",
)
load("@prelude//platforms/apple:platforms_map.bzl", "APPLE_PLATFORMS_MAP")
load("@prelude//utils:buckconfig.bzl", "read")

_SUPPORTED_IOS_PLATFORMS = [
    ios_platforms.IPHONEOS_ARM64,
    ios_platforms.IPHONESIMULATOR_ARM64,
    ios_platforms.IPHONESIMULATOR_X86_64,
]

_SUPPORTED_MACOS_PLATFORMS = [
    mac_platforms.MACOS_ARM64,
    mac_platforms.MACOS_X86_64,
]

_SUPPORTED_MAC_CATALYST_PLATFORMS = [
    mac_catalyst_platforms.MACCATALYST_ARM64,
    mac_catalyst_platforms.MACCATALYST_X86_64,
]

_ANALYSIS_CONSTRAINTS = ["ovr_config//bitcode/constraints:bitcode"]
_DEFAULT_ANALYSIS_IOS_PLATFORM = ios_platforms.IPHONEOS_ARM64
_DEFAULT_ANALYSIS_MACOS_PLATFORM = mac_platforms.MACOS_X86_64

DEFAULT_SUPPORTED_CXX_PLATFORMS = _SUPPORTED_IOS_PLATFORMS

def apple_target_platforms(
        base_name,
        platform_rule,
        constraint_values = None,  # Constraint values added to all generated platforms
        visibility = None,
        deps = None,
        cxx_platforms_constraint_values = None,  # Must be a map of a supported cxx platform to a list of constraint values
        build_mode_constraint_values = None,  # Must be a map of a supported build mode to a list of constraint values
        supported_cxx_platforms = DEFAULT_SUPPORTED_CXX_PLATFORMS,  # Cxx platforms to generate platforms for
        supported_build_modes = APPLE_BUILD_MODES):  # Build modes to generate platforms for
    """ Define architecture and sdk specific platforms alongside the base platform. """

    # Form defaults
    constraint_values = constraint_values or []
    cxx_platforms_constraint_values = cxx_platforms_constraint_values or {}
    build_mode_constraint_values = build_mode_constraint_values or {}
    visibility = visibility or ["PUBLIC"]
    deps = deps or []

    _validate_cxx_platforms_constraint_values(base_name, cxx_platforms_constraint_values, supported_cxx_platforms)
    _validate_build_mode_constraint_values(base_name, build_mode_constraint_values, supported_build_modes)

    # Define the generated platforms
    for platform in supported_cxx_platforms:
        platform_dep = get_default_target_platform_for_platform(platform)
        cxx_platform_constraints = cxx_platforms_constraint_values.get(platform, [])
        if is_mobile_platform(platform) or is_buck2_mac_platform(platform):
            for build_mode in supported_build_modes:
                build_mode_constraints = build_mode_constraint_values.get(build_mode, []) + BUILD_MODE_TO_CONSTRAINTS_MAP.get(build_mode)
                _define_platform(
                    base_name,
                    platform,
                    build_mode,
                    constraint_values + cxx_platform_constraints + build_mode_constraints,
                    visibility,
                    deps + [platform_dep],
                    platform_rule,
                )
        else:
            _define_platform(
                base_name,
                platform,
                None,
                constraint_values + cxx_platform_constraints,
                visibility,
                deps + [platform_dep],
                platform_rule,
            )

    # Define the base platform in case it is needed (example: to be a dep of another platform)
    platform_rule(
        name = base_name,
        constraint_values = constraint_values,
        visibility = visibility,
        deps = deps,
    )

    analysis_platform = _get_analysis_platform_for_supported_platforms(supported_cxx_platforms)
    analysis_platform_dep = get_default_target_platform_for_platform(analysis_platform)
    analysis_platform_build_mode_constraints = build_mode_constraint_values.get(get_build_mode_debug(), [])

    platform_rule(
        name = base_name + "-analysis",
        constraint_values = constraint_values + analysis_platform_build_mode_constraints + _ANALYSIS_CONSTRAINTS,
        visibility = ["PUBLIC"],
        deps = deps + [analysis_platform_dep],
    )

def config_backed_apple_target_platform(target_platform = None, platform = None, build_mode = None):
    platform = _get_default_platform() if platform == None else platform
    build_mode = get_build_mode() if build_mode == None else build_mode
    if target_platform == None:
        return get_default_target_platform_for_platform(platform)

    return _get_generated_name(target_platform, platform, build_mode)

def get_default_target_platform_for_platform(sdk_arch) -> [str, None]:
    data = APPLE_PLATFORMS_MAP.get(sdk_arch)
    if data != None:
        return data.target_platform

    return None

def set_apple_platforms(platform, base_config_backed_target_platform, kwargs):
    def get_supported_platforms():
        if not is_buck2():
            return None
        if platform in _SUPPORTED_IOS_PLATFORMS:
            return _SUPPORTED_IOS_PLATFORMS
        elif platform in _SUPPORTED_MACOS_PLATFORMS:
            return _SUPPORTED_MACOS_PLATFORMS
        elif platform in _SUPPORTED_MAC_CATALYST_PLATFORMS:
            return _SUPPORTED_MAC_CATALYST_PLATFORMS
        else:
            return None

    supported_platforms = get_supported_platforms()
    if not supported_platforms:
        return kwargs

    # If we've already defined the apple platforms, we can avoid having to process them again.
    if APPLE_PLATFORMS_KEY in kwargs:
        return kwargs

    apple_platforms = {}
    for platform in supported_platforms:
        for build_mode in APPLE_BUILD_MODES:
            identifier = "{}-{}".format(platform, build_mode)
            if base_config_backed_target_platform:
                apple_platforms[identifier] = config_backed_apple_target_platform(base_config_backed_target_platform, platform, build_mode)
            else:
                base_target_platform = _get_base_target_platform_for_platform(platform)
                if not base_target_platform:
                    fail("A valid base target platform is required!")
                apple_platforms[identifier] = apple_build_mode_backed_platform(base_target_platform, platform, build_mode)

    kwargs[APPLE_PLATFORMS_KEY] = apple_platforms

    return kwargs

def _get_generated_name(base_name, platform, build_mode):
    platform_and_build_mode_name = apple_build_mode_backed_platform(platform, platform, build_mode)
    return "{}-{}".format(base_name, platform_and_build_mode_name)

def _get_default_platform():
    platform = read("cxx", "default_platform")
    return platform if platform != None else ios_platforms.IPHONESIMULATOR_X86_64

def _define_platform(base_name, platform, build_mode, constraint_values, visibility, deps, platform_rule):
    # @lint-ignore BUCKLINT - We set the visibility to PUBLIC directly and can bypass fb_native
    platform_rule(
        name = _get_generated_name(base_name, platform, build_mode),
        constraint_values = constraint_values,
        visibility = visibility,
        deps = deps,
    )

def _get_base_target_platform_for_platform(sdk_arch) -> [str, None]:
    data = APPLE_PLATFORMS_MAP.get(sdk_arch)
    if data != None:
        return data.base_target_platform

    return None

def _get_analysis_platform_for_supported_platforms(supported_cxx_platforms):
    # For determining the platform deps to use for the base platform, we inspect the supported
    # cxx platforms, giving precedence to iOS platforms.
    for platform in _SUPPORTED_IOS_PLATFORMS:
        if platform in supported_cxx_platforms:
            return _DEFAULT_ANALYSIS_IOS_PLATFORM

    for platform in _SUPPORTED_MACOS_PLATFORMS:
        if platform in supported_cxx_platforms:
            return _DEFAULT_ANALYSIS_MACOS_PLATFORM

    return _DEFAULT_ANALYSIS_IOS_PLATFORM

def _validate_cxx_platforms_constraint_values(base_name, cxx_platforms_constraint_values, supported_cxx_platforms):
    if type(cxx_platforms_constraint_values) != type({}):
        fail("cxx_platforms_constraint_values must be a map of platform to constraint values!")
    for platform, platform_values in cxx_platforms_constraint_values.items():
        if platform not in supported_cxx_platforms:
            fail("\n\nProviding platform constraints for an unsupported platform!\nBase platform: {}\nCXX Platform: {} with values {}\nSupported platforms: {}\n".format(
                base_name,
                platform,
                platform_values,
                ", ".join(supported_cxx_platforms),
            ))

def _validate_build_mode_constraint_values(base_name, build_mode_constraint_values, supported_build_modes):
    if type(build_mode_constraint_values) != type({}):
        fail("build_mode_constraint_values must be a map of build mode to constraint values!")
    for build_mode, build_mode_values in build_mode_constraint_values.items():
        if build_mode not in supported_build_modes:
            fail("\n\nProviding build mode constraints for an unsupported build mode!\nBase platform: {}\nBuild mode: {} with values {}\nSupported build modes: {}\n".format(
                base_name,
                build_mode,
                build_mode_values,
                ", ".join(supported_build_modes),
            ))
