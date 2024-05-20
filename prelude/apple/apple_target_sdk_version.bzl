# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//cxx:preprocessor.bzl", "CPreprocessor", "CPreprocessorArgs")
load(":apple_sdk.bzl", "get_apple_sdk_name")

_TARGET_TRIPLE_MAP = {
    "appletvos": "{architecture}-apple-tvos{version}",
    "appletvsimulator": "{architecture}-apple-tvos{version}-simulator",
    "iphoneos": "{architecture}-apple-ios{version}",
    "iphonesimulator": "{architecture}-apple-ios{version}-simulator",
    "maccatalyst": "{architecture}-apple-ios{version}-macabi",
    "macosx": "{architecture}-apple-macosx{version}",
    "visionos": "{architecture}-apple-xros{version}",
    "visionsimulator": "{architecture}-apple-xros{version}-simulator",
    "watchos": "{architecture}-apple-watchos{version}",
    "watchsimulator": "{architecture}-apple-watchos{version}-simulator",
}

# TODO(T112099448): In the future, the min version flag should live on the apple_toolchain()
# TODO(T113776898): Switch to -mtargetos= flag which should live on the apple_toolchain()
_APPLE_MIN_VERSION_FLAG_SDK_MAP = {
    "iphoneos": "-mios-version-min",
    "iphonesimulator": "-mios-simulator-version-min",
    "maccatalyst": "-mios-version-min",  # Catalyst uses iOS min version flags
    "macosx": "-mmacosx-version-min",
    "watchos": "-mwatchos-version-min",
    "watchsimulator": "-mwatchsimulator-version-min",
}

_APPLE_MIN_VERSION_CLAMP_MAP = {
    "maccatalyst": "13.1",  # Earliest possible Catalyst version
}

# Compares and returns the the maximum of two version numbers. Assumes
# they are both formatted as dot-separted strings (e.g "14.0.3").
# If they are otherwise equal but one is longer, the longer is returned.
def max_sdk_version(left: str, right: str):
    left_components = left.split(".")
    right_components = right.split(".")
    for component in zip(left_components, right_components):
        diff = int(component[0]) - int(component[1])
        if diff < 0:
            return right
        if diff > 0:
            return left

    length_diff = len(left_components) - len(right_components)
    if length_diff < 0:
        return right
    else:
        return left

# Returns the target SDK version for apple_(binary|library) and uses
# apple_toolchain() min version as a fallback. This is the central place
# where the version for a particular node is defined, no other places
# should be accessing `attrs.target_sdk_version` or `attrs.min_version`.
def get_min_deployment_version_for_node(ctx: AnalysisContext) -> str:
    toolchain_min_version = ctx.attrs._apple_toolchain[AppleToolchainInfo].min_version
    min_version = getattr(ctx.attrs, "target_sdk_version", None) or toolchain_min_version
    clamp_version = _APPLE_MIN_VERSION_CLAMP_MAP.get(get_apple_sdk_name(ctx))
    if clamp_version:
        min_version = max_sdk_version(min_version, clamp_version)

    return min_version

def get_versioned_target_triple(ctx: AnalysisContext) -> str:
    apple_toolchain_info = ctx.attrs._apple_toolchain[AppleToolchainInfo]
    architecture = apple_toolchain_info.architecture
    if architecture == None:
        fail("Need to set `architecture` field of apple_toolchain(), target: {}".format(ctx.label))

    target_sdk_version = get_min_deployment_version_for_node(ctx)
    sdk_name = apple_toolchain_info.sdk_name
    target_triple_format_str = _TARGET_TRIPLE_MAP.get(sdk_name)
    if target_triple_format_str == None:
        fail("Could not find target triple for sdk = {}".format(sdk_name))

    return target_triple_format_str.format(architecture = architecture, version = target_sdk_version)

# Returns the min deployment flag to pass to the compiler + linker
def _get_min_deployment_version_target_flag(ctx: AnalysisContext) -> [None, str]:
    target_sdk_version = get_min_deployment_version_for_node(ctx)
    sdk_name = get_apple_sdk_name(ctx)
    min_version_flag = _APPLE_MIN_VERSION_FLAG_SDK_MAP.get(sdk_name)
    if min_version_flag == None:
        fail("Could not determine min version flag for SDK {}".format(sdk_name))

    return "{}={}".format(min_version_flag, target_sdk_version)

# There are two main ways in which we can pass target SDK version:
# - versioned target triple
# - unversioned target triple + version flag
#
# A versioned target triple overrides any version flags and requires
# additional flags to disable the warning/error (`-Woverriding-t-option`),
# so we prefer to use an unversioned target triple + version flag.
#
# Furthermore, we want to ensure that there's _exactly one_ version flag
# on a compiler/link line. This makes debugging easier and avoids issues
# with multiple layers each adding/overriding target SDK. It also makes
# it easier to switch to versioned target triple.
#
# There are exactly two ways in which to specify the target SDK:
# - apple_toolchain.min_version sets the default value
# - apple_(binary|library).target_sdk_version sets the per-target value
#
# apple_toolchain() rules should _never_ add any version flags because
# the rule does _not_ know whether a particular target will request a
# non-default value. Otherwise, we end up with multiple version flags,
# one added by the toolchain and then additional overrides by targets.

def get_min_deployment_version_target_linker_flags(ctx: AnalysisContext) -> list[str]:
    return ["-target", get_versioned_target_triple(ctx)]

def get_min_deployment_version_target_preprocessor_flags(ctx: AnalysisContext) -> list[CPreprocessor]:
    min_version_flag = _get_min_deployment_version_target_flag(ctx)
    if min_version_flag == None:
        return []

    args = cmd_args(min_version_flag)
    return [CPreprocessor(
        args = CPreprocessorArgs(args = [args]),
    )]
