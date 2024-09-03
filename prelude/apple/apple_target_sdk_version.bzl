# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//cxx:preprocessor.bzl", "CPreprocessor", "CPreprocessorArgs")
load("@prelude//cxx:target_sdk_version.bzl", "get_target_sdk_version")
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

_MACCATALYST_IOS_TO_MACOS_VERSION_MAP = {
    "13.0": "10.15",  # Catalina
    "13.1": "10.15",
    "13.2": "10.15.1",
    "13.3": "10.15.2",
    "13.4": "10.15.4",
    "13.5": "10.15.5",
    "13.6": "10.15.5",  # Xcode reported 10.15
    "14.0": "11.0",  # Big Sur
    "14.1": "11.0",
    "14.2": "11.0",
    "14.3": "11.1",
    "14.4": "11.2",
    "14.5": "11.3",
    "14.6": "11.4",
    "14.7": "11.5",
    "15.0": "12.0",  # Monterey
    "15.1": "12.0",  # Xcode reported 10.15
    "15.2": "12.1",
    "15.3": "12.2",
    "15.4": "12.3",
    "15.5": "12.4",
    "15.6": "12.5",
    "16.0": "13.0",  # Ventura
    "16.1": "13.0",
    "16.2": "13.1",
    "16.3": "13.2",
    "16.4": "13.3",
    "16.5": "13.4",
    "16.6": "13.5",
    "17.0": "14.0",  # Sonoma
    "17.1": "14.1",
    "17.2": "14.2",
    "17.3": "14.3",
    "17.4": "14.4",
    "17.5": "14.5",
    "18.0": "15.0",
    "18.1": "15.1",
}

_SDK_NAME_TO_PLATFORM_NAME_OVERRIDE_MAP = {
    "maccatalyst": "macosx",
}

def get_platform_version_for_sdk_version(sdk_name: str, sdk_version: str) -> str:
    if sdk_name == "maccatalyst":
        macos_version = _MACCATALYST_IOS_TO_MACOS_VERSION_MAP.get(sdk_version, None)
        if macos_version == None:
            fail("No macos version for maccatalyst version {}".format(sdk_version))
        return macos_version

    return sdk_version

def get_platform_name_for_sdk(sdk_name: str) -> str:
    return _SDK_NAME_TO_PLATFORM_NAME_OVERRIDE_MAP.get(sdk_name, sdk_name)

# Returns the target_sdk_version specified for this build, falling
# back to the toolchain version when unset.
def get_min_deployment_version_for_node(ctx: AnalysisContext) -> str:
    version = get_target_sdk_version(ctx)
    if version == None:
        fail("No target_sdk_version set on target or toolchain")

    return version

def get_versioned_target_triple(ctx: AnalysisContext) -> str:
    target_sdk_version = get_min_deployment_version_for_node(ctx) or ""
    return _get_target_triple(ctx, target_sdk_version)

def get_unversioned_target_triple(ctx: AnalysisContext) -> str:
    return _get_target_triple(ctx, "")

def _get_target_triple(ctx: AnalysisContext, target_sdk_version: str) -> str:
    apple_toolchain_info = ctx.attrs._apple_toolchain[AppleToolchainInfo]
    architecture = apple_toolchain_info.architecture
    if architecture == None:
        fail("Need to set `architecture` field of apple_toolchain(), target: {}".format(ctx.label))

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
    min_version_flag = _get_min_deployment_version_target_flag(ctx)
    return [min_version_flag] if min_version_flag != None else []

def get_min_deployment_version_target_preprocessor_flags(ctx: AnalysisContext) -> list[CPreprocessor]:
    min_version_flag = _get_min_deployment_version_target_flag(ctx)
    if min_version_flag == None:
        return []

    args = cmd_args(min_version_flag)
    return [CPreprocessor(
        args = CPreprocessorArgs(args = [args]),
    )]
