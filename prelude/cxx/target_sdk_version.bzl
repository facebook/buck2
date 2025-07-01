# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_platform_info", "get_cxx_toolchain_info")

def version_is_greater(left: str, right: str) -> bool:
    # Assumes version strings are in dotted format 1.2.4.
    # After comparing components the longer remainder is
    # considered larger.
    left_components = left.split(".")
    right_components = right.split(".")
    for pair in zip(left_components, right_components):
        x = int(pair[0])
        y = int(pair[1])
        if x < y:
            return False
        elif x > y:
            return True

    return len(left_components) > len(right_components)

def get_toolchain_target_sdk_version(ctx: AnalysisContext) -> [None, str]:
    min_version = ctx.attrs.min_sdk_version
    target_version = ctx.attrs.target_sdk_version
    if min_version == None and target_version == None:
        return None
    elif min_version != None and target_version == None:
        return min_version
    elif min_version == None and target_version != None:
        fail("Cannot set target_sdk_version without min_sdk_version")
    elif version_is_greater(min_version, target_version):
        warning("Target SDK version {} is less than minimum supported version {}".format(target_version, min_version))
        return min_version
    else:
        return target_version

def get_target_sdk_version(ctx: AnalysisContext) -> [None, str]:
    toolchain_target_sdk_version = get_cxx_toolchain_info(ctx).target_sdk_version
    target_sdk_version = getattr(ctx.attrs, "target_sdk_version", None)
    if toolchain_target_sdk_version == None and target_sdk_version == None:
        return None
    elif toolchain_target_sdk_version != None and target_sdk_version == None:
        return toolchain_target_sdk_version
    elif toolchain_target_sdk_version == None and target_sdk_version != None:
        return target_sdk_version
    elif version_is_greater(target_sdk_version, toolchain_target_sdk_version):
        # The requested target_sdk_version on the toolchain must be >=
        # the version set on the target, which should be the minimum
        # allowed for this version to build.
        fail("{} has target_sdk_version {}, which is larger than the toolchain target_sdk_version of {}".format(
            ctx.label,
            target_sdk_version,
            toolchain_target_sdk_version,
        ))
    else:
        return toolchain_target_sdk_version

_PLATFORM_TARGET_TRIPLE_MAP = {
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

def _format_target_triple(ctx: AnalysisContext, version: str) -> str:
    platform_info = get_cxx_platform_info(ctx)
    platform_components = platform_info.name.split("-")
    if platform_components[0] not in _PLATFORM_TARGET_TRIPLE_MAP:
        fail("missing target triple for {}".format(platform_components[0]))

    triple_format_str = _PLATFORM_TARGET_TRIPLE_MAP[platform_components[0]]
    return triple_format_str.format(architecture = platform_components[1], version = version)

def get_target_triple(ctx: AnalysisContext) -> [None, str]:
    target_sdk_version = get_target_sdk_version(ctx)
    if target_sdk_version == None:
        return None

    return _format_target_triple(ctx, target_sdk_version)

def get_unversioned_target_triple(ctx: AnalysisContext) -> str:
    return _format_target_triple(ctx, "")

def get_target_sdk_version_flags(ctx: AnalysisContext) -> list[str]:
    if not (hasattr(ctx.attrs, "_cxx_toolchain") or hasattr(ctx.attrs, "_apple_toolchain")):
        return []

    target_triple = get_target_triple(ctx)
    if target_triple == None:
        return []

    return ["-target", target_triple]
