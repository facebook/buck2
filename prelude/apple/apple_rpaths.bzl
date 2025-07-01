# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_platform_info")
load("@prelude//cxx:target_sdk_version.bzl", "get_target_sdk_version", "version_is_greater")

def _rpath_flags_for_paths(paths: list[str]) -> list[str]:
    return ["-Wl,-rpath," + p for p in paths]

_MACOS_BINARY_RPATHS = _rpath_flags_for_paths([
    # @executable_path = MyApp.app/Contents/MacOS
    "@executable_path/../Frameworks",
])

_MACOS_EXTENSION_RPATHS = _MACOS_BINARY_RPATHS + _rpath_flags_for_paths([
    # @executable_path = MyApp.app/Contents/PlugIns/MyExtension.appex/Contents/MacOS
    "@executable_path/../../../../Frameworks",
])

_MACOS_LIBRARY_RPATHS = _rpath_flags_for_paths([
    # @executable_path = MyApp.app/Contents/MacOS
    "@executable_path/../Frameworks",
    # @loader_path = MyApp.app/Contents/Frameworks/MyFramework.framework
    "@loader_path/Frameworks",
])

_MACOS_TEST_RPATHS = _rpath_flags_for_paths([
    # loader_path = MyApp.app/Contents/PlugIns/TestBundle.xctest/Contents/MacOS
    "@loader_path/../Frameworks",
    # executable_path = MyApp.app/Contents/MacOS
    "@executable_path/../Frameworks",
])

_IOS_BINARY_RPATHS = _rpath_flags_for_paths([
    # @executable_path = MyApp.app
    "@executable_path/Frameworks",
])

_IOS_EXTENSION_RPATHS = _IOS_BINARY_RPATHS + _rpath_flags_for_paths([
    # @executable_path = MyApp.app/PlugIns/MyExtension.appex
    "@executable_path/../../Frameworks",
])

_IOS_LIBRARY_RPATHS = _rpath_flags_for_paths([
    # @executable_path = MyApp.app
    "@executable_path/Frameworks",
    # @loader_path = MyApp.app/Frameworks/MyFramework.framework
    "@loader_path/Frameworks",
])

_IOS_TEST_RPATHS = _rpath_flags_for_paths([
    # loader_path = MyApp.app/PlugIns/TestBundle.xctest
    "@loader_path/Frameworks",
    # executable_path = MyApp.app
    "@executable_path/Frameworks",
])

_SWIFT_LIB_RPATHS = _rpath_flags_for_paths([
    # Required for concurrency backport compatibility pre iOS 15.0
    "/usr/lib/swift",
])

_CONCURRENCY_INTRODUCED_VERSIONS = {
    # https://github.com/DougGregor/swift/blob/ebd7b4975558ae00e7f42ea0597ad91ab05ca4ca/stdlib/private/StdlibUnittest/CMakeLists.txt#L26
    "appletvos": "15.0",
    "appletvsimulator": "15.0",
    "iphoneos": "15.0",
    "iphonesimulator": "15.0",
    "maccatalyst": "15.0",
    "macosx": "12.0",
    "visionos": "1.0",
    "visionsimulator": "1.0",
    "watchos": "8.0",
    "watchsimulator": "8.0",
}

def _get_platform_name(ctx: AnalysisContext) -> str:
    return get_cxx_platform_info(ctx).name.split("-")[0]

def _is_mac_binary(platform: str) -> bool:
    return platform.startswith("mac")

def _swift_lib_rpaths(ctx: AnalysisContext, platform: str) -> list[str]:
    target_sdk_version = get_target_sdk_version(ctx)
    concurrency_introduced_version = _CONCURRENCY_INTRODUCED_VERSIONS[platform]

    if version_is_greater(concurrency_introduced_version, target_sdk_version):
        return _SWIFT_LIB_RPATHS
    else:
        return []

def get_rpath_flags_for_apple_binary(ctx: AnalysisContext) -> list[str]:
    platform = _get_platform_name(ctx)
    if _is_mac_binary(platform):
        if ctx.attrs.application_extension:
            return _swift_lib_rpaths(ctx, platform) + _MACOS_EXTENSION_RPATHS
        else:
            return _swift_lib_rpaths(ctx, platform) + _MACOS_BINARY_RPATHS
    elif ctx.attrs.application_extension:
        return _swift_lib_rpaths(ctx, platform) + _IOS_EXTENSION_RPATHS
    else:
        return _swift_lib_rpaths(ctx, platform) + _IOS_BINARY_RPATHS

def get_rpath_flags_for_library(ctx: AnalysisContext) -> list[str]:
    platform = _get_platform_name(ctx)
    if _is_mac_binary(platform):
        return _swift_lib_rpaths(ctx, platform) + _MACOS_LIBRARY_RPATHS
    else:
        return _swift_lib_rpaths(ctx, platform) + _IOS_LIBRARY_RPATHS

def get_rpath_flags_for_tests(ctx: AnalysisContext) -> list[str]:
    platform = _get_platform_name(ctx)
    if _is_mac_binary(platform):
        return _swift_lib_rpaths(ctx, platform) + _MACOS_TEST_RPATHS
    else:
        return _swift_lib_rpaths(ctx, platform) + _IOS_TEST_RPATHS
