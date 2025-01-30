# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":apple_sdk.bzl", "get_apple_sdk_name")
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")

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

def _is_mac_binary(ctx: AnalysisContext) -> bool:
    return get_apple_sdk_name(ctx).startswith("mac")

def get_rpath_flags_for_apple_binary(ctx: AnalysisContext) -> list[str]:
    if not ctx.attrs._apple_toolchain[AppleToolchainInfo].prelude_rpaths:
        return []

    if _is_mac_binary(ctx):
        if ctx.attrs.application_extension:
            return _MACOS_EXTENSION_RPATHS
        else:
            return _MACOS_BINARY_RPATHS
    elif ctx.attrs.application_extension:
        return _IOS_EXTENSION_RPATHS
    else:
        return _IOS_BINARY_RPATHS

def get_rpath_flags_for_library(ctx: AnalysisContext) -> list[str]:
    if not ctx.attrs._apple_toolchain[AppleToolchainInfo].prelude_rpaths:
        return []

    if _is_mac_binary(ctx):
        return _MACOS_LIBRARY_RPATHS
    else:
        return _IOS_LIBRARY_RPATHS

def get_rpath_flags_for_tests(ctx: AnalysisContext) -> list[str]:
    if not ctx.attrs._apple_toolchain[AppleToolchainInfo].prelude_rpaths:
        return []

    if _is_mac_binary(ctx):
        return _MACOS_TEST_RPATHS
    else:
        return _IOS_TEST_RPATHS
