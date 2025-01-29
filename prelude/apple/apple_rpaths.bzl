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

def get_rpath_flags_for_apple_binary(ctx: AnalysisContext) -> list[str]:
    if not ctx.attrs._apple_toolchain[AppleToolchainInfo].prelude_rpaths:
        return []

    is_mac_bundle = get_apple_sdk_name(ctx).startswith("mac")
    if is_mac_bundle:
        rpaths = [
            # @executable_path = MyApp.app/Contents/MacOS
            "@executable_path/../Frameworks",
        ] + ([
            # @executable_path = MyApp.app/Contents/PlugIns/MyExtension.appex/Contents/MacOS
            "@executable_path/../../../../Frameworks",
        ] if ctx.attrs.application_extension else [])
    else:
        rpaths = [
            # @executable_path = MyApp.app
            "@executable_path/Frameworks",
        ] + ([
            # @executable_path = MyApp.app/PlugIns/MyExtension.appex
            "@executable_path/../../Frameworks",
        ] if ctx.attrs.application_extension else [])

    return _rpath_flags_for_paths(rpaths)
