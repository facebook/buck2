# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:genrule.bzl", "genrule_attributes")
load("@prelude//decls:common.bzl", "buck")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//js:js_bundle.bzl", "js_bundle_impl")
load("@prelude//js:js_bundle_genrule.bzl", "js_bundle_genrule_impl")
load("@prelude//js:js_library.bzl", "js_library_impl")

def _select_platform():
    return select({
        "DEFAULT": select({
            "DEFAULT": "android",
            "config//os/constraints:iphoneos": "ios",
            "config//os/constraints:macos": "macos",
            "config//os/constraints:windows": "windows",
        }),
        "config//react-native:macos": "macos",
        "fbsource//tools/build_defs/js/constraints/metro_js_platform_override:android": "android",
        "fbsource//tools/build_defs/js/constraints/metro_js_platform_override:ios": "ios",
        "fbsource//tools/build_defs/js/constraints/metro_js_platform_override:macos": "macos",
        "fbsource//tools/build_defs/js/constraints/metro_js_platform_override:vr": "vr",
        "fbsource//tools/build_defs/js/constraints/metro_js_platform_override:windows": "windows",
    })

def _is_release():
    return select({
        "DEFAULT": select({
            "DEFAULT": select({
                "DEFAULT": False,
                "fbsource//tools/build_defs/android/config:build_mode_opt": True,
            }),
            "config//build_mode/constraints:release": True,
        }),
        "config//runtime:fbcode": select({
            "DEFAULT": False,
            "config//build_mode/constraints:opt": True,
        }),
    })

def _select_asset_dest_path_resolver():
    return select({
        "DEFAULT": None,
        "fbsource//tools/build_defs/js/config:asset_dest_path_resolver_android": "android",
        "fbsource//tools/build_defs/js/config:asset_dest_path_resolver_generic": "generic",
    })

implemented_rules = {
    "js_bundle": js_bundle_impl,
    "js_bundle_genrule": js_bundle_genrule_impl,
    "js_library": js_library_impl,
}

extra_attributes = {
    "js_bundle": {
        "worker": attrs.exec_dep(),
        "_android_toolchain": toolchains_common.android(),
        "_is_release": attrs.bool(
            default = _is_release(),
        ),
        "_platform": attrs.string(
            default = _select_platform(),
        ),
    },
    "js_bundle_genrule": genrule_attributes() | {
        "has_content_based_path": attrs.bool(default = select({
            "DEFAULT": False,
            "config//features/apple:content_based_path_hashing_enabled": True,
        })),
        "type": attrs.string(
            default = "js_bundle_genrule",
        ),
        "_exec_os_type": buck.exec_os_type_arg(),
        "_is_release": attrs.bool(
            default = _is_release(),
        ),
        "_platform": attrs.string(
            default = _select_platform(),
        ),
    },
    "js_library": {
        "worker": attrs.exec_dep(),
        "_asset_dest_path_resolver": attrs.option(
            attrs.string(),
            default = _select_asset_dest_path_resolver(),
        ),
        "_is_release": attrs.bool(
            default = _is_release(),
        ),
        "_platform": attrs.string(
            default = _select_platform(),
        ),
    },
}
