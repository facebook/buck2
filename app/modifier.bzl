# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//buck2/cfg/experimental:modifiers.bzl", "modifiers")

def buck2_modifiers():
    # **WARNING**: This is not vetted for correctness and should only be used in fbcode/buck2.
    # A somewhat hacked together list of modifiers to enable mode-free builds and opt-by-default-cxx builds.
    # This currently only works for linux and mac but not for cross-building (ex. build mac from linux)
    # - Mode-free builds: Users can use `-m opt` or `--modifier opt` instead of `@fbcode//mode/opt` to trigger builds
    #   E2e tests build with opt buck2 by default.
    # - Opt-by-default cxx: Dev mode builds of buck2 comes with optimized, sanitizer-free cxx deps by default, making
    #   dev mode buck2 significantly more usable
    #
    # This is a demo that all of this could be done with modifiers. Most of these modifiers should be set on higher-level
    # PACKAGE files like fbcode/PACKAGE or fbsource/PACKAGE.
    #
    # Known problems:
    # - We have to explicitly disable link groups at the moment because link group macros are supposed to only be turned
    #   on for dev mode and it checks for this by reading the dev mode buckconfig, but the opt modifier also uses dev
    #   mode buckconfig, and as a result the presence of link group map breaks our opt modifier build.

    return [
        modifiers.conditional({
            "DEFAULT": modifiers.conditional({
                "DEFAULT": "ovr_config//build_mode/default_opt_cxx:disabled",
                "ovr_config//build_mode:dev": "ovr_config//build_mode/default_opt_cxx:enabled",
                "ovr_config//build_mode:opt": "ovr_config//build_mode/default_opt_cxx:enabled",
            }),
            # Opt by default cxx toolchain would override the thin-lto toolchain, so don't use opt by default toolchain
            # if thin-lto is present
            "ovr_config//build_mode/constraints:lto-thin": "ovr_config//build_mode/default_opt_cxx:disabled",
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": None,
                "ovr_config//os:linux": modifiers.conditional({
                    "DEFAULT": modifiers.conditional({
                        "DEFAULT": "ovr_config//build_mode/constraints:static",
                        "ovr_config//build_mode:dev": "ovr_config//build_mode/constraints:shared",
                    }),
                    "ovr_config//build_mode:asan": "ovr_config//build_mode/constraints:static_pic",
                }),
            }),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": None,
                "ovr_config//os:linux": modifiers.conditional({
                    "DEFAULT": "ovr_config//build_mode/constraints:split-dwarf-single",
                    "ovr_config//build_mode:dev": None,
                }),
            }),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": (
                "ovr_config//build_mode:no-san"
            ),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": "ovr_config//build_mode/constraints:fbcode-build-info-mode-stable",
                "ovr_config//build_mode:opt": "ovr_config//build_mode/constraints:fbcode-build-info-mode-full",
            }),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": "ovr_config//build_mode/constraints:python-default-package-style-inplace",
                "ovr_config//build_mode:opt": "ovr_config//build_mode/constraints:python-default-package-style-standalone",
            }),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": None,
                "ovr_config//os:macos": "ovr_config//build_mode/constraints:fbcode-build-info-ldflags-accepted",
            }),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": None,
                "ovr_config//os:macos": "ovr_config//build_mode/constraints:fbcode-custom-allocators-enabled",
            }),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": None,
                "ovr_config//runtime:fbcode": modifiers.conditional({
                    "ovr_config//cpu:arm64": "ovr_config//runtime/constraints:platform010-aarch64",
                    "ovr_config//cpu:x86_64": "ovr_config//runtime/constraints:platform010",
                }),
            }),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": None,
                "ovr_config//runtime:fbcode": modifiers.conditional({
                    "ovr_config//cpu:arm64": "ovr_config//cpu/constraints:nosve2",
                    "ovr_config//cpu:x86_64": "ovr_config//cpu/constraints:sve2",
                }),
            }),
        }),
        modifiers.conditional({
            "DEFAULT": None,
            "ovr_config//build_mode/default_opt_cxx:enabled": modifiers.conditional({
                "DEFAULT": None,
                "ovr_config//os:linux": modifiers.conditional({
                    "DEFAULT": None,
                    "ovr_config//cpu:arm64": "ovr_config//cpu/constraints:armv8.4a",
                }),
            }),
        }),
    ]

def disable_buck2_modifiers():
    return ["ovr_config//build_mode/default_opt_cxx:disabled"]
