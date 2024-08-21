# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//buck2/cfg/experimental:modifiers.bzl", "modifiers")

def buck2_modifiers():
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
            "ovr_config//build_mode/default_opt_cxx:enabled": (
                "ovr_config//build_mode:no-san"
            ),
            # Unfortunately, setting `ovr_config//build_mode:no-san` like this is a bit problematic when using sanitizer
            # modefiles because the no-san value here would override the sanitizer constraint set by those modefiles
            # in the target platform, meaning we would always get sanitizer disabled no matter what sanitizer modefile
            # is used. To work around this, explicitly check that we are not using any sanitizer modefile by checking
            # that the `fbcode.sanitizer` buckconfig is set to the default value "address-undefined-dev". We can undo this
            # change in the future when our CI is updated to use sanitizer modifiers instead of sanitizer modefiles.
        }) if read_config("fbcode", "sanitizer") == "address-undefined-dev" else None,
    ]

def disable_buck2_modifiers():
    return ["ovr_config//build_mode/default_opt_cxx:disabled"]
