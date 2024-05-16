# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:prelude.bzl", "native")

# NB: Meta engineers should not use this! Please use tp2 instead:
# https://fburl.com/wiki/oyy0fi5j
#
# If a system has a package installed and that package provides a `.pc` file
# this rule can be used to make that library visible to other rules. The `name`
# of this rule should be the pkg-config name. For example, if
# `pkg-config --libs gtest` prints out the flags to link against gtest, then
# `external_pkgconfig_library(name = "gtest")` would allow other rules to
# depend on gtest.
#
# WARNING: dependencies are not resolved by pkg-config, so these must be specified
# manually with `deps`. Additionally, ABI/platform differences are not handled
# by this rule so be careful not to cache it in Remote Execution etc to prevent
# different machines from reusing the outputs of these rules.
def external_pkgconfig_library(
        name,
        visibility = ["PUBLIC"],
        labels = [],
        default_target_platform = "prelude//platforms:default",
        deps = []):
    pkg_config_cflags = name + "__pkg_config_cflags"
    native.genrule(
        name = pkg_config_cflags,
        default_target_platform = default_target_platform,
        out = "out",
        cmd = "pkg-config --cflags {} > $OUT".format(name),
        remote = False,
    )
    pkg_config_libs = name + "__pkg_config_libs"
    native.genrule(
        name = pkg_config_libs,
        default_target_platform = default_target_platform,
        out = "out",
        cmd = "pkg-config --libs {} > $OUT".format(name),
        remote = False,
    )

    labels = list(labels)
    labels.append("third-party:pkg-config:{}".format(name))

    native.prebuilt_cxx_library(
        name = name,
        default_target_platform = default_target_platform,
        visibility = visibility,
        exported_preprocessor_flags = ["@$(location :{})".format(pkg_config_cflags)],
        exported_linker_flags = ["@$(location :{})".format(pkg_config_libs)],
        exported_deps = deps,
        labels = labels,
    )
