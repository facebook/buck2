# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode_macros//build_defs:native_rules.bzl", "buck_filegroup")

def export_prelude(srcs):
    buck_filegroup(
        name = "files",
        srcs = srcs,
    )

    buck_filegroup(
        name = "prelude",
        srcs = srcs,
    )
