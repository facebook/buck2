# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode_macros//build_defs:native_rules.bzl", "buck_filegroup")
load("@fbsource//tools/build_defs/buck2:is_buck2.bzl", "is_buck2")

def export_prelude(srcs):
    buck_filegroup(
        name = "files",
        srcs = srcs,
    )

    # Re-export filegroups that are behind package boundary violations for
    # Buck2.
    if is_buck2():
        buck_filegroup(
            name = "prelude",
            srcs = {
                "": ":files",
                "os_lookup": "prelude//os_lookup:files",
            },
        )
    else:
        buck_filegroup(
            name = "prelude",
            srcs = srcs,
        )
