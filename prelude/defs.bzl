# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbsource//tools/build_defs/buck2:is_buck2.bzl", "is_buck2")
load(":native.bzl", "native")

def export_prelude(srcs):
    native.filegroup(
        name = "files",
        srcs = srcs,
        visibility = ["PUBLIC"],
    )

    # Re-export filegroups that are behind package boundary violations for
    # Buck2.
    if is_buck2():
        # Tests want BUCK.v2 instead of TARGETS.v2
        native.genrule(
            name = "copy_android_constraint",
            out = "BUCK.v2",
            cmd = "cp $(location prelude//android/constraints:files)/TARGETS.v2 $OUT",
            visibility = ["PUBLIC"],
        )

        native.filegroup(
            name = "prelude",
            srcs = {
                "": ":files",
                "android/constraints/BUCK.v2": ":copy_android_constraint",
            },
            visibility = ["PUBLIC"],
        )
    else:
        native.filegroup(
            name = "prelude",
            srcs = srcs,
            visibility = ["PUBLIC"],
        )
