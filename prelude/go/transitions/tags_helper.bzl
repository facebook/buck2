# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:buckconfig.bzl", "read_list")

allowed_build_tags = read_list("go", "allowed_build_tags", default = [], root_cell = True)

def tag_to_constrant_value():
    return {tag: "prelude//go/tags/constraints:{}".format(tag) for tag in allowed_build_tags}

def selects_for_tags():
    selects = []
    for tag in allowed_build_tags:
        selects += select({
            "DEFAULT": [],
            "prelude//go/tags/constraints:{}".format(tag): [tag],
        })

    return selects
