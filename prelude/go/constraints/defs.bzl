# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:native.bzl", "native")
load("@prelude//go/transitions:tags_helper.bzl", "allowed_tags")

def generate_tag_constraints():
    for tag in allowed_tags:
        setting_name = "tag_" + tag
        native.constraint_setting(
            name = "tag_" + tag,
            visibility = ["PUBLIC"],
        )

        native.constraint_value(
            name = setting_name + "__value",
            constraint_setting = ":" + setting_name,
            visibility = ["PUBLIC"],
        )
