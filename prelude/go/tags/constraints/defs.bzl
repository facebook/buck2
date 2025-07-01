# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:native.bzl", "native")
load("@prelude//go/transitions:tags_helper.bzl", "allowed_build_tags")

def generate_tag_constraints():
    for tag in allowed_build_tags:
        setting_name = "setting__" + tag
        native.constraint_setting(
            name = setting_name,
            visibility = ["PUBLIC"],
        )

        native.constraint_value(
            name = tag,
            constraint_setting = ":" + setting_name,
            visibility = ["PUBLIC"],
        )
