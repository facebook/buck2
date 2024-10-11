# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":tr.bzl", "tr")

def _my_java_library(ctx):
    _ = ctx  # buildifier: disable=unused-variable
    fail("we don't build it in test")

my_java_library = rule(
    impl = _my_java_library,
    attrs = {
        "java_version": attrs.int(),
    },
    cfg = tr,
)
