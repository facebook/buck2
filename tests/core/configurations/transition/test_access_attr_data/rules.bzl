# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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
