# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _tr(platform, refs, attrs):
    if attrs.java_version != 14:
        fail("java_version must be 14 in this test")
    _ = refs  # buildifier: disable=unused-variable
    return platform

tr = transition(
    impl = _tr,
    refs = {},
    attrs = [
        "java_version",
    ],
)
