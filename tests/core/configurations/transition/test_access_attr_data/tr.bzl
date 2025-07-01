# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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
