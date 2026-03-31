# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":kotlin_library.bzl", "kotlin_library_impl")
load(":kotlin_test.bzl", "kotlin_test_impl")

implemented_rules = {
    "kotlin_library": kotlin_library_impl,
    "kotlin_test": kotlin_test_impl,
}
