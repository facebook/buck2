# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//nodejs:nodejs_binary.bzl", "nodejs_binary_impl")
load("@prelude//nodejs:nodejs_library.bzl", "nodejs_library_impl")
load("@prelude//nodejs:nodejs_test.bzl", "nodejs_test_impl")

implemented_rules = {
    "nodejs_binary": nodejs_binary_impl,
    "nodejs_library": nodejs_library_impl,
    "nodejs_test": nodejs_test_impl,
}
