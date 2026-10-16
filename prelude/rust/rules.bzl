# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":rust_binary.bzl", "rust_binary", "rust_test")
load(":rust_library.bzl", "rust_library")

rules = {
    "rust_binary": rust_binary,
    "rust_library": rust_library,
    "rust_test": rust_test,
}
