# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":julia_binary.bzl", "julia_binary")
load(":julia_library.bzl", "julia_jll_library", "julia_library")
load(":julia_test.bzl", "julia_test")

rules = {
    "julia_binary": julia_binary,
    "julia_jll_library": julia_jll_library,
    "julia_library": julia_library,
    "julia_test": julia_test,
}
