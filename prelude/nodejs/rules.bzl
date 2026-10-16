# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":nodejs_binary.bzl", "nodejs_binary")
load(":nodejs_library.bzl", "nodejs_library")
load(":nodejs_node_modules.bzl", "nodejs_node_modules")
load(":nodejs_test.bzl", "nodejs_test")

rules = {
    "nodejs_binary": nodejs_binary,
    "nodejs_library": nodejs_library,
    "nodejs_node_modules": nodejs_node_modules,
    "nodejs_test": nodejs_test,
}
