# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//go_bootstrap:go_bootstrap.bzl", "go_bootstrap_binary")
load(":go_binary.bzl", "go_binary")
load(":go_exported_library.bzl", "go_exported_library")
load(":go_library.bzl", "go_library")
load(":go_stdlib.bzl", "go_stdlib")
load(":go_test.bzl", "go_test")

rules = {
    "go_binary": go_binary,
    "go_bootstrap_binary": go_bootstrap_binary,
    "go_exported_library": go_exported_library,
    "go_library": go_library,
    "go_stdlib": go_stdlib,
    "go_test": go_test,
}
