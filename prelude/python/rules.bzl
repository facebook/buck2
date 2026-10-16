# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//python_bootstrap:python_bootstrap.bzl", "python_bootstrap_binary", "python_bootstrap_library")
load(":cxx_python_extension.bzl", "cxx_python_extension")
load(":prebuilt_python_library.bzl", "prebuilt_python_library")
load(":python_binary.bzl", "python_binary")
load(":python_library.bzl", "python_library")
load(":python_needed_coverage_test.bzl", "python_needed_coverage_test")
load(":python_runtime_bundle.bzl", "python_runtime_bundle")
load(":python_test.bzl", "python_test")

rules = {
    "cxx_python_extension": cxx_python_extension,
    "prebuilt_python_library": prebuilt_python_library,
    "python_binary": python_binary,
    "python_bootstrap_binary": python_bootstrap_binary,
    "python_bootstrap_library": python_bootstrap_library,
    "python_library": python_library,
    "python_needed_coverage_test": python_needed_coverage_test,
    "python_runtime_bundle": python_runtime_bundle,
    "python_test": python_test,
}
