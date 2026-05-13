# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Loads `@fbcode//buck2/tests:buck_e2e.bzl` resolve to the shim cell because
# `fbcode` is aliased to the shim cell in OSS. Forward to the real file in the
# buck2 root cell so the e2e tests can actually be parsed.

load(
    "@gh_facebook_buck2//tests:buck_e2e.bzl",
    _buck2_core_tests = "buck2_core_tests",
    _buck2_e2e_test = "buck2_e2e_test",
    _buck_e2e_test = "buck_e2e_test",
)

buck2_core_tests = _buck2_core_tests
buck2_e2e_test = _buck2_e2e_test
buck_e2e_test = _buck_e2e_test
