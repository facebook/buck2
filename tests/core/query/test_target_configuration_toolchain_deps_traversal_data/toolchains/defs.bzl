# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _toolchain(
        # starlark-lint-disable unused-argument
        ctx):  # @unused
    fail("we do not run analysis in query tests")

toolchain = rule(
    impl = _toolchain,
    attrs = {"dep": attrs.exec_dep()},
    is_toolchain_rule = True,
)
