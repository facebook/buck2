# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _rule_for_test_kill_impl(
        # starlark-lint-disable unused-argument
        ctx):  # @unused
    return [DefaultInfo()]

rule_for_test_kill = rule(
    impl = _rule_for_test_kill_impl,
    attrs = {},
)
