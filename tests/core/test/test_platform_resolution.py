# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env


@buck_test()
@env("BUCK2_ALLOW_INTERNAL_TEST_RUNNER_DO_NOT_USE", "1")
async def test_platform_resolution(buck: Buck) -> None:
    # Setup is such that test target is incompatible with testee's default
    # target platform.
    await buck.test(
        ":my_rule",
        test_executor="",
    )
