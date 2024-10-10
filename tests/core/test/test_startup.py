# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env


@buck_test()
@env("BUCK2_TEST_TPX_USE_TCP", "true")
async def test_tcp_startup_fail(buck: Buck) -> None:
    # Python is a binary that will just fail when we give it our executor args
    # but works on any platform. It's a bit dumb but it'll do
    await expect_failure(
        buck.test("...", test_executor="python3"),
        stderr_regex="Executor exited before connecting",
    )
