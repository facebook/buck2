# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env


@buck_test()
@env("BUCK2_ALLOW_INTERNAL_TEST_RUNNER_DO_NOT_USE", "1")
async def test_platform_resolution(buck: Buck) -> None:
    await buck.test(
        ":my_test",
        test_executor="",
    )
    res = await buck.log("what-ran")
    assert "MY_RESOURCE_ID=42" in res.stdout


@buck_test(skip_for_os=["windows", "darwin"], disable_daemon_cgroup=False)
@env("BUCK2_ALLOW_INTERNAL_TEST_RUNNER_DO_NOT_USE", "1")
async def test_local_resource_broker_survives_cgroup_cleanup(buck: Buck) -> None:
    await buck.test(
        ":my_daemon_test",
        test_executor="",
    )
    res = await buck.log("what-ran")
    assert "BROKER_PID=" in res.stdout


@buck_test()
@env("BUCK2_ALLOW_INTERNAL_TEST_RUNNER_DO_NOT_USE", "1")
async def test_two_resource_types_from_one_broker(buck: Buck) -> None:
    # Two resource types served by the same target: the setup command runs once per type and the
    # test is rejected because its resource states come from the same target.
    await expect_failure(
        buck.test(":two_types_one_broker", test_executor=""),
        stderr_regex="supposed to come from a different target",
    )
    res = await buck.log("what-ran")
    assert res.stdout.count("test.local_resource_setup") == 2
