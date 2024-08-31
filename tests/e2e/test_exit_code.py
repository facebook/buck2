# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import asyncio
import sys
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import ExitCodeV2
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env

TARGET_BASE = "fbcode//buck2/tests/targets/errors"


def append_mode(mut_l: List[str]) -> None:
    if sys.platform == "darwin":
        mut_l.append("@fbcode//mode/mac")
    elif sys.platform == "win32":
        mut_l.append("@fbcode//mode/win")


@buck_test(inplace=True)
async def test_exit_code_build_success(buck: Buck) -> None:
    argv = [TARGET_BASE + ":build_success"]
    append_mode(argv)
    result = await buck.build(*argv)
    assert result.process.returncode == ExitCodeV2.SUCCESS.value


# The source contains a syntax error.
@buck_test(inplace=True)
async def test_exit_code_build_compile_error(buck: Buck) -> None:
    argv = [TARGET_BASE + ":build_compile_error"]
    append_mode(argv)
    await expect_failure(buck.build(*argv), exit_code=ExitCodeV2.USER_ERROR)


# The source of the test binary contains a syntax error.
@buck_test(inplace=True)
async def test_exit_code_test_compile_error(buck: Buck) -> None:
    argv = [TARGET_BASE + ":test_compile_error"]
    append_mode(argv)
    await expect_failure(buck.test(*argv), exit_code=ExitCodeV2.USER_ERROR)


# Deliberately cause RE (infra) error.
@buck_test(inplace=True)
async def test_exit_code_re_error(buck: Buck) -> None:
    argv = [
        TARGET_BASE + ":build_success",
        "--remote-only",
        "-c",
        "build.default_remote_execution_use_case=something-bad",
    ]
    append_mode(argv)
    await expect_failure(buck.build(*argv), exit_code=ExitCodeV2.INFRA_ERROR)


# Deliberately cause a daemon connection failure.
@buck_test(inplace=True)
@env("BUCK2_TEST_FAIL_BUCKD_AUTH", "true")
# This test case spawns a loose daemon that we can't connect to. On windows
# this loose daemon will keep holding onto buck-out files after test case finishes
# and prevent other processes from changing them, so set a termination timeout
# of 20 seconds so that this loose daemon gets killed before test case finishes.
@env("BUCK2_TERMINATE_AFTER", "15")
async def test_exit_code_fail_buckd_auth_for_unknown_reason(buck: Buck) -> None:
    argv = [TARGET_BASE + ":build_success"]
    append_mode(argv)
    await expect_failure(
        buck.build(*argv), exit_code=ExitCodeV2.DAEMON_CONNECTION_FAILURE
    )
    await asyncio.sleep(
        20
    )  # Makes sure the daemon terminates before test case finishes
