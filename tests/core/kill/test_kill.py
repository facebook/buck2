#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe


import platform

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env


_BUCK_TEST_DECORATOR = buck_test(
    # On windows, we get an error of form
    # "The process cannot access the file because it is being used by another process"
    # when trying to kill the daemon with sqlite materializer state enabled. This is most
    # likely because we don't kill all child processes of the daemon and so the sqlite process
    # is still running and accessing the sqlite db file when being killed. Given this is a
    # pre-existing issue, we disable sqlite materializer state on windows for now.
    extra_buck_config={
        "buck2": {"sqlite_materializer_state": "false"},
    }
    if platform.system() == "Windows"
    else {},
)


@_BUCK_TEST_DECORATOR
@env("BUCK2_TEST_FAIL_BUCKD_AUTH", "true")
async def test_kill_error(buck: Buck) -> None:
    # Performing a build should fail, since we will not be able to authenticate to the
    # buck daemon
    await expect_failure(buck.build("//:abc"), stderr_regex="injected auth error")

    # Kill should succeed, even though we cannot authenticate to the daemon
    await buck.kill()


@_BUCK_TEST_DECORATOR
@env("BUCK2_TEST_FAIL_BUCKD_AUTH", "true")
async def test_clean_error(buck: Buck) -> None:
    # Performing a build should fail, since we will not be able to authenticate to the
    # buck daemon
    await expect_failure(buck.build("//:abc"), stderr_regex="injected auth error")

    # Clean should succeed, even though we cannot authenticate to the daemon
    await buck.clean()
