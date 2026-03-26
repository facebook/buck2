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
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(skip_for_os=["windows", "darwin"], disable_daemon_cgroup=False)
async def test_network_isolated(buck: Buck) -> None:
    """Verify that network_access='none' blocks network in a new network namespace."""
    # The test command tries to bind to localhost. With network isolation the
    # loopback interface is DOWN, so the bind fails and the test exits non-zero.
    await expect_failure(
        buck.test(
            "root//:network_isolated",
            "--local-only",
            "--no-remote-cache",
        ),
    )


@buck_test(skip_for_os=["windows", "darwin"], disable_daemon_cgroup=False)
async def test_network_accessible(buck: Buck) -> None:
    """Control: verify network works when network_access is not set."""
    await buck.test(
        "root//:network_accessible",
        "--local-only",
        "--no-remote-cache",
    )


@buck_test(skip_for_os=["windows", "darwin"])
async def test_network_isolation_requires_cgroups(buck: Buck) -> None:
    """Without cgroups, network_access='none' is a no-op and network stays up."""
    # disable_daemon_cgroup defaults to True, so no cgroups here.
    # The bind should succeed even though network_access = "none".
    await buck.test(
        "root//:network_isolated",
        "--local-only",
        "--no-remote-cache",
    )


@buck_test()
def test_nop(buck: Buck) -> None:
    # Pytest gets upset if we have no windows or mac tests in this file
    pass
