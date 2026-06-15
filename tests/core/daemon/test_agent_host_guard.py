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

# Cgroup of an agent-originated daemon; `BUCK2_TEST_DAEMON_ORIGINATING_CGROUP`
# overrides the real cgroup, which is unavailable since tests disable the cgroup spawner.
_AGENT_CGROUP = "/user.slice/3pai_sandbox.slice/buck2.scope"
_NON_AGENT_CGROUP = "/user.slice/user-1000.slice/buck2.scope"
_FAIL_MESSAGE = "Builds are blocked on this host for coding agents"


@buck_test()
async def test_agent_host_guard_disabled(buck: Buck) -> None:
    # No glob configured -> feature off, build succeeds even from an agent cgroup.
    await buck.build(
        ":pass",
        env={"BUCK2_TEST_DAEMON_ORIGINATING_CGROUP": _AGENT_CGROUP},
    )


@buck_test()
async def test_agent_host_guard_denied(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            ":pass",
            "-c",
            "buck2.agent_hostname_fail_glob=*",
            "-c",
            f"buck2.agent_hostname_fail_message={_FAIL_MESSAGE}",
            env={"BUCK2_TEST_DAEMON_ORIGINATING_CGROUP": _AGENT_CGROUP},
        ),
        stderr_regex=_FAIL_MESSAGE,
    )


@buck_test()
async def test_agent_host_guard_non_agent_cgroup(buck: Buck) -> None:
    # Hostname matches but the daemon is not agent-originated -> build succeeds.
    await buck.build(
        ":pass",
        "-c",
        "buck2.agent_hostname_fail_glob=*",
        "-c",
        f"buck2.agent_hostname_fail_message={_FAIL_MESSAGE}",
        env={"BUCK2_TEST_DAEMON_ORIGINATING_CGROUP": _NON_AGENT_CGROUP},
    )
