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
from buck2.tests.e2e_util.helper.utils import filter_events, random_string


@buck_test(skip_for_os=["windows", "darwin"], disable_daemon_cgroup=False)
async def test_orphan_pids_killed(buck: Buck) -> None:
    await buck.build(
        "root//:spawn_orphan",
        "--no-remote-cache",
        "--local-only",
        "-c",
        f"test.cache_buster={random_string()}",
    )

    events = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "OrphanProcessesKilled",
    )

    assert len(events) > 0, "Expected at least one OrphanProcessesKilled instant event"

    orphan_processes = events[0]["orphan_processes"]
    assert len(orphan_processes) > 0, (
        f"Expected at least one orphan process, got: {orphan_processes}"
    )

    # The orphan should be the 'setsid' process that escaped the process group
    comms = [p["comm"] for p in orphan_processes]
    assert any("setsid" in c for c in comms), (
        f"Expected to find a 'setsid' orphan process, got comms: {comms}"
    )


@buck_test()
def test_nop(buck: Buck) -> None:
    # Pytest gets upset if we have no windows or mac tests in this file
    pass


@buck_test(skip_for_os=["windows", "darwin"], disable_daemon_cgroup=False)
async def test_no_orphan_same_pg_timeout(buck: Buck) -> None:
    # Build a target that spawns a background process in the same process
    # group. The action has a short timeout, so it will be cancelled via
    # killpg, which kills the background process too. Cgroup cleanup should
    # find no remaining processes, so no OrphanProcessesKilled event.
    await expect_failure(
        buck.build(
            "root//:spawn_same_pg_timeout",
            "--no-remote-cache",
            "--local-only",
            "-c",
            f"test.cache_buster={random_string()}",
        ),
        stderr_regex="timed out after",
    )

    events = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "OrphanProcessesKilled",
    )

    assert len(events) == 0, (
        f"Expected no OrphanProcessesKilled events for same-process-group child, got: {events}"
    )
