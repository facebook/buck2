# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import typing

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(skip_for_os=["darwin", "windows"], disable_daemon_cgroup=False)
async def test_metrics_cgroup_no_resource_control(buck: Buck) -> None:
    write_config(buck, resource_control=False)
    snapshot = await start_daemon_and_get_snapshot(buck)
    assert snapshot["allprocs_cgroup"] is None
    assert snapshot["forkserver_actions_cgroup"] is None


@buck_test(skip_for_os=["darwin", "windows"], disable_daemon_cgroup=False)
async def test_metrics_cgroup_resource_control(buck: Buck) -> None:
    write_config(buck, resource_control=True)
    snapshot = await start_daemon_and_get_snapshot(buck)
    # Daemon should have allocated at least 500KB of anon memory
    assert snapshot["allprocs_cgroup"]["anon"] >= (
        snapshot["forkserver_actions_cgroup"]["anon"] + 500000
    )
    assert (
        snapshot["allprocs_cgroup"]["file"]
        >= snapshot["forkserver_actions_cgroup"]["file"]
    )
    assert (
        snapshot["allprocs_cgroup"]["kernel"]
        >= snapshot["forkserver_actions_cgroup"]["kernel"]
    )


# Placeholder for tests to be listed successfully on non-Linux platforms.
async def test_noop() -> None:
    pass


def write_config(buck: Buck, *, resource_control: bool) -> None:
    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2_resource_control]\n")
        buckconfig.write(f"status = {'required' if resource_control else 'off'}\n")


async def start_daemon_and_get_snapshot(buck: Buck) -> dict[str, typing.Any]:
    # Start the daemon
    await buck.targets(":")

    # Get the snapshot
    status_result = await buck.status("--snapshot")
    status_data = json.loads(status_result.stdout)
    snapshot = status_data["snapshot"]
    return snapshot
