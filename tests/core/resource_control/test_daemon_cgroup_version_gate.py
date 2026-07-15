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
async def test_version_gate_enables_cgroup(buck: Buck) -> None:
    """When min_version_for_gated_status is set to a version <= the binary's
    DAEMON_CGROUP_VERSION, resource control should be enabled (status =
    if_available)."""

    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2_resource_control]\n")
        # Version 1 is the current DAEMON_CGROUP_VERSION, so this should enable.
        buckconfig.write("min_version_for_gated_status = 1\n")

    snapshot = await start_daemon_and_get_snapshot(buck)
    assert snapshot["allprocs_cgroup"] is not None


@buck_test(skip_for_os=["darwin", "windows"], disable_daemon_cgroup=False)
async def test_version_gated_default_status_overrides_gated_default(buck: Buck) -> None:
    """When the version gate passes, version_gated_default_status overrides the
    default gated status of if_available."""

    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2_resource_control]\n")
        buckconfig.write("min_version_for_gated_status = 1\n")
        buckconfig.write("version_gated_default_status = off\n")

    snapshot = await start_daemon_and_get_snapshot(buck)
    assert snapshot["allprocs_cgroup"] is None


@buck_test(skip_for_os=["darwin", "windows"], disable_daemon_cgroup=False)
async def test_explicit_status_overrides_gated_one(buck: Buck) -> None:
    """When the explicit status is present it takes precedence."""

    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2_resource_control]\n")
        buckconfig.write("status = off\n")
        buckconfig.write("min_version_for_gated_status = 1\n")
        buckconfig.write("version_gated_default_status = required\n")

    snapshot = await start_daemon_and_get_snapshot(buck)
    assert snapshot["allprocs_cgroup"] is None


@buck_test(skip_for_os=["darwin", "windows"], disable_daemon_cgroup=False)
async def test_version_gate_disables_cgroup_when_version_too_high(buck: Buck) -> None:
    """When min_version_for_gated_status is set to a version higher than the
    binary's DAEMON_CGROUP_VERSION, resource control should remain off."""

    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2_resource_control]\n")
        # Version 9999 is higher than any DAEMON_CGROUP_VERSION, so this should not enable.
        buckconfig.write("min_version_for_gated_status = 9999\n")

    snapshot = await start_daemon_and_get_snapshot(buck)
    assert snapshot["allprocs_cgroup"] is None


@buck_test(skip_for_os=["darwin", "windows"], disable_daemon_cgroup=False)
async def test_version_gate_not_set_status_off(buck: Buck) -> None:
    """When min_version_for_gated_status is not set and status is off,
    resource control should be off."""

    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2_resource_control]\n")
        buckconfig.write("status = off\n")

    snapshot = await start_daemon_and_get_snapshot(buck)
    assert snapshot["allprocs_cgroup"] is None


# Placeholder for tests to be listed successfully on non-Linux platforms.
async def test_noop() -> None:
    pass


async def start_daemon_and_get_snapshot(buck: Buck) -> dict[str, typing.Any]:
    await buck.targets(":")
    status_result = await buck.status("--snapshot")
    status_data = json.loads(status_result.stdout)
    return status_data["snapshot"]
