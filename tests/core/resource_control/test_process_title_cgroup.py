# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import subprocess

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(skip_for_os=["darwin", "windows"], disable_daemon_cgroup=False)
async def test_process_title_with_cgroup(buck: Buck) -> None:
    """Verify that the daemon process title is set correctly when spawned
    via systemd-run (the cgroup/resource control path)."""

    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2_resource_control]\n")
        buckconfig.write("status_if_min_daemon_cgroup_version = 1\n")

    await buck.targets(":")
    status = await buck.status()
    status_data = json.loads(status.stdout)
    pid = status_data["process_info"]["pid"]

    # Confirm the daemon is running inside a cgroup (systemd-run was used).
    snapshot_result = await buck.status("--snapshot")
    snapshot = json.loads(snapshot_result.stdout)["snapshot"]
    assert snapshot["allprocs_cgroup"] is not None

    # Verify the process title is set via the exec -a wrapper.
    out = subprocess.check_output(["ps", "-o", "cmd=", str(pid)]).strip()
    assert out.startswith(b"buck2d["), f"Expected 'buck2d[...' but got {out!r}"


# Placeholder so pytest doesn't complain on non-Linux platforms.
async def test_noop() -> None:
    pass
