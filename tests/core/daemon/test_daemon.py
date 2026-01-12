# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import platform
import re
import subprocess
import time
from pathlib import Path

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env


@buck_test()
@env("BUCK2_TESTING_INACTIVITY_TIMEOUT", "true")
async def test_inactivity_timeout(buck: Buck) -> None:
    #######################################################
    # Recommend running this test in opt mode
    # Otherwise the command that is run here
    # could take longer than 1 second to finish
    # causing this test to be flaky
    #######################################################

    # this will start the daemon
    await buck.targets("//:rule")

    time.sleep(1)  # 1 sec timeout

    # check it's dead
    for _ in range(20):
        time.sleep(1)
        result = await buck.status()
        if result.stderr.splitlines()[-1] == "no buckd running":
            stderr = await buck.daemon_stderr()
            assert "inactivity timeout elapsed" in stderr
            return

    raise AssertionError("Server did not die in 20 seconds")


@buck_test()
@pytest.mark.parametrize(
    "corrupt",
    ["not-json", '{"valid-json", "but-not-valid-data"}'],
)
async def test_corrupted_buckd_info(buck: Buck, corrupt: str) -> None:
    await buck.targets("//:rule")

    daemon_dir = await buck.get_daemon_dir()
    with open(f"{daemon_dir}/buckd.info") as f:
        # Check file exists and valid.
        json.load(f)

    # Kill that daemon now to avoid having making a mess and leaving 2 daemons
    # around.
    await buck.kill()

    with open(f"{daemon_dir}/buckd.info", "w") as f:
        f.write(corrupt)

    await buck.targets("//:rule")


@buck_test()
async def test_process_title(buck: Buck) -> None:
    await buck.build()  # Start the daemon
    status = await buck.status()
    status = json.loads(status.stdout)
    pid = status["process_info"]["pid"]

    if platform.system() == "Darwin":
        out = subprocess.check_output(["ps", "-o", "comm=", str(pid)]).strip()
        assert out.startswith(b"buck2d[")
    elif platform.system() == "Linux":
        out = subprocess.check_output(["ps", "-o", "cmd=", str(pid)]).strip()
        assert out.startswith(b"buck2d[")
    elif platform.system() == "Windows":
        # We guarantee no value there.
        pass
    else:
        raise Exception("Unknown platform")


@buck_test()
async def test_status_fields(buck: Buck) -> None:
    await buck.build()  # Start the daemon
    status = await buck.status()
    status = json.loads(status.stdout)
    assert status["valid_working_directory"]
    assert status["valid_buck_out_mount"]


@buck_test()
async def test_status_all(buck: Buck) -> None:
    # this will start the daemons
    await buck.server()

    status = await buck.status()
    status = json.loads(status.stdout)
    pid = status["process_info"]["pid"]

    status_all = await buck.status("--all")
    status_all = json.loads(status_all.stdout)
    for status in status_all:
        if status["process_info"]["pid"] == pid:
            return
    raise Exception(
        f"buckd status for pid {pid} not found in {json.dumps(status_all, indent=2)}"
    )


@buck_test()
@env("BUCK_LOG", "buck2_client_ctx::daemon::client::kill=debug")
async def test_no_buckd_kills_existing_daemon(buck: Buck) -> None:
    await buck.audit("cell")  # Start the daemon
    result = await buck.audit("cell", "--no-buckd")  # Kill the existing daemon
    assert "Killing daemon with PID" in result.stderr


@buck_test()
async def test_buck_out_is_cache_dir(buck: Buck) -> None:
    await buck.targets(":")  # Start a daemon
    root = await buck.root()
    assert (
        (Path(root.stdout.strip()) / "buck-out" / "v2" / "CACHEDIR.TAG")
        .read_text(encoding="utf-8")
        .startswith("Signature: 8a477f597d28d172789f06886806bc55")
    )


@buck_test()
async def test_prev_daemon_dir(buck: Buck) -> None:
    await buck.targets(":")  # Start a daemon
    await buck.kill()
    await buck.targets(":")  # Start another daemon

    def extract_pid(stderr: str) -> int:
        pid = [re.match(r".* PID: (\d+)", line) for line in stderr.splitlines()]
        pid = list(filter(None, pid))
        assert len(pid) == 1, pid[0]
        return int(pid[0].group(1))

    new_daemon_stderr = await buck.daemon_stderr()
    killed_daemon_stderr = await buck.prev_daemon_stderr()

    # check logs contain buckd pid and don't match
    assert extract_pid(new_daemon_stderr) != extract_pid(killed_daemon_stderr)

    assert "triggered shutdown: `buck kill` was invoked" in killed_daemon_stderr
