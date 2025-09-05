# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import asyncio
import json
import os
import subprocess
import tempfile
import time
import uuid

from buck2.tests.e2e.forkserver.cgroup_v2 import Cgroup

from buck2.tests.e2e.forkserver.forkserver_client import Buck2ForkserverClient

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def start_buck_forkserver(
    buck: Buck, socket_path: str, cgroup_pool_size: int
) -> subprocess.Popen[str]:
    # clean up the socket file if it exists, otherwise the forkserver will raise an error
    if os.path.exists(socket_path):
        os.remove(socket_path)

    # create a temporary folder
    state_dir = tempfile.mkdtemp(prefix="buck_forkserver_state_dir_")

    resource_control = {
        "status": "Required",
        "memory_max": None,
        "memory_max_per_action": None,
        "hybrid_execution_memory_limit_gibibytes": None,
        "enable_action_cgroup_pool": True,
        "memory_pressure_threshold_percent": None,
        "cgroup_pool_size": cgroup_pool_size,
    }

    buck_command = buck.construct_buck_command(
        "forkserver",
        "--socket-path",
        socket_path,
        "--state-dir",
        state_dir,
        "--resource-control",
        json.dumps(resource_control),
    )

    systemd_cmd_prefix = [
        "systemd-run",
        "--user",
        "--scope",
        "--quiet",
        "--collect",  # Automatically garbage-collect the unit after the process exits
        "--property=Delegate=yes",
        "--slice=e2e_test_forkserver_daemon",
        "--slice-inherit",
        "--working-directory=" + state_dir,
        "--unit=forkserver_e2e_test_" + str(uuid.uuid4()),
    ]

    cmd = systemd_cmd_prefix + buck_command

    print(f"Launching forkserver: {' '.join(cmd)}")

    process = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    print(f"Forkserver started with PID: {process.pid}")

    time.sleep(1)  # wait for the forkserver to start

    # check forkserver is alive
    return_code = process.poll()
    if return_code is not None:
        message = f"Forkserver exited with return code: {return_code}"

        # get the stdout and stderr
        stdout, stderr = process.communicate()

        message += f"\nstdout: {stdout}"
        message += f"\nstderr: {stderr}"

        raise Exception(message)

    return process


def generate_random_socket_path() -> str:
    return "/tmp/buck_forkserver_e2e_test_" + str(uuid.uuid4()) + ".sock"


async def forkserver_run_sleep_task(
    client: Buck2ForkserverClient,
    sleep_time: int,
) -> asyncio.Task:
    task = asyncio.create_task(
        asyncio.to_thread(client.run_command, "sleep", argv=[str(sleep_time)])
    )
    # Force the event loop to schedule once to ensure the task starts executing
    await asyncio.sleep(0)
    return task


@buck_test(inplace=True)
async def test_cgroup_pool_created(buck: Buck) -> None:
    socket_file = generate_random_socket_path()

    process = start_buck_forkserver(
        buck,
        socket_file,
        cgroup_pool_size=8,
    )
    pid = process.pid
    root_cgroup = Cgroup.from_pid(pid)

    try:
        root_cgroup = Cgroup.from_pid(pid)

        assert root_cgroup.procs == [pid]

        unix_file = "unix:" + socket_file
        with Buck2ForkserverClient(unix_file) as client:
            # send one request to let the forkserver initialize
            client.set_log_filter("info")

            assert root_cgroup.procs == []

            assert len(root_cgroup.list_subcgroups()) == 2

            print(root_cgroup.list_subcgroups())

            cgroup_pool = root_cgroup.get_subcgroup("actions_cgroup_pool")
            # We moved the forkserver to this cgroup after the forkserver initialized
            forkserver_process_cgroup = root_cgroup.get_subcgroup("process")

            assert cgroup_pool is not None
            assert forkserver_process_cgroup is not None

            worker_0_cgroup = cgroup_pool.get_subcgroup("worker_000")

            assert worker_0_cgroup is not None

            assert len(cgroup_pool.list_subcgroups()) == 8
            assert forkserver_process_cgroup.procs == [pid]

            assert "memory" in worker_0_cgroup.controllers

            task = await forkserver_run_sleep_task(client, 2)
            await asyncio.sleep(0.5)

            peak = worker_0_cgroup.memory_peak
            assert peak is not None and peak > 0
            assert worker_0_cgroup.procs != []

            # wait for the task to finish
            await task
    finally:
        process.terminate()

    # wait for some time to ensure the forkserver is cleaned up
    await asyncio.sleep(1)
    # ensured cleaned up
    assert not root_cgroup.path.exists()
