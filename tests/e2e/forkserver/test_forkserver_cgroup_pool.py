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
        "memory_high": None,
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


@buck_test(inplace=True)
async def test_cgroup_pool_worker_reuse(buck: Buck) -> None:
    """Test that worker cgroups are properly reused after tasks complete."""
    socket_file = generate_random_socket_path()

    process = start_buck_forkserver(
        buck,
        socket_file,
        cgroup_pool_size=1,
    )
    pid = process.pid
    root_cgroup = Cgroup.from_pid(pid)

    try:
        unix_file = "unix:" + socket_file
        with Buck2ForkserverClient(unix_file) as client:
            # Initialize the forkserver
            client.set_log_filter("info")

            cgroup_pool = root_cgroup.get_subcgroup("actions_cgroup_pool")
            assert cgroup_pool is not None

            # Verify initial pool size
            assert len(cgroup_pool.list_subcgroups()) == 1

            worker_0_cgroup = cgroup_pool.get_subcgroup("worker_000")
            assert worker_0_cgroup is not None

            # Run first task
            task1 = await forkserver_run_sleep_task(client, 1)
            await asyncio.sleep(0.2)

            # Verify worker is in use
            assert worker_0_cgroup.procs != []
            first_peak = worker_0_cgroup.memory_peak
            assert first_peak is not None and first_peak > 0

            # Wait for first task to complete
            await task1

            # Small delay to allow cleanup
            await asyncio.sleep(0.1)

            # Worker should be free now (no processes)
            assert worker_0_cgroup.procs == []

            # Run second task - should reuse the same worker
            task2 = await forkserver_run_sleep_task(client, 1)
            await asyncio.sleep(0.2)

            # Verify the same worker is reused
            assert worker_0_cgroup.procs != []

            # Wait for second task to complete
            await task2

    finally:
        process.terminate()

    await asyncio.sleep(1)
    assert not root_cgroup.path.exists()


@buck_test(inplace=True)
async def test_cgroup_pool_capacity_overflow(buck: Buck) -> None:
    """Test that pool creates new workers when capacity is exceeded."""
    socket_file = generate_random_socket_path()

    process = start_buck_forkserver(
        buck,
        socket_file,
        cgroup_pool_size=2,  # Small initial capacity
    )
    pid = process.pid
    root_cgroup = Cgroup.from_pid(pid)

    try:
        unix_file = "unix:" + socket_file
        with Buck2ForkserverClient(unix_file) as client:
            # Initialize the forkserver
            client.set_log_filter("info")

            cgroup_pool = root_cgroup.get_subcgroup("actions_cgroup_pool")
            assert cgroup_pool is not None

            # Initial pool should have 2 workers
            initial_workers = len(cgroup_pool.list_subcgroups())
            assert initial_workers == 2

            # Start 3 concurrent tasks to exceed initial capacity
            tasks = []
            for _ in range(3):
                task = await forkserver_run_sleep_task(client, 2)
                tasks.append(task)

            # Wait a bit for tasks to be assigned to workers
            await asyncio.sleep(0.5)

            # Pool should have expanded to 3 workers
            final_workers = len(cgroup_pool.list_subcgroups())
            assert final_workers == 3

            # Verify worker naming (worker_000, worker_001, worker_002)
            worker_names = sorted([w.path.name for w in cgroup_pool.list_subcgroups()])
            expected_names = ["worker_000", "worker_001", "worker_002"]
            assert worker_names == expected_names

            # Verify all workers have the memory controller enabled
            for worker in cgroup_pool.list_subcgroups():
                assert "memory" in worker.controllers

            # Wait for all tasks to complete
            for task in tasks:
                await task

    finally:
        process.terminate()

    await asyncio.sleep(1)
    assert not root_cgroup.path.exists()


@buck_test(inplace=True)
async def test_cgroup_pool_concurrent_workers(buck: Buck) -> None:
    """Test multiple concurrent workers in different cgroups."""
    socket_file = generate_random_socket_path()

    process = start_buck_forkserver(
        buck,
        socket_file,
        cgroup_pool_size=4,
    )
    pid = process.pid
    root_cgroup = Cgroup.from_pid(pid)

    try:
        unix_file = "unix:" + socket_file
        with Buck2ForkserverClient(unix_file) as client:
            # Initialize the forkserver
            client.set_log_filter("info")

            cgroup_pool = root_cgroup.get_subcgroup("actions_cgroup_pool")
            assert cgroup_pool is not None

            # Start 3 concurrent long-running tasks
            tasks = []
            for _ in range(3):
                task = await forkserver_run_sleep_task(client, 3)
                tasks.append(task)

            # Wait for tasks to be assigned
            await asyncio.sleep(0.5)

            # Count how many workers are currently in use
            active_workers = 0
            worker_with_procs = []
            for worker in cgroup_pool.list_subcgroups():
                if worker.procs:
                    active_workers += 1
                    worker_with_procs.append(worker.path.name)

                    # Verify each active worker has memory usage
                    peak = worker.memory_peak
                    assert peak is not None and peak > 0

            # Should have 3 active workers
            assert active_workers == 3
            print(f"Active workers: {worker_with_procs}")

            # Verify workers are using different cgroups
            assert len(set(worker_with_procs)) == 3

            # Wait for all tasks to complete
            for task in tasks:
                await task

            # Wait for cleanup
            await asyncio.sleep(0.2)

            # All workers should be free now
            for worker in cgroup_pool.list_subcgroups():
                assert worker.procs == []

    finally:
        process.terminate()

    await asyncio.sleep(1)
    assert not root_cgroup.path.exists()


@buck_test(inplace=True)
async def test_cgroup_pool_worker_naming_large_capacity(buck: Buck) -> None:
    """Test worker naming convention for large pool sizes."""
    socket_file = generate_random_socket_path()

    process = start_buck_forkserver(
        buck,
        socket_file,
        cgroup_pool_size=1005,  # Test naming for workers > 1000
    )
    pid = process.pid
    root_cgroup = Cgroup.from_pid(pid)

    try:
        unix_file = "unix:" + socket_file
        with Buck2ForkserverClient(unix_file) as client:
            # Initialize the forkserver
            client.set_log_filter("info")

            cgroup_pool = root_cgroup.get_subcgroup("actions_cgroup_pool")
            assert cgroup_pool is not None

            # Check that we have the expected number of workers
            workers = cgroup_pool.list_subcgroups()
            assert len(workers) == 1005

            # Test naming convention for different ranges
            worker_names = [w.path.name for w in workers]

            # First few should be zero-padded to 3 digits
            assert "worker_000" in worker_names
            assert "worker_001" in worker_names
            assert "worker_999" in worker_names

            # Workers >= 1000 should not be zero-padded
            assert "worker_1000" in worker_names
            assert "worker_1004" in worker_names

            # Verify no incorrect naming
            assert (
                "worker_1000" in worker_names
            )  # Should not be worker_1000 (no padding)
            assert "worker_01000" not in worker_names  # Should not be over-padded

    finally:
        process.terminate()

    await asyncio.sleep(1)
    assert not root_cgroup.path.exists()


@buck_test(inplace=True)
async def test_cgroup_pool_memory_isolation(buck: Buck) -> None:
    """Test that memory tracking works correctly for isolated cgroups."""
    socket_file = generate_random_socket_path()

    process = start_buck_forkserver(
        buck,
        socket_file,
        cgroup_pool_size=2,
    )
    pid = process.pid
    root_cgroup = Cgroup.from_pid(pid)

    try:
        unix_file = "unix:" + socket_file
        with Buck2ForkserverClient(unix_file) as client:
            # Initialize the forkserver
            client.set_log_filter("info")

            cgroup_pool = root_cgroup.get_subcgroup("actions_cgroup_pool")
            assert cgroup_pool is not None

            worker_0 = cgroup_pool.get_subcgroup("worker_000")
            worker_1 = cgroup_pool.get_subcgroup("worker_001")
            assert worker_0 is not None
            assert worker_1 is not None

            # Both workers should start with no memory usage
            assert worker_0.memory_peak is None or worker_0.memory_peak == 0
            assert worker_1.memory_peak is None or worker_1.memory_peak == 0

            # Run task in first worker
            task1 = await forkserver_run_sleep_task(client, 2)
            await asyncio.sleep(0.5)

            # First worker should have memory usage, second should not
            peak_0_after_task1 = worker_0.memory_peak
            peak_1_after_task1 = worker_1.memory_peak

            assert peak_0_after_task1 is not None and peak_0_after_task1 > 0
            # Second worker should still be unused
            assert peak_1_after_task1 is None or peak_1_after_task1 == 0

            # Start second task (should use second worker)
            task2 = await forkserver_run_sleep_task(client, 2)
            await asyncio.sleep(0.5)

            # Now both workers should have memory usage
            peak_0_after_task2 = worker_0.memory_peak
            peak_1_after_task2 = worker_1.memory_peak

            assert peak_0_after_task2 is not None and peak_0_after_task2 > 0
            assert peak_1_after_task2 is not None and peak_1_after_task2 > 0

            # Wait for tasks to complete
            await task1
            await task2

    finally:
        process.terminate()

    await asyncio.sleep(1)
    assert not root_cgroup.path.exists()


@buck_test(inplace=True)
async def test_cgroup_pool_structure_integrity(buck: Buck) -> None:
    """Test the overall cgroup structure and hierarchy."""
    socket_file = generate_random_socket_path()

    process = start_buck_forkserver(
        buck,
        socket_file,
        cgroup_pool_size=5,
    )
    pid = process.pid
    root_cgroup = Cgroup.from_pid(pid)

    try:
        unix_file = "unix:" + socket_file
        with Buck2ForkserverClient(unix_file) as client:
            # Initialize the forkserver
            client.set_log_filter("info")

            # Verify root cgroup structure
            subcgroups = root_cgroup.list_subcgroups()
            assert len(subcgroups) == 2

            subcgroup_names = {cg.path.name for cg in subcgroups}
            assert subcgroup_names == {"actions_cgroup_pool", "process"}

            # Verify forkserver process cgroup
            process_cgroup = root_cgroup.get_subcgroup("process")
            assert process_cgroup is not None
            assert process_cgroup.procs == [pid]

            # Verify pool cgroup structure
            pool_cgroup = root_cgroup.get_subcgroup("actions_cgroup_pool")
            assert pool_cgroup is not None
            assert pool_cgroup.procs == []  # Should have no direct processes

            # Verify subtree control is enabled for memory
            assert "memory" in pool_cgroup.subtree_control

            # Verify all worker cgroups exist and are properly configured
            workers = pool_cgroup.list_subcgroups()
            assert len(workers) == 5

            for i, worker in enumerate(sorted(workers, key=lambda w: w.path.name)):
                expected_name = f"worker_{i:03d}"
                assert worker.path.name == expected_name

                # Each worker should have memory controller available
                assert "memory" in worker.controllers

                # Workers should start with no processes
                assert worker.procs == []

    finally:
        process.terminate()

    await asyncio.sleep(1)
    assert not root_cgroup.path.exists()
