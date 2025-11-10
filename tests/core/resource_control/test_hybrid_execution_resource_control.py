# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from __future__ import annotations

import json
import os
import re
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import filter_events, timestamp_ms


# To not fail listing on Mac or Windows
def test_dummy() -> None:
    pass


def configure_memory_limit(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write("hybrid_execution_memory_limit_gibibytes = 0\n")


def configure_freezing_with_pressure(buck: Buck, kill_and_retry: bool) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write("enable_suspension = true\n")
        f.write("memory_pressure_threshold_percent = 0\n")
        if kill_and_retry:
            f.write("preferred_action_suspend_strategy = kill_and_retry\n")


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK2_HARD_ERROR", "panic")
async def test_no_local_action_when_full_hybrid_given_memory_pressure(
    buck: Buck,
) -> None:
    configure_memory_limit(buck)
    await buck.build(
        ":merge_100",
        "--no-remote-cache",
        "-c",
        "build.use_limited_hybrid=False",
        "-c",
        "build.execution_platforms=//:platforms",
    )
    commands_per_action: List[List[Dict[str, Any]]] = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
        "commands",
    )
    commands = [c for cs in commands_per_action for c in cs]
    commands = [
        cmd
        for c in commands
        if (cmd := _get(c, "details", "command_kind", "command")) is not None
    ]
    assert len(commands) > 0

    assert not any(
        "LocalCommand" in c or "OmittedLocalCommand" in c for c in commands
    ), "Actions should be forced to run on RE since memory limit is set to 0 and we are always under memory pressure"


@dataclass
class _ActionExecution:
    span_id: int
    start_timestamp_ms: int
    end_timestamp_ms: int

    def __lt__(self: _ActionExecution, other: _ActionExecution) -> bool:
        return self.start_timestamp_ms < other.start_timestamp_ms


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK2_HARD_ERROR", "panic")
async def test_local_actions_throttled_when_limited_hybrid_given_memory_pressure(
    buck: Buck,
) -> None:
    configure_memory_limit(buck)
    await buck.build(
        ":merge_100",
        "--no-remote-cache",
        "-c",
        "build.use_limited_hybrid=True",
        "-c",
        "build.execution_platforms=//:platforms",
        "-c",
        "test.prefer_local=True",
    )

    actions = []
    result = await buck.log("show")
    for line in result.stdout.splitlines():
        json_object = json.loads(line)
        action_end: Optional[Dict[str, Any]] = _get(
            json_object, "Event", "data", "SpanEnd", "data", "ActionExecution"
        )
        if action_end is not None:
            span_id: Optional[int] = _get(json_object, "Event", "span_id")
            assert span_id is not None, f"Failed to span ID out of {json_object}"
            timestamp_value = _get(json_object, "Event", "timestamp")
            assert (
                timestamp_value is not None
            ), f"Failed to parse timestamp out of {json_object}"

            action_commands = _get(action_end, "commands")
            assert len(action_commands) == 1

            start_timestamp_value = _get(
                action_commands[0], "details", "metadata", "start_time"
            )
            start_timestamp = timestamp_ms(*start_timestamp_value)

            end_timestamp_value = _get(json_object, "Event", "timestamp")
            assert (
                end_timestamp_value is not None
            ), f"Failed to parse timestamp out of {json_object}"
            end_timestamp = timestamp_ms(*end_timestamp_value)

            a = _ActionExecution(
                span_id=span_id,
                start_timestamp_ms=start_timestamp,
                end_timestamp_ms=end_timestamp,
            )
            actions.append(a)

    actions.sort()

    for lhs, rhs in zip(actions, actions[1:]):
        assert (
            lhs.end_timestamp_ms <= rhs.start_timestamp_ms
        ), f"Execution of action {lhs} is overlapping with {rhs}"


def _get(data: Dict[str, Any], *key: str) -> Any:
    for k in key:
        data = data.get(k)
        if data is None:
            return None

    return data


def _use_some_memory_args(buck: Buck) -> list[str]:
    return [
        "-c",
        f"use_some_memory.path={os.environ["USE_SOME_MEMORY_BIN"]}",
    ]


@buck_test(skip_for_os=["darwin", "windows"])
async def test_memory_pressure_telemetry(
    buck: Buck,
) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write("memory_high_per_action = 1048576\n")  # 1 MiB

    await buck.build(
        ":allocate_10_10M",
        "--no-remote-cache",
        "-c",
        "build.use_limited_hybrid=False",
        "-c",
        "build.execution_platforms=//:platforms",
    )

    memory_pressure = await filter_events(
        buck, "Event", "data", "Instant", "data", "MemoryPressure"
    )

    # We can't reliably predict how many events will be fired and how high the pressure % will reach,
    # but it should be more than 0 with how low we set the memory_high as and obviously % should be <= 100
    assert len(memory_pressure) > 0
    last_pressure = memory_pressure[-1]
    peak_value = last_pressure["peak_pressure"]
    assert (
        peak_value <= 100
    ), f"Expected % peak_pressure to be at most 100, got {peak_value}"


@buck_test(skip_for_os=["darwin", "windows"])
async def test_resource_control_events_created(
    buck: Buck,
) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write("status = required\n")
        f.write("enable_action_cgroup_pool_v2 = true\n")
        f.write(f"memory_high_action_cgroup_pool = {200 * 1024 * 1024}\n")  # 200 MiB
        f.write("enable_suspension = true\n")
        f.write("memory_pressure_threshold_percent = 1\n")

    await buck.build(
        "prelude//:freeze_unfreeze_target",
        "--no-remote-cache",
        "-c",
        "build.use_limited_hybrid=False",
        "-c",
        "build.execution_platforms=//:platforms",
        "--local-only",
        *_use_some_memory_args(buck),
    )

    event = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "ResourceControlEvents",
    )

    assert len(list(filter(lambda e: e["kind"] == "Scheduled", event))) > 0
    assert len(list(filter(lambda e: e["kind"] != "Scheduled", event))) > 0


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK2_HARD_ERROR", "panic")
async def test_local_action_running_count(
    buck: Buck,
) -> None:
    await buck.build(
        ":sleep_merge",
        "--no-remote-cache",
        "--local-only",
    )

    local_action_running_count = await filter_events(
        buck, "Event", "data", "Instant", "data", "LocalActionRunningCount"
    )

    assert len(local_action_running_count) > 0


# get the daemon cgroup path
def get_daemon_cgroup_path(pid: int) -> Path:
    cgroup_path = f"/proc/{pid}/cgroup"

    with open(cgroup_path, "r") as f:
        for line in f:
            # cgroup v2 format: 0::/path/to/cgroup
            if line.startswith("0::"):
                cgroup_relative_path = line.strip().split("::", 1)[1]
                return Path(f"/sys/fs/cgroup{cgroup_relative_path}")

    raise Exception(f"Could not find cgroup v2 entry for PID {pid}")


async def get_daemon_pid(buck: Buck) -> int:
    result = await buck.status()
    stdout = result.stdout
    status = json.loads(stdout)
    pid = status["process_info"]["pid"]
    return pid


# This tests set the memory.high for the buck2 cgroup slice (contains daemon, forkserver and workers cgroups) to 5GB
# The value here is only for testing the unset and restore functionality during freezing.
# The freezing is coming from the memory pressure, limited by memory_high_action_cgroup_pool here
@buck_test(skip_for_os=["darwin", "windows"])
async def test_parent_slice_memory_high_unset_and_restore(
    buck: Buck,
) -> None:
    memory_high_total = 5 * 1024 * 1024 * 1024  # 5 GB
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write(f"memory_high_action_cgroup_pool = {200 * 1024 * 1024}\n")  # 200 MiB
        f.write("enable_suspension = true\n")
        f.write("memory_pressure_threshold_percent = 1\n")
        f.write(f"memory_high = {memory_high_total}\n")

    # start buck2 daemon
    await buck.server()

    pid = await get_daemon_pid(buck)
    daemon_cgroup_path = get_daemon_cgroup_path(pid)
    # the cgroup that contains daemon, forkserver and workers cgroups
    slice_cgroup_path = daemon_cgroup_path.parent

    # We don't set memory.high for the daemon cgroup
    with open(daemon_cgroup_path / "memory.high", "r") as f:
        daemon_memory_high = f.read().strip()
    assert daemon_memory_high == "max"

    with open(slice_cgroup_path / "memory.high", "r") as f:
        slice_memory_high = f.read().strip()
    if slice_memory_high == "max":
        slice_memory_high = memory_high_total
    else:
        slice_memory_high = int(slice_memory_high)
    assert slice_memory_high == memory_high_total

    # Variables to store the memory.high values
    delayed_memory_high = None

    def read_memory_high_after_delay(slice_cgroup_path: Path) -> None:
        """Function to read memory.high after a 10-second delay"""
        nonlocal delayed_memory_high
        time.sleep(10)
        try:
            # One of the action is frozen at this point in the thread,
            # so that the memory.high value should be set to max
            with open(slice_cgroup_path / "memory.high", "r") as f:
                delayed_memory_high = f.read().strip()
        except Exception as e:
            print(f"Error reading memory.high after delay: {e}")

    # Start the thread to read memory.high after 10 seconds
    memory_reader_thread = threading.Thread(
        target=read_memory_high_after_delay, args=(slice_cgroup_path,)
    )
    memory_reader_thread.start()

    await buck.build(
        "prelude//:freeze_unfreeze_target",
        "--no-remote-cache",
        "-c",
        "build.use_limited_hybrid=False",
        "-c",
        "build.execution_platforms=//:platforms",
        "--local-only",
        *_use_some_memory_args(buck),
    )

    commands = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
        "commands",
    )

    # There are three commands, only one was suspended:
    suspended_commands = [
        c[0] for c in commands if c[0]["details"]["metadata"]["suspend_count"]
    ]
    assert len(suspended_commands) == 1
    for command in suspended_commands:
        assert command["details"]["metadata"]["suspend_duration"] is not None

    memory_reader_thread.join()
    assert (delayed_memory_high) == "max"
    # The memory.high value should be restored to the original value
    with open(slice_cgroup_path / "memory.high", "r") as f:
        slice_memory_high_at_end = f.read().strip()
        if slice_memory_high_at_end == "max":
            slice_memory_high_at_end = memory_high_total
        else:
            slice_memory_high_at_end = int(slice_memory_high_at_end)

    assert slice_memory_high_at_end == memory_high_total


@buck_test(skip_for_os=["darwin", "windows"])
async def test_percentage_of_ancestor_memory_limit(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write("memory_high = 50%\n")

    # start buck2 daemon
    await buck.server()

    pid = await get_daemon_pid(buck)
    daemon_cgroup_path = get_daemon_cgroup_path(pid)
    # the parent of the cgroup that contains daemon, forkserver and workers cgroups
    parent_cgroup_path = daemon_cgroup_path.parent.parent.parent

    try:
        parent_cgroup_memory_high = 200 * 1024 * 1024 * 1024  # 10 GB
        with open(parent_cgroup_path / "memory.high", "w") as f:
            f.write(str(parent_cgroup_memory_high))

        # restart buck2 daemon to make the parent memory.high value effective
        await buck.kill()
        await buck.server()

        pid = await get_daemon_pid(buck)
        daemon_cgroup_path = get_daemon_cgroup_path(pid)
        # the cgroup that contains daemon, forkserver and workers cgroups
        slice_cgroup_path = daemon_cgroup_path.parent.parent
        with open(slice_cgroup_path / "memory.high", "r") as f:
            slice_memory_high = int(f.read().strip())
        assert slice_memory_high == (parent_cgroup_memory_high * 0.5)
    finally:
        # reset the parent memory.high value to max
        with open(parent_cgroup_path / "memory.high", "w") as f:
            f.write("max")


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK_LOG", "buck2_resource_control::action_cgroups=trace")
async def test_reading_ancestor_cgroup_constraints(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write("status = required\n")
        f.write("enable_suspension = true\n")

    # start buck2 daemon
    await buck.server()

    pid = await get_daemon_pid(buck)
    daemon_cgroup_path = get_daemon_cgroup_path(pid)
    # the cgroup that contains all the isolation dirs buck2 cgroup slices (buck2.slice)
    buck2_slice_cgroup_path: Path = daemon_cgroup_path.parent.parent.parent

    memory_high = 200 * 1024 * 1024 * 1024  # 200 GB
    memory_max = 300 * 1024 * 1024 * 1024  # 300 GB
    memory_swap_max = 301 * 1024 * 1024 * 1024  # 301 GB
    memory_swap_high = 201 * 1024 * 1024 * 1024  # 201 GB

    def set_buck_slice_cgroup_memory(memory_file_name: str, val: int | str) -> None:
        with open(buck2_slice_cgroup_path / memory_file_name, "w") as f:
            f.write(str(val))

    try:
        set_buck_slice_cgroup_memory("memory.high", memory_high)
        set_buck_slice_cgroup_memory("memory.max", memory_max)
        set_buck_slice_cgroup_memory("memory.swap.max", memory_swap_max)
        set_buck_slice_cgroup_memory("memory.swap.high", memory_swap_high)

        # restart buck2 daemon to make the parent memory values value effective
        await buck.kill()
        await buck.server()

        deamon_std = await buck.daemon_stderr()

        # Parse the log to extract AncestorCgroupConstraints values
        constraints_found = False
        for line in deamon_std.splitlines():
            if "AncestorCgroupConstraints" in line:
                constraints_found = True
                # Extract memory values using regex
                memory_max_match = re.search(r"memory_max: Some\((\d+)\)", line)
                memory_high_match = re.search(r"memory_high: Some\((\d+)\)", line)
                memory_swap_max_match = re.search(
                    r"memory_swap_max: Some\((\d+)\)", line
                )
                memory_swap_high_match = re.search(
                    r"memory_swap_high: Some\((\d+)\)", line
                )

                assert memory_max_match, f"memory_max not found in line: {line}"
                extracted_memory_max = int(memory_max_match.group(1))
                assert (
                    extracted_memory_max == memory_max
                ), f"Expected memory_max {memory_max}, got {extracted_memory_max}"

                assert memory_high_match, f"memory_high not found in line: {line}"
                extracted_memory_high = int(memory_high_match.group(1))
                assert (
                    extracted_memory_high == memory_high
                ), f"Expected memory_high {memory_high}, got {extracted_memory_high}"

                assert (
                    memory_swap_max_match
                ), f"memory_swap_max not found in line: {line}"
                extracted_memory_swap_max = int(memory_swap_max_match.group(1))
                assert (
                    extracted_memory_swap_max == memory_swap_max
                ), f"Expected memory_swap_max {memory_swap_max}, got {extracted_memory_swap_max}"

                assert (
                    memory_swap_high_match
                ), f"memory_swap_high not found in line: {line}"
                extracted_memory_swap_high = int(memory_swap_high_match.group(1))
                assert (
                    extracted_memory_swap_high == memory_swap_high
                ), f"Expected memory_swap_high {memory_swap_high}, got {extracted_memory_swap_high}"

                break

        assert (
            constraints_found
        ), "AncestorCgroupConstraints log not found in daemon stderr"
    finally:
        # reset the parent memory values to max
        set_buck_slice_cgroup_memory("memory.high", "max")
        set_buck_slice_cgroup_memory("memory.max", "max")
        set_buck_slice_cgroup_memory("memory.swap.max", "max")
        set_buck_slice_cgroup_memory("memory.swap.high", "max")


@buck_test(skip_for_os=["darwin", "windows"])
async def test_daemon_id_in_cgroup_path(buck: Buck) -> None:
    await buck.server()

    result = await buck.status()
    status = json.loads(result.stdout)
    daemon_id = status["daemon_constraints"]["daemon_id"]

    daemon_cgroup_path = get_daemon_cgroup_path(status["process_info"]["pid"])

    assert daemon_id.replace("-", "_") in str(daemon_cgroup_path)
