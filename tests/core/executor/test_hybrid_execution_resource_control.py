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
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

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


def configure_freezing_with_pressure(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write("enable_action_cgroup_pool = true\n")
        f.write("enable_action_freezing = true\n")
        f.write("memory_pressure_threshold_percent = 0\n")


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

    pressure_starts = await filter_events(
        buck,
        "Event",
        "data",
        "SpanStart",
        "data",
        "MemoryPressure",
    )
    pressure_ends = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "MemoryPressure",
    )
    assert len(pressure_starts) == 1
    assert len(pressure_ends) == 1


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


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK2_HARD_ERROR", "panic")
async def test_action_freezing(
    buck: Buck,
) -> None:
    configure_freezing_with_pressure(buck)
    await buck.build(
        ":sleep_merge",
        "--no-remote-cache",
        "-c",
        "build.use_limited_hybrid=True",
        "-c",
        "build.execution_platforms=//:platforms",
        "-c",
        "test.prefer_local=True",
        "--local-only",
    )

    result = await buck.log("show")
    frozen_count = 0
    for line in result.stdout.splitlines():
        json_object = json.loads(line)
        action_end: Optional[Dict[str, Any]] = _get(
            json_object, "Event", "data", "SpanEnd", "data", "ActionExecution"
        )
        if action_end is not None:
            action_commands = _get(action_end, "commands")
            assert len(action_commands) == 1

            metadata = _get(action_commands[0], "details", "metadata")
            was_frozen = metadata["was_frozen"]
            if was_frozen:
                frozen_count += 1

    # Check that at least one action was frozen (and the command didn't block indefinitely)
    assert frozen_count > 0


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK2_HARD_ERROR", "panic")
async def test_action_freezing_stress_test(
    buck: Buck,
) -> None:
    configure_freezing_with_pressure(buck)

    # Stress test that nothing breaks with fast running actions (faster than memory tracker ticks)
    await buck.build(
        ":merge_100",
        "--no-remote-cache",
        "-c",
        "build.use_limited_hybrid=True",
        "-c",
        "build.execution_platforms=//:platforms",
        "-c",
        "test.prefer_local=True",
        "--local-only",
    )


@buck_test(skip_for_os=["darwin", "windows"])
async def test_memory_pressure_telemetry(
    buck: Buck,
) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write("status = required\n")
        f.write("enable_action_cgroup_pool = true\n")
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
