# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events, timestamp_ms


# To not fail listing on Mac or Windows
def test_dummy() -> None:
    pass


@buck_test(skip_for_os=["darwin", "windows"])
async def test_no_local_action_when_full_hybrid_given_memory_pressure(
    buck: Buck,
) -> None:
    # Also configs for `buck2_resource_control` section are passed vis `.buckconfig`
    await buck.build(
        ":plate",
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
async def test_local_actions_throttled_when_limited_hybrid_given_memory_pressure(
    buck: Buck,
) -> None:
    # Also configs for `buck2_resource_control` section are passed vis `.buckconfig`
    await buck.build(
        ":plate",
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


# pyre-ignore[3]: Return type must be specified as type other than `Any`
def _get(data: Dict[str, Any], *key: str) -> Any:
    for k in key:
        data = data.get(k)
        if data is None:
            return None

    return data
