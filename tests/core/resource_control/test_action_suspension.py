# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from __future__ import annotations

import os

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import filter_events


# To not fail listing on Mac or Windows
def test_dummy() -> None:
    pass


def _configure(buck: Buck, kill_and_retry: bool, pressure_limit: int) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2_resource_control]\n")
        f.write(f"memory_pressure_threshold_percent = {pressure_limit}\n")
        if kill_and_retry:
            f.write("preferred_freeze_strategy = kill_and_retry\n")


def _use_some_memory_args(buck: Buck) -> list[str]:
    return [
        "-c",
        f"use_some_memory.path={os.environ["USE_SOME_MEMORY_BIN"]}",
        "--no-remote-cache",
        "--local-only",
    ]


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK2_HARD_ERROR", "panic")
@pytest.mark.parametrize("kill_and_retry", [True, False])
async def test_action_freezing(
    buck: Buck,
    kill_and_retry: bool,
) -> None:
    _configure(buck, kill_and_retry, 0)
    await buck.build(
        ":sleep_10",
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

    frozen_count = 0
    for command in commands:
        if command[0]["details"]["metadata"]["was_frozen"] is True:
            frozen_count += 1
            assert command[0]["details"]["metadata"]["freeze_duration"] is not None

    # Check that at least one action was frozen (and the command didn't block indefinitely)
    assert frozen_count > 0

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


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK2_HARD_ERROR", "panic")
@pytest.mark.parametrize("kill_and_retry", [True, False])
async def test_action_suspend_stress_test(
    buck: Buck,
    kill_and_retry: bool,
) -> None:
    _configure(buck, kill_and_retry, 0)

    # Stress test that nothing breaks with fast running actions (faster than memory tracker ticks)
    await buck.build(
        ":very_fast_100",
        *_use_some_memory_args(buck),
    )

    _configure(buck, kill_and_retry, 1)
    # And check again without memory pressure
    await buck.build(
        ":very_fast_100",
        *_use_some_memory_args(buck),
    )


@buck_test(skip_for_os=["darwin", "windows"])
@pytest.mark.parametrize("kill_and_retry", [True, False])
async def test_suspend_one_of_two(
    buck: Buck,
    kill_and_retry: bool,
) -> None:
    _configure(buck, kill_and_retry, 1)

    await buck.build(
        ":two_mutually_incompatible",
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

    suspend_count = 0
    not_suspend_count = 0
    for command in commands:
        if command[0]["details"]["metadata"]["was_frozen"] is True:
            suspend_count += 1
            assert command[0]["details"]["metadata"]["freeze_duration"] is not None
        else:
            not_suspend_count += 1

    # Check that we froze only one of the two actions we ran
    assert suspend_count == 1
    assert not_suspend_count == 1
