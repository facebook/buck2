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
from pathlib import Path

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
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
        "--show-full-simple-output",
        "-c",
        f"use_some_memory.path={os.environ["USE_SOME_MEMORY_BIN"]}",
        "--no-remote-cache",
        "--local-only",
    ]


async def _check_suspends(buck: Buck, kill_and_retry: bool, res: BuckResult) -> int:
    # First check the reported suspensions
    actions = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
    )
    reported_suspends = {}
    num_suspended_actions = 0
    for action in actions:
        if action["name"]["category"] != "memory_allocating_actions":
            continue
        ident = action["name"]["identifier"]
        command_meta = action["commands"][-1]["details"]["metadata"]
        if command_meta["was_frozen"] is True:
            num_suspended_actions += 1
            # Json representation of a duration is number of us
            duration = command_meta["freeze_duration"] / 1000
            assert duration is not None
            reported_suspends[ident] = duration
        else:
            reported_suspends[ident] = None

    paths = Path(res.stdout.strip()).read_text().splitlines()
    paths = [buck.cwd / p for p in paths]
    assert len(paths) == len(reported_suspends)

    if not kill_and_retry:
        # Then compare them to the detected suspensions
        for p in paths:
            contents = p.read_text()
            ident = p.name
            total_duration = None
            for line in contents.splitlines():
                if line.startswith("freeze_detected_ms "):
                    this_duration = int(line[len("freeze_detected_ms ") :])
                    total_duration = (total_duration or 0) + this_duration
            if total_duration is not None:
                assert abs(reported_suspends[ident] - total_duration) < 500
            else:
                # FIXME(JakobDegen): We should assert here, but we can't because in some cases
                # we suspend actions immediately after starting them, meaning they can't detect
                # it.
                # assert (reported_suspends[ident] or 0) < 500
                pass
    else:
        # TODO(JakobDegen): Detect kill and retry
        pass

    return num_suspended_actions


@buck_test(skip_for_os=["darwin", "windows"])
@env("BUCK2_HARD_ERROR", "panic")
@pytest.mark.parametrize("kill_and_retry", [True, False])
async def test_action_suspend(
    buck: Buck,
    kill_and_retry: bool,
) -> None:
    _configure(buck, kill_and_retry, 0)
    res = await buck.build_without_report(
        ":sleep_10",
        *_use_some_memory_args(buck),
    )

    num_suspends = await _check_suspends(buck, kill_and_retry, res)
    assert num_suspends > 0

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

    res = await buck.build_without_report(
        ":two_mutually_incompatible",
        *_use_some_memory_args(buck),
    )

    num_suspends = await _check_suspends(buck, kill_and_retry, res)
    assert num_suspends == 1
