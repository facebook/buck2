# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
from typing import Any, Dict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events, random_string


def _use_some_memory_args(buck: Buck) -> list[str]:
    return [
        "root//:use_some_memory",
        "--no-remote-cache",
        "--local-only",
        "-c",
        f"test.cache_buster={random_string()}",
        "-c",
        f"use_some_memory.path={os.environ['USE_SOME_MEMORY_BIN']}",
    ]


@buck_test(skip_for_os=["windows", "darwin"])
async def test_memory_reporting_disabled(buck: Buck) -> None:
    env = {"BUCK2_TEST_RESOURCE_CONTROL_CONFIG": '{"status":"Off"}'}

    await buck.build(
        *_use_some_memory_args(buck),
        env=env,
    )

    events = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
        "commands",
    )
    for commands in events:
        for c in commands:
            assert c["details"]["metadata"]["execution_stats"]["memory_peak"] is None


async def get_matching_details(buck: Buck) -> Dict[str, Any]:
    events = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
    )
    for action in events:
        if action["name"]["category"] == "use_some_memory":
            return action["commands"][-1]["details"]

    raise AssertionError("did not find the expected target")


@buck_test(skip_for_os=["windows", "darwin"])
async def test_memory_reporting(buck: Buck) -> None:
    await buck.build(
        *_use_some_memory_args(buck),
    )

    details = await get_matching_details(buck)
    assert "OmittedLocalCommand" in details["command_kind"]["command"]

    memory_peak = details["metadata"]["execution_stats"]["memory_peak"]
    assert memory_peak > 10000000
    assert memory_peak < 15000000


@buck_test(skip_for_os=["windows", "darwin"])
async def test_memory_reporting_in_test(buck: Buck) -> None:
    await buck.test(
        *_use_some_memory_args(buck),
    )

    events = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "TestEnd",
    )
    assert len(events) == 1
    details = events[0]["command_report"]["details"]

    memory_peak = details["metadata"]["execution_stats"]["memory_peak"]
    assert memory_peak > 10000000
    assert memory_peak < 15000000


@buck_test()
def test_nop(buck: Buck) -> None:
    # Pytest gets upset if we have no windows or mac tests in this file
    pass
