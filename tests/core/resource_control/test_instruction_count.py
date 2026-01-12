# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
from typing import Any, Dict, List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events, random_string


def helper_bin_flags() -> List[str]:
    return [
        "-c",
        f"three_billion_instructions.path={os.environ['THREE_BILLION_INSTRUCTIONS_BIN']}",
    ]


@buck_test(skip_for_os=["windows", "darwin"])
async def test_instruction_count_disabled(buck: Buck) -> None:
    await buck.build(
        "root//:three_billion_instructions",
        "-c",
        "buck2.miniperf2=false",
        "--no-remote-cache",
        "--local-only",
        "-c",
        f"test.cache_buster={random_string()}",
        *helper_bin_flags(),
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
            assert c["details"]["metadata"].get("execution_stats") is None


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
        if action["name"]["category"] == "three_billion_instructions":
            return action["commands"][-1]["details"]

    raise AssertionError("did not find the expected target")


@buck_test(skip_for_os=["windows", "darwin"])
async def test_instruction_count_enabled(buck: Buck) -> None:
    await buck.build(
        "root//:three_billion_instructions",
        "-c",
        "buck2.miniperf2=true",
        "--no-remote-cache",
        "--local-only",
        "-c",
        f"test.cache_buster={random_string()}",
        *helper_bin_flags(),
    )

    details = await get_matching_details(buck)
    assert "OmittedLocalCommand" in details["command_kind"]["command"]

    # Check that we are within 20%
    instruction_count = details["metadata"]["execution_stats"]["cpu_instructions_user"]
    # FIXME(JakobDegen): Are we really expecting downward variation? Why? Leave a comment
    assert instruction_count > 2700000000
    assert instruction_count < 3300000000


@buck_test(skip_for_os=["windows", "darwin"])
async def test_instruction_count_remote(buck: Buck) -> None:
    await buck.build(
        "root//:three_billion_instructions",
        "--no-remote-cache",
        "--write-to-cache-anyway",
        "--prefer-remote",
        *helper_bin_flags(),
    )

    details = await get_matching_details(buck)
    assert not details["command_kind"]["command"]["RemoteCommand"]["cache_hit"]

    # Check that we are within 10%
    instruction_count = details["metadata"]["execution_stats"]["cpu_instructions_user"]
    assert instruction_count > 2850000000
    assert instruction_count < 3150000000

    # Check we also get it on a cache hit.

    await buck.kill()
    await buck.build(
        "root//:three_billion_instructions",
        "--prefer-remote",
        *helper_bin_flags(),
    )

    details = await get_matching_details(buck)
    assert details["command_kind"]["command"]["RemoteCommand"]["cache_hit"]

    # Check that we are within 10%
    instruction_count = details["metadata"]["execution_stats"]["cpu_instructions_user"]
    assert instruction_count > 2850000000
    assert instruction_count < 3150000000


@buck_test()
def test_instruction_count_nop(buck: Buck) -> None:
    # Pytest gets upset if we have no windows or mac tests in this file
    pass
