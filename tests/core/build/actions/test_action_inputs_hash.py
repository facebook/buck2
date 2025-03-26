# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

from buck2.tests.e2e_util.helper.utils import filter_events


async def get_last_action_inputs_hash(buck: Buck) -> str:
    events = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
    )

    # We do it this way, because otherwise filter_events will filter out
    # the action_inputs_hash when it is None.
    return events[-1]["action_inputs_hash"]


@buck_test()
async def test_hash_not_calculated(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2]\n")
        f.write("  compute_action_inputs_hash_enabled = false\n")

    await buck.build("//:simple_write", "--target-platforms=//:default1")
    assert await get_last_action_inputs_hash(buck) is None


@buck_test()
async def test_simple_write_action_inputs_hash(buck: Buck) -> None:
    await buck.build("//:simple_write", "--target-platforms=//:default1")
    action_inputs_hash1 = await get_last_action_inputs_hash(buck)
    assert action_inputs_hash1 is not None

    await buck.build("//:simple_write", "--target-platforms=//:default2")
    action_inputs_hash2 = await get_last_action_inputs_hash(buck)
    assert action_inputs_hash1 == action_inputs_hash2

    await buck.build("//:simple_write", "--target-platforms=//:platform1")
    action_inputs_hash3 = await get_last_action_inputs_hash(buck)
    assert action_inputs_hash3 != action_inputs_hash2

    await buck.build("//:simple_write", "--target-platforms=//:platform2")
    action_inputs_hash4 = await get_last_action_inputs_hash(buck)

    assert action_inputs_hash4 != action_inputs_hash2
    assert action_inputs_hash4 != action_inputs_hash3
