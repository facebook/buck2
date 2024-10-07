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


@buck_test()
async def test_build_system_info(buck: Buck) -> None:
    await buck.build(
        "//:test",
    )

    system_info = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "SystemInfo",
    )
    assert len(system_info) == 1
    assert system_info[0]["system_total_memory_bytes"] > 0
    assert system_info[0]["total_disk_space_bytes"] > 0
