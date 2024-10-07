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
async def test_emit_console_preferences(buck: Buck) -> None:
    await buck.build("-c", "ui.thread_line_limit=30")

    max_lines = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "ConsolePreferences",
        "max_lines",
    )

    assert max_lines
    assert max_lines[0] == 30
