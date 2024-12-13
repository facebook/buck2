# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from typing import Any, Dict, Optional

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events


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
    commands_per_action = await filter_events(
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


def _get(data: Dict[str, Any], *key: str) -> Optional[Dict[str, Any]]:
    for k in key:
        data = data.get(k)
        if data is None:
            return None

    return data
