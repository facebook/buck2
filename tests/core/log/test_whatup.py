# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_whatup_command(buck: Buck) -> None:
    await buck.build("//:long_build")

    log = (await buck.log("show")).stdout.strip()
    log_file = tempfile.NamedTemporaryFile(
        suffix=".json-lines", mode="w+", delete=False
    )
    # Truncate log when analysis started
    with log_file as f:
        lines = log.splitlines()
        for line in lines:
            f.write(line + "\n")
            if "AnalysisStage" in line:
                break
        f.close()

    ext = await buck.log("whatup", log_file.name)
    assert "running analysis" in ext.stderr


@buck_test()
async def test_whatup_after_command(buck: Buck) -> None:
    await buck.build("//:long_build", "--local-only", "--no-remote-cache")

    # Get event log
    log = (await buck.log("show")).stdout.strip()
    elapsed = [0, 0]

    # Get first timestamp
    lines = log.splitlines()
    first_event = json.loads(lines[1])
    first_timestamp = first_event["Event"]["timestamp"]
    # Get timestamp where rule execution starts
    for line in lines:
        if "Execute" in line:
            event = json.loads(line)
            # Calculate elapsed seconds, we add 1 to give some padding in order to catch the open span
            elapsed[0] = (event["Event"]["timestamp"][0] - first_timestamp[0]) + 1
            # Calculate elapsed millliseconds
            elapsed[1] = (
                event["Event"]["timestamp"][1] - first_timestamp[1]
            ) // 1000000
            break

    # Verify rule execution appears when running whatup at that timestamp
    action_start = (elapsed[0] * 1000) + abs(elapsed[1])
    ext = (await buck.log("whatup", "--after", str(action_start))).stderr.strip()
    assert "action (run_python)" in ext
