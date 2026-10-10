# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import asyncio
import json
import time
import typing

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_stats_are_reported(buck: Buck) -> None:
    snapshot = await start_daemon_and_get_snapshot(buck)
    assert snapshot["standalone_ttl_refresh"]["passes"] == 0


@buck_test()
async def test_a_pass_runs(buck: Buck) -> None:
    # A local-only build gives no digest an expiration, so the pass has nothing to ask the CAS
    # about; what is under test is that the loop sweeps, scans and reports on schedule.
    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("\n[buck2]\nttl_refresh_frequency_seconds = 1\n")
    await start_daemon_and_get_snapshot(buck)

    deadline = time.time() + 60
    while True:
        stats = (await start_daemon_and_get_snapshot(buck))["standalone_ttl_refresh"]
        if stats["passes"] > 0:
            break
        assert time.time() < deadline, f"no refresh pass completed, last stats: {stats}"
        await asyncio.sleep(1)

    assert stats["errors"] == 0
    assert stats["last_pass_candidates"] == 0


async def start_daemon_and_get_snapshot(buck: Buck) -> dict[str, typing.Any]:
    await buck.targets(":")
    status = json.loads((await buck.status("--snapshot")).stdout)
    return status["snapshot"]
