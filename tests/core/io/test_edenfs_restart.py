# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, eden_restart


# Graceful takeover is not supported on Windows.
# The test repo's .buckconfig opts into `buck2.detect_eden_restart`, which defaults to
# off while the fail-fast behavior is rolled out.
@buck_test(setup_eden=True, skip_for_os=["windows"])
async def test_eden_graceful_restart_fails_commands_fast(buck: Buck) -> None:
    # Warm the daemon; this captures the Eden daemon identity at I/O provider creation.
    await buck.targets(":")

    # Restart detection is wired through the Eden I/O provider, so make sure it is in use.
    status = json.loads((await buck.status()).stdout)
    assert status["io_provider"] == "eden", status["io_provider"]

    eden_dir = Path(buck.cwd).parent / "eden"
    eden_restart(eden_dir, buck._env)

    # The mount survived the takeover, but the buck2 daemon's cached identity no longer
    # matches: the next command must fail fast instead of hanging on stale handles.
    await expect_failure(
        buck.targets(":"),
        stderr_regex="EdenFS restarted \\(pid \\d+ -> \\d+, start unixtime .+\\)",
    )
