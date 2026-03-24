# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

# TODO(scottcao): Fix this by making buck2 with stale working directory kill itself


import asyncio
import json
import os
import platform
import subprocess
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, eden_remove


def assert_process_alive(pid: int) -> None:
    if platform.system() == "Windows":
        result = subprocess.run(
            ["tasklist", "/FI", f"PID eq {pid}", "/NH"],
            capture_output=True,
            text=True,
        )
        assert str(pid) in result.stdout, f"Process {pid} is not alive"
    else:
        os.kill(pid, 0)


@buck_test(setup_eden=True, skip_final_kill=True)
async def test_daemon_survives_checkout_removal(buck: Buck) -> None:
    # Start the daemon
    await buck.server()

    status = json.loads((await buck.status()).stdout)
    pid = status["process_info"]["pid"]

    assert_process_alive(pid)

    project_dir = Path(buck.cwd)
    eden_dir = project_dir.parent / "eden"
    eden_remove(eden_dir, project_dir, buck._env)

    assert not project_dir.exists(), f"Eden checkout was not removed: {project_dir}"

    # Pause for some time to show that Buck did not react to checkout removal.
    # We use 65 seconds for now since Buck checks for stale buck-out every 60 seconds.
    await asyncio.sleep(65)
    assert_process_alive(pid)
