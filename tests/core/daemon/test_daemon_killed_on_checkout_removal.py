# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import csv
import json
import platform
import re
import subprocess
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, eden_remove, env
from buck2.tests.e2e_util.helper.golden import golden, sanitize_daemon_stderr


def _get_process_name(pid: int) -> str | None:
    system = platform.system()
    if system == "Windows":
        result = subprocess.run(
            ["tasklist", "/FI", f"PID eq {pid}", "/FO", "CSV", "/NH"],
            capture_output=True,
            text=True,
        )
        for row in csv.reader(result.stdout.splitlines()):
            if len(row) >= 2 and row[1] == str(pid):
                return row[0]
        return None

    if system == "Darwin":
        command = ["ps", "-o", "comm=", str(pid)]
    elif system == "Linux":
        command = ["ps", "-o", "cmd=", str(pid)]
    else:
        raise Exception(f"Unknown platform: {system}")

    result = subprocess.run(command, capture_output=True, text=True)
    return result.stdout.strip() or None


@buck_test(setup_eden=True, skip_final_kill=True)
@env("BUCK2_TESTING_CHECKER_INTERVAL_SECONDS", "1")
async def test_daemon_killed_on_checkout_removal(buck: Buck) -> None:
    # Start the daemon and capture its PID and daemon dir before removal.
    await buck.server()

    status = json.loads((await buck.status()).stdout)
    pid = status["process_info"]["pid"]
    process_name = _get_process_name(pid)
    assert process_name is not None, f"Could not find daemon process {pid}"
    daemon_dir = await buck.get_daemon_dir()

    project_dir = Path(buck.cwd)
    eden_dir = project_dir.parent / "eden"
    eden_remove(eden_dir, project_dir, buck._env)

    assert not project_dir.exists(), f"Eden checkout was not removed: {project_dir}"

    # Wait for the daemon to detect the missing project root and shut down.
    await asyncio.sleep(20)
    if _get_process_name(pid) == process_name:
        raise AssertionError("Server did not die in 20 seconds")

    # Process is dead. Verify the shutdown reason in daemon stderr.
    stderr = (daemon_dir / "buckd.stderr").read_text()
    # Replace the project root path before general sanitization.
    # sanitize_daemon_stderr only handles /data/users/ (Linux);
    # macOS scratch paths (e.g. /var/folders/…) need explicit replacement.
    stderr = stderr.replace(str(project_dir), "<SCRATCH_PATH>")
    stderr = sanitize_daemon_stderr(stderr)
    # Strip daemon_listener startup log lines — they can change between
    # versions and aren't what this test validates.
    stderr = re.sub(r"^.*daemon_listener:.*\n", "", stderr, flags=re.MULTILINE)

    if platform.system() == "Windows":
        # Windows produces a different OS error (e.g. "The system cannot
        # find the path specified" instead of "No such file or directory"),
        # so skip the golden comparison and only check the key message so
        # that we don't have to update the golden file on 2 OSes every time
        # the error message changes.
        assert "is no longer accessible" in stderr
    else:
        golden(
            output=stderr,
            rel_path="fixtures/test_daemon_killed_on_checkout_removal.golden.txt",
        )
