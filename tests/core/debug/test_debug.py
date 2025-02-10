# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os.path
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_debug_crash(buck: Buck) -> None:
    # If the first operation immediately does a panic then we fail to connect.
    # While that's not great, having some panics is better than none, so test once after we spawn.
    await buck.build()
    result = await buck.debug("crash", "panic")
    assert "explicitly requested panic" in result.stderr
    # Our crash output should include a stack trace.
    assert "stack backtrace:" in result.stderr


@buck_test()
async def test_debug_exe(buck: Buck) -> None:
    result = await buck.debug("exe")
    path = result.stdout.strip()
    assert os.path.exists(path)


@buck_test()
async def test_debug_allocative(buck: Buck, tmp_path: Path) -> None:
    # Start the server.
    await buck.uquery("root//:")

    file_path = tmp_path / "profile"

    output = await buck.debug("allocative", "--output", str(file_path))
    assert os.path.exists(f"{file_path}/flame.src")
    assert os.path.exists(f"{file_path}/flame.svg")
    assert "Allocative profile written to" in output.stderr

    await buck.debug("allocative")
    assert os.path.exists(buck.cwd / "allocative-out" / "flame.src")
    assert os.path.exists(buck.cwd / "allocative-out" / "flame.svg")


@buck_test()
async def test_debug_filestatus(buck: Buck) -> None:
    # Start the server.
    await buck.uquery("root//:")
    # FIXME(JakobDegen): `.` is an error
    output = await buck.debug("file-status", "TARGETS.fixture")
    assert "No mismatches detected" in output.stderr


@buck_test(skip_for_os=["windows", "darwin"])
async def test_thread_dump(buck: Buck) -> None:
    # Make sure we don't start a daemon if there isn't one
    await expect_failure(
        buck.debug("thread-dump"),
        stderr_regex="No running buck daemon",
    )
    # Start the daemon
    await buck.uquery("root//:")
    output = await buck.debug("thread-dump")
    assert "frame #0" in output.stdout
