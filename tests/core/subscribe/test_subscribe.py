# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import os

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_subscribe(buck: Buck) -> None:
    path = (await buck.targets("//:stage1", "--show-output")).stdout.strip().split()[1]

    # Buck2 wants normalized paths here.
    path = path.replace("\\", "/")

    expect = os.environ["BUCK2_EXPECT"]
    args = [
        "--buck2",
        buck.path_to_executable,
        path,
    ]

    if buck.isolation_prefix is not None:
        args.extend(
            [
                "--isolation-dir",
                buck.isolation_prefix,
            ]
        )

    proc = await asyncio.create_subprocess_exec(
        expect,
        *args,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        cwd=buck.cwd,
        env=buck._env,
    )

    await buck.build("//:stage2")

    # We don't expect this to actually take anywhere near 20 seconds, but on CI
    # on a busy host this could take a while.
    (stdout, stderr) = await asyncio.wait_for(proc.communicate(), timeout=20)
    assert proc.returncode == 0
    assert stdout.strip().decode("utf-8") == path


@buck_test()
async def test_active_commands(buck: Buck) -> None:
    async with await buck.subscribe("--active-commands") as subscribe:
        msg = await subscribe.read_message()
        commands = msg["response"]["ActiveCommandsSnapshot"]["active_commands"]
        assert len(commands) == 1
        assert "subscribe" in commands[0]["argv"]


@buck_test()
async def test_disconnect_eof(buck: Buck) -> None:
    async with await buck.subscribe() as subscribe:
        subscribe.stdin.close()
        msg = await subscribe.read_message()
        assert "EOF" in msg["response"]["Goodbye"]["reason"]


@buck_test()
async def test_disconnect_error(buck: Buck) -> None:
    with pytest.raises(BuckException):
        async with await buck.subscribe() as subscribe:
            subscribe.stdin.write(b"x")
            subscribe.stdin.close()
            msg = await subscribe.read_message()
            assert "Error parsing request" in msg["response"]["Goodbye"]["reason"]


@buck_test()
async def test_unknown_request_error(buck: Buck) -> None:
    with pytest.raises(BuckException):
        async with await buck.subscribe() as subscribe:
            subscribe.stdin.write(b"\x00")  # Would decode to a None request
