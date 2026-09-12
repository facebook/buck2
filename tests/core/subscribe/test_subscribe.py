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

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
from buck2.tests.e2e_util.buck_workspace import buck_test, env


# Length-prefixed protobuf frame for:
# `SubscriptionRequest { subscribe_to_active_commands: SubscribeToActiveCommands {} }`.
# This test intentionally uses the raw frame to verify stdin requests keep the
# daemon alive without going through the CLI's `--active-commands` helper. The
# wire shape is stable enough for this test: the subscription API is part of
# Buck2's client/daemon protocol, and the existing field number for
# `subscribe_to_active_commands` must remain backward-compatible.
SUBSCRIBE_TO_ACTIVE_COMMANDS_REQUEST = b"\x02\x22\x00"


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
@env("BUCK2_TESTING_INACTIVITY_TIMEOUT", "true")
async def test_requests_keep_daemon_alive(buck: Buck) -> None:
    async with await buck.subscribe() as subscribe:
        subscribe.stdin.write(SUBSCRIBE_TO_ACTIVE_COMMANDS_REQUEST)
        await subscribe.stdin.drain()
        await subscribe.read_message()

        pid = json.loads((await buck.status()).stdout)["process_info"]["pid"]

        for _ in range(3):
            await asyncio.sleep(0.6)
            subscribe.stdin.write(SUBSCRIBE_TO_ACTIVE_COMMANDS_REQUEST)
            await subscribe.stdin.drain()
            await subscribe.read_message()

        status = json.loads((await buck.status()).stdout)
        assert status["process_info"]["pid"] == pid
        assert subscribe._process.returncode is None


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
